module PSD3.CodeAtlas.Tabs.InteractiveGraph where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Lens (use)
import Effect.Uncurried (mkEffectFn3)
import PSD3.Internal.Simulation.Types (_handle)
import Data.Array (filter)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int as Data.Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (null, toNullable)
import Data.Number (cos, log)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen as H
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, mergeSelections, on, openSelection, selectUnder, setAttributes, updateJoin)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2, addTickFunction, init, start, update)
import PSD3.CodeAtlas.Types (ModuleGraphData, ModuleInfo)
import PSD3.Data.Node (D3Link_Unswizzled, D3Link_Swizzled, D3_SimulationNode(..), D3_VxyFxy, D3_XY)
import PSD3.Internal.Attributes.Sugar (classed, fill, radius, remove, strokeColor, strokeOpacity, strokeWidth, text, transform', viewBox, width, height, x, x1, x2, y, y1, y2)
import Type.Row (type (+))
import PSD3.Internal.FFI (clearHighlights_, filterToConnectedNodes_, highlightConnectedNodes_, keyIsID_, simdragHorizontal_, unpinAllNodes_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute(..))
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (D3SimulationState_, ForceType(..), RegularForceType(..), Step(..), allNodes)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, MouseEvent(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import Utility (getWindowWidthHeight)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Ref as Ref
import Effect (Effect)

-- | Module node data type (using row composition)
type ModuleNodeData row = ( id :: String, name :: String, loc :: Int, path :: String | row )
type ModuleNode = D3_SimulationNode (ModuleNodeData + D3_XY + D3_VxyFxy + ())
type ModuleNodeRecord = Record (ModuleNodeData + D3_XY + D3_VxyFxy + ())

-- | Module link types
type ModuleLinkData = ( id :: String )

-- | Unsafe unboxing functions (needed for attribute callbacks from FFI)
unboxModuleNode :: Datum_ -> ModuleNodeRecord
unboxModuleNode datum =
  let (D3SimNode d) = unsafeCoerce datum
  in d

-- | Accessor functions for module node data
datum_ = {
    id: _.id <<< unboxModuleNode
  , x: _.x <<< unboxModuleNode
  , y: _.y <<< unboxModuleNode
  , name: _.name <<< unboxModuleNode
  , loc: _.loc <<< unboxModuleNode
  , path: _.path <<< unboxModuleNode
}

-- | Compute node radius from LOC: log(LOC) + 10
nodeRadius :: Int -> Number
nodeRadius loc = log (toNumber loc) + 10.0
  where toNumber = Data.Int.toNumber

-- | Compute color from module path
nodeColor :: String -> String
nodeColor path
  | String.take 4 path == "src/" =
      if String.contains (String.Pattern "src/lib/PSD3") path then "#2E7D32"
      else if String.contains (String.Pattern "src/lib/") path then "#4CAF50"
      else if String.contains (String.Pattern "src/website/") path then "#D32F2F"
      else "#1976D2"
  | otherwise = "#757575"

-- | Accessor functions for dependency link data (for swizzled links)
-- These extract node records from swizzled link data
unboxLink :: forall r. Datum_ -> { source :: ModuleNodeRecord, target :: ModuleNodeRecord | r }
unboxLink = unsafeCoerce

link_ = {
    source: _.source <<< unboxLink
  , target: _.target <<< unboxLink
}

-- | Compute topological layers for modules based on dependency depth
computeLayers :: Array ModuleInfo -> Map String Int
computeLayers modules =
  let
    moduleNames = Set.fromFoldable $ modules <#> _.name
    sourceDeps :: Map String (Set String)
    sourceDeps = Map.fromFoldable $ modules <#> \m ->
      Tuple m.name (Set.fromFoldable $ filter (\dep -> Set.member dep moduleNames) m.depends)

    computeDepth :: String -> Map String Int -> Int
    computeDepth name depths =
      case Map.lookup name depths of
        Just d -> d
        Nothing ->
          case Map.lookup name sourceDeps of
            Nothing -> 0
            Just deps ->
              if Set.isEmpty deps
                then 0
                else
                  let depDepths = Array.fromFoldable deps <#> \dep -> computeDepth dep depths
                  in case maximum depDepths of
                       Just maxDepth -> maxDepth + 1
                       Nothing -> 0

    go :: Map String Int -> Array String -> Map String Int
    go depths names =
      case Array.uncons names of
        Nothing -> depths
        Just { head: name, tail: rest } ->
          let depth = computeDepth name depths
              depths' = Map.insert name depth depths
          in go depths' rest
  in
    go Map.empty (modules <#> _.name)

-- | Convert module graph data to simulation nodes with topological layering
modulesToNodes :: Array ModuleInfo -> Array ModuleNode
modulesToNodes modules =
  let
    layers = computeLayers modules
    layerSpacing = 150.0
    layerValues = Map.values layers # Array.fromFoldable
    maxLayer = fromMaybe 0 $ maximum layerValues
    verticalOffset = Data.Int.toNumber maxLayer * layerSpacing * 0.5
    toNumber = Data.Int.toNumber
  in
    Array.mapWithIndex (\i m ->
      let
        layer = fromMaybe 0 $ Map.lookup m.name layers
        yPos = verticalOffset - (toNumber layer * layerSpacing)
        theta = toNumber i * 0.5
        r = 50.0 + toNumber i * 2.0
      in
        D3SimNode
          { id: m.name
          , name: m.name
          , loc: m.loc
          , path: m.path
          , x: r * cos theta
          , y: yPos
          , vx: 0.0
          , vy: 0.0
          , fx: null
          , fy: toNullable (Just yPos)
          }
    ) modules

-- | Convert module dependencies to simulation links (UNSWIZZLED: IDs only)
-- | These will be passed to the simulation which will swizzle them (replace IDs with node references)
modulesToLinks :: Array ModuleInfo -> Array D3Link_Unswizzled
modulesToLinks modules =
  let sourceModuleNames = Set.fromFoldable $ modules <#> _.name
      -- Helper to extract link record from opaque type
      unpackLink :: D3Link_Unswizzled -> { id :: String, source :: String, target :: String }
      unpackLink = unsafeCoerce
      -- Helper to construct opaque type from record
      packLink :: { id :: String, source :: String, target :: String } -> D3Link_Unswizzled
      packLink = unsafeCoerce
  in Array.concat $ modules <#> \m ->
       filter (\link -> Set.member (unpackLink link).target sourceModuleNames)
         (m.depends <#> \dep -> packLink { id: m.name <> "->" <> dep, source: m.name, target: dep })

-- | Build adjacency map for connected nodes lookup
-- | Works with UNSWIZZLED links (IDs only) since we're just mapping ID relationships
buildAdjacencyMap :: Array D3Link_Unswizzled -> Map String (Set String)
buildAdjacencyMap links =
  let unpackLink :: D3Link_Unswizzled -> { source :: String, target :: String }
      unpackLink = unsafeCoerce
      addEdge acc link =
        let linkRec = unpackLink link
            sourceSet = fromMaybe Set.empty $ Map.lookup linkRec.source acc
            targetSet = fromMaybe Set.empty $ Map.lookup linkRec.target acc
        in Map.insert linkRec.source (Set.insert linkRec.target sourceSet)
             $ Map.insert linkRec.target (Set.insert linkRec.source targetSet) acc
  in Array.foldl addEdge Map.empty links

-- | Initialize the interactive graph (called once)
initialize :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  ModuleGraphData ->
  m { svg :: D3Selection_
    , zoomGroup :: D3Selection_
    , nodesGroup :: D3Selection_
    , linksGroup :: D3Selection_
    , moduleNodes :: Array ModuleNode
    , moduleLinks :: Array D3Link_Unswizzled
    , adjacencyMap :: Map String (Set String)
    }
initialize graphData = do
  let sourceModules = filter (\m -> String.take 4 m.path == "src/") graphData.modules
      moduleNodes = modulesToNodes sourceModules
      moduleLinks = modulesToLinks sourceModules
      adjacencyMap = buildAdjacencyMap moduleLinks

  let layers = computeLayers sourceModules
      layerValues = Map.values layers # Array.fromFoldable
      maxLayer = fromMaybe 0 $ maximum layerValues
  liftEffect $ Console.log $ "Sample module LOCs: " <> show (map _.loc $ Array.take 5 sourceModules)
  liftEffect $ Console.log $ "Total source modules: " <> show (Array.length sourceModules)
  liftEffect $ Console.log $ "Max layer: " <> show maxLayer

  (Tuple w h) <- liftEffect getWindowWidthHeight
  root <- attach "div.svg-container"
  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "module-graph" ]
  zoomGroup <- appendTo svg Group [ classed "zoom-group" ]
  linksGroup <- appendTo zoomGroup Group [ classed "link", strokeColor "#999", strokeOpacity 0.4 ]
  nodesGroup <- appendTo zoomGroup Group [ classed "node", strokeColor "#fff", strokeWidth 1.5 ]

  -- Define forces
  let collisionRadius :: Datum_ -> Index_ -> Number
      collisionRadius datum _ = nodeRadius (_.loc (unsafeCoerce datum))
      forces =
        [ createForce "manyBody" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-150.0), F.thetaVal 0.9, F.distanceMinVal 1.0 ]
        , createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusFn collisionRadius ]
        , createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.3 ]
        , createLinkForce Nothing [ F.distanceVal 100.0 ]
        ]
      activeForces = Set.fromFoldable [ "manyBody", "collision", "center", "links" ]

  -- Initialize simulation
  _ <- init
    { nodes: moduleNodes
    , links: moduleLinks
    , forces
    , activeForces
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: keyIsID_
    , ticks: Map.fromFoldable []
    }

  -- Add zoom behavior
  _ <- svg `on` Zoom
    { extent: ZoomExtent { top: 0.0, left: 0.0, bottom: h, right: w }
    , scale: ScaleExtent 0.1 4.0
    , name: "ModuleGraph"
    , target: zoomGroup
    }

  -- Add unpin button
  simHandle <- use _handle
  let unpinHandler = mkEffectFn3 \_ _ _ -> pure $ unpinAllNodes_ simHandle
  unpinButton <- appendTo svg Group [ classed "unpin-button" ]
  _ <- appendTo unpinButton Rect
    [ x (-w / 2.0 + 10.0)
    , y (-h / 2.0 + 10.0)
    , width 80.0
    , height 30.0
    , fill "#f39c12"
    , radius 5.0
    , classed "unpin-button-bg"
    ]
  _ <- appendTo unpinButton Text
    [ x (-w / 2.0 + 50.0)
    , y (-h / 2.0 + 25.0)
    , text "Unpin All"
    , fill "#fff"
    , classed "unpin-button-text"
    ]
  setAttributes unpinButton [ OnT' MouseClick unpinHandler ]

  pure { svg, zoomGroup, nodesGroup, linksGroup, moduleNodes, moduleLinks, adjacencyMap }

-- | Update the graph with new filtered data (following Spago pattern)
updateGraph :: forall d row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ d | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodesGroup :: D3Selection_ d
  , linksGroup :: D3Selection_ d
  , zoomGroup :: D3Selection_ d
  , filteredNodes :: Array ModuleNode
  , filteredLinks :: Array D3Link_Unswizzled
  , adjacencyMap :: Map String (Set String)
  } ->
  m Unit
updateGraph { nodesGroup, linksGroup, zoomGroup, filteredNodes, filteredLinks, adjacencyMap } = do
  -- Step 1: Use update API to handle simulation data merging and link swizzling
  -- Data is already filtered, so we pass Nothing for the filter predicates
  enhanced <- update
    { nodes: Just filteredNodes
    , links: Just filteredLinks
    , nodeFilter: Nothing  -- Data already filtered
    , linkFilter: Nothing  -- Data already filtered
    , activeForces: Nothing
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Step 2: Open selections for DOM operations
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- Step 3: Apply General Update Pattern to nodes
  node' <- updateJoin node Group enhanced.nodes keyIsID_

  -- Enter: create new groups with circles and text
  nodeEnter <- appendTo node'.enter Group [ classed "node-group" ]
  _ <- appendTo nodeEnter Circle
    [ radius (nodeRadius <<< datum_.loc)
    , fill (nodeColor <<< datum_.path)
    , classed "node-circle"
    ]
  _ <- appendTo nodeEnter Text
    [ text datum_.name
    , classed "node-label"
    ]

  -- Exit: remove old nodes
  setAttributes node'.exit [ remove ]

  -- Update: modify existing nodes (update their circle and text children)
  setAttributes node'.update [ classed "node-group" ]
  updateCircles <- selectUnder node'.update (show Circle)
  setAttributes updateCircles
    [ radius (nodeRadius <<< datum_.loc)
    , fill (nodeColor <<< datum_.path)
    ]
  updateLabels <- selectUnder node'.update (show Text)
  setAttributes updateLabels [ text datum_.name ]

  -- Merge enter and update selections
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- Add hover handlers for highlighting
  let onMouseEnter = mkEffectFn3 \_ datum _ -> do
        let nodeId = datum_.id datum
            connected = fromMaybe Set.empty $ Map.lookup nodeId adjacencyMap
            connectedIds = Set.toUnfoldable connected :: Array String
            allHighlighted = Array.cons nodeId connectedIds
        pure $ highlightConnectedNodes_ zoomGroup allHighlighted

      onMouseLeave = mkEffectFn3 \_ _ _ -> do
        pure $ clearHighlights_ zoomGroup

  setAttributes mergedNodes
    [ OnT' MouseEnter onMouseEnter
    , OnT' MouseLeave onMouseLeave
    ]

  -- Add drag behavior
  _ <- mergedNodes `on` Drag (CustomDrag "moduleGraph" simdragHorizontal_)

  -- Step 4: Apply General Update Pattern to links
  link' <- updateJoin link Line enhanced.links keyIsID_

  -- Enter: create new lines with attributes
  linkEnter <- appendTo link'.enter Line [ strokeWidth 1.5, strokeColor "#999" ]

  -- Exit: remove old links
  setAttributes link'.exit [ remove ]

  -- Update: modify existing links
  setAttributes link'.update [ strokeWidth 1.5, strokeColor "#999" ]

  -- Merge enter and update selections
  mergedLinks <- mergeSelections linkEnter link'.update

  -- Step 5: Set up tick functions for animation
  let translateNode :: Datum_ -> String
      translateNode d = "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")"
  addTickFunction "nodes" $ Step mergedNodes [ transform' (unsafeCoerce translateNode) ]
  addTickFunction "links" $ Step mergedLinks
    [ x1 (_.x <<< link_.source)
    , y1 (_.y <<< link_.source)
    , x2 (_.x <<< link_.target)
    , y2 (_.y <<< link_.target)
    ]

-- | Main entry point: draw the interactive graph
drawInteractiveGraph :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  ModuleGraphData -> String -> m Unit
drawInteractiveGraph graphData selector = do
  -- Initialize the graph structure and get references
  { svg, zoomGroup, nodesGroup, linksGroup, moduleNodes, moduleLinks, adjacencyMap } <- initialize graphData

  -- NOTE: Event Handler Limitation
  -- =================================
  -- Event callbacks run in Effect, but updateGraph requires the full monadic context
  -- (SimulationM2). We can't call updateGraph from the click handler!
  --
  -- There are two solutions:
  --
  -- 1. EXPEDIENT (used here): Drop down to FFI and directly manipulate the simulation.
  --    - Pro: Simple, works immediately
  --    - Con: Bypasses the declarative update API, doesn't update PureScript state,
  --           doesn't use enter/update/exit pattern for smooth DOM transitions
  --
  -- 2. PROPER (see CodeExplorer): Use Halogen state management
  --    - Event callback raises Halogen action
  --    - Action updates component state (filter predicates)
  --    - Calls drawing function with filtered data
  --    - Drawing function uses update API + General Update Pattern
  --    - Pro: Fully declarative, smooth transitions, maintains PureScript state
  --    - Con: More complex, requires Halogen event listener infrastructure
  --
  -- For this exploratory Interactive tab, we use approach #1 to demonstrate the
  -- concept quickly. For production code, see D3.Viz.Spago.Draw for the proper pattern.

  -- Get simulation handle for FFI-based filtering (expedient approach)
  simHandle <- use _handle

  -- EXPEDIENT: Click handler uses FFI to filter simulation directly
  let onClick = mkEffectFn3 \_ datum _ -> do
        let nodeId = datum_.id datum
            connected = fromMaybe Set.empty $ Map.lookup nodeId adjacencyMap
            connectedIds = Set.toUnfoldable connected :: Array String
            allConnected = Array.cons nodeId connectedIds
        -- Directly manipulate simulation via FFI (bypasses update API)
        pure $ filterToConnectedNodes_ simHandle keyIsID_ allConnected

  -- Perform initial render with all nodes using the PROPER pattern
  -- (This demonstrates how updates SHOULD work when called from the monadic context)
  updateGraph
    { nodesGroup
    , linksGroup
    , zoomGroup
    , filteredNodes: moduleNodes
    , filteredLinks: moduleLinks
    , adjacencyMap
    }

  -- Add click handlers to the initial node selection
  -- NOTE: These handlers persist even when the simulation is filtered via FFI
  initialNodes <- openSelection nodesGroup (show Group)
  setAttributes initialNodes [ OnT' MouseClick onClick ]

  -- Start the simulation
  start
  pure unit
