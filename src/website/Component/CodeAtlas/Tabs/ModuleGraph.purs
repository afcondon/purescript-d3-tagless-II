module PSD3.CodeAtlas.Tabs.ModuleGraph where

import Prelude

import Data.Nullable (Nullable)

import Control.Monad.State (class MonadState)
import Control.Monad.State.Class (get, modify_)
import Data.Lens (use)
import Effect.Uncurried (mkEffectFn3)
import PSD3.Internal.Simulation.Types (_handle)
import Data.Array (filter, mapMaybe, length, group)
import Data.Array as Array
import Data.Foldable (maximum, traverse_)
import Data.Int as Data.Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (null, toNullable)
import Data.Number (ceil, cos, log, pow, sin, sqrt)
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen as H
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, on, setAttributes, simpleJoin)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2, addTickFunction, init, start, update)
import PSD3.Internal.Simulation.Functions (simulationSetVariable)
import PSD3.CodeAtlas.Types (ModuleGraphData, ModuleInfo)
import PSD3.Data.Node (D3Link_Unswizzled, D3Link_Swizzled, D3_SimulationNode(..), D3_VxyFxy, D3_XY)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, text, transform', viewBox, width, x, x1, x2, y, y1, y2)
import Type.Row (type (+))
import PSD3.Internal.FFI (clearHighlights_, getNodes_, highlightConnectedNodes_, keyIsID_, simdragHorizontal_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute(..))
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), SimVariable(..), Step(..), allNodes)
import PSD3.Internal.Types (D3Selection_, D3Simulation_, Datum_, Element(..), Index_, MouseEvent(..), Selector)
import PSD3.Internal.Simulation.Types as SimTypes
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import Utility (getWindowWidthHeight)
import Unsafe.Coerce (unsafeCoerce)

-- FFI for setting node positions and restarting simulation
foreign import setNodePosition_ :: forall d. D3_SimulationNode d -> Nullable Number -> Nullable Number -> D3_SimulationNode d
foreign import restartSimulationWithCurrentAlpha_ :: D3Simulation_ -> D3Simulation_
foreign import unpinAllNodesCompletely_ :: D3Simulation_ -> D3Simulation_
foreign import transitionNodesToGrid_ :: forall node. Array node -> Effect Unit -> Effect Unit

-- | Module node data type (using row composition)
type ModuleNodeData row = ( id :: String, name :: String, loc :: Int, path :: String | row )
type ModuleNode = D3_SimulationNode (ModuleNodeData + D3_XY + D3_VxyFxy + ())
type ModuleNodeRecord = Record (ModuleNodeData + D3_XY + D3_VxyFxy + ())

-- | Module link types
type ModuleLinkData = ( id :: String )
type ModuleLinkSwizzled = { source :: ModuleNodeRecord, target :: ModuleNodeRecord | ModuleLinkData }

-- | Unsafe unboxing functions (needed for attribute callbacks from FFI)
unboxModuleNode :: Datum_ -> ModuleNodeRecord
unboxModuleNode datum =
  let (D3SimNode d) = unsafeCoerce datum
  in d

unboxModuleLink :: Datum_ -> ModuleLinkSwizzled
unboxModuleLink datum = unsafeCoerce datum

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
-- lib modules = green shades, website modules = red shades, etc
nodeColor :: String -> String
nodeColor path
  | String.take 4 path == "src/" =
      if String.contains (String.Pattern "src/lib/PSD3") path then "#2E7D32"  -- dark green for lib
      else if String.contains (String.Pattern "src/lib/") path then "#4CAF50"  -- medium green for other lib
      else if String.contains (String.Pattern "src/website/") path then "#D32F2F"  -- red for website
      else "#1976D2"  -- blue for other src
  | otherwise = "#757575"  -- gray for packages

-- | Accessor functions for dependency link data
link_ = {
    source: _.source <<< unboxModuleLink
  , target: _.target <<< unboxModuleLink
}

-- | Compute topological layers for modules based on dependency depth
-- | Layer 0 = no dependencies within source modules
-- | Layer N = depends on modules in layers 0..N-1
computeLayers :: Array ModuleInfo -> Map String Int
computeLayers modules =
  let
    -- Create set of all module names for quick lookup
    moduleNames = Set.fromFoldable $ modules <#> _.name

    -- Filter dependencies to only include source modules
    sourceDeps :: Map String (Set String)
    sourceDeps = Map.fromFoldable $ modules <#> \m ->
      Tuple m.name (Set.fromFoldable $ filter (\dep -> Set.member dep moduleNames) m.depends)

    -- Recursively compute depth
    computeDepth :: String -> Map String Int -> Int
    computeDepth name depths =
      case Map.lookup name depths of
        Just d -> d  -- Already computed
        Nothing ->
          case Map.lookup name sourceDeps of
            Nothing -> 0  -- No dependencies (shouldn't happen)
            Just deps ->
              if Set.isEmpty deps
                then 0  -- No dependencies
                else
                  -- Max depth of dependencies + 1
                  let depDepths = Array.fromFoldable deps <#> \dep -> computeDepth dep depths
                  in case maximum depDepths of
                       Just maxDepth -> maxDepth + 1
                       Nothing -> 0

    -- Compute all depths (this is inefficient but simple)
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
    layerSpacing = 150.0  -- vertical space between layers
    -- Calculate max layer for centering
    layerValues = Map.values layers # Array.fromFoldable
    maxLayer = fromMaybe 0 $ maximum layerValues
    -- Vertical offset to center the graph (move it down so it's centered at y=0)
    verticalOffset = toNumber maxLayer * layerSpacing * 0.5
    toNumber = Data.Int.toNumber
  in
    Array.mapWithIndex (\i m ->
      let
        layer = fromMaybe 0 $ Map.lookup m.name layers
        -- Pin y-coordinate based on layer (fy = fixed y)
        -- Start from verticalOffset, go down by layer * spacing
        yPos = verticalOffset - (toNumber layer * layerSpacing)
        -- Use spiral for x-coordinate within layer
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
          , fx: null  -- x is free to move
          , fy: toNullable (Just yPos)  -- y is pinned to layer
          }
    ) modules

-- | Grid layout helper: convert index to grid coordinates
numberToGridPoint :: Int -> Int -> { x :: Number, y :: Number }
numberToGridPoint columns i =
  let
    col = Data.Int.toNumber (i `mod` columns)
    row = Data.Int.toNumber (i / columns)
  in { x: col, y: row }

-- | Grid layout helper: offset coordinates
offsetXY :: { x :: Number, y :: Number } -> { x :: Number, y :: Number } -> { x :: Number, y :: Number }
offsetXY offset xy = { x: xy.x + offset.x, y: xy.y + offset.y }

-- | Grid layout helper: scale coordinates
scalePoint :: Number -> Number -> { x :: Number, y :: Number } -> { x :: Number, y :: Number }
scalePoint xFactor yFactor xy = { x: xy.x * xFactor, y: xy.y * yFactor }

-- | Calculate grid positions for modules
-- | Returns modules with updated x, y, fx, fy for grid layout
modulesToGridLayout :: Array ModuleNode -> Array ModuleNode
modulesToGridLayout nodes =
  let
    moduleCount = length nodes
    -- Calculate square grid dimensions
    columns = Data.Int.floor $ ceil $ sqrt $ Data.Int.toNumber moduleCount
    -- Center the grid
    offset = -((Data.Int.toNumber columns) / 2.0)

    -- Update each module with grid position
    updateNode :: Int -> ModuleNode -> ModuleNode
    updateNode i (D3SimNode node) =
      let
        gridXY = scalePoint 150.0 150.0 $ offsetXY { x: offset, y: offset } $ numberToGridPoint columns i
      in
        D3SimNode (node {
          x = gridXY.x,
          y = gridXY.y,
          fx = toNullable (Just gridXY.x),  -- Pin to grid position
          fy = toNullable (Just gridXY.y)
        })
  in
    Array.mapWithIndex updateNode nodes

-- | Transition nodes to grid layout with smooth animation
-- | Call this to toggle between force layout and grid layout
-- | Note: This is a placeholder - full implementation will require access to node selections
transitionToGridLayout :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SimulationM2 D3Selection_ m =>
  Boolean -> m Unit
transitionToGridLayout toGrid = do
  simHandle <- use SimTypes._handle
  let currentNodes = getNodes_ simHandle :: Array ModuleNode

  if toGrid
    then do
      -- Transition TO grid layout
      liftEffect $ Console.log "Transitioning TO grid layout"
      liftEffect $ Console.log $ "Applying grid layout to " <> show (length currentNodes) <> " nodes"

      -- Calculate grid positions
      let gridNodes = modulesToGridLayout currentNodes

      -- Stop simulation so tick doesn't fight the transition
      simulationSetVariable $ Alpha 0.0

      -- Start D3 transition with callback to pin nodes when done
      liftEffect $ transitionNodesToGrid_ gridNodes do
        Console.log "Transition complete, pinning nodes"
        -- Set fx/fy on all nodes to lock them in grid positions
        traverse_ identity $ Array.zipWith updateNodePosition currentNodes gridNodes
        -- Restart simulation with low alpha
        let _ = restartSimulationWithCurrentAlpha_ simHandle
        pure unit

      pure unit
    else do
      -- Transition FROM grid layout to force layout
      liftEffect $ Console.log "Transitioning FROM grid to force layout"

      -- Unpin all nodes (both fx and fy) so they can move freely
      _ <- liftEffect $ pure $ unpinAllNodesCompletely_ simHandle

      -- Reheat the simulation with full alpha for energetic return to force layout
      simulationSetVariable $ Alpha 1.0
      simulationSetVariable $ AlphaTarget 0.0
      _ <- liftEffect $ pure $ restartSimulationWithCurrentAlpha_ simHandle

      pure unit
  where
    -- Helper to update a node's position from source to target
    updateNodePosition :: ModuleNode -> ModuleNode -> Effect Unit
    updateNodePosition target (D3SimNode source) = do
      -- Set fx and fy to pin the node to grid position (mutates in place)
      let _ = setNodePosition_ target source.fx source.fy
      pure unit

-- | Convert module dependencies to simulation links
modulesToLinks :: Array ModuleInfo -> Array D3Link_Unswizzled
modulesToLinks modules =
  let sourceModuleNames = Set.fromFoldable $ modules <#> _.name
      packLink :: { id :: String, source :: String, target :: String } -> D3Link_Unswizzled
      packLink = unsafeCoerce
  in Array.concat $ modules <#> \m ->
       -- Only create links to dependencies that are also in our source module set
       mapMaybe (\dep ->
         if Set.member dep sourceModuleNames
           then Just $ packLink { id: m.name <> "->" <> dep, source: m.name, target: dep }
           else Nothing
       ) m.depends

-- | Draw the module graph visualization
drawModuleGraph :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  ModuleGraphData -> String -> m Unit
drawModuleGraph graphData selector = do
  -- Filter to source modules only
  let sourceModules = filter (\m -> String.take 4 m.path == "src/") graphData.modules
      nodes = modulesToNodes sourceModules
      links = modulesToLinks sourceModules

      -- Build adjacency map for highlighting connected nodes
      -- Maps node ID to set of connected node IDs (both incoming and outgoing)
      adjacencyMap :: Map.Map String (Set.Set String)
      adjacencyMap =
        let unpackLink :: D3Link_Unswizzled -> { source :: String, target :: String }
            unpackLink = unsafeCoerce
            addEdge acc link =
              let linkRec = unpackLink link
                  sourceSet = fromMaybe Set.empty $ Map.lookup linkRec.source acc
                  targetSet = fromMaybe Set.empty $ Map.lookup linkRec.target acc
              in Map.insert linkRec.source (Set.insert linkRec.target sourceSet)
                   $ Map.insert linkRec.target (Set.insert linkRec.source targetSet) acc
        in Array.foldl addEdge Map.empty links

  -- Debug: log LOC and layer values
  let layers = computeLayers sourceModules
      layerValues = Map.values layers # Array.fromFoldable
      maxLayer = fromMaybe 0 $ maximum layerValues
  liftEffect $ Console.log $ "Sample module LOCs: " <> show (map _.loc $ Array.take 5 sourceModules)
  liftEffect $ Console.log $ "Total source modules: " <> show (Array.length sourceModules)
  liftEffect $ Console.log $ "Max layer: " <> show maxLayer
  liftEffect $ Console.log $ "Sample layers: " <> show (Array.take 10 layerValues)

  (Tuple w h) <- liftEffect getWindowWidthHeight
  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "module-graph" ]
  -- Create a group to be the zoom target
  zoomGroup <- appendTo svg Group [ classed "zoom-group" ]
  linksGroup <- appendTo zoomGroup Group [ classed "link", strokeColor "#999", strokeOpacity 0.4 ]
  nodesGroup <- appendTo zoomGroup Group [ classed "node", strokeColor "#fff", strokeWidth 1.5 ]

  -- Define forces
  -- Note: collision force needs dynamic radius based on LOC
  let collisionRadius :: Datum_ -> Index_ -> Number
      collisionRadius datum idx = nodeRadius (_.loc (unsafeCoerce datum))
      forces =
        [ createForce "manyBody" (RegularForce ForceManyBody) allNodes [ F.strength (-150.0), F.theta 0.9, F.distanceMin 1.0 ]
        , createForce "collision" (RegularForce ForceCollide) allNodes [ F.radius collisionRadius ]
        , createForce "center" (RegularForce ForceCenter) allNodes [ F.x 0.0, F.y 0.0, F.strength 0.3 ]
        , createLinkForce Nothing [ F.distance 100.0 ]
        ]
      activeForces = Set.fromFoldable [ "manyBody", "collision", "center", "links" ]

  -- Initialize simulation
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes
    , links
    , forces
    , activeForces
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: keyIsID_
    , ticks: Map.fromFoldable []
    }

  -- Join data to DOM
  -- Create a group for each node
  nodeGroups <- simpleJoin nodesGroup Group nodesInSim keyIsID_
  setAttributes nodeGroups [ classed "node-group" ]

  -- Append circle to each group
  circles <- appendTo nodeGroups Circle
    [ radius (nodeRadius <<< datum_.loc)
    , fill (nodeColor <<< datum_.path)
    , classed "node-circle"
    ]

  -- Append text label to each group
  labels <- appendTo nodeGroups Text
    [ text datum_.name
    , classed "node-label"
    ]

  -- Add hover handlers for highlighting connected nodes
  let onMouseEnter = mkEffectFn3 \event datum this -> do
        let nodeId = datum_.id datum
            connected = fromMaybe Set.empty $ Map.lookup nodeId adjacencyMap
            connectedIds = Set.toUnfoldable connected :: Array String
            -- Include the hovered node itself in the highlight
            allHighlighted = Array.cons nodeId connectedIds
        pure $ highlightConnectedNodes_ zoomGroup allHighlighted

      onMouseLeave = mkEffectFn3 \event datum this -> do
        pure $ clearHighlights_ zoomGroup

  setAttributes nodeGroups
    [ OnT' MouseEnter onMouseEnter
    , OnT' MouseLeave onMouseLeave
    ]

  linksSelection <- simpleJoin linksGroup Line linksInSim keyIsID_
  setAttributes linksSelection [ strokeWidth 1.5, strokeColor "#999" ]

  -- Add tick functions - position the groups using transform
  let translateNode :: Datum_ -> String
      translateNode d = "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")"
  addTickFunction "nodes" $ Step nodeGroups [ transform' translateNode ]
  addTickFunction "links" $ Step linksSelection
    [ x1 (_.x <<< link_.source)
    , y1 (_.y <<< link_.source)
    , x2 (_.x <<< link_.target)
    , y2 (_.y <<< link_.target)
    ]

  -- Add interactions
  _ <- nodeGroups `on` Drag (CustomDrag "moduleGraph" simdragHorizontal_)
  _ <- svg `on` Zoom
    { extent: ZoomExtent { top: 0.0, left: 0.0, bottom: h, right: w }
    , scale: ScaleExtent 0.1 4.0
    , name: "ModuleGraph"
    , target: zoomGroup
    }

  -- Add unpin button
  simHandle <- use _handle
  let unpinHandler = mkEffectFn3 \event datum this -> do
        let _ = unpinAllNodesCompletely_ simHandle
        pure unit

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
  unpinText <- appendTo unpinButton Text
    [ x (-w / 2.0 + 50.0)
    , y (-h / 2.0 + 25.0)
    , text "Unpin All"
    , fill "#fff"
    , classed "unpin-button-text"
    ]
  setAttributes unpinButton [ OnT' MouseClick unpinHandler ]

  -- Start the simulation
  start
  pure unit
