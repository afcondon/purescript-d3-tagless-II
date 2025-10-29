module PSD3.CodeAtlas.Tabs.ExpandableBubbles where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad (when)
import Data.Array (filter)
import Data.Array as Array
import Data.Foldable (maximum, traverse_)
import Data.Int as Data.Int
import Data.Lens (use)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldl)
import Data.Nullable (null, toNullable)
import Data.Number (cos, log, sqrt)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn3)
import Halogen as H
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, mergeSelections, on, openSelection, selectUnder, setAttributes, updateJoin)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2, addTickFunction, init, start, update)
import PSD3.CodeAtlas.Types (DeclarationsData, FunctionCallsData, ModuleGraphData, ModuleInfo)
import PSD3.Data.Node (D3Link_Unswizzled, D3Link_Swizzled, D3_SimulationNode(..), D3_VxyFxy, D3_XY)
import PSD3.Internal.Attributes.Sugar (classed, fill, onMouseEventEffectful, radius, remove, strokeColor, strokeOpacity, strokeWidth, text, transform', viewBox, width, height, x, x1, x2, y, y1, y2)
import PSD3.Internal.FFI (addModuleArrowMarker_, clearHighlights_, drawInterModuleDeclarationLinks_, filterToConnectedNodes_, highlightConnectedNodes_, keyIsID_, simdragHorizontal_, unpinAllNodes_, unsafeSetField_, updateBubbleRadii_, updateNodeExpansion_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute(..))
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (D3SimulationState_, ForceType(..), RegularForceType(..), Step(..), allNodes, _handle)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, MouseEvent(..))
import PSD3.Internal.Attributes.Instances (AttributeSetter(..), Attr(..), AttrBuilder(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- | Module node data - extended to track expansion state
type BubbleNodeData row =
  ( id :: String
  , name :: String
  , loc :: Int
  , path :: String
  , expanded :: Boolean  -- New: track if this module is expanded
  | row
  )

type BubbleNode = D3_SimulationNode (BubbleNodeData + D3_XY + D3_VxyFxy + ())
type BubbleNodeRecord = Record (BubbleNodeData + D3_XY + D3_VxyFxy + ())

-- | Unsafe unboxing functions
unboxBubbleNode :: Datum_ -> BubbleNodeRecord
unboxBubbleNode datum =
  let (D3SimNode d) = unsafeCoerce datum
  in d

-- | Accessor functions for bubble node data
datum_ = {
    id: _.id <<< unboxBubbleNode
  , x: _.x <<< unboxBubbleNode
  , y: _.y <<< unboxBubbleNode
  , name: _.name <<< unboxBubbleNode
  , loc: _.loc <<< unboxBubbleNode
  , path: _.path <<< unboxBubbleNode
  , expanded: _.expanded <<< unboxBubbleNode
}

-- | Compute node radius from LOC
-- | When expanded, we'll multiply this by a factor
nodeRadius :: Boolean -> Int -> Number
nodeRadius expanded loc =
  let baseRadius = (sqrt (Data.Int.toNumber loc)) * 0.3 + 5.0
  in if expanded
     then baseRadius * 4.0  -- Expanded nodes are 4x larger to show labeled declarations
     else baseRadius

-- | Compute color from module path
nodeColor :: String -> String
nodeColor path
  | String.take 4 path == "src/" =
      if String.contains (Pattern "src/lib/PSD3") path then "#2E7D32"
      else if String.contains (Pattern "src/lib/") path then "#4CAF50"
      else if String.contains (Pattern "src/website/") path then "#D32F2F"
      else "#1976D2"
  | otherwise = "#757575"

-- | Convert module graph data to bubble nodes (all start collapsed)
modulesToBubbleNodes :: Array ModuleInfo -> Array BubbleNode
modulesToBubbleNodes modules =
  Array.mapWithIndex (\i m ->
    let toNumber = Data.Int.toNumber
        theta = toNumber i * 0.5
        r = 50.0 + toNumber i * 2.0
    in
      D3SimNode
        { id: m.name
        , name: m.name
        , loc: m.loc
        , path: m.path
        , expanded: false  -- All start collapsed
        , x: r * cos theta
        , y: 0.0
        , vx: 0.0
        , vy: 0.0
        , fx: null
        , fy: null
        }
  ) modules

-- | Convert module dependencies to simulation links
modulesToLinks :: Array ModuleInfo -> Array D3Link_Unswizzled
modulesToLinks modules =
  let sourceModuleNames = Set.fromFoldable $ modules <#> _.name
      unpackLink :: D3Link_Unswizzled -> { id :: String, source :: String, target :: String }
      unpackLink = unsafeCoerce
      packLink :: { id :: String, source :: String, target :: String } -> D3Link_Unswizzled
      packLink = unsafeCoerce
  in Array.concat $ modules <#> \m ->
       filter (\link -> Set.member (unpackLink link).target sourceModuleNames)
         (m.depends <#> \dep -> packLink { id: m.name <> "->" <> dep, source: m.name, target: dep })

-- | Accessor functions for dependency links
unboxLink :: forall r. Datum_ -> { source :: BubbleNodeRecord, target :: BubbleNodeRecord | r }
unboxLink = unsafeCoerce

link_ = {
    source: _.source <<< unboxLink
  , target: _.target <<< unboxLink
}

-- | Build adjacency map for connected nodes lookup
buildAdjacencyMap :: Array D3Link_Unswizzled -> Map String (Set String)
buildAdjacencyMap links =
  let unpackLink :: D3Link_Unswizzled -> { source :: String, target :: String }
      unpackLink = unsafeCoerce
      addEdge acc link =
        let linkRec = unpackLink link
            sourceSet = fromMaybe Set.empty $ Map.lookup linkRec.source acc
            targetSet = fromMaybe Set.empty $ Map.lookup linkRec.target acc
        in acc
           # Map.insert linkRec.source (Set.insert linkRec.target sourceSet)
           # Map.insert linkRec.target (Set.insert linkRec.source targetSet)
  in foldl addEdge Map.empty links

-- | Initialize the expandable bubbles graph
initialize :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  ModuleGraphData ->
  DeclarationsData ->
  m { svg :: D3Selection_
    , zoomGroup :: D3Selection_
    , nodesGroup :: D3Selection_
    , linksGroup :: D3Selection_
    , bubbleNodes :: Array BubbleNode
    , bubbleLinks :: Array D3Link_Unswizzled
    , declarationsData :: DeclarationsData
    , adjacencyMap :: Map String (Set String)
    }
initialize graphData declsData = do
  let sourceModules = filter (\m -> String.take 4 m.path == "src/") graphData.modules
      bubbleNodes = modulesToBubbleNodes sourceModules
      bubbleLinks = modulesToLinks sourceModules
      adjacencyMap = buildAdjacencyMap bubbleLinks

  liftEffect $ Console.log $ "Expandable Bubbles initialized with " <> show (Array.length sourceModules) <> " modules"

  -- Log LOC values and computed radii for debugging
  liftEffect $ Array.take 5 sourceModules # traverse_ \m -> do
    let r = nodeRadius false m.loc
    Console.log $ m.name <> " - LOC: " <> show m.loc <> ", radius: " <> show r

  (Tuple w h) <- liftEffect getWindowWidthHeight
  root <- attach "div.svg-container"
  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "bubble-graph" ]

  -- Add arrowhead marker definition for module links via FFI
  liftEffect $ addModuleArrowMarker_ svg

  zoomGroup <- appendTo svg Group [ classed "zoom-group" ]
  linksGroup <- appendTo zoomGroup Group [ classed "link", strokeColor "#999", strokeOpacity 0.4 ]
  nodesGroup <- appendTo zoomGroup Group [ classed "node", strokeColor "#fff", strokeWidth 1.5 ]

  -- Define forces (collision will need to be dynamic based on expansion)
  let collisionRadius :: Datum_ -> Index_ -> Number
      collisionRadius datum _ =
        let node = unsafeCoerce datum :: BubbleNodeRecord
        in nodeRadius node.expanded node.loc

      forces =
        [ createForce "manyBody" (RegularForce ForceManyBody) allNodes [ F.strength (-150.0), F.theta 0.9, F.distanceMin 1.0 ]
        , createForce "collision" (RegularForce ForceCollide) allNodes [ F.radius collisionRadius ]
        , createForce "center" (RegularForce ForceCenter) allNodes [ F.x 0.0, F.y 0.0, F.strength 0.3 ]
        , createLinkForce Nothing [ F.distance 100.0 ]
        ]
      activeForces = Set.fromFoldable [ "manyBody", "collision", "center", "links" ]

  -- Initialize simulation
  _ <- init
    { nodes: bubbleNodes
    , links: bubbleLinks
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
    , name: "BubbleGraph"
    , target: zoomGroup
    }

  pure { svg, zoomGroup, nodesGroup, linksGroup, bubbleNodes, bubbleLinks, declarationsData: declsData, adjacencyMap }

-- | Update the graph with new data (including expansion states)
updateGraph :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodesGroup :: D3Selection_
  , linksGroup :: D3Selection_
  , nodes :: Array BubbleNode
  , links :: Array D3Link_Unswizzled
  } ->
  m Unit
updateGraph { nodesGroup, linksGroup, nodes, links } = do
  -- Update simulation data
  enhanced <- update
    { nodes: Just nodes
    , links: Just links
    , nodeFilter: Nothing
    , linkFilter: Nothing
    , activeForces: Nothing
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Open selections
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- Apply General Update Pattern to nodes
  node' <- updateJoin node Group enhanced.nodes keyIsID_

  -- Enter: create new groups with circles
  nodeEnter <- appendTo node'.enter Group [ classed "node-group" ]
  _ <- appendTo nodeEnter Circle
    [ radius (\d -> nodeRadius (datum_.expanded d) (datum_.loc d))
    , fill (nodeColor <<< datum_.path)
    , classed "node-circle"
    ]
  _ <- appendTo nodeEnter Text
    [ text datum_.name
    , classed "node-label"
    ]

  -- Exit: remove old nodes
  setAttributes node'.exit [ remove ]

  -- Update: modify existing nodes
  setAttributes node'.update [ classed "node-group" ]
  updateCircles <- selectUnder node'.update (show Circle)
  setAttributes updateCircles
    [ radius (\d -> nodeRadius (datum_.expanded d) (datum_.loc d))
    , fill (nodeColor <<< datum_.path)
    ]
  updateLabels <- selectUnder node'.update (show Text)
  setAttributes updateLabels [ text datum_.name ]

  -- Merge enter and update selections
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- Add drag behavior
  _ <- mergedNodes `on` Drag (CustomDrag "bubbleGraph" simdragHorizontal_)

  -- Apply General Update Pattern to links
  link' <- updateJoin link Line enhanced.links keyIsID_

  -- Enter: create new lines with arrowheads
  linkEnter <- appendTo link'.enter Line
    [ strokeWidth 1.5
    , strokeColor "#999"
    , AttrT (AttributeSetter "marker-end" (StringAttr (Static "url(#module-arrow)")))
    ]

  -- Exit: remove old links
  setAttributes link'.exit [ remove ]

  -- Update: modify existing links
  setAttributes link'.update
    [ strokeWidth 1.5
    , strokeColor "#999"
    , AttrT (AttributeSetter "marker-end" (StringAttr (Static "url(#module-arrow)")))
    ]

  -- Merge enter and update selections
  mergedLinks <- mergeSelections linkEnter link'.update

  -- Set up tick functions for animation
  let translateNode :: Datum_ -> String
      translateNode d = "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")"
  addTickFunction "nodes" $ Step mergedNodes [ transform' translateNode ]
  addTickFunction "links" $ Step mergedLinks
    [ x1 (_.x <<< link_.source)
    , y1 (_.y <<< link_.source)
    , x2 (_.x <<< link_.target)
    , y2 (_.y <<< link_.target)
    ]

-- | Main entry point: draw the expandable bubbles graph
drawExpandableBubbles :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  ModuleGraphData ->
  DeclarationsData ->
  FunctionCallsData ->
  String ->
  m Unit
drawExpandableBubbles graphData declsData callsData selector = do
  -- Initialize the graph
  { svg, zoomGroup, nodesGroup, linksGroup, bubbleNodes, bubbleLinks, declarationsData, adjacencyMap } <- initialize graphData declsData

  -- Get simulation handle for reheating
  simHandle <- use _handle

  -- Track whether we've already filtered to a subgraph
  hasFilteredRef <- liftEffect $ Ref.new false

  -- Click handler: first click prunes + expands, subsequent clicks just expand/collapse
  let onClick _ datum _ = do
        let clickedId = datum_.id datum
            clickedNode = unboxBubbleNode datum

        hasFiltered <- Ref.read hasFilteredRef

        -- Only filter on the first click
        when (not hasFiltered) do
          let connected = fromMaybe Set.empty $ Map.lookup clickedId adjacencyMap
              connectedIds = Set.toUnfoldable connected :: Array String
              allConnected = Array.cons clickedId connectedIds

          Console.log $ "First click: filtering to " <> show (Array.length allConnected) <> " connected modules"
          liftEffect $ pure $ filterToConnectedNodes_ simHandle keyIsID_ allConnected
          Ref.write true hasFilteredRef

        -- Toggle the expanded field directly on the node (it's a mutable JS object)
        let newExpanded = not clickedNode.expanded
            _ = unsafeSetField_ "expanded" newExpanded datum

        Console.log $ clickedId <> if newExpanded then " expanded" else " collapsed"

        -- Expand/collapse with internal structure
        let _ = updateNodeExpansion_ simHandle nodeRadius declarationsData callsData datum

        -- Update inter-module declaration links
        drawInterModuleDeclarationLinks_ zoomGroup nodeRadius declarationsData callsData

  -- Render initial state with all nodes collapsed
  updateGraph
    { nodesGroup
    , linksGroup
    , nodes: bubbleNodes
    , links: bubbleLinks
    }

  -- Add click handlers to node groups
  initialNodes <- openSelection nodesGroup (show Group)
  setAttributes initialNodes [ onMouseEventEffectful MouseClick onClick ]

  -- Start the simulation
  start
  pure unit
