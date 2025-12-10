-- | Simple Force Layout for Network Datasets
-- |
-- | A minimal force-directed graph visualization for exploring
-- | network datasets. Supports rich node/link attributes for
-- | visual encoding (size, color, opacity) and filtering.
module D3.Viz.ForcePlayground.Simple
  ( initSimpleForce
  , SimpleForceState
  , ExitingNode
  , NetworkSimulation
  , NetworkNodeRow
  , NetworkLinkRow
  -- Force control
  , ForceId(..)
  , toggleForce
  , isForceEnabled
  , setForcesEnabled
  , reheatSimulation
  -- Link weight toggle
  , setUseLinkWeights
  , getUseLinkWeights
  -- Node filtering with transitions
  , filterByCategory
  , showAllNodes
  , getVisibleCount
  , getTotalCount
  ) where

import Prelude

import Data.Array (filter, length, elem, index)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import D3.Viz.ForcePlayground.Model (NetworkModel, NetworkNode)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, ForceXConfig, ForceYConfig, LinkDynamicConfig)
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Links (swizzleLinksByIndex, filterLinksToSubset)
import PSD3.Transition.Tick as Tick
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, on, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Selection.Types (ElementType(..)) as ET
import PSD3v2.VizTree.Tree as T

-- | Row types for the simulation (must match extra fields in NetworkNode/NetworkLink)
type NetworkNodeRow = (name :: String, group :: Int, sizeClass :: Int, importance :: Number, subgraph :: Int)
type NetworkLinkRow = (weight :: Number, linkType :: Int, distance :: Number)

-- | Concrete simulation type
type NetworkSimulation = Sim.Simulation NetworkNodeRow NetworkLinkRow

-- | Exiting node with frozen position and transition progress
type ExitingNode = Tick.Transitioning NetworkNode

-- | Node ready for rendering with computed visual state
type RenderNode =
  { node :: NetworkNode
  , enterProgress :: Maybe Number  -- Just 0.0-1.0 if entering, Nothing if not
  , exitProgress :: Maybe Number   -- Just 0.0-1.0 if exiting, Nothing if not
  }

-- | Swizzled link for rendering (with full node refs and link attributes)
type SwizzledLink =
  { source :: NetworkNode
  , target :: NetworkNode
  , weight :: Number
  , linkType :: Int
  , index :: Int
  }

-- | Scene data for rendering
type SceneData =
  { nodes :: Array RenderNode
  , links :: Array SwizzledLink
  }

-- | Identifiers for the forces we use
data ForceId
  = ForceCharge
  | ForceCollide
  | ForceLink
  | ForceX
  | ForceY

derive instance eqForceId :: Eq ForceId
derive instance ordForceId :: Ord ForceId

-- | Get the string name used in the simulation
forceIdName :: ForceId -> String
forceIdName = case _ of
  ForceCharge -> "charge"
  ForceCollide -> "collision"
  ForceLink -> "links"
  ForceX -> "forceX"
  ForceY -> "forceY"

-- | Get the ForceSpec for a force
forceIdSpec :: ForceId -> ForceSpec
forceIdSpec = case _ of
  ForceCharge -> ManyBody "charge" defaultManyBody { strength = -30.0, distanceMax = 200.0 }
  ForceCollide -> Collide "collision" defaultCollide { radius = 10.0, strength = 0.7, iterations = 1 }
  ForceLink -> Link "links" defaultLink { distance = 40.0, strength = 0.4, iterations = 1 }
  ForceX -> PositionX "forceX" defaultForceX
  ForceY -> PositionY "forceY" defaultForceY

-- | State for the simple force layout
type SimpleForceState =
  { model :: NetworkModel
  , simulation :: NetworkSimulation
  , containerSelector :: String
  , enabledForces :: Set ForceId  -- Track which forces are active
  , useLinkWeights :: Boolean     -- Whether link strength is from data
  -- Filtering state with transitions
  , visibleNodeIds :: Array Int       -- IDs of currently visible nodes
  , enteringProgress :: Map Int Tick.Progress  -- nodeId → progress (0→1)
  , exitingNodes :: Array ExitingNode -- Nodes being animated out
  }

-- | SVG dimensions
svgWidth :: Number
svgWidth = 800.0

svgHeight :: Number
svgHeight = 600.0

-- | Transition speed: progress increment per tick
-- | At 60fps, 0.03 ≈ 33 ticks ≈ 0.55 seconds (snappy)
transitionDelta :: Tick.TickDelta
transitionDelta = 0.03

-- | Color palette for node categories (D3 category10)
categoryColors :: Array String
categoryColors =
  [ "#1f77b4"  -- blue (Research)
  , "#ff7f0e"  -- orange (Industry)
  , "#2ca02c"  -- green (Government)
  , "#d62728"  -- red (Community)
  , "#9467bd"  -- purple
  , "#8c564b"  -- brown
  , "#e377c2"  -- pink
  , "#7f7f7f"  -- gray
  , "#bcbd22"  -- olive
  , "#17becf"  -- cyan
  ]

-- | Get color for a category/group
categoryColor :: Int -> String
categoryColor g = fromMaybe "#69b3a2" (index categoryColors (g `mod` 10))

-- | Color palette for link types (muted)
linkTypeColors :: Array String
linkTypeColors =
  [ "#6baed6"  -- light blue (Collaboration)
  , "#fd8d3c"  -- light orange (Citation)
  , "#74c476"  -- light green (Funding)
  , "#e377c2"  -- pink (Communication)
  ]

-- | Get color for a link type
linkTypeColor :: Int -> String
linkTypeColor t = fromMaybe "#999" (index linkTypeColors (t `mod` 4))

-- | Calculate node radius based on sizeClass and importance
nodeRadius :: NetworkNode -> Number
nodeRadius node =
  let
    baseSize = case node.sizeClass of
      0 -> 3.0   -- small
      1 -> 5.0   -- medium
      _ -> 7.0   -- large
    -- Importance adds 0-3 extra pixels
    importanceBonus = node.importance * 3.0
  in
    baseSize + importanceBonus

-- | Default forceX config (weak pull toward center)
defaultForceX :: ForceXConfig
defaultForceX = { x: 0.0, strength: 0.1 }

-- | Default forceY config (weak pull toward center)
defaultForceY :: ForceYConfig
defaultForceY = { y: 0.0, strength: 0.1 }

-- | All forces enabled by default
allForces :: Set ForceId
allForces = Set.fromFoldable [ForceX, ForceY, ForceCharge, ForceCollide, ForceLink]

-- | Initialize the simple force layout
initSimpleForce :: NetworkModel -> String -> Effect (Ref SimpleForceState)
initSimpleForce model containerSelector = do
  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes model.nodes sim
  Sim.setLinks model.links sim

  -- Add all forces initially
  Sim.addForce (forceIdSpec ForceX) sim
  Sim.addForce (forceIdSpec ForceY) sim
  Sim.addForce (forceIdSpec ForceCharge) sim
  Sim.addForce (forceIdSpec ForceCollide) sim
  Sim.addForce (forceIdSpec ForceLink) sim

  -- Start with all nodes visible, all entering
  let allNodeIds = map _.id model.nodes
  let initialEntering = Tick.startProgress allNodeIds Map.empty

  -- Create state ref
  stateRef <- Ref.new
    { model
    , simulation: sim
    , containerSelector
    , enabledForces: allForces
    , useLinkWeights: false
    , visibleNodeIds: allNodeIds
    , enteringProgress: initialEntering
    , exitingNodes: []
    }

  -- Render initial SVG structure
  runD3v2M $ renderSVGContainer containerSelector

  -- Set up tick callback
  Sim.onTick (onSimulationTick stateRef) sim

  -- Start simulation
  Sim.start sim

  -- Attach pinning drag after first tick renders nodes
  -- We use a small delay to ensure DOM is ready
  _ <- scheduleAttachPinningDrag sim

  pure stateRef

-- | Schedule drag attachment after a short delay (to ensure DOM is ready)
foreign import scheduleAfterRender :: Effect Unit -> Effect Unit

scheduleAttachPinningDrag :: NetworkSimulation -> Effect Unit
scheduleAttachPinningDrag sim = scheduleAfterRender do
  nodeCircles <- Sim.querySelectorElements "#network-nodes circle"
  Sim.attachPinningDrag nodeCircles sim

-- | Create the SVG container structure
renderSVGContainer :: String -> D3v2M Unit
renderSVGContainer containerSelector = do
  container <- select containerSelector

  svg <- appendChild ET.SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "network-force-svg"
    , class_ "network-force"
    ]
    container

  zoomGroup <- appendChild ET.Group [ id_ "network-zoom-group", class_ "zoom-group" ] svg
  _ <- appendChild ET.Group [ id_ "network-links", class_ "links" ] zoomGroup
  _ <- appendChild ET.Group [ id_ "network-nodes", class_ "nodes" ] zoomGroup

  -- Attach zoom behavior
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#network-zoom-group") svg

  pure unit

-- | Called on each simulation tick
-- | Advances transition progress AND re-renders
onSimulationTick :: Ref SimpleForceState -> Effect Unit
onSimulationTick stateRef = do
  state <- Ref.read stateRef

  -- Advance and filter transitions using library functions
  let { active: stillEntering } = Tick.tickProgressMap transitionDelta state.enteringProgress
  let { active: stillExiting } = Tick.tickTransitions transitionDelta state.exitingNodes

  -- Update state with advanced transitions
  Ref.write
    ( state
        { enteringProgress = stillEntering
        , exitingNodes = stillExiting
        }
    )
    stateRef

  -- Render with current state
  renderVisualization stateRef

-- | Render visualization with current state
renderVisualization :: Ref SimpleForceState -> Effect Unit
renderVisualization stateRef = do
  state <- Ref.read stateRef

  -- Get current node positions from simulation
  currentNodes <- Sim.getNodes state.simulation

  -- Build render nodes: only visible nodes, with transition state
  let visibleNodes = filter (\n -> elem n.id state.visibleNodeIds) currentNodes

  let renderNodes = map
        (\n ->
          { node: n
          , enterProgress: Map.lookup n.id state.enteringProgress
          , exitProgress: Nothing
          }
        )
        visibleNodes

  -- Add exiting nodes (with frozen positions and exit progress)
  let exitingRenderNodes = map
        (\e ->
          { node: e.item
          , enterProgress: Nothing
          , exitProgress: Just e.progress
          }
        )
        state.exitingNodes

  let allRenderNodes = renderNodes <> exitingRenderNodes

  -- Create links only between visible nodes (not exiting)
  let visibleLinks = filterLinksToSubset _.id visibleNodes state.model.links
  let swizzledLinks = swizzleLinksByIndex _.id visibleNodes visibleLinks \src tgt i link ->
        { source: src, target: tgt, weight: link.weight, linkType: link.linkType, index: i }

  -- Create scene data
  let scene = { nodes: allRenderNodes, links: swizzledLinks }

  -- Render
  runD3v2M $ renderScene scene

-- | Render the scene
renderScene :: SceneData -> D3v2M Unit
renderScene scene = do
  -- Render links
  linksGroup <- select "#network-links"
  let linksTree = createLinksTree scene
  _ <- renderTree linksGroup linksTree

  -- Render nodes
  nodesGroup <- select "#network-nodes"
  let nodesTree = createNodesTree scene
  _ <- renderTree nodesGroup nodesTree

  pure unit

-- | Create nodes tree with transition-aware visual properties
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree scene =
  T.sceneNestedJoin "nodes" "circle"
    [ scene ]
    (_.nodes)
    ( \rn -> T.elem ET.Circle
        [ cx rn.node.x
        , cy rn.node.y
        , radius (radiusForRenderNode rn)
        , fill (fillForRenderNode rn)
        , stroke (strokeForRenderNode rn)
        , strokeWidth (strokeWidthForRenderNode rn)
        , opacity (opacityForRenderNode rn)
        ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- =============================================================================
-- Visual Property Interpolation (Tick-Driven)
-- =============================================================================

-- | Get radius for a node based on transition state
-- | Entering: starts large (20), shrinks to normal
-- | Exiting: starts normal, grows large (20) before disappearing
radiusForRenderNode :: RenderNode -> Number
radiusForRenderNode rn =
  let baseRadius = nodeRadius rn.node
  in case rn.enterProgress, rn.exitProgress of
    Just p, _ -> Tick.lerp (baseRadius * 3.0) baseRadius p  -- Entering: large → normal
    _, Just p -> Tick.lerp baseRadius (baseRadius * 3.0) p  -- Exiting: normal → large (pop)
    _, _ -> baseRadius

-- | Get fill color for a node based on transition state
fillForRenderNode :: RenderNode -> String
fillForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just _, _ -> "#2ca02c"  -- Green flash for entering
    _, Just _ -> "#d62728"  -- Red flash for exiting
    _, _ -> categoryColor rn.node.group  -- Normal category color

-- | Get stroke color for a node based on transition state
strokeForRenderNode :: RenderNode -> String
strokeForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just _, _ -> "#98df8a"  -- Light green for entering
    _, Just _ -> "#ff9896"  -- Light red for exiting
    _, _ -> "#fff"

-- | Get stroke width for a node based on transition state
strokeWidthForRenderNode :: RenderNode -> Number
strokeWidthForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just p, _ -> Tick.lerp 4.0 1.5 p  -- Thick → normal
    _, Just p -> Tick.lerp 1.5 4.0 p  -- Normal → thick
    _, _ -> 1.5

-- | Get opacity for a node based on transition state
-- | Exiting: fades from 1 → 0
opacityForRenderNode :: RenderNode -> Number
opacityForRenderNode rn =
  let baseOpacity = 0.7 + rn.node.importance * 0.3
  in case rn.exitProgress of
    Just p -> Tick.lerp baseOpacity 0.0 p  -- Fade out
    _ -> baseOpacity

-- | Create links tree with rich visual encoding
createLinksTree :: SceneData -> T.Tree SceneData
createLinksTree scene =
  T.sceneNestedJoin "links" "line"
    [ scene ]
    (_.links)
    ( \link -> T.elem ET.Line
        [ x1 link.source.x
        , y1 link.source.y
        , x2 link.target.x
        , y2 link.target.y
        , strokeWidth (0.5 + link.weight * 2.0)  -- 0.5-2.5 based on weight
        , stroke (linkTypeColor link.linkType)
        , opacity (0.3 + link.weight * 0.4)      -- 0.3-0.7 based on weight
        ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- =============================================================================
-- Force Controls
-- =============================================================================

-- | Check if a force is currently enabled
isForceEnabled :: ForceId -> Ref SimpleForceState -> Effect Boolean
isForceEnabled forceId stateRef = do
  state <- Ref.read stateRef
  pure $ Set.member forceId state.enabledForces

-- | Toggle a force on/off, returns new enabled state
toggleForce :: ForceId -> Ref SimpleForceState -> Effect Boolean
toggleForce forceId stateRef = do
  state <- Ref.read stateRef
  let wasEnabled = Set.member forceId state.enabledForces

  if wasEnabled
    then do
      -- Remove the force
      Sim.removeForce (forceIdName forceId) state.simulation
      Ref.modify_ (_ { enabledForces = Set.delete forceId state.enabledForces }) stateRef
      -- Reheat to show effect
      Sim.reheat state.simulation
      pure false
    else do
      -- Add the force back
      Sim.addForce (forceIdSpec forceId) state.simulation
      Ref.modify_ (_ { enabledForces = Set.insert forceId state.enabledForces }) stateRef
      -- Reheat to show effect
      Sim.reheat state.simulation
      pure true

-- | Set which forces are enabled (for presets)
-- | Returns the new set of enabled forces
setForcesEnabled :: Set ForceId -> Ref SimpleForceState -> Effect (Set ForceId)
setForcesEnabled targetForces stateRef = do
  state <- Ref.read stateRef

  -- Calculate changes
  let currentForces = state.enabledForces
  let toRemove = Set.difference currentForces targetForces
  let toAdd = Set.difference targetForces currentForces

  -- Remove forces that shouldn't be enabled
  for_ (Set.toUnfoldable toRemove :: Array ForceId) \forceId ->
    Sim.removeForce (forceIdName forceId) state.simulation

  -- Add forces that should be enabled
  for_ (Set.toUnfoldable toAdd :: Array ForceId) \forceId ->
    Sim.addForce (forceIdSpec forceId) state.simulation

  -- Update state
  Ref.modify_ (_ { enabledForces = targetForces }) stateRef

  -- Reheat to show effect
  Sim.reheat state.simulation

  pure targetForces

-- | Reheat the simulation (restart animation)
reheatSimulation :: Ref SimpleForceState -> Effect Unit
reheatSimulation stateRef = do
  state <- Ref.read stateRef
  Sim.reheat state.simulation

-- =============================================================================
-- Link Weight Toggle
-- =============================================================================

-- | Get whether link weights are being used for force strength
getUseLinkWeights :: Ref SimpleForceState -> Effect Boolean
getUseLinkWeights stateRef = do
  state <- Ref.read stateRef
  pure state.useLinkWeights

-- | Set whether link force strength should come from link weight data
-- | When true: links with higher weight pull more strongly
-- | When false: all links have same strength (0.4)
setUseLinkWeights :: Boolean -> Ref SimpleForceState -> Effect Unit
setUseLinkWeights useWeights stateRef = do
  state <- Ref.read stateRef

  -- Only do something if the Link force is enabled
  when (Set.member ForceLink state.enabledForces) do
    -- Remove current link force
    Sim.removeForce (forceIdName ForceLink) state.simulation

    if useWeights
      then do
        -- Add dynamic link force that uses link.weight
        let dynamicConfig :: LinkDynamicConfig _
            dynamicConfig =
              { distance: 40.0
              , strengthAccessor: \link -> link.weight  -- Read weight from link data
              , iterations: 1
              }
        let forceHandle = Core.createLinkDynamic dynamicConfig
        _ <- Core.initializeLinkForce forceHandle state.model.nodes state.model.links
        -- The simulation needs to track this force - we use the same name
        Sim.addForceHandle "links" forceHandle state.simulation
      else do
        -- Add standard link force
        Sim.addForce (forceIdSpec ForceLink) state.simulation

  -- Update state
  Ref.modify_ (_ { useLinkWeights = useWeights }) stateRef

  -- Reheat to show effect
  Sim.reheat state.simulation

-- =============================================================================
-- Node Filtering with Transitions
-- =============================================================================

-- | Filter nodes to show only specified categories (0=Research, 1=Industry, 2=Government, 3=Community)
-- | Nodes not in the filter will animate out; newly visible nodes will animate in
filterByCategory :: Array Int -> Ref SimpleForceState -> Effect Unit
filterByCategory categories stateRef = do
  state <- Ref.read stateRef

  -- Calculate new visible IDs based on category filter
  let newVisibleIds = map _.id $ filter (\n -> elem n.group categories) state.model.nodes

  -- Find nodes to remove and add
  let toRemove = filter (\id -> not (elem id newVisibleIds)) state.visibleNodeIds
  let toAdd = filter (\id -> not (elem id state.visibleNodeIds)) newVisibleIds

  -- Get current node positions for exiting animation (freeze their positions)
  currentNodes <- Sim.getNodes state.simulation
  let exitingNodeData = filter (\n -> elem n.id toRemove) currentNodes
  let newExiting = Tick.startTransitions exitingNodeData

  -- Remove from entering if they were still entering
  let cleanedEntering = Array.foldl (\m id -> Map.delete id m) state.enteringProgress toRemove

  -- Add entering transitions for new nodes (start at progress 0)
  let newEntering = Tick.startProgress toAdd cleanedEntering

  -- Update state
  Ref.write
    ( state
        { visibleNodeIds = newVisibleIds
        , enteringProgress = newEntering
        , exitingNodes = state.exitingNodes <> newExiting
        }
    )
    stateRef

  -- Reheat simulation so remaining nodes settle
  Sim.reheat state.simulation

-- | Show all nodes (reset filter)
showAllNodes :: Ref SimpleForceState -> Effect Unit
showAllNodes stateRef = do
  state <- Ref.read stateRef
  let allIds = map _.id state.model.nodes

  -- Find newly visible nodes (were hidden, now visible)
  let currentlyHidden = filter (\id -> not (elem id state.visibleNodeIds)) allIds
  let newEntering = Tick.startProgress currentlyHidden state.enteringProgress

  -- Clear exiting nodes and set all as visible
  Ref.write
    ( state
        { visibleNodeIds = allIds
        , enteringProgress = newEntering
        , exitingNodes = []
        }
    )
    stateRef

  Sim.reheat state.simulation

-- | Get count of currently visible nodes
getVisibleCount :: Ref SimpleForceState -> Effect Int
getVisibleCount stateRef = do
  state <- Ref.read stateRef
  pure $ length state.visibleNodeIds

-- | Get total count of nodes
getTotalCount :: Ref SimpleForceState -> Effect Int
getTotalCount stateRef = do
  state <- Ref.read stateRef
  pure $ length state.model.nodes
