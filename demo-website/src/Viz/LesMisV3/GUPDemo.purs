-- | Les Misérables Force Graph with GUP (General Update Pattern)
-- |
-- | Combines:
-- | - Force simulation for continuous position updates
-- | - Tick-driven transitions for enter/exit animations
-- |
-- | KEY INSIGHT: Simulation tick drives EVERYTHING - positions AND transitions.
-- | No CSS transitions needed. All visual properties interpolated in PureScript.
-- |
-- | NO FFI - uses only library functionality.
module D3.Viz.LesMisV3.GUPDemo
  ( LesMisGUPState
  , ExitingNode
  , LesMisSimulation
  , LesMisNodeRow
  , LesMisLinkRow
  , initGUPDemo
  , addRandomNodes
  , removeRandomNodes
  , resetToFull
  ) where

import Prelude

import Data.Array as Array
import Data.Array (filter, length, (!!))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import D3.Viz.LesMisV3.Model (LesMisModel, LesMisNode)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)
import PSD3.ForceEngine.Links (filterLinksToSubset, swizzleLinksByIndex)
import PSD3.Transition.Tick as Tick
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, on, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Selection.Types (ElementType(..)) as ET
import PSD3v2.VizTree.Tree as T

-- =============================================================================
-- Types
-- =============================================================================

-- | Row types for the simulation (extra fields beyond x, y, vx, vy / source, target)
type LesMisNodeRow = (id :: String, group :: Int, fx :: Nullable Number, fy :: Nullable Number, index :: Int)
type LesMisLinkRow = (value :: Number)

-- | Concrete simulation type for Les Misérables
type LesMisSimulation = Sim.Simulation LesMisNodeRow LesMisLinkRow

-- | Exiting node uses library's Transitioning type
type ExitingNode = Tick.Transitioning LesMisNode

-- | Node ready for rendering with computed visual state
type RenderNode =
  { node :: LesMisNode
  , enterProgress :: Maybe Number -- Just 0.0-1.0 if entering, Nothing if not
  , exitProgress :: Maybe Number -- Just 0.0-1.0 if exiting, Nothing if not
  }

-- | Swizzled link for rendering (node references instead of indices)
type SwizzledLink =
  { source :: LesMisNode
  , target :: LesMisNode
  , value :: Number
  , index :: Int
  , isExiting :: Boolean
  }

-- | Scene data for the Tree API
type SceneData =
  { nodes :: Array RenderNode
  , links :: Array SwizzledLink
  }

-- | State for the GUP demo
-- | Tracks transitions with progress, driven by simulation tick
type LesMisGUPState =
  { fullModel :: LesMisModel -- Original full dataset
  , visibleNodeIds :: Array String -- Currently visible node IDs
  , enteringProgress :: Map String Tick.Progress -- nodeId → progress (0→1)
  , exitingNodes :: Array ExitingNode -- Nodes being animated out with progress
  , simulation :: LesMisSimulation -- Force simulation handle
  , containerSelector :: String -- DOM container
  }

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 900.0

svgHeight :: Number
svgHeight = 600.0

-- | Transition speed: progress increment per tick
-- | At 60fps, 0.025 ≈ 40 ticks ≈ 0.67 seconds
transitionDelta :: Tick.TickDelta
transitionDelta = 0.025

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the GUP demo
-- | Returns a state ref for controlling the visualization
initGUPDemo :: LesMisModel -> String -> Effect (Ref LesMisGUPState)
initGUPDemo model containerSelector = do
  -- Start with all nodes visible, all entering
  let allNodeIds = map _.id model.nodes
  let initialEntering = Tick.startProgress allNodeIds Map.empty

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes model.nodes sim
  Sim.setLinks model.links sim

  -- Add forces
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Create state ref
  stateRef <- Ref.new
    { fullModel: model
    , visibleNodeIds: allNodeIds
    , enteringProgress: initialEntering
    , exitingNodes: []
    , simulation: sim
    , containerSelector
    }

  -- Render initial SVG structure
  runD3v2M $ renderSVGContainer containerSelector

  -- Set up tick callback - this drives EVERYTHING
  Sim.onTick (onSimulationTick stateRef) sim

  -- Start simulation
  Sim.start sim

  pure stateRef

-- =============================================================================
-- GUP Operations
-- =============================================================================

-- | Add N random nodes that aren't currently visible
addRandomNodes :: Int -> Ref LesMisGUPState -> Effect Unit
addRandomNodes count stateRef = do
  state <- Ref.read stateRef

  let
    hiddenIds = filter (\id -> not (Array.elem id state.visibleNodeIds))
      (map _.id state.fullModel.nodes)

  -- Pick random hidden nodes to add
  nodesToAdd <- pickRandom count hiddenIds
  let newVisible = state.visibleNodeIds <> nodesToAdd

  -- Add entering transitions for new nodes (start at progress 0)
  let newEntering = Tick.startProgress nodesToAdd state.enteringProgress

  -- Update state
  Ref.write
    ( state
        { visibleNodeIds = newVisible
        , enteringProgress = newEntering
        }
    )
    stateRef

  -- Reheat simulation so nodes settle into new positions
  Sim.reheat state.simulation

-- | Remove N random visible nodes
removeRandomNodes :: Int -> Ref LesMisGUPState -> Effect Unit
removeRandomNodes count stateRef = do
  state <- Ref.read stateRef

  -- Pick random visible nodes to remove
  nodesToRemove <- pickRandom count state.visibleNodeIds
  let newVisible = filter (\id -> not (Array.elem id nodesToRemove)) state.visibleNodeIds

  -- Get the actual node data for exiting nodes (freeze their positions)
  currentNodes <- Sim.getNodes state.simulation
  let exitingNodeData = filter (\n -> Array.elem n.id nodesToRemove) currentNodes
  let newExiting = Tick.startTransitions exitingNodeData

  -- Remove from entering if they were still entering
  let newEntering = Array.foldl (\m id -> Map.delete id m) state.enteringProgress nodesToRemove

  -- Update state
  Ref.write
    ( state
        { visibleNodeIds = newVisible
        , enteringProgress = newEntering
        , exitingNodes = state.exitingNodes <> newExiting
        }
    )
    stateRef

  -- Reheat simulation so remaining nodes settle
  Sim.reheat state.simulation

-- | Reset to full dataset
resetToFull :: Ref LesMisGUPState -> Effect Unit
resetToFull stateRef = do
  state <- Ref.read stateRef
  let allIds = map _.id state.fullModel.nodes

  -- Find newly visible nodes (were hidden, now visible)
  let currentlyHidden = filter (\id -> not (Array.elem id state.visibleNodeIds)) allIds
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

-- =============================================================================
-- Simulation Tick Handler - THE HEART OF TICK-DRIVEN TRANSITIONS
-- =============================================================================

-- | Called on each simulation tick
-- | Advances transition progress AND re-renders
onSimulationTick :: Ref LesMisGUPState -> Effect Unit
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

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Create the SVG container structure
renderSVGContainer :: String -> D3v2M Unit
renderSVGContainer containerSelector = do
  container <- select containerSelector

  svg <- appendChild ET.SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "lesmis-gup-svg"
    , class_ "lesmis-gup"
    ]
    container

  zoomGroup <- appendChild ET.Group [ id_ "lesmis-gup-zoom-group", class_ "zoom-group" ] svg
  _ <- appendChild ET.Group [ id_ "lesmis-gup-links", class_ "links" ] zoomGroup
  _ <- appendChild ET.Group [ id_ "lesmis-gup-nodes", class_ "nodes" ] zoomGroup

  -- Attach zoom behavior
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#lesmis-gup-zoom-group") svg

  pure unit

-- | Render visualization with current state
renderVisualization :: Ref LesMisGUPState -> Effect Unit
renderVisualization stateRef = do
  state <- Ref.read stateRef

  -- Get current node positions from simulation
  currentNodes <- Sim.getNodes state.simulation

  -- Build render nodes with transition state
  let visibleNodes = filter (\n -> Array.elem n.id state.visibleNodeIds) currentNodes

  let
    renderNodes = map
      ( \n ->
          { node: n
          , enterProgress: Map.lookup n.id state.enteringProgress
          , exitProgress: Nothing
          }
      )
      visibleNodes

  -- Add exiting nodes (with frozen positions and exit progress)
  let
    exitingRenderNodes = map
      ( \e ->
          { node: e.item
          , enterProgress: Nothing
          , exitProgress: Just e.progress
          }
      )
      state.exitingNodes

  let allRenderNodes = renderNodes <> exitingRenderNodes

  -- Create links only between visible nodes (not exiting)
  let visibleLinks = filterLinksToSubset _.index visibleNodes state.fullModel.links
  let
    swizzledLinks = swizzleLinksByIndex _.index visibleNodes visibleLinks \src tgt i link ->
      { source: src, target: tgt, value: link.value, index: i, isExiting: false }

  -- Create scene data
  let scene = { nodes: allRenderNodes, links: swizzledLinks }

  -- Render
  runD3v2M $ renderGUPScene scene

-- | Render the scene
renderGUPScene :: SceneData -> D3v2M Unit
renderGUPScene scene = do
  -- Render nodes
  nodesGroup <- select "#lesmis-gup-nodes"
  let nodesTree = createNodesTree scene
  _ <- renderTree nodesGroup nodesTree

  -- Render links
  linksGroup <- select "#lesmis-gup-links"
  let linksTree = createLinksTree scene
  _ <- renderTree linksGroup linksTree

  pure unit

-- | Create nodes tree with tick-driven visual properties
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree scene =
  T.sceneNestedJoin "nodes" "circle"
    [ scene ]
    (_.nodes)
    ( \rn -> T.elem ET.Circle
        [ cx rn.node.x
        , cy rn.node.y
        , radius (radiusForNode rn)
        , fill (fillForNode rn)
        , opacity (opacityForNode rn)
        , stroke "#fff"
        , strokeWidth 1.5
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
-- | Entering: starts large (20), shrinks to normal (5)
-- | Exiting: starts normal (5), grows large (20) before disappearing
-- | Normal: 5
radiusForNode :: RenderNode -> Number
radiusForNode { enterProgress, exitProgress } =
  case enterProgress, exitProgress of
    Just p, _ -> Tick.lerp 20.0 5.0 p -- Entering: large → normal
    _, Just p -> Tick.lerp 5.0 20.0 p -- Exiting: normal → large (pop)
    _, _ -> 5.0 -- Normal

-- | Get fill color for a node based on transition state
fillForNode :: RenderNode -> String
fillForNode { enterProgress, exitProgress } =
  case enterProgress, exitProgress of
    Just _, _ -> "#2ca02c" -- Green for entering
    _, Just _ -> "#8c564b" -- Brown for exiting
    _, _ -> "#7f7f7f" -- Gray for normal

-- | Get opacity for a node based on transition state
-- | Exiting: fades from 1 → 0
opacityForNode :: RenderNode -> Number
opacityForNode { exitProgress } =
  case exitProgress of
    Just p -> Tick.lerp 1.0 0.0 p -- Fade out
    _ -> 1.0

-- | Create links tree
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
        , strokeWidth (sqrt link.value)
        , stroke "#999"
        , opacity 0.6
        ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Pick N random elements from an array
pickRandom :: forall a. Int -> Array a -> Effect (Array a)
pickRandom n arr = do
  if n <= 0 || length arr == 0 then pure []
  else do
    indices <- pickRandomIndices n (length arr) []
    pure $ Array.mapMaybe (\i -> arr !! i) indices

pickRandomIndices :: Int -> Int -> Array Int -> Effect (Array Int)
pickRandomIndices 0 _ acc = pure acc
pickRandomIndices _ 0 acc = pure acc
pickRandomIndices n maxIdx acc = do
  idx <- randomInt 0 (maxIdx - 1)
  if Array.elem idx acc then pickRandomIndices n maxIdx acc -- Try again
  else pickRandomIndices (n - 1) maxIdx (acc <> [ idx ])
