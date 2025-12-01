-- | Les Misérables Force Graph with GUP (General Update Pattern)
-- |
-- | Combines:
-- | - Force simulation for continuous position updates
-- | - Tick-driven transitions for enter/exit animations
-- |
-- | KEY INSIGHT: Simulation tick drives EVERYTHING - positions AND transitions.
-- | No CSS transitions needed. All visual properties interpolated in PureScript.
-- |
-- | IMPORTANT: Uses PSD3v2 Selection API directly, NOT Tree API.
-- | Tree API does data joins on every call which is O(n) per tick.
-- | For simulations, we bind data once with appendData, then update positions via FFI.
module Viz.LesMis.GUPDemo
  ( LesMisGUPState
  , SwizzledLink_
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
import Viz.LesMis.Model (LesMisModel, LesMisNode)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)
import PSD3.Transition.Tick as Tick
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns)
import Web.DOM.Element (Element)

-- =============================================================================
-- FFI for fast tick updates (bypasses PureScript array traversal)
-- =============================================================================

-- | Update node positions directly via D3 selection - O(n) DOM updates only
foreign import updateNodePositions :: forall s d. D3v2Selection_ s Element d -> Effect Unit

-- | Update link positions directly via D3 selection
foreign import updateLinkPositions :: forall s d. D3v2Selection_ s Element d -> Effect Unit

-- =============================================================================
-- Types
-- =============================================================================

-- | Row types for the simulation (extra fields beyond x, y, vx, vy / source, target)
type LesMisNodeRow = ( id :: String, group :: Int, fx :: Nullable Number, fy :: Nullable Number, index :: Int )
type LesMisLinkRow = ( value :: Number )

-- | Concrete simulation type for Les Misérables
type LesMisSimulation = Sim.Simulation LesMisNodeRow LesMisLinkRow

-- | Exiting node uses library's Transitioning type
type ExitingNode = Tick.Transitioning LesMisNode

-- | Swizzled link type (forward declaration for state type)
type SwizzledLink_ =
  { source :: LesMisNode
  , target :: LesMisNode
  , value :: Number
  }

-- | State for the GUP demo
-- | Tracks transitions with progress, driven by simulation tick
type LesMisGUPState =
  { fullModel :: LesMisModel                    -- Original full dataset
  , visibleNodeIds :: Array String              -- Currently visible node IDs
  , enteringProgress :: Map String Tick.Progress -- nodeId → progress (0→1)
  , exitingNodes :: Array ExitingNode           -- Nodes being animated out with progress
  , simulation :: LesMisSimulation              -- Force simulation handle
  , containerSelector :: String                 -- DOM container
  , nodeSelection :: D3v2Selection_ SBoundOwns Element LesMisNode  -- Bound node selection
  , linkSelection :: D3v2Selection_ SBoundOwns Element SwizzledLink_  -- Bound link selection with swizzled refs
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

-- | Simple link swizzle for initial setup (full node array, array indices match)
swizzleLinksSimple :: Array LesMisNode -> Array { source :: Int, target :: Int, value :: Number } -> Array SwizzledLink_
swizzleLinksSimple nodes links = Array.mapMaybe swizzle links
  where
  swizzle link = do
    src <- nodes Array.!! link.source
    tgt <- nodes Array.!! link.target
    pure { source: src, target: tgt, value: link.value }

-- | Initialize the GUP demo
-- | Returns a state ref for controlling the visualization
initGUPDemo :: LesMisModel -> String -> Effect (Ref LesMisGUPState)
initGUPDemo model containerSelector = do
  -- Start with all nodes visible, all entering
  let allNodeIds = map _.id model.nodes
  let initialEntering = Tick.startProgress allNodeIds Map.empty

  -- IMPORTANT: Swizzle links BEFORE simulation setup!
  -- D3's forceLink mutates link objects, replacing indices with node refs.
  -- We need to do our own swizzle first while indices are still integers.
  let swizzledLinks = swizzleLinksSimple model.nodes model.links

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes model.nodes sim
  Sim.setLinks model.links sim  -- This mutates model.links (indices → node refs)

  -- Add forces
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Get nodes after simulation has initialized their positions
  currentNodes <- Sim.getNodes sim

  -- Render initial SVG structure and get bound selections
  { nodeSel, linkSel } <- runD3v2M $ renderInitial containerSelector currentNodes swizzledLinks

  -- Create state ref
  stateRef <- Ref.new
    { fullModel: model
    , visibleNodeIds: allNodeIds
    , enteringProgress: initialEntering
    , exitingNodes: []
    , simulation: sim
    , containerSelector
    , nodeSelection: nodeSel
    , linkSelection: linkSel
    }

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

  let hiddenIds = filter (\id -> not (Array.elem id state.visibleNodeIds))
                         (map _.id state.fullModel.nodes)

  -- Pick random hidden nodes to add
  nodesToAdd <- pickRandom count hiddenIds
  let newVisible = state.visibleNodeIds <> nodesToAdd

  -- Add entering transitions for new nodes (start at progress 0)
  let newEntering = Tick.startProgress nodesToAdd state.enteringProgress

  -- Update state
  Ref.write (state { visibleNodeIds = newVisible
                   , enteringProgress = newEntering
                   }) stateRef

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
  Ref.write (state { visibleNodeIds = newVisible
                   , enteringProgress = newEntering
                   , exitingNodes = state.exitingNodes <> newExiting
                   }) stateRef

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
  Ref.write (state { visibleNodeIds = allIds
                   , enteringProgress = newEntering
                   , exitingNodes = []
                   }) stateRef

  Sim.reheat state.simulation

-- =============================================================================
-- Simulation Tick Handler - THE HEART OF TICK-DRIVEN TRANSITIONS
-- =============================================================================

-- | Called on each simulation tick
-- | Updates positions via fast FFI path (no Tree API, no data join)
onSimulationTick :: Ref LesMisGUPState -> Effect Unit
onSimulationTick stateRef = do
  state <- Ref.read stateRef

  -- Advance and filter transitions using library functions
  let { active: stillEntering } = Tick.tickProgressMap transitionDelta state.enteringProgress
  let { active: stillExiting } = Tick.tickTransitions transitionDelta state.exitingNodes

  -- Update state with advanced transitions
  Ref.write (state { enteringProgress = stillEntering
                   , exitingNodes = stillExiting
                   }) stateRef

  -- Fast path: update positions directly via FFI (no data join!)
  updateNodePositions state.nodeSelection
  updateLinkPositions state.linkSelection

-- =============================================================================
-- Initial Rendering (uses PSD3v2 Selection API, NOT Tree API)
-- =============================================================================

-- | Create the SVG container and bind data ONCE
-- | Returns the bound selections for fast tick updates
renderInitial
  :: String
  -> Array LesMisNode
  -> Array SwizzledLink_
  -> D3v2M { nodeSel :: D3v2Selection_ SBoundOwns Element LesMisNode
           , linkSel :: D3v2Selection_ SBoundOwns Element SwizzledLink_
           }
renderInitial containerSelector nodes links = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "lesmis-gup-svg"
    , class_ "lesmis-gup"
    ] container

  zoomGroup <- appendChild Group [ id_ "lesmis-gup-zoom-group", class_ "zoom-group" ] svg
  linksGroup <- appendChild Group [ id_ "lesmis-gup-links", class_ "links" ] zoomGroup
  nodesGroup <- appendChild Group [ id_ "lesmis-gup-nodes", class_ "nodes" ] zoomGroup

  -- Bind node data ONCE with appendData (not Tree API)
  nodeSel <- appendData Circle nodes
    [ cx (_.x :: LesMisNode -> Number)
    , cy (_.y :: LesMisNode -> Number)
    , radius (const 5.0 :: LesMisNode -> Number)
    , fill (const "#7f7f7f" :: LesMisNode -> String)
    , stroke "#fff"
    , strokeWidth 1.5
    ] nodesGroup

  -- Bind link data ONCE with appendData (swizzled links have node references)
  -- Note: Initial positions set here, then FFI updates on tick
  linkSel <- appendData Line links
    [ x1 (_.source.x :: SwizzledLink_ -> Number)
    , y1 (_.source.y :: SwizzledLink_ -> Number)
    , x2 (_.target.x :: SwizzledLink_ -> Number)
    , y2 (_.target.y :: SwizzledLink_ -> Number)
    , stroke "#666"
    , strokeWidth 1.5
    , opacity 0.8
    ] linksGroup

  -- Attach zoom behavior
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#lesmis-gup-zoom-group") svg

  pure { nodeSel, linkSel }

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Pick N random elements from an array
pickRandom :: forall a. Int -> Array a -> Effect (Array a)
pickRandom n arr = do
  if n <= 0 || length arr == 0
    then pure []
    else do
      indices <- pickRandomIndices n (length arr) []
      pure $ Array.mapMaybe (\i -> arr !! i) indices

pickRandomIndices :: Int -> Int -> Array Int -> Effect (Array Int)
pickRandomIndices 0 _ acc = pure acc
pickRandomIndices _ 0 acc = pure acc
pickRandomIndices n maxIdx acc = do
  idx <- randomInt 0 (maxIdx - 1)
  if Array.elem idx acc
    then pickRandomIndices n maxIdx acc  -- Try again
    else pickRandomIndices (n - 1) maxIdx (acc <> [idx])
