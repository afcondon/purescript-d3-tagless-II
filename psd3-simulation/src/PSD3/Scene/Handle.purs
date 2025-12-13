-- | Scene Handle
-- |
-- | Opaque handle for scene-based visualizations.
-- | Wraps simulation + scene engine + transition state.
-- | User never sees Refs - just pure-looking Effect operations.
-- |
-- | This is the high-level API for sophisticated visualizations with:
-- | - Choreographed scene transitions (Init → Interpolate → Finalize)
-- | - Node filtering (for focus/neighborhood views)
-- | - GUP-style enter/exit animations
-- |
-- | Example usage:
-- | ```purescript
-- | -- Create handle with callbacks
-- | handle <- Scene.create config callbacks nodes links renderFn
-- |
-- | -- Transition to a scene
-- | Scene.transitionTo treeFormScene handle
-- |
-- | -- In your animation loop
-- | continuing <- Scene.onTick handle
-- | when continuing $ requestAnimationFrame (const $ ...)
-- |
-- | -- Clean up
-- | Scene.destroy handle
-- | ```
module PSD3.Scene.Handle
  ( -- * Opaque Handle
    SceneHandle

    -- * Configuration
  , HandleConfig
  , Callbacks

    -- * Lifecycle
  , create
  , destroy

    -- * Scene Transitions
  , transitionTo
  , isTransitioning
  , getTransitionProgress
  , getCurrentScene

    -- * Node Operations
  , getAllNodes
  , filterNodes
  , showAllNodes
  , transformNodes

    -- * Rendering
  , onTick

    -- * Advanced: Direct Access
  , getSimulation
  , reheat

    -- * Re-exports
  , module Types
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Render (GroupId)
import PSD3.Scene.Engine as Engine
import PSD3.Scene.Types
  ( SceneConfig
  , EngineMode(..)
  , PositionMap
  , NodeRule
  ) as Types
import PSD3.Simulation.Scene as SimScene

-- =============================================================================
-- Opaque Handle
-- =============================================================================

-- | Opaque handle to a scene-based visualization.
-- |
-- | User cannot access internal state directly.
-- | All operations go through the module's exported functions.
newtype SceneHandle nodeRow linkRow = SceneHandle (Ref (HandleState nodeRow linkRow))

-- | Internal state (hidden from user)
type HandleState nodeRow linkRow =
  { simulation :: Sim.Simulation nodeRow linkRow
  , engine :: Engine.SceneEngine (Sim.SimulationNode nodeRow)
  , callbacks :: Callbacks nodeRow
  , allNodes :: Array (Sim.SimulationNode nodeRow)  -- Full node set (for restore)
  , visibleNodeIds :: Set.Set String                 -- Currently visible (for filtering)
  , nodesGroupId :: GroupId
  , linksGroupId :: Maybe GroupId
  }

-- =============================================================================
-- Configuration
-- =============================================================================

-- | Configuration for creating a handle
type HandleConfig =
  { containerSelector :: String
  , nodesGroupId :: GroupId        -- CSS selector for nodes group
  , linksGroupId :: Maybe GroupId  -- CSS selector for links group (optional)
  }

-- | Callbacks for events (D3 → User code)
-- |
-- | These are called when D3 events fire. Use them to notify your
-- | UI framework (Halogen, React, etc.) of user interactions.
type Callbacks nodeRow =
  { onNodeClick :: Sim.SimulationNode nodeRow -> Effect Unit
  , onNodeHover :: Sim.SimulationNode nodeRow -> Effect Unit
  , onNodeLeave :: Effect Unit
  , onTransitionComplete :: Effect Unit
  }

-- =============================================================================
-- Lifecycle
-- =============================================================================

-- | Create a new scene handle.
-- |
-- | Sets up:
-- | - D3 force simulation with the provided nodes/links
-- | - Scene engine for managing transitions
-- | - Event binding via callbacks
-- |
-- | The render function is called on every tick with current node positions.
create
  :: forall nodeRow linkRow
   . HandleConfig
  -> Callbacks nodeRow
  -> Array (Sim.SimulationNode nodeRow)
  -> Array { source :: Int, target :: Int | linkRow }
  -> (Array (Sim.SimulationNode nodeRow) -> Effect Unit)  -- Render function
  -> Effect (SceneHandle nodeRow linkRow)
create config callbacks nodes links renderFn = do
  -- Create simulation with default config
  simulation <- Sim.create Sim.defaultConfig

  -- Set nodes and links
  Sim.setNodes nodes simulation
  Sim.setLinks links simulation

  -- Set up tick handler - get current nodes each tick
  let tickHandler = do
        currentNodes <- Sim.getNodes simulation
        renderFn currentNodes
  Sim.onTick tickHandler simulation

  -- Create engine adapter
  let adapter = mkAdapter simulation
  engine <- Engine.createEngine adapter

  -- Track all node IDs as visible initially
  let nodeIds = Set.fromFoldable $ map (\n -> show n.id) nodes

  -- Create state ref
  stateRef <- Ref.new
    { simulation
    , engine
    , callbacks
    , allNodes: nodes
    , visibleNodeIds: nodeIds
    , nodesGroupId: config.nodesGroupId
    , linksGroupId: config.linksGroupId
    }

  pure $ SceneHandle stateRef

-- | Create an engine adapter for the simulation.
-- |
-- | This bridges the generic SceneEngine with our specific simulation.
mkAdapter
  :: forall nodeRow linkRow
   . Sim.Simulation nodeRow linkRow
  -> Engine.EngineAdapter (Sim.SimulationNode nodeRow)
mkAdapter sim =
  { getNodes: Sim.getNodes sim
  , capturePositions: capturePositions
  , interpolatePositions: \start target progress ->
      Sim.interpolatePositionsInPlace start target progress sim
  , updatePositions: \positions ->
      Sim.updatePositionsInPlace positions sim
  , applyRulesInPlace: \rules -> applyRulesInPlace rules sim
  , reinitializeForces: reinitializeForces sim
  , reheat: Sim.reheat sim
  }

-- | Capture current positions from nodes
capturePositions :: forall nodeRow. Array (Sim.SimulationNode nodeRow) -> Types.PositionMap
capturePositions nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes

-- | Apply rules in place (mutates simulation nodes)
-- | Uses FFI to preserve object identity for D3 data binding
applyRulesInPlace
  :: forall nodeRow linkRow
   . Array (Types.NodeRule (Sim.SimulationNode nodeRow))
  -> Sim.Simulation nodeRow linkRow
  -> Effect Unit
applyRulesInPlace rules sim = do
  let _ = SimScene.applyRulesInPlace_ rules sim.nodes
  pure unit

-- | Re-initialize all forces after node data changes
reinitializeForces :: forall nodeRow linkRow. Sim.Simulation nodeRow linkRow -> Effect Unit
reinitializeForces sim = do
  nodes <- Ref.read sim.nodes
  forces <- Ref.read sim.forces
  for_ (Map.toUnfoldable forces :: Array (Tuple String Core.ForceHandle)) \(Tuple _ handle) -> do
    _ <- Core.initializeForce handle nodes
    pure unit

-- | Clean up resources.
-- |
-- | Stops the simulation and releases references.
destroy :: forall nodeRow linkRow. SceneHandle nodeRow linkRow -> Effect Unit
destroy (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  Sim.stop state.simulation

-- =============================================================================
-- Scene Transitions
-- =============================================================================

-- | Transition to a new scene.
-- |
-- | Triggers the three-phase lifecycle:
-- | 1. Init rules - prepare starting state (e.g., move nodes to root)
-- | 2. Interpolation - animate positions from start to target
-- | 3. Final rules - set up stable state (e.g., pin/unpin nodes)
-- |
-- | The transition is driven by `onTick` calls.
transitionTo
  :: forall nodeRow linkRow
   . Types.SceneConfig (Sim.SimulationNode nodeRow)
  -> SceneHandle nodeRow linkRow
  -> Effect Unit
transitionTo scene (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  Engine.transitionTo scene state.engine
  -- Reheat to drive the transition
  Sim.reheat state.simulation

-- | Check if a transition is currently in progress.
isTransitioning :: forall nodeRow linkRow. SceneHandle nodeRow linkRow -> Effect Boolean
isTransitioning (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  Engine.isTransitioning state.engine

-- | Get transition progress (0.0 to 1.0).
-- |
-- | Returns Nothing if no transition is in progress.
getTransitionProgress
  :: forall nodeRow linkRow
   . SceneHandle nodeRow linkRow
  -> Effect (Maybe Number)
getTransitionProgress (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  Engine.getTransitionProgress state.engine

-- | Get the current scene configuration (if any).
getCurrentScene
  :: forall nodeRow linkRow
   . SceneHandle nodeRow linkRow
  -> Effect (Maybe (Types.SceneConfig (Sim.SimulationNode nodeRow)))
getCurrentScene (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  Engine.getCurrentScene state.engine

-- =============================================================================
-- Node Operations
-- =============================================================================

-- | Get all nodes (with current positions).
getAllNodes
  :: forall nodeRow linkRow
   . SceneHandle nodeRow linkRow
  -> Effect (Array (Sim.SimulationNode nodeRow))
getAllNodes (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  Sim.getNodes state.simulation

-- | Filter visible nodes by predicate.
-- |
-- | Nodes that don't match are hidden (but kept in simulation).
-- | Use `showAllNodes` to restore.
-- |
-- | This is useful for neighborhood/focus views.
filterNodes
  :: forall nodeRow linkRow
   . (Sim.SimulationNode nodeRow -> Boolean)
  -> SceneHandle nodeRow linkRow
  -> Effect Unit
filterNodes predicate (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  nodes <- Sim.getNodes state.simulation
  let visibleIds = Set.fromFoldable $
        Array.mapMaybe (\n -> if predicate n then Just (show n.id) else Nothing) nodes
  Ref.modify_ (_ { visibleNodeIds = visibleIds }) stateRef

-- | Show all nodes (restore from filter).
showAllNodes :: forall nodeRow linkRow. SceneHandle nodeRow linkRow -> Effect Unit
showAllNodes (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  nodes <- Sim.getNodes state.simulation
  let allIds = Set.fromFoldable $ map (\n -> show n.id) nodes
  Ref.modify_ (_ { visibleNodeIds = allIds }) stateRef

-- | Transform nodes in place.
-- |
-- | Applies a function to all nodes, mutating them directly.
-- | Useful for setting fx/fy (pinning) or updating positions.
-- |
-- | After transformation, forces are re-initialized and simulation reheated.
transformNodes
  :: forall nodeRow linkRow
   . (Sim.SimulationNode nodeRow -> Sim.SimulationNode nodeRow)
  -> SceneHandle nodeRow linkRow
  -> Effect Unit
transformNodes transform (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  -- Create a single rule that applies to all nodes
  let rule :: Types.NodeRule (Sim.SimulationNode nodeRow)
      rule = { name: "transform", select: const true, apply: transform }
  applyRulesInPlace [rule] state.simulation
  reinitializeForces state.simulation
  Sim.reheat state.simulation

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Tick handler - call this from your animation loop.
-- |
-- | Advances interpolation during transitions, then calls the render function.
-- | Returns `true` if animation should continue, `false` if stable.
-- |
-- | Typical usage:
-- | ```purescript
-- | let loop = do
-- |       continuing <- Scene.onTick handle
-- |       when continuing $ requestAnimationFrame (const loop)
-- | loop
-- | ```
onTick :: forall nodeRow linkRow. SceneHandle nodeRow linkRow -> Effect Boolean
onTick (SceneHandle stateRef) = do
  state <- Ref.read stateRef

  -- Delegate to engine (handles interpolation + rules)
  wasTransitioning <- Engine.isTransitioning state.engine
  stillTransitioning <- Engine.tick state.engine

  -- Notify callback if transition just completed
  when (wasTransitioning && not stillTransitioning) do
    state.callbacks.onTransitionComplete

  pure stillTransitioning

-- =============================================================================
-- Advanced: Direct Access
-- =============================================================================

-- | Get the underlying simulation (for advanced use cases).
-- |
-- | Use with caution - direct manipulation may interfere with scene transitions.
getSimulation
  :: forall nodeRow linkRow
   . SceneHandle nodeRow linkRow
  -> Effect (Sim.Simulation nodeRow linkRow)
getSimulation (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  pure state.simulation

-- | Reheat the simulation manually.
-- |
-- | Useful when you've made changes outside the normal transition flow.
reheat :: forall nodeRow linkRow. SceneHandle nodeRow linkRow -> Effect Unit
reheat (SceneHandle stateRef) = do
  state <- Ref.read stateRef
  Sim.reheat state.simulation
