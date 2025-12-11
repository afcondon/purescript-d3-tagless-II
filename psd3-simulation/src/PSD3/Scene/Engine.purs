-- | Scene Engine
-- |
-- | The orchestrator for scene transitions and stable states.
-- |
-- | Two modes of operation:
-- | - **Interpolation Engine**: During transitions, smoothly moves nodes
-- |   from start to target positions with configurable easing.
-- | - **Physics Engine**: After transitions complete (in Physics mode),
-- |   delegates to the D3 force simulation.
-- |
-- | The engine is generic over both node type and simulation type.
-- | Apps provide adapter functions so the engine doesn't need to know
-- | about simulation internals.
-- |
-- | Example usage:
-- | ```purescript
-- | engine <- createEngine
-- |   { getNodes: Sim.getNodes mySimulation
-- |   , setNodes: \nodes -> Sim.setNodes nodes mySimulation
-- |   , interpolatePositions: \start target progress ->
-- |       Sim.interpolatePositionsInPlace start target progress mySimulation
-- |   , updatePositions: \positions ->
-- |       Sim.updatePositionsInPlace positions mySimulation
-- |   , applyRulesInPlace: \rules ->
-- |       Sim.applyRulesInPlace rules mySimulation
-- |   , reinitializeForces: reinitMyForces
-- |   , reheat: Sim.reheat mySimulation
-- |   }
-- |
-- | transitionTo treeFormScene engine
-- | ```
module PSD3.Scene.Engine
  ( -- * Engine Type
    SceneEngine
  , EngineAdapter
  -- * Creation
  , createEngine
  -- * Operations
  , transitionTo
  , tick
  , tickWithDelta
  -- * Queries
  , getCurrentScene
  , isTransitioning
  , getTransitionProgress
  -- * Re-exports
  , module Types
  ) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.Scene.Transition (calculateProgress, isComplete)
import PSD3.Scene.Types
  ( SceneConfig
  , TransitionConfig
  , TransitionState
  , EngineMode(..)
  , PositionMap
  , NodeRule
  ) as Types

-- =============================================================================
-- Engine Adapter
-- =============================================================================

-- | Adapter functions that connect the engine to your simulation.
-- |
-- | The engine is generic - it doesn't know about `Sim.Simulation` directly.
-- | Instead, you provide these adapter functions that do the actual work.
-- |
-- | This keeps the engine decoupled and theoretically usable with
-- | non-D3 simulations.
type EngineAdapter node =
  { -- | Get current nodes from simulation
    getNodes :: Effect (Array node)

  -- | Capture positions from nodes as a PositionMap
  -- | The adapter knows how to extract id, x, y from your node type
  , capturePositions :: Array node -> Types.PositionMap

  -- | Interpolate positions between start and target
  -- | Parameters: startPositions, targetPositions, progress (0-1)
  , interpolatePositions :: Types.PositionMap -> Types.PositionMap -> Number -> Effect Unit

  -- | Set positions directly (used when transition completes)
  , updatePositions :: Types.PositionMap -> Effect Unit

  -- | Apply rules to nodes in place
  , applyRulesInPlace :: Array (Types.NodeRule node) -> Effect Unit

  -- | Re-initialize forces after node data changes
  -- | D3 forces cache values, so we need to tell them to re-read
  , reinitializeForces :: Effect Unit

  -- | Reheat the simulation (restart force calculations)
  , reheat :: Effect Unit
  }

-- =============================================================================
-- Engine State
-- =============================================================================

-- | Internal engine state
type EngineState node =
  { currentScene :: Maybe (Types.SceneConfig node)
  , transition :: Maybe (Types.TransitionState node)
  }

-- | Initial (empty) engine state
initialState :: forall node. EngineState node
initialState =
  { currentScene: Nothing
  , transition: Nothing
  }

-- =============================================================================
-- Scene Engine
-- =============================================================================

-- | Opaque handle to the scene engine.
-- |
-- | Create with `createEngine`, then use `transitionTo` and `tick`.
newtype SceneEngine node = SceneEngine
  { adapter :: EngineAdapter node
  , stateRef :: Ref (EngineState node) -- see note in footer about use of Ref
  }

-- =============================================================================
-- Creation
-- =============================================================================

-- | Create a new scene engine.
-- |
-- | The engine starts with no active scene. Call `transitionTo` to
-- | begin your first scene transition.
createEngine
  :: forall node
   . EngineAdapter node
  -> Effect (SceneEngine node)
createEngine adapter = do
  stateRef <- Ref.new initialState
  pure $ SceneEngine { adapter, stateRef }

-- =============================================================================
-- Operations
-- =============================================================================

-- | Start a transition to a new scene.
-- |
-- | If a transition is already in progress, this is ignored.
-- | (Consider: should we allow interrupting transitions?)
-- |
-- | The transition lifecycle:
-- | 1. Apply scene's `initRules` to prepare starting state
-- | 2. Capture current positions as start positions
-- | 3. Calculate target positions via scene's `layout` function
-- | 4. Begin interpolation (handled by `tick`)
-- |
-- | Note: The node type must have `id`, `x`, `y` fields for position capture.
-- | This is enforced at the call site when constructing the EngineAdapter.
transitionTo
  :: forall node
   . Types.SceneConfig node
  -> SceneEngine node
  -> Effect Unit
transitionTo targetScene (SceneEngine { adapter, stateRef }) = do
  state <- Ref.read stateRef

  -- Don't transition if already transitioning
  case state.transition of
    Just _ -> pure unit -- Already transitioning, ignore
    Nothing -> do
      -- Phase 1: Apply init rules
      adapter.applyRulesInPlace targetScene.initRules

      -- Capture positions AFTER init rules (nodes may have moved)
      nodesAfterInit <- adapter.getNodes
      let startPositions = adapter.capturePositions nodesAfterInit

      -- Calculate target positions
      let targetPositions = targetScene.layout nodesAfterInit

      -- Set transition state
      Ref.write
        { currentScene: state.currentScene
        , transition: Just
            { targetScene
            , startPositions
            , targetPositions
            , progress: 0.0
            , elapsed: 0.0
            }
        }
        stateRef

      -- Start/continue the simulation tick loop
      adapter.reheat

-- | Advance the engine by one tick.
-- |
-- | Call this from your simulation's tick handler.
-- | Uses a default delta of ~16ms (60fps).
-- |
-- | Returns `true` if a transition is in progress, `false` if stable.
tick
  :: forall node
   . SceneEngine node
  -> Effect Boolean
tick = tickWithDelta 16.67 -- ~60fps

-- | Advance the engine by a specified time delta.
-- |
-- | Use this if you have actual frame timing available.
-- |
-- | Returns `true` if a transition is in progress, `false` if stable.
tickWithDelta
  :: forall node
   . Number -- ^ Delta time in milliseconds
  -> SceneEngine node
  -> Effect Boolean
tickWithDelta deltaMs (SceneEngine { adapter, stateRef }) = do
  state <- Ref.read stateRef

  case state.transition of
    Just t -> do
      runInterpolationEngine deltaMs t adapter stateRef state
      pure true

    Nothing -> do
      runPhysicsEngine state
      pure false

-- =============================================================================
-- Interpolation Engine (during transitions)
-- =============================================================================

-- | Run the interpolation engine for one tick.
runInterpolationEngine
  :: forall node
   . Number -- ^ Delta time
  -> Types.TransitionState node
  -> EngineAdapter node
  -> Ref (EngineState node)
  -> EngineState node
  -> Effect Unit
runInterpolationEngine deltaMs t adapter stateRef state = do
  let newElapsed = t.elapsed + deltaMs
  let config = t.targetScene.transition

  if isComplete config newElapsed then completeTransition t adapter stateRef state
  else do
    -- Calculate eased progress
    let easedProgress = calculateProgress config newElapsed

    -- Interpolate positions
    adapter.interpolatePositions t.startPositions t.targetPositions easedProgress

    -- Update state with new elapsed time
    Ref.write
      (state { transition = Just (t { elapsed = newElapsed, progress = easedProgress }) })
      stateRef

-- | Complete a transition and enter stable mode.
completeTransition
  :: forall node
   . Types.TransitionState node
  -> EngineAdapter node
  -> Ref (EngineState node)
  -> EngineState node
  -> Effect Unit
completeTransition t adapter stateRef _state = do
  -- Set final positions
  adapter.updatePositions t.targetPositions

  -- Phase 3: Apply final rules
  nodes <- adapter.getNodes
  let rules = t.targetScene.finalRules nodes
  adapter.applyRulesInPlace rules

  -- Re-initialize forces (they cache values)
  adapter.reinitializeForces

  -- Enter stable mode
  case t.targetScene.stableMode of
    Types.Physics -> adapter.reheat
    Types.Static -> pure unit

  -- Update state: transition complete, scene is current
  Ref.write
    { currentScene: Just t.targetScene
    , transition: Nothing
    }
    stateRef

-- =============================================================================
-- Physics Engine (stable state)
-- =============================================================================

-- | Run the physics engine (stable state).
-- |
-- | Currently a no-op - the D3 simulation runs itself.
-- | This function exists as a hook for future extensions.
runPhysicsEngine
  :: forall node
   . EngineState node
  -> Effect Unit
runPhysicsEngine _state = pure unit

-- =============================================================================
-- Queries
-- =============================================================================

-- | Get the current scene configuration (if any).
getCurrentScene
  :: forall node
   . SceneEngine node
  -> Effect (Maybe (Types.SceneConfig node))
getCurrentScene (SceneEngine { stateRef }) = do
  state <- Ref.read stateRef
  pure state.currentScene

-- | Check if a transition is in progress.
isTransitioning
  :: forall node
   . SceneEngine node
  -> Effect Boolean
isTransitioning (SceneEngine { stateRef }) = do
  state <- Ref.read stateRef
  pure $ isJust state.transition

-- | Get the current transition progress (0.0 to 1.0).
-- |
-- | Returns `Nothing` if no transition is in progress.
getTransitionProgress
  :: forall node
   . SceneEngine node
  -> Effect (Maybe Number)
getTransitionProgress (SceneEngine { stateRef }) = do
  state <- Ref.read stateRef
  pure $ map _.progress state.transition

-- | Footer note about Ref
--| Why Ref Instead of State Monad?

--| The Ref is justified here. Looking at the tick function signature:

--| tick :: forall node. SceneEngine node -> Effect Boolean

--| The key constraint is that ticks are invoked by D3's internal timer (requestAnimationFrame), not by
--|  your code in a continuous monadic computation. The engine state must persist across:

--| 1. Multiple separate Effect invocations - each tick is its own Effect call
--| 2. External timing - D3 controls when ticks happen, not you
--| 3. Adapter functions in Effect - getNodes, interpolatePositions etc. are all effectful

--| Alternative Design (State Monad)

--| -- Would require:
--| tick :: EngineState node -> Effect (Tuple Boolean (EngineState node))

--| Problems with this:
--| 1. Caller must thread state through every tick callback manually
--| 2. No encapsulation - state management leaks to every call site
--| 3. Risk of desync - caller might forget to use updated state
--| 4. No actual purity gain - still mutable state, just externalized

--| The Ref Advantage

--| The Ref encapsulates mutable state within the engine handle:
--| - Clean API: just call tick engine
--| - Engine manages its own transition progress
--| - Caller doesn't need to know about internal state shape
--| - Mirrors D3's own design (simulation objects are mutable)

--| Bottom line: Ref is the right choice when state must persist across multiple effectful callbacks
--| driven by external timing (like requestAnimationFrame loops). State/StateT is better when you
--| control the entire computation sequence in a single monadic run.