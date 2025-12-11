-- | Scene Engine
-- |
-- | Tick-based scene orchestration with adapter pattern.
-- |
-- | The engine manages transitions between scenes using a three-phase lifecycle:
-- | 1. Initialize: Apply init rules to prepare starting state
-- | 2. Transition: Interpolate positions from start to target
-- | 3. Finalize: Apply final rules and enter stable mode
-- |
-- | The engine is generic over the node type. Apps provide an adapter
-- | record with functions that interact with their specific simulation.
-- |
-- | Example usage:
-- | ```purescript
-- | -- Create adapter for your simulation
-- | adapter = mkAdapter mySimulation
-- |
-- | -- Create engine
-- | engineRef <- createEngine adapter
-- |
-- | -- Transition to a scene
-- | transitionTo treeFormScene engineRef
-- |
-- | -- Call tick from your simulation's tick handler
-- | Sim.onTick (\_ -> tick engineRef) mySimulation
-- | ```
module PSD3.Scene.Engine
  ( -- * Engine Types
    SceneEngine
  , EngineState  -- Required for type synonym
  , EngineAdapter
  -- * Creation
  , createEngine
  -- * Operations
  , transitionTo
  , tick
  -- * Queries
  , getCurrentScene
  , isTransitioning
  , getTransitionProgress
  -- * Configuration
  , defaultTransitionDelta
  -- * Re-exports
  , module Types
  ) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.Scene.Types
  ( SceneConfig
  , TransitionState
  , EngineMode(..)
  , PositionMap
  , NodeRule
  ) as Types
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Engine Adapter
-- =============================================================================

-- | Adapter functions that connect the engine to your simulation.
-- |
-- | The engine is generic - it doesn't know about `Sim.Simulation` directly.
-- | Instead, you provide these adapter functions that do the actual work.
-- |
-- | Example adapter for ce-website:
-- | ```purescript
-- | mkAdapter :: CESimulation -> EngineAdapter SimNode
-- | mkAdapter sim =
-- |   { getNodes: Sim.getNodes sim
-- |   , capturePositions: \nodes -> Object.fromFoldable $
-- |       map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes
-- |   , interpolatePositions: \start target progress ->
-- |       Sim.interpolatePositionsInPlace start target progress sim
-- |   , updatePositions: \positions ->
-- |       Sim.updatePositionsInPlace positions sim
-- |   , applyRulesInPlace: \rules ->
-- |       applyRulesInPlace_ rules sim.nodes
-- |   , reinitializeForces: reinitForcesFor sim
-- |   , reheat: Sim.reheat sim
-- |   }
-- | ```
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
  { adapter :: EngineAdapter node
  , currentScene :: Maybe (Types.SceneConfig node)
  , transition :: Maybe (Types.TransitionState node)
  , transitionDelta :: Tick.TickDelta  -- Progress increment per tick
  }

-- | The scene engine, stored in a Ref for mutation across tick callbacks
type SceneEngine node = Ref (EngineState node)

-- =============================================================================
-- Configuration
-- =============================================================================

-- | Default transition delta: ~2 seconds at 60fps
defaultTransitionDelta :: Tick.TickDelta
defaultTransitionDelta = Tick.ticksForDuration 2000

-- =============================================================================
-- Creation
-- =============================================================================

-- | Create a new scene engine.
-- |
-- | The engine starts with no active scene. Call `transitionTo` to
-- | begin your first scene transition.
-- |
-- | Uses default transition duration of ~2 seconds.
createEngine
  :: forall node
   . EngineAdapter node
  -> Effect (SceneEngine node)
createEngine adapter = Ref.new
  { adapter
  , currentScene: Nothing
  , transition: Nothing
  , transitionDelta: defaultTransitionDelta
  }

-- =============================================================================
-- Operations
-- =============================================================================

-- | Start a transition to a new scene.
-- |
-- | If a transition is already in progress, this is ignored.
-- |
-- | The transition lifecycle:
-- | 1. Apply scene's `initRules` to prepare starting state
-- | 2. Capture current positions as start positions
-- | 3. Calculate target positions via scene's `layout` function
-- | 4. Begin interpolation (handled by `tick`)
transitionTo
  :: forall node
   . Types.SceneConfig node
  -> SceneEngine node
  -> Effect Unit
transitionTo targetScene engineRef = do
  state <- Ref.read engineRef

  -- Don't transition if already transitioning
  case state.transition of
    Just _ -> pure unit -- Already transitioning, ignore
    Nothing -> do
      -- Phase 1: Apply init rules
      state.adapter.applyRulesInPlace targetScene.initRules

      -- Capture positions AFTER init rules (nodes may have moved)
      nodesAfterInit <- state.adapter.getNodes
      let startPositions = state.adapter.capturePositions nodesAfterInit

      -- Calculate target positions
      let targetPositions = targetScene.layout nodesAfterInit

      -- Set transition state
      Ref.write
        ( state
            { transition = Just
                { targetScene
                , startPositions
                , targetPositions
                , progress: 0.0
                }
            }
        )
        engineRef

      -- Start/continue the simulation tick loop
      state.adapter.reheat

-- | Advance the engine by one tick.
-- |
-- | Call this from your simulation's tick handler.
-- |
-- | Returns `true` if a transition is in progress, `false` if stable.
tick
  :: forall node
   . SceneEngine node
  -> Effect Boolean
tick engineRef = do
  state <- Ref.read engineRef

  case state.transition of
    Just t -> do
      runInterpolationEngine t engineRef state
      pure true

    Nothing -> do
      runStableEngine state
      pure false

-- =============================================================================
-- Interpolation Engine (during transitions)
-- =============================================================================

-- | Run the interpolation engine for one tick.
runInterpolationEngine
  :: forall node
   . Types.TransitionState node
  -> SceneEngine node
  -> EngineState node
  -> Effect Unit
runInterpolationEngine t engineRef state = do
  let newProgress = min 1.0 (t.progress + state.transitionDelta)

  if newProgress >= 1.0
    then completeTransition t engineRef state
    else do
      -- Calculate eased progress
      let easedProgress = Tick.easeInOutCubic newProgress

      -- Interpolate positions
      state.adapter.interpolatePositions t.startPositions t.targetPositions easedProgress

      -- Update state with new progress
      Ref.write
        (state { transition = Just (t { progress = newProgress }) })
        engineRef

-- | Complete a transition and enter stable mode.
completeTransition
  :: forall node
   . Types.TransitionState node
  -> SceneEngine node
  -> EngineState node
  -> Effect Unit
completeTransition t engineRef state = do
  -- Set final positions
  state.adapter.updatePositions t.targetPositions

  -- Phase 3: Apply final rules
  nodes <- state.adapter.getNodes
  let rules = t.targetScene.finalRules nodes
  state.adapter.applyRulesInPlace rules

  -- Re-initialize forces (they cache values)
  state.adapter.reinitializeForces

  -- Enter stable mode
  case t.targetScene.stableMode of
    Types.Physics -> state.adapter.reheat
    Types.Static -> pure unit

  -- Update state: transition complete, scene is current
  Ref.write
    ( state
        { currentScene = Just t.targetScene
        , transition = Nothing
        }
    )
    engineRef

-- =============================================================================
-- Stable Engine (after transitions)
-- =============================================================================

-- | Run the stable engine (no-op currently).
-- |
-- | In Physics mode, D3 runs the simulation.
-- | In Static mode, nothing needs to happen.
runStableEngine
  :: forall node
   . EngineState node
  -> Effect Unit
runStableEngine _state = pure unit

-- =============================================================================
-- Queries
-- =============================================================================

-- | Get the current scene configuration (if any).
getCurrentScene
  :: forall node
   . SceneEngine node
  -> Effect (Maybe (Types.SceneConfig node))
getCurrentScene engineRef = do
  state <- Ref.read engineRef
  pure state.currentScene

-- | Check if a transition is in progress.
isTransitioning
  :: forall node
   . SceneEngine node
  -> Effect Boolean
isTransitioning engineRef = do
  state <- Ref.read engineRef
  pure $ isJust state.transition

-- | Get the current transition progress (0.0 to 1.0).
-- |
-- | Returns `Nothing` if no transition is in progress.
getTransitionProgress
  :: forall node
   . SceneEngine node
  -> Effect (Maybe Number)
getTransitionProgress engineRef = do
  state <- Ref.read engineRef
  pure $ map _.progress state.transition
