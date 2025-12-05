-- | Simulation Events
-- |
-- | Event types and callback system for force simulations.
-- | Designed for integration with UI frameworks like Halogen.
-- |
-- | Key design principles:
-- | 1. Callbacks are `Effect Unit` - no monadic overhead in hot path
-- | 2. All callbacks are optional (stored in Refs, default to no-op)
-- | 3. Event types are for documentation/Halogen integration, not for internal use
module PSD3.ForceEngine.Events
  ( -- * Event Types (for Halogen integration)
    SimulationEvent(..)
    -- * Callback Configuration
  , SimulationCallbacks
  , defaultCallbacks
    -- * Callback Setters (individual, for flexibility)
  , onSimulationTick
  , onSimulationStart
  , onSimulationStop
  , onAlphaDecay
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- =============================================================================
-- Event Types
-- =============================================================================

-- | Events that can occur during simulation lifecycle.
-- |
-- | These types are primarily for Halogen integration via subscriptions.
-- | Internally, callbacks are invoked directly without constructing these values.
-- |
-- | Usage with Halogen:
-- | ```purescript
-- | handleAction (SimEvent event) = case event of
-- |   Tick -> liftEffect updateDOMPositions
-- |   Started -> H.modify_ _ { simRunning = true }
-- |   Stopped -> H.modify_ _ { simRunning = false }
-- |   AlphaDecayed alpha -> when (alpha < 0.1) $ liftEffect doSomething
-- | ```
data SimulationEvent
  = Tick              -- ^ Called every animation frame while simulation runs
  | Started           -- ^ Simulation started (or reheated from stopped state)
  | Stopped           -- ^ Simulation cooled down and stopped
  | AlphaDecayed Number  -- ^ Alpha crossed a threshold (e.g., 0.5, 0.1, 0.01)

derive instance Eq SimulationEvent

instance Show SimulationEvent where
  show Tick = "Tick"
  show Started = "Started"
  show Stopped = "Stopped"
  show (AlphaDecayed a) = "AlphaDecayed " <> show a

-- =============================================================================
-- Callback Configuration
-- =============================================================================

-- | Callbacks for simulation events.
-- |
-- | All callbacks are stored in Refs to allow dynamic updates and to keep
-- | the tick loop allocation-free (no closures created per-tick).
-- |
-- | The callbacks are `Effect Unit` rather than taking event data because:
-- | 1. Performance: No boxing/unboxing of event data in hot path
-- | 2. Simplicity: Consumers can read simulation state directly if needed
-- | 3. Flexibility: The consumer decides what data they need
type SimulationCallbacks =
  { onTick :: Ref (Effect Unit)
      -- ^ Called every tick. Use for DOM updates (updateGroupPositions).
      -- ^ WARNING: Keep this callback fast! Runs 60x/second.
  , onStart :: Ref (Effect Unit)
      -- ^ Called when simulation starts or is reheated from stopped state.
  , onStop :: Ref (Effect Unit)
      -- ^ Called when simulation stops (alpha < alphaMin).
  , onAlphaThreshold :: Ref (Number -> Effect Unit)
      -- ^ Called when alpha crosses thresholds (0.5, 0.1, 0.01).
      -- ^ Useful for progressive rendering or UI updates.
  }

-- | Create default callbacks (all no-ops).
-- |
-- | Usage:
-- | ```purescript
-- | callbacks <- defaultCallbacks
-- | Ref.write updateDOMPositions callbacks.onTick
-- | sim <- createWithCallbacks config callbacks
-- | ```
defaultCallbacks :: Effect SimulationCallbacks
defaultCallbacks = do
  tickRef <- Ref.new (pure unit)
  startRef <- Ref.new (pure unit)
  stopRef <- Ref.new (pure unit)
  alphaRef <- Ref.new (\_ -> pure unit)
  pure
    { onTick: tickRef
    , onStart: startRef
    , onStop: stopRef
    , onAlphaThreshold: alphaRef
    }

-- =============================================================================
-- Callback Setters
-- =============================================================================

-- | Set the tick callback.
-- |
-- | WARNING: This callback runs 60x/second during simulation.
-- | Keep it fast! Avoid allocations, complex logic, or console.log.
-- |
-- | Good:
-- | ```purescript
-- | onSimulationTick (updateGroupPositions nodes) callbacks
-- | ```
-- |
-- | Bad:
-- | ```purescript
-- | onSimulationTick (do
-- |   nodes <- Ref.read nodesRef  -- Extra ref read
-- |   log "tick"                   -- Console I/O
-- |   H.tell ...                   -- Halogen message (allocates)
-- | ) callbacks
-- | ```
onSimulationTick :: Effect Unit -> SimulationCallbacks -> Effect Unit
onSimulationTick callback cbs = Ref.write callback cbs.onTick

-- | Set the start callback.
-- |
-- | Called when simulation starts running or is reheated from stopped state.
-- | Safe to do heavier work here (only called occasionally).
onSimulationStart :: Effect Unit -> SimulationCallbacks -> Effect Unit
onSimulationStart callback cbs = Ref.write callback cbs.onStart

-- | Set the stop callback.
-- |
-- | Called when simulation stops (alpha < alphaMin).
-- | Safe to do heavier work here.
onSimulationStop :: Effect Unit -> SimulationCallbacks -> Effect Unit
onSimulationStop callback cbs = Ref.write callback cbs.onStop

-- | Set the alpha threshold callback.
-- |
-- | Called when alpha crosses common thresholds: 0.5, 0.1, 0.01.
-- | The callback receives the new alpha value.
-- |
-- | Useful for:
-- | - Progressive rendering (show more detail as simulation settles)
-- | - UI updates (e.g., hide "Simulating..." spinner)
-- | - Analytics (track how long simulations run)
onAlphaDecay :: (Number -> Effect Unit) -> SimulationCallbacks -> Effect Unit
onAlphaDecay callback cbs = Ref.write callback cbs.onAlphaThreshold
