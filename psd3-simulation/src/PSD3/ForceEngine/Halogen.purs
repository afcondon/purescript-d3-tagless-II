-- | Halogen Integration for Force Simulations
-- |
-- | Provides subscription-based API for integrating simulations with Halogen components.
-- |
-- | Usage:
-- | ```purescript
-- | handleAction Initialize = do
-- |   sim <- liftEffect $ Sim.create defaultConfig
-- |   emitter <- liftEffect $ subscribeToSimulation sim
-- |   void $ H.subscribe $ emitter <#> \event -> case event of
-- |     Sim.Tick -> UpdateNodePositions
-- |     Sim.Started -> SimStarted
-- |     Sim.Stopped -> SimStopped
-- |     Sim.AlphaDecayed alpha -> AlphaChanged alpha
-- | ```
module PSD3.ForceEngine.Halogen
  ( subscribeToSimulation
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Halogen.Subscription as HS
import PSD3.ForceEngine.Events (SimulationEvent(..), SimulationCallbacks)
import PSD3.ForceEngine.Simulation (Simulation, getCallbacks)
import Data.Maybe (Maybe(..))

-- | Create a Halogen subscription emitter for simulation events.
-- |
-- | This function wires up the simulation's callback system to emit
-- | Halogen-compatible events. The emitter can be used with `H.subscribe`.
-- |
-- | Note: The simulation must have been created with `createWithCallbacks`.
-- | If created with plain `create`, this function returns an emitter that
-- | never fires.
-- |
-- | Example:
-- | ```purescript
-- | -- In Halogen component Initialize:
-- | callbacks <- liftEffect defaultCallbacks
-- | sim <- liftEffect $ createWithCallbacks config callbacks
-- | emitter <- liftEffect $ subscribeToSimulation sim
-- | void $ H.subscribe $ emitter <#> SimulationEvent
-- |
-- | -- In handleAction:
-- | handleAction (SimulationEvent event) = case event of
-- |   Tick -> liftEffect updateDOMPositions
-- |   Started -> H.modify_ _ { simRunning = true }
-- |   Stopped -> H.modify_ _ { simRunning = false }
-- |   AlphaDecayed alpha -> when (alpha < 0.1) doSomething
-- | ```
subscribeToSimulation :: forall row linkRow.
  Simulation row linkRow
  -> Effect (HS.Emitter SimulationEvent)
subscribeToSimulation sim = do
  { emitter, listener } <- HS.create

  -- Wire up callbacks to emit events
  case getCallbacks sim of
    Nothing -> pure unit  -- No callbacks configured, emitter will never fire
    Just cbs -> wireUpCallbacks listener cbs

  pure emitter

-- | Internal: Wire up simulation callbacks to emit events
wireUpCallbacks :: HS.Listener SimulationEvent -> SimulationCallbacks -> Effect Unit
wireUpCallbacks listener cbs = do
  -- Wire tick callback
  Ref.write (HS.notify listener Tick) cbs.onTick

  -- Wire start callback
  Ref.write (HS.notify listener Started) cbs.onStart

  -- Wire stop callback
  Ref.write (HS.notify listener Stopped) cbs.onStop

  -- Wire alpha threshold callback
  Ref.write (\alpha -> HS.notify listener (AlphaDecayed alpha)) cbs.onAlphaThreshold
