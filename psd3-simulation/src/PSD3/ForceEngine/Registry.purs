-- | Simulation Registry
-- |
-- | Named simulation registry for debugging and coordination.
-- | Allows tracking multiple simulations by name, useful when:
-- | - Running multiple visualizations simultaneously
-- | - Debugging simulation behavior via browser console
-- | - Coordinating simulations (e.g., pausing main when popup opens)
-- |
-- | Usage:
-- | ```purescript
-- | -- Create and register a simulation
-- | sim <- create defaultConfig
-- | register "main-explorer" sim
-- |
-- | -- Later, from browser console or debug code
-- | mainSim <- lookup "main-explorer"
-- | case mainSim of
-- |   Just s -> stop s
-- |   Nothing -> pure unit
-- |
-- | -- List all registered simulations
-- | names <- listSimulations
-- | log $ "Running simulations: " <> show names
-- | ```
module PSD3.ForceEngine.Registry
  ( -- * Types
    AnySimulation
    -- * Registration
  , register
  , unregister
    -- * Lookup
  , lookup
  , listSimulations
    -- * Bulk Operations
  , stopAll
  , clearRegistry
    -- * Debug helpers (for browser console)
  , debugRegistry
    -- * Unsafe (for advanced use)
  , unsafeFromAny
  ) where

import Prelude

import Data.Array as Array
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import PSD3.ForceEngine.Simulation (Simulation, stop, isRunning, getAlpha)

-- =============================================================================
-- Registry Storage (module-level mutable state)
-- =============================================================================

-- | Internal registry storage.
-- | Uses existential encoding to store simulations with different row types.
-- | The trade-off is we can only perform operations that work on any simulation.
foreign import data AnySimulation :: Type

foreign import toAny :: forall row linkRow. Simulation row linkRow -> AnySimulation
foreign import unsafeFromAny :: forall row linkRow. AnySimulation -> Simulation row linkRow

-- | Global registry ref. Created once at module load time.
registryRef :: Ref (Map String AnySimulation)
registryRef = unsafePerformEffect $ Ref.new Map.empty

-- =============================================================================
-- Registration
-- =============================================================================

-- | Register a simulation with a name.
-- |
-- | If a simulation with this name already exists, it will be replaced.
-- | The old simulation is NOT stopped automatically.
-- |
-- | ```purescript
-- | sim <- create defaultConfig
-- | register "call-graph" sim
-- | ```
register :: forall row linkRow. String -> Simulation row linkRow -> Effect Unit
register name sim = do
  Ref.modify_ (Map.insert name (toAny sim)) registryRef

-- | Unregister a simulation by name.
-- |
-- | Does nothing if no simulation with this name exists.
-- | The simulation is NOT stopped automatically.
unregister :: String -> Effect Unit
unregister name = do
  Ref.modify_ (Map.delete name) registryRef

-- =============================================================================
-- Lookup
-- =============================================================================

-- | Look up a simulation by name.
-- |
-- | WARNING: The returned simulation has an unknown row type.
-- | Only use operations that work on any simulation (stop, isRunning, getAlpha).
-- | Calling getNodes will require an unsafe cast.
lookup :: String -> Effect (Maybe AnySimulation)
lookup name = do
  registry <- Ref.read registryRef
  pure $ Map.lookup name registry

-- | List all registered simulation names.
listSimulations :: Effect (Array String)
listSimulations = do
  registry <- Ref.read registryRef
  pure $ Array.fromFoldable $ Map.keys registry

-- =============================================================================
-- Bulk Operations
-- =============================================================================

-- | Stop all registered simulations.
-- |
-- | Useful when cleaning up, e.g., when navigating away from a page.
stopAll :: Effect Unit
stopAll = do
  registry <- Ref.read registryRef
  let sims = Map.values registry
  void $ traverse (\s -> stop (unsafeFromAny s :: Simulation () ())) sims

-- | Clear the registry without stopping simulations.
-- |
-- | Use `stopAll` first if you want to stop them.
clearRegistry :: Effect Unit
clearRegistry = Ref.write Map.empty registryRef

-- =============================================================================
-- Debug Helpers
-- =============================================================================

-- | Print debug information about all registered simulations.
-- |
-- | Designed for use from browser console:
-- | ```javascript
-- | // In browser console:
-- | import('./output/PSD3.ForceEngine.Registry/index.js').then(m => m.debugRegistry())
-- | ```
debugRegistry :: Effect Unit
debugRegistry = do
  registry <- Ref.read registryRef
  Console.log "=== Simulation Registry ==="
  if Map.isEmpty registry
    then Console.log "  (empty)"
    else do
      let entries = Map.toUnfoldable registry :: Array (Tuple String AnySimulation)
      void $ traverse debugEntry entries
  Console.log "=========================="
  where
  debugEntry (Tuple name anySim) = do
    let sim = unsafeFromAny anySim :: Simulation () ()
    running <- isRunning sim
    alpha <- getAlpha sim
    Console.log $ "  " <> name <> ": " <>
      (if running then "RUNNING" else "stopped") <>
      " (alpha=" <> show alpha <> ")"
