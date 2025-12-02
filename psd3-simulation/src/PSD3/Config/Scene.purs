-- | Simulation Setup Configuration Module
-- |
-- | A SimulationSetup represents a complete D3 simulation configuration:
-- | - Which forces are active
-- | - Simulation parameters (alpha, decay rates, etc.)
-- |
-- | Setups are composable and can be merged, filtered, or transformed.
module PSD3.Config.Scene
  ( Label
  , SimulationParams
  , defaultSimParams
  , fastSimParams
  , slowSimParams
  , SimulationSetup(..)
  , setup
  , setupWithParams
  , withDescription
  , withSimParams
  , withAlpha
  , withAlphaTarget
  , addForce
  , removeForce
  , keepForces
  , excludeForces
  , replaceForce
  , updateForce
  , mergeSetups
  , getForce
  , hasForce
  , forceNames
  , forceCount
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import PSD3.Config.Force (ForceConfig(..))
-- Label is just String
type Label = String

-- | Simulation-level parameters
-- | These control the physics engine behavior
type SimulationParams = {
    alpha         :: Number  -- Current energy level (1.0 = max, 0.0 = stopped)
  , alphaTarget   :: Number  -- Target energy (simulation runs until alpha ≤ alphaTarget)
  , alphaMin      :: Number  -- Minimum alpha before stopping
  , alphaDecay    :: Number  -- How quickly energy dissipates per tick
  , velocityDecay :: Number  -- Friction/damping factor for node velocities
}

-- | Default simulation parameters (matches d3-force defaults)
defaultSimParams :: SimulationParams
defaultSimParams = {
    alpha: 1.0
  , alphaTarget: 0.0
  , alphaMin: 0.001
  , alphaDecay: 0.0228     -- ~300 iterations to reach alphaMin
  , velocityDecay: 0.4     -- Moderate friction
}

-- | Fast convergence parameters (for quick transitions)
fastSimParams :: SimulationParams
fastSimParams = defaultSimParams {
    alphaDecay = 0.05      -- ~100 iterations
  , velocityDecay = 0.6    -- Higher friction
}

-- | Slow convergence parameters (for smooth, gradual layouts)
slowSimParams :: SimulationParams
slowSimParams = defaultSimParams {
    alphaDecay = 0.01      -- ~500 iterations
  , velocityDecay = 0.2    -- Lower friction
}

-- | Complete scene configuration
newtype SimulationSetup = SimulationSetup {
    name        :: String              -- Scene name (for debugging/display)
  , description :: Maybe String        -- Optional description
  , forces      :: Array ForceConfig   -- Active forces in this scene
  , simParams   :: SimulationParams    -- Simulation parameters
}

derive instance Newtype SimulationSetup _

-- | Create a setup with default simulation parameters
setup :: String -> Array ForceConfig -> SimulationSetup
setup name forces = SimulationSetup {
    name
  , description: Nothing
  , forces
  , simParams: defaultSimParams
}

-- | Create a setup with custom simulation parameters
setupWithParams :: String -> Array ForceConfig -> SimulationParams -> SimulationSetup
setupWithParams name forces simParams = SimulationSetup {
    name
  , description: Nothing
  , forces
  , simParams
}

-- | Add description to a setup
withDescription :: String -> SimulationSetup -> SimulationSetup
withDescription desc = over SimulationSetup (_ { description = Just desc })

-- | Update simulation parameters
withSimParams :: SimulationParams -> SimulationSetup -> SimulationSetup
withSimParams params = over SimulationSetup (_ { simParams = params })

-- | Set alpha (energy level) for a setup
withAlpha :: Number -> SimulationSetup -> SimulationSetup
withAlpha alpha (SimulationSetup config) = SimulationSetup $ config { simParams = config.simParams { alpha = alpha } }

-- | Set alpha target (simulation runs until alpha ≤ target)
withAlphaTarget :: Number -> SimulationSetup -> SimulationSetup
withAlphaTarget target (SimulationSetup config) = SimulationSetup $ config { simParams = config.simParams { alphaTarget = target } }

-- =============================================================================
-- Setup Composition
-- =============================================================================

-- | Add a force to a setup
addForce :: ForceConfig -> SimulationSetup -> SimulationSetup
addForce force (SimulationSetup config) = SimulationSetup $ config { forces = A.snoc config.forces force }

-- | Remove a force from a setup by name
removeForce :: Label -> SimulationSetup -> SimulationSetup
removeForce name (SimulationSetup config) = SimulationSetup $ config { forces = A.filter (\(ForceConfig f) -> f.name /= name) config.forces }

-- | Keep only specified forces
keepForces :: Array Label -> SimulationSetup -> SimulationSetup
keepForces names (SimulationSetup config) = SimulationSetup $ config { forces = A.filter (\(ForceConfig f) -> f.name `A.elem` names) config.forces }

-- | Remove specified forces
excludeForces :: Array Label -> SimulationSetup -> SimulationSetup
excludeForces names (SimulationSetup config) = SimulationSetup $ config { forces = A.filter (\(ForceConfig f) -> not (f.name `A.elem` names)) config.forces }

-- | Replace a force in a setup (by name)
replaceForce :: ForceConfig -> SimulationSetup -> SimulationSetup
replaceForce newForce (SimulationSetup config) =
  let (ForceConfig nf) = newForce
  in SimulationSetup $ config {
    forces = map (\f@(ForceConfig fc) -> if fc.name == nf.name then newForce else f) config.forces
  }

-- | Update a force in a setup using a transformation function
updateForce :: Label -> (ForceConfig -> ForceConfig) -> SimulationSetup -> SimulationSetup
updateForce name fn (SimulationSetup config) = SimulationSetup $ config {
  forces = map (\f@(ForceConfig fc) -> if fc.name == name then fn f else f) config.forces
}

-- | Merge two setups
-- | Forces from the second setup override those with the same name from the first
-- | Simulation params from the second setup are used
mergeSetups :: SimulationSetup -> SimulationSetup -> SimulationSetup
mergeSetups (SimulationSetup s1) (SimulationSetup s2) =
  let
    setup2ForceNames = map (\(ForceConfig f) -> f.name) s2.forces
    -- Get forces from setup1 that aren't in setup2
    uniqueFromSetup1 = A.filter (\(ForceConfig f) -> not (f.name `A.elem` setup2ForceNames)) s1.forces
  in SimulationSetup {
    name: s1.name <> "+" <> s2.name
  , description: Nothing
  , forces: uniqueFromSetup1 <> s2.forces
  , simParams: s2.simParams
  }

-- =============================================================================
-- Query Functions
-- =============================================================================

-- | Get a force from a setup by name
getForce :: Label -> SimulationSetup -> Maybe ForceConfig
getForce name (SimulationSetup config) = A.find (\(ForceConfig f) -> f.name == name) config.forces

-- | Check if a setup has a force with the given name
hasForce :: Label -> SimulationSetup -> Boolean
hasForce name (SimulationSetup config) = A.any (\(ForceConfig f) -> f.name == name) config.forces

-- | Get all force names in a setup
forceNames :: SimulationSetup -> Array Label
forceNames (SimulationSetup config) = map (\(ForceConfig fc) -> fc.name) config.forces

-- | Count forces in a setup
forceCount :: SimulationSetup -> Int
forceCount (SimulationSetup config) = A.length config.forces

-- =============================================================================
-- Display
-- =============================================================================

instance Show SimulationSetup where
  show sc@(SimulationSetup config) =
    "Scene \"" <> config.name <> "\" [" <> show (forceCount sc) <> " forces]: " <>
    A.intercalate ", " (map (\(ForceConfig f) -> f.name) config.forces)
