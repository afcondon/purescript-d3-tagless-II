-- | Scene Configuration Module
-- |
-- | A scene represents a complete simulation state:
-- | - Which forces are active
-- | - Simulation parameters (alpha, decay rates, etc.)
-- | - Optionally, node positions/constraints
-- |
-- | Scenes are composable and can be merged, filtered, or transformed.
module PSD3.Config.Scene where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import PSD3.Config.Force (ForceConfig(..))
import PSD3.Internal.Attributes.Instances (Label)

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
newtype SceneConfig = SceneConfig {
    name        :: String              -- Scene name (for debugging/display)
  , description :: Maybe String        -- Optional description
  , forces      :: Array ForceConfig   -- Active forces in this scene
  , simParams   :: SimulationParams    -- Simulation parameters
}

derive instance Newtype SceneConfig _

-- | Create a scene with default simulation parameters
scene :: String -> Array ForceConfig -> SceneConfig
scene name forces = SceneConfig {
    name
  , description: Nothing
  , forces
  , simParams: defaultSimParams
}

-- | Create a scene with custom simulation parameters
sceneWithParams :: String -> Array ForceConfig -> SimulationParams -> SceneConfig
sceneWithParams name forces simParams = SceneConfig {
    name
  , description: Nothing
  , forces
  , simParams
}

-- | Add description to a scene
withDescription :: String -> SceneConfig -> SceneConfig
withDescription desc = over SceneConfig (_ { description = Just desc })

-- | Update simulation parameters
withSimParams :: SimulationParams -> SceneConfig -> SceneConfig
withSimParams params = over SceneConfig (_ { simParams = params })

-- | Set alpha (energy level) for a scene
withAlpha :: Number -> SceneConfig -> SceneConfig
withAlpha alpha (SceneConfig config) = SceneConfig $ config { simParams = config.simParams { alpha = alpha } }

-- | Set alpha target (simulation runs until alpha ≤ target)
withAlphaTarget :: Number -> SceneConfig -> SceneConfig
withAlphaTarget target (SceneConfig config) = SceneConfig $ config { simParams = config.simParams { alphaTarget = target } }

-- =============================================================================
-- Scene Composition
-- =============================================================================

-- | Add a force to a scene
addForce :: ForceConfig -> SceneConfig -> SceneConfig
addForce force (SceneConfig config) = SceneConfig $ config { forces = A.snoc config.forces force }

-- | Remove a force from a scene by name
removeForce :: Label -> SceneConfig -> SceneConfig
removeForce name (SceneConfig config) = SceneConfig $ config { forces = A.filter (\(ForceConfig f) -> f.name /= name) config.forces }

-- | Keep only specified forces
keepForces :: Array Label -> SceneConfig -> SceneConfig
keepForces names (SceneConfig config) = SceneConfig $ config { forces = A.filter (\(ForceConfig f) -> f.name `A.elem` names) config.forces }

-- | Remove specified forces
excludeForces :: Array Label -> SceneConfig -> SceneConfig
excludeForces names (SceneConfig config) = SceneConfig $ config { forces = A.filter (\(ForceConfig f) -> not (f.name `A.elem` names)) config.forces }

-- | Replace a force in a scene (by name)
replaceForce :: ForceConfig -> SceneConfig -> SceneConfig
replaceForce newForce (SceneConfig config) =
  let (ForceConfig nf) = newForce
  in SceneConfig $ config {
    forces = map (\f@(ForceConfig fc) -> if fc.name == nf.name then newForce else f) config.forces
  }

-- | Update a force in a scene using a transformation function
updateForce :: Label -> (ForceConfig -> ForceConfig) -> SceneConfig -> SceneConfig
updateForce name fn (SceneConfig config) = SceneConfig $ config {
  forces = map (\f@(ForceConfig fc) -> if fc.name == name then fn f else f) config.forces
}

-- | Merge two scenes
-- | Forces from the second scene override those with the same name from the first
-- | Simulation params from the second scene are used
mergeScenes :: SceneConfig -> SceneConfig -> SceneConfig
mergeScenes (SceneConfig s1) (SceneConfig s2) =
  let
    scene2ForceNames = map (\(ForceConfig f) -> f.name) s2.forces
    -- Get forces from scene1 that aren't in scene2
    uniqueFromScene1 = A.filter (\(ForceConfig f) -> not (f.name `A.elem` scene2ForceNames)) s1.forces
  in SceneConfig {
    name: s1.name <> "+" <> s2.name
  , description: Nothing
  , forces: uniqueFromScene1 <> s2.forces
  , simParams: s2.simParams
  }

-- =============================================================================
-- Query Functions
-- =============================================================================

-- | Get a force from a scene by name
getForce :: Label -> SceneConfig -> Maybe ForceConfig
getForce name (SceneConfig config) = A.find (\(ForceConfig f) -> f.name == name) config.forces

-- | Check if a scene has a force with the given name
hasForce :: Label -> SceneConfig -> Boolean
hasForce name (SceneConfig config) = A.any (\(ForceConfig f) -> f.name == name) config.forces

-- | Get all force names in a scene
forceNames :: SceneConfig -> Array Label
forceNames (SceneConfig config) = map (\(ForceConfig fc) -> fc.name) config.forces

-- | Count forces in a scene
forceCount :: SceneConfig -> Int
forceCount (SceneConfig config) = A.length config.forces

-- =============================================================================
-- Display
-- =============================================================================

instance Show SceneConfig where
  show sc@(SceneConfig config) =
    "Scene \"" <> config.name <> "\" [" <> show (forceCount sc) <> " forces]: " <>
    A.intercalate ", " (map (\(ForceConfig f) -> f.name) config.forces)
