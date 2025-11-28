-- | Configuration Application Module
-- |
-- | This module bridges the gap between immutable PureScript configurations
-- | and the mutable d3-force JavaScript simulation.
-- |
-- | Key functions:
-- | - createForceHandle: Create a fresh d3 force handle from configuration
-- | - applySceneConfig: Apply a complete scene to a simulation
-- |
-- | Design principle: d3 force handles are ephemeral runtime artifacts.
-- | They are created from configurations and discarded when no longer needed.
module PSD3.Config.Apply where

import Prelude

import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import PSD3.Config.Force (AttrValue(..), ForceConfig(..), ForceFilter(..), ForceParams(..), ForceType(..))
import PSD3.Config.Scene (SceneConfig(..), SimulationParams)
import PSD3.Internal.FFI (D3Attr_, D3ForceHandle_, disableTick_, forceCenter_, forceCollideFn_, forceLink_, forceMany_, forceRadial_, forceX_, forceY_, putForceInSimulation_, setAlphaDecay_, setAlphaMin_, setAlphaTarget_, setAlpha_, setAsNullForceInSimulation_, setForceDistanceMax_, setForceDistanceMin_, setForceDistance_, setForceIterations_, setForceRadius_, setForceStrength_, setForceTheta_, setForceX_, setForceY_, setVelocityDecay_)
import PSD3.Internal.Types (D3Simulation_, Datum_)
import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Core Application Functions
-- =============================================================================

-- | Apply a complete scene configuration to a simulation
-- | This is the main entry point for scene transitions
-- |
-- | Steps:
-- | 1. Clear all existing tick functions (prevents stale callbacks)
-- | 2. Remove all existing forces from simulation
-- | 3. Create fresh d3 handles for each force in the scene
-- | 4. Apply parameters to each handle (including filters)
-- | 5. Add handles to simulation
-- | 6. Update simulation parameters
-- |
-- | Note: After calling applySceneConfig, use genericUpdateSimulation to
-- | register appropriate tick functions for the new scene.
applySceneConfig :: SceneConfig -> D3Simulation_ -> Effect Unit
applySceneConfig (SceneConfig config) simulation = do
  -- Step 1: Clear all existing tick functions
  -- This prevents stale tick functions from running on data they don't understand
  -- (e.g., linkTickAttrs expecting swizzled links running on raw links)
  clearAllTickFunctions simulation

  -- Step 2: Clear all existing forces
  removeAllForces simulation

  -- Step 2-4: Create and apply each force
  for_ config.forces \forceConfig -> do
    let (ForceConfig fc) = forceConfig
    let handle = createForceHandle forceConfig
    applyForceParams handle fc.filter fc.params
    let _ = putForceInSimulation_ simulation fc.name handle
    pure unit

  -- Step 5: Update simulation parameters
  applySimulationParams config.simParams simulation

-- | Clear all tick functions from a simulation
-- | This prevents stale tick callbacks from running during scene transitions
-- | D3 tick functions are namespaced like "tick.nodes", "tick.links"
clearAllTickFunctions :: D3Simulation_ -> Effect Unit
clearAllTickFunctions simulation = do
  -- Common tick function names - these are the labels we use
  let tickNames = ["nodes", "links"]
  for_ tickNames \name -> do
    let _ = disableTick_ simulation name
    pure unit

-- | Remove all forces from a simulation
-- | Gets the force names that are currently in the simulation and removes them
removeAllForces :: D3Simulation_ -> Effect Unit
removeAllForces simulation = do
  -- Common force names - we remove these if they exist
  -- (There's no way to query which forces exist, so we try to remove common ones)
  let commonNames = [
        "charge", "charge2", "chargetree", "chargePack"
      , "collision", "collide2", "collidePack"
      , "center", "centerStrong"
      , "links"
      , "packageOrbit", "moduleOrbit"
      , "clusterX", "clusterX_M", "clusterY", "clusterY_M"
      , "forceX", "forceY"
      , "radial"
      ]
  for_ commonNames \name -> do
    let _ = setAsNullForceInSimulation_ simulation name
    pure unit

-- | Create a fresh d3 force handle from a configuration
-- | This creates a new JavaScript object with default parameters
createForceHandle :: ForceConfig -> D3ForceHandle_
createForceHandle (ForceConfig config) = case config.forceType of
  ForceManyBody -> forceMany_ unit
  ForceCenter   -> forceCenter_ unit
  ForceCollide  -> forceCollideFn_ unit
  ForceX        -> forceX_ unit
  ForceY        -> forceY_ unit
  ForceRadial   -> forceRadial_ unit
  ForceLink     -> forceLink_ unit

-- | Apply parameters to a force handle
-- | This calls the d3 setter methods (.strength(), .radius(), etc.)
-- | Filters are applied by converting static values to filter functions
applyForceParams :: D3ForceHandle_ -> Maybe ForceFilter -> ForceParams -> Effect Unit
applyForceParams handle maybeFilter params = case params of
  ManyBodyParams p -> do
    applyParam handle maybeFilter "strength" p.strength
    applyParam handle Nothing "theta" p.theta
    applyParam handle Nothing "distanceMin" p.distanceMin
    applyParam handle Nothing "distanceMax" p.distanceMax

  CenterParams p -> do
    applyParam handle Nothing "x" p.x
    applyParam handle Nothing "y" p.y
    applyParam handle maybeFilter "strength" p.strength

  CollideParams p -> do
    applyParam handle maybeFilter "radius" p.radius
    applyParam handle maybeFilter "strength" p.strength
    applyParam handle Nothing "iterations" p.iterations

  ForceXParams p -> do
    applyParam handle Nothing "x" p.x
    applyParam handle maybeFilter "strength" p.strength

  ForceYParams p -> do
    applyParam handle Nothing "y" p.y
    applyParam handle maybeFilter "strength" p.strength

  RadialParams p -> do
    applyParam handle maybeFilter "radius" p.radius
    applyParam handle maybeFilter "strength" p.strength
    applyParam handle Nothing "x" p.x
    applyParam handle Nothing "y" p.y

  LinkParams p -> do
    applyParam handle Nothing "distance" p.distance
    applyParam handle maybeFilter "strength" p.strength
    applyParam handle Nothing "iterations" p.iterations

-- | Apply a single parameter to a force handle
-- | If a filter is provided AND this is a filterable param, apply the filter
applyParam :: D3ForceHandle_ -> Maybe ForceFilter -> String -> AttrValue Number -> Effect Unit
applyParam handle maybeFilter paramName attrValue =
  case maybeFilter of
    -- No filter - apply value directly
    Nothing ->
      setParam handle paramName attrValue

    -- Has filter - apply filter to the attribute value
    Just (ForceFilter filter) ->
      setParam handle paramName (applyFilterToAttr filter.predicate attrValue)

-- | Set a parameter on a force handle
-- | Calls the appropriate FFI setter based on parameter name
setParam :: D3ForceHandle_ -> String -> AttrValue Number -> Effect Unit
setParam handle paramName attrValue = do
  let value = attrValueToJS attrValue
  let _ = case paramName of
        "strength"    -> setForceStrength_ handle value
        "radius"      -> setForceRadius_ handle value
        "theta"       -> setForceTheta_ handle value
        "distanceMin" -> setForceDistanceMin_ handle value
        "distanceMax" -> setForceDistanceMax_ handle value
        "iterations"  -> setForceIterations_ handle value
        "x"           -> setForceX_ handle value
        "y"           -> setForceY_ handle value
        "distance"    -> setForceDistance_ handle value
        _             -> handle  -- Unknown parameter - return handle unchanged
  pure unit

-- | Convert an AttrValue to a JavaScript value (Number or Function)
-- | This is what actually gets passed to d3's setter methods
-- | IMPORTANT: DynamicIndexedValue functions are curried in PureScript but D3
-- | expects uncurried JavaScript functions. We use mkFn2 to convert.
attrValueToJS :: AttrValue Number -> D3Attr_
attrValueToJS = case _ of
  StaticValue n          -> unsafeCoerce n
  DynamicValue fn        -> unsafeCoerce fn
  DynamicIndexedValue fn -> unsafeCoerce (mkFn2 fn)

-- | Apply a filter to an attribute value
-- | Converts static values to functions that return default (0.0) for filtered-out nodes
applyFilterToAttr :: (Datum_ -> Boolean) -> AttrValue Number -> AttrValue Number
applyFilterToAttr predicate = case _ of
  StaticValue val ->
    DynamicValue \datum ->
      if predicate datum then val else 0.0

  DynamicValue fn ->
    DynamicValue \datum ->
      if predicate datum then fn datum else 0.0

  DynamicIndexedValue fn ->
    DynamicIndexedValue \datum idx ->
      if predicate datum then fn datum idx else 0.0

-- | Apply simulation parameters to a simulation
applySimulationParams :: SimulationParams -> D3Simulation_ -> Effect Unit
applySimulationParams params simulation = do
  let _ = setAlpha_ simulation params.alpha
  let _ = setAlphaTarget_ simulation params.alphaTarget
  let _ = setAlphaMin_ simulation params.alphaMin
  let _ = setAlphaDecay_ simulation params.alphaDecay
  let _ = setVelocityDecay_ simulation params.velocityDecay
  pure unit

-- =============================================================================
-- Helper Functions for Incremental Updates
-- =============================================================================

-- | Add a single force to the simulation without removing existing forces
-- | Useful for live updates from ForceControlPanel
addForceToSimulation :: ForceConfig -> D3Simulation_ -> Effect Unit
addForceToSimulation forceConfig simulation = do
  let (ForceConfig fc) = forceConfig
  let handle = createForceHandle forceConfig
  applyForceParams handle fc.filter fc.params
  let _ = putForceInSimulation_ simulation fc.name handle
  pure unit

-- | Remove a single force from the simulation
removeForceFromSimulation :: String -> D3Simulation_ -> Effect Unit
removeForceFromSimulation name simulation = do
  let _ = setAsNullForceInSimulation_ simulation name
  pure unit

-- | Update a force's parameters without recreating it
-- | Note: This still creates a fresh handle to ensure clean state
updateForceParams :: ForceConfig -> D3Simulation_ -> Effect Unit
updateForceParams forceConfig simulation = do
  let (ForceConfig fc) = forceConfig
  -- Create fresh handle with new params
  let handle = createForceHandle forceConfig
  applyForceParams handle fc.filter fc.params
  -- Replace in simulation (putForce replaces if name exists)
  let _ = putForceInSimulation_ simulation fc.name handle
  pure unit
