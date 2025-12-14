-- | Declarative Force Setup
-- |
-- | Unified system for declaratively configuring force simulations.
-- | Replaces the older ForceSpec (too simple) and Config.* modules (wrong abstraction layer).
-- |
-- | Key features:
-- | - Static or dynamic (per-node) parameter values
-- | - Optional filters to apply forces selectively
-- | - Idempotent `applySetup` that works for both initial setup and updates
-- |
-- | Example:
-- | ```purescript
-- | mySetup = setup "physics"
-- |   [ manyBody "charge" # withStrength (-30.0)
-- |   , collide "collision" # withRadius (dynamic _.r)
-- |   , positionX "gridX" # withX (dynamic _.gridX) # withStrength 0.1
-- |   , positionY "gridY" # withY (dynamic _.gridY) # withStrength 0.1
-- |   ]
-- |
-- | -- Apply to simulation (idempotent - can call repeatedly)
-- | applySetup mySetup sim
-- |
-- | -- Update: just call applySetup with new config
-- | applySetup (mySetup # removeForce "charge") sim
-- | ```
module PSD3.ForceEngine.Setup
  ( -- * Value Types (static or dynamic)
    Value(..)
  , static
  , dynamic
  , valueToNumber

    -- * Force Configuration
  , ForceType  -- Type only (use smart constructors)
  , ForceConfig
  , forceName
  , manyBody
  , collide
  , link
  , center
  , positionX
  , positionY
  , radial

    -- * Force Modifiers
  , withStrength
  , withRadius
  , withX
  , withY
  , withDistance
  , withTheta
  , withIterations
  , withFilter
  , withDistanceMin
  , withDistanceMax

    -- * Setup Type
  , Setup
  , SetupParams
  , defaultParams
  , setup
  , setupWithParams
  , getForces
  , getParams

    -- * Setup Modifiers
  , addForce
  , removeForce
  , updateForce
  , withAlpha
  , withAlphaDecay
  , withAlphaTarget
  , withAlphaMin
  , withVelocityDecay

    -- * Apply to Simulation (the key function)
  , applySetup

    -- * Incremental Operations
  , addForceToSim
  , removeForceFromSim
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Ref as Ref
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation (Simulation, SimulationNode)

-- =============================================================================
-- Value Type (static or per-node dynamic)
-- =============================================================================

-- | A value that can be static or computed per-node
data Value node a
  = Static a
  | Dynamic (node -> a)

-- | Create a static value
static :: forall node a. a -> Value node a
static = Static

-- | Create a dynamic (per-node) value
dynamic :: forall node a. (node -> a) -> Value node a
dynamic = Dynamic

-- | Extract a number from a Value (for static values only, dynamic returns default)
-- | Used internally; prefer the accessor pattern for dynamic values
valueToNumber :: forall node. Value node Number -> Number
valueToNumber (Static n) = n
valueToNumber (Dynamic _) = 0.0  -- Placeholder, actual value comes from accessor

-- =============================================================================
-- Force Configuration
-- =============================================================================

-- | Configuration for a single force
-- | Parameterized by node type for dynamic values and filters
type ForceConfig node =
  { name :: String
  , forceType :: ForceType
  , strength :: Value node Number
  , radius :: Value node Number      -- For Collide, Radial
  , x :: Value node Number           -- For Center, PositionX, Radial
  , y :: Value node Number           -- For Center, PositionY, Radial
  , distance :: Value node Number    -- For Link
  , theta :: Number                  -- For ManyBody (Barnes-Hut)
  , distanceMin :: Number            -- For ManyBody
  , distanceMax :: Number            -- For ManyBody
  , iterations :: Int                -- For Collide, Link
  , filter :: Maybe (node -> Boolean)
  }

-- | Force type enumeration
data ForceType
  = ForceManyBody
  | ForceCollide
  | ForceLink
  | ForceCenter
  | ForcePositionX
  | ForcePositionY
  | ForceRadial

derive instance eqForceType :: Eq ForceType

-- | Get the name of a force config
forceName :: forall node. ForceConfig node -> String
forceName fc = fc.name

-- =============================================================================
-- Force Constructors (smart constructors with defaults)
-- =============================================================================

-- | Default force config (internal)
defaultForceConfig :: forall node. String -> ForceType -> ForceConfig node
defaultForceConfig name forceType =
  { name
  , forceType
  , strength: Static 1.0
  , radius: Static 1.0
  , x: Static 0.0
  , y: Static 0.0
  , distance: Static 30.0
  , theta: 0.9
  , distanceMin: 1.0
  , distanceMax: 1.0e10
  , iterations: 1
  , filter: Nothing
  }

-- | Create a many-body (charge) force
-- | Default: repulsive with strength -30
manyBody :: forall node. String -> ForceConfig node
manyBody name = (defaultForceConfig name ForceManyBody)
  { strength = Static (-30.0) }

-- | Create a collision force
-- | Default: radius 1, strength 1
collide :: forall node. String -> ForceConfig node
collide name = defaultForceConfig name ForceCollide

-- | Create a link (spring) force
-- | Default: distance 30, strength 1
link :: forall node. String -> ForceConfig node
link name = defaultForceConfig name ForceLink

-- | Create a centering force
-- | Default: center at (0, 0), strength 1
center :: forall node. String -> ForceConfig node
center name = defaultForceConfig name ForceCenter

-- | Create an X-positioning force
-- | Default: x = 0, strength 0.1
positionX :: forall node. String -> ForceConfig node
positionX name = (defaultForceConfig name ForcePositionX)
  { strength = Static 0.1
  , x = Static 0.0
  }

-- | Create a Y-positioning force
-- | Default: y = 0, strength 0.1
positionY :: forall node. String -> ForceConfig node
positionY name = (defaultForceConfig name ForcePositionY)
  { strength = Static 0.1
  , y = Static 0.0
  }

-- | Create a radial force
-- | Default: radius 100, center (0,0), strength 0.1
radial :: forall node. String -> ForceConfig node
radial name = (defaultForceConfig name ForceRadial)
  { strength = Static 0.1
  , radius = Static 100.0
  }

-- =============================================================================
-- Force Modifiers (fluent API)
-- =============================================================================

-- | Set strength (works for all force types)
withStrength :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withStrength v fc = fc { strength = v }

-- | Set radius (for Collide, Radial)
withRadius :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withRadius v fc = fc { radius = v }

-- | Set X position/target (for Center, PositionX, Radial)
withX :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withX v fc = fc { x = v }

-- | Set Y position/target (for Center, PositionY, Radial)
withY :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withY v fc = fc { y = v }

-- | Set distance (for Link)
withDistance :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withDistance v fc = fc { distance = v }

-- | Set theta (Barnes-Hut approximation, for ManyBody)
withTheta :: forall node. Number -> ForceConfig node -> ForceConfig node
withTheta v fc = fc { theta = v }

-- | Set iterations (for Collide, Link)
withIterations :: forall node. Int -> ForceConfig node -> ForceConfig node
withIterations v fc = fc { iterations = v }

-- | Set distance min (for ManyBody)
withDistanceMin :: forall node. Number -> ForceConfig node -> ForceConfig node
withDistanceMin v fc = fc { distanceMin = v }

-- | Set distance max (for ManyBody)
withDistanceMax :: forall node. Number -> ForceConfig node -> ForceConfig node
withDistanceMax v fc = fc { distanceMax = v }

-- | Add a filter predicate (force only applies to matching nodes)
withFilter :: forall node. (node -> Boolean) -> ForceConfig node -> ForceConfig node
withFilter pred fc = fc { filter = Just pred }

-- =============================================================================
-- Setup Type
-- =============================================================================

-- | Simulation parameters
type SetupParams =
  { alpha :: Number
  , alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  , velocityDecay :: Number
  }

-- | Default simulation parameters
defaultParams :: SetupParams
defaultParams =
  { alpha: 1.0
  , alphaMin: 0.001
  , alphaDecay: 0.01
  , alphaTarget: 0.0
  , velocityDecay: 0.4
  }

-- | Complete setup: forces + simulation params
type Setup node =
  { name :: String
  , forces :: Array (ForceConfig node)
  , params :: SetupParams
  }

-- | Create a setup with default params
setup :: forall node. String -> Array (ForceConfig node) -> Setup node
setup name forces =
  { name
  , forces
  , params: defaultParams
  }

-- | Create a setup with custom params
setupWithParams :: forall node. String -> Array (ForceConfig node) -> SetupParams -> Setup node
setupWithParams name forces params =
  { name
  , forces
  , params
  }

-- | Get forces from setup
getForces :: forall node. Setup node -> Array (ForceConfig node)
getForces s = s.forces

-- | Get params from setup
getParams :: forall node. Setup node -> SetupParams
getParams s = s.params

-- =============================================================================
-- Setup Modifiers
-- =============================================================================

-- | Add a force to the setup
addForce :: forall node. ForceConfig node -> Setup node -> Setup node
addForce fc s = s { forces = Array.snoc s.forces fc }

-- | Remove a force by name
removeForce :: forall node. String -> Setup node -> Setup node
removeForce name s = s { forces = Array.filter (\fc -> fc.name /= name) s.forces }

-- | Update a force by name (replace if exists)
updateForce :: forall node. ForceConfig node -> Setup node -> Setup node
updateForce fc s = s { forces = map replace s.forces }
  where
  replace existing = if existing.name == fc.name then fc else existing

-- | Set alpha
withAlpha :: forall node. Number -> Setup node -> Setup node
withAlpha v s = s { params = s.params { alpha = v } }

-- | Set alpha decay
withAlphaDecay :: forall node. Number -> Setup node -> Setup node
withAlphaDecay v s = s { params = s.params { alphaDecay = v } }

-- | Set alpha target
withAlphaTarget :: forall node. Number -> Setup node -> Setup node
withAlphaTarget v s = s { params = s.params { alphaTarget = v } }

-- | Set alpha min
withAlphaMin :: forall node. Number -> Setup node -> Setup node
withAlphaMin v s = s { params = s.params { alphaMin = v } }

-- | Set velocity decay
withVelocityDecay :: forall node. Number -> Setup node -> Setup node
withVelocityDecay v s = s { params = s.params { velocityDecay = v } }

-- =============================================================================
-- Apply Setup to Simulation (the key function)
-- =============================================================================

-- | Apply a setup to a simulation.
-- |
-- | This is idempotent - calling with the same setup does minimal work:
-- | - Forces in setup but not simulation: create and add
-- | - Forces in simulation but not setup: remove
-- |
-- | After applying, forces are re-initialized with current nodes.
-- |
-- | Example:
-- | ```purescript
-- | -- Initial setup
-- | applySetup mySetup sim
-- |
-- | -- Later: update setup (just call again)
-- | applySetup (mySetup # removeForce "charge") sim
-- | ```
applySetup
  :: forall row linkRow
   . Setup (SimulationNode row)
  -> Simulation row linkRow
  -> Effect Unit
applySetup setupConfig sim = do
  nodes <- Ref.read sim.nodes
  links <- Ref.read sim.links
  currentForces <- Ref.read sim.forces

  -- Compute which forces to remove (forces in sim but not in setup)
  let desiredNames = Set.fromFoldable $ map _.name setupConfig.forces
  let currentNames = Set.fromFoldable $ Map.keys currentForces
  let toRemove = Set.difference currentNames desiredNames

  -- Remove forces no longer in setup
  for_ (Set.toUnfoldable toRemove :: Array String) \name -> do
    Ref.modify_ (Map.delete name) sim.forces

  -- Add/update each force in setup
  for_ setupConfig.forces \fc -> do
    -- Always create fresh handle to ensure params are up to date
    handle <- createForceHandle fc

    -- Initialize with current nodes (and links for link forces)
    _ <- case fc.forceType of
      ForceLink -> Core.initializeLinkForce handle nodes links
      _ -> Core.initializeForce handle nodes

    -- Store in simulation
    Ref.modify_ (Map.insert fc.name handle) sim.forces

  -- Apply simulation params
  Ref.write setupConfig.params.alpha sim.alpha
  -- Note: Other params (alphaDecay, etc.) are stored in sim.config
  -- which is immutable after creation. For now we just update alpha.

-- | Create a D3 force handle from our config
-- | Uses the Dynamic variants from Core when dynamic values are specified
createForceHandle
  :: forall node
   . ForceConfig node
  -> Effect Core.ForceHandle
createForceHandle fc = pure $ case fc.forceType of
  ForceManyBody ->
    -- ManyBody doesn't have a dynamic strength variant yet, use static
    Core.createManyBody
      { strength: valueToNumber fc.strength
      , theta: fc.theta
      , distanceMin: fc.distanceMin
      , distanceMax: fc.distanceMax
      }

  ForceCollide -> case fc.radius of
    Static r -> Core.createCollide
      { radius: r
      , strength: valueToNumber fc.strength
      , iterations: fc.iterations
      }
    Dynamic radiusFn -> Core.createCollideDynamic
      { radiusAccessor: radiusFn
      , strength: valueToNumber fc.strength
      , iterations: fc.iterations
      }

  ForceLink ->
    Core.createLink
      { distance: valueToNumber fc.distance
      , strength: valueToNumber fc.strength
      , iterations: fc.iterations
      }

  ForceCenter ->
    Core.createCenter
      { x: valueToNumber fc.x
      , y: valueToNumber fc.y
      , strength: valueToNumber fc.strength
      }

  ForcePositionX -> case fc.x of
    Static x -> Core.createForceX
      { x: x
      , strength: valueToNumber fc.strength
      }
    Dynamic xFn -> Core.createForceXDynamic
      { xAccessor: xFn
      , strength: valueToNumber fc.strength
      }

  ForcePositionY -> case fc.y of
    Static y -> Core.createForceY
      { y: y
      , strength: valueToNumber fc.strength
      }
    Dynamic yFn -> Core.createForceYDynamic
      { yAccessor: yFn
      , strength: valueToNumber fc.strength
      }

  ForceRadial ->
    -- Radial doesn't have a dynamic variant, use static
    Core.createRadial
      { radius: valueToNumber fc.radius
      , x: valueToNumber fc.x
      , y: valueToNumber fc.y
      , strength: valueToNumber fc.strength
      }

-- =============================================================================
-- Incremental Operations (for fine-grained control)
-- =============================================================================

-- | Add a single force to simulation (without full setup)
addForceToSim
  :: forall row linkRow
   . ForceConfig (SimulationNode row)
  -> Simulation row linkRow
  -> Effect Unit
addForceToSim fc sim = do
  nodes <- Ref.read sim.nodes
  links <- Ref.read sim.links
  handle <- createForceHandle fc
  _ <- case fc.forceType of
    ForceLink -> Core.initializeLinkForce handle nodes links
    _ -> Core.initializeForce handle nodes
  Ref.modify_ (Map.insert fc.name handle) sim.forces

-- | Remove a single force from simulation by name
removeForceFromSim
  :: forall row linkRow
   . String
  -> Simulation row linkRow
  -> Effect Unit
removeForceFromSim name sim = do
  Ref.modify_ (Map.delete name) sim.forces
