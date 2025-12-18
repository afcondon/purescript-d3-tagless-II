-- | Tests for Force Configuration
-- |
-- | Tests the pure data structures and smart constructors for force configuration:
-- | - Smart constructors (manyBodyForce, centerForce, etc.)
-- | - Default parameter values
-- | - Parameter update functions (withStrength, withRadius, withDistance)
-- | - Filter operations (withFilter, withoutFilter)
module Test.Config.ForceSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number (infinity)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

-- Import the module under test
import PSD3.Config.Force as Force

-- =============================================================================
-- Helper to extract static value
-- =============================================================================

-- | Extract a static value from AttrValue for testing
extractStatic :: forall a. Force.AttrValue a -> Maybe a
extractStatic (Force.StaticValue a) = Just a
extractStatic _ = Nothing

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Force Configuration Tests ---"

  testManyBodyDefaults
  testCenterDefaults
  testCollideDefaults
  testForceXDefaults
  testForceYDefaults
  testRadialDefaults
  testLinkDefaults
  testWithStrength
  testWithRadius
  testWithDistance
  testFilterOperations
  testForceTypeEquality

-- | Test: ManyBody force defaults
testManyBodyDefaults :: Effect Unit
testManyBodyDefaults = do
  log "\n  ManyBody Force Defaults:"

  let force = Force.manyBodyForce "charge"
  let config = unwrap force

  -- Check name
  assert' ("Name should be 'charge', got " <> config.name) (config.name == "charge")
  log $ "    name: " <> config.name

  -- Check force type
  assert' "Type should be ForceManyBody" (config.forceType == Force.ForceManyBody)
  log $ "    type: " <> show config.forceType

  -- Check parameters
  case config.params of
    Force.ManyBodyParams p -> do
      -- strength default: -30.0 (repulsive)
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be -30.0, got " <> show s) (s == -30.0)
          log $ "    strength: " <> show s <> " (default repulsive)"
        Nothing -> assert' "strength should be static" false

      -- theta default: 0.9 (Barnes-Hut approximation)
      case extractStatic p.theta of
        Just t -> do
          assert' ("theta should be 0.9, got " <> show t) (t == 0.9)
          log $ "    theta: " <> show t <> " (Barnes-Hut)"
        Nothing -> assert' "theta should be static" false

      -- distanceMin default: 1.0
      case extractStatic p.distanceMin of
        Just d -> do
          assert' ("distanceMin should be 1.0, got " <> show d) (d == 1.0)
          log $ "    distanceMin: " <> show d
        Nothing -> assert' "distanceMin should be static" false

      -- distanceMax default: infinity
      case extractStatic p.distanceMax of
        Just d -> do
          assert' ("distanceMax should be infinity, got " <> show d) (d == infinity)
          log $ "    distanceMax: infinity"
        Nothing -> assert' "distanceMax should be static" false

    _ -> assert' "Should have ManyBodyParams" false

  -- Check filter is None
  case config.filter of
    Nothing -> log "    filter: none"
    Just _ -> assert' "Filter should be Nothing by default" false

  log "  ✓ ManyBody force defaults correct"

-- | Test: Center force defaults
testCenterDefaults :: Effect Unit
testCenterDefaults = do
  log "\n  Center Force Defaults:"

  let force = Force.centerForce "center"
  let config = unwrap force

  assert' "Type should be ForceCenter" (config.forceType == Force.ForceCenter)
  log $ "    type: " <> show config.forceType

  case config.params of
    Force.CenterParams p -> do
      -- x default: 0.0
      case extractStatic p.x of
        Just x -> do
          assert' ("x should be 0.0, got " <> show x) (x == 0.0)
          log $ "    x: " <> show x
        Nothing -> assert' "x should be static" false

      -- y default: 0.0
      case extractStatic p.y of
        Just y -> do
          assert' ("y should be 0.0, got " <> show y) (y == 0.0)
          log $ "    y: " <> show y
        Nothing -> assert' "y should be static" false

      -- strength default: 1.0
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be 1.0, got " <> show s) (s == 1.0)
          log $ "    strength: " <> show s
        Nothing -> assert' "strength should be static" false

    _ -> assert' "Should have CenterParams" false

  log "  ✓ Center force defaults correct"

-- | Test: Collide force defaults
testCollideDefaults :: Effect Unit
testCollideDefaults = do
  log "\n  Collide Force Defaults:"

  -- Collide requires radius parameter
  let force = Force.collideForce "collision" (Force.StaticValue 10.0)
  let config = unwrap force

  assert' "Type should be ForceCollide" (config.forceType == Force.ForceCollide)
  log $ "    type: " <> show config.forceType

  case config.params of
    Force.CollideParams p -> do
      -- radius: user-provided
      case extractStatic p.radius of
        Just r -> do
          assert' ("radius should be 10.0, got " <> show r) (r == 10.0)
          log $ "    radius: " <> show r <> " (user-provided)"
        Nothing -> assert' "radius should be static" false

      -- strength default: 1.0
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be 1.0, got " <> show s) (s == 1.0)
          log $ "    strength: " <> show s
        Nothing -> assert' "strength should be static" false

      -- iterations default: 1.0
      case extractStatic p.iterations of
        Just i -> do
          assert' ("iterations should be 1.0, got " <> show i) (i == 1.0)
          log $ "    iterations: " <> show i
        Nothing -> assert' "iterations should be static" false

    _ -> assert' "Should have CollideParams" false

  log "  ✓ Collide force defaults correct"

-- | Test: ForceX defaults
testForceXDefaults :: Effect Unit
testForceXDefaults = do
  log "\n  ForceX Defaults:"

  let force = Force.forceX "xPosition" (Force.StaticValue 200.0)
  let config = unwrap force

  assert' "Type should be ForceX" (config.forceType == Force.ForceX)
  log $ "    type: " <> show config.forceType

  case config.params of
    Force.ForceXParams p -> do
      case extractStatic p.x of
        Just x -> do
          assert' ("x should be 200.0, got " <> show x) (x == 200.0)
          log $ "    x: " <> show x
        Nothing -> assert' "x should be static" false

      -- strength default: 0.1
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be 0.1, got " <> show s) (s == 0.1)
          log $ "    strength: " <> show s
        Nothing -> assert' "strength should be static" false

    _ -> assert' "Should have ForceXParams" false

  log "  ✓ ForceX defaults correct"

-- | Test: ForceY defaults
testForceYDefaults :: Effect Unit
testForceYDefaults = do
  log "\n  ForceY Defaults:"

  let force = Force.forceY "yPosition" (Force.StaticValue 300.0)
  let config = unwrap force

  assert' "Type should be ForceY" (config.forceType == Force.ForceY)
  log $ "    type: " <> show config.forceType

  case config.params of
    Force.ForceYParams p -> do
      case extractStatic p.y of
        Just y -> do
          assert' ("y should be 300.0, got " <> show y) (y == 300.0)
          log $ "    y: " <> show y
        Nothing -> assert' "y should be static" false

      -- strength default: 0.1
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be 0.1, got " <> show s) (s == 0.1)
          log $ "    strength: " <> show s
        Nothing -> assert' "strength should be static" false

    _ -> assert' "Should have ForceYParams" false

  log "  ✓ ForceY defaults correct"

-- | Test: Radial force defaults
testRadialDefaults :: Effect Unit
testRadialDefaults = do
  log "\n  Radial Force Defaults:"

  let force = Force.radialForce "orbital" 150.0
  let config = unwrap force

  assert' "Type should be ForceRadial" (config.forceType == Force.ForceRadial)
  log $ "    type: " <> show config.forceType

  case config.params of
    Force.RadialParams p -> do
      case extractStatic p.radius of
        Just r -> do
          assert' ("radius should be 150.0, got " <> show r) (r == 150.0)
          log $ "    radius: " <> show r
        Nothing -> assert' "radius should be static" false

      -- strength default: 0.1
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be 0.1, got " <> show s) (s == 0.1)
          log $ "    strength: " <> show s
        Nothing -> assert' "strength should be static" false

      -- x default: 0.0
      case extractStatic p.x of
        Just x -> do
          assert' ("x should be 0.0, got " <> show x) (x == 0.0)
          log $ "    x: " <> show x
        Nothing -> assert' "x should be static" false

      -- y default: 0.0
      case extractStatic p.y of
        Just y -> do
          assert' ("y should be 0.0, got " <> show y) (y == 0.0)
          log $ "    y: " <> show y
        Nothing -> assert' "y should be static" false

    _ -> assert' "Should have RadialParams" false

  log "  ✓ Radial force defaults correct"

-- | Test: Link force defaults
testLinkDefaults :: Effect Unit
testLinkDefaults = do
  log "\n  Link Force Defaults:"

  let force = Force.linkForce "links"
  let config = unwrap force

  assert' "Type should be ForceLink" (config.forceType == Force.ForceLink)
  log $ "    type: " <> show config.forceType

  case config.params of
    Force.LinkParams p -> do
      -- distance default: 30.0
      case extractStatic p.distance of
        Just d -> do
          assert' ("distance should be 30.0, got " <> show d) (d == 30.0)
          log $ "    distance: " <> show d
        Nothing -> assert' "distance should be static" false

      -- strength default: 1.0
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be 1.0, got " <> show s) (s == 1.0)
          log $ "    strength: " <> show s
        Nothing -> assert' "strength should be static" false

      -- iterations default: 1.0
      case extractStatic p.iterations of
        Just i -> do
          assert' ("iterations should be 1.0, got " <> show i) (i == 1.0)
          log $ "    iterations: " <> show i
        Nothing -> assert' "iterations should be static" false

    _ -> assert' "Should have LinkParams" false

  log "  ✓ Link force defaults correct"

-- | Test: withStrength modifier
testWithStrength :: Effect Unit
testWithStrength = do
  log "\n  withStrength Modifier:"

  -- Modify manyBody strength
  let force = Force.manyBodyForce "charge" # Force.withStrength (-50.0)
  let config = unwrap force

  case config.params of
    Force.ManyBodyParams p -> do
      case extractStatic p.strength of
        Just s -> do
          assert' ("strength should be -50.0, got " <> show s) (s == -50.0)
          log $ "    manyBody withStrength(-50.0): " <> show s
        Nothing -> assert' "strength should be static" false
    _ -> assert' "Should have ManyBodyParams" false

  -- Modify center strength
  let centerForce = Force.centerForce "center" # Force.withStrength 0.5
  let centerConfig = unwrap centerForce

  case centerConfig.params of
    Force.CenterParams p -> do
      case extractStatic p.strength of
        Just s -> do
          assert' ("center strength should be 0.5, got " <> show s) (s == 0.5)
          log $ "    center withStrength(0.5): " <> show s
        Nothing -> assert' "strength should be static" false
    _ -> assert' "Should have CenterParams" false

  log "  ✓ withStrength works correctly"

-- | Test: withRadius modifier
testWithRadius :: Effect Unit
testWithRadius = do
  log "\n  withRadius Modifier:"

  -- Modify collide radius
  let force = Force.collideForce "collision" (Force.StaticValue 10.0)
            # Force.withRadius (Force.StaticValue 25.0)
  let config = unwrap force

  case config.params of
    Force.CollideParams p -> do
      case extractStatic p.radius of
        Just r -> do
          assert' ("radius should be 25.0, got " <> show r) (r == 25.0)
          log $ "    collide withRadius(25.0): " <> show r
        Nothing -> assert' "radius should be static" false
    _ -> assert' "Should have CollideParams" false

  -- Modify radial radius
  let radial = Force.radialForce "orbital" 100.0
             # Force.withRadius (Force.StaticValue 200.0)
  let radialConfig = unwrap radial

  case radialConfig.params of
    Force.RadialParams p -> do
      case extractStatic p.radius of
        Just r -> do
          assert' ("radial radius should be 200.0, got " <> show r) (r == 200.0)
          log $ "    radial withRadius(200.0): " <> show r
        Nothing -> assert' "radius should be static" false
    _ -> assert' "Should have RadialParams" false

  log "  ✓ withRadius works correctly"

-- | Test: withDistance modifier
testWithDistance :: Effect Unit
testWithDistance = do
  log "\n  withDistance Modifier:"

  let force = Force.linkForce "links" # Force.withDistance 50.0
  let config = unwrap force

  case config.params of
    Force.LinkParams p -> do
      case extractStatic p.distance of
        Just d -> do
          assert' ("distance should be 50.0, got " <> show d) (d == 50.0)
          log $ "    link withDistance(50.0): " <> show d
        Nothing -> assert' "distance should be static" false
    _ -> assert' "Should have LinkParams" false

  -- withDistance should be no-op for non-link forces
  let manyBody = Force.manyBodyForce "charge" # Force.withDistance 999.0
  let mbConfig = unwrap manyBody
  -- Should still be ManyBodyParams, unchanged
  case mbConfig.params of
    Force.ManyBodyParams _ -> log "    withDistance on manyBody: no-op (expected)"
    _ -> assert' "Should still have ManyBodyParams" false

  log "  ✓ withDistance works correctly"

-- | Test: Filter operations
testFilterOperations :: Effect Unit
testFilterOperations = do
  log "\n  Filter Operations:"

  -- Create a filter
  let packageFilter = Force.ForceFilter
        { description: "packages only"
        , predicate: \_ -> true  -- simplified for testing
        }

  -- Add filter to force
  let force = Force.manyBodyForce "charge" # flip Force.withFilter packageFilter
  let config = unwrap force

  case config.filter of
    Just (Force.ForceFilter f) -> do
      assert' "Filter description should match" (f.description == "packages only")
      log $ "    withFilter: " <> f.description
    Nothing -> assert' "Filter should be set" false

  -- Remove filter
  let noFilter = Force.withoutFilter force
  let noFilterConfig = unwrap noFilter

  case noFilterConfig.filter of
    Nothing -> log "    withoutFilter: removed filter"
    Just _ -> assert' "Filter should be removed" false

  log "  ✓ Filter operations work correctly"

-- | Test: ForceType equality
testForceTypeEquality :: Effect Unit
testForceTypeEquality = do
  log "\n  ForceType Equality:"

  assert' "ForceManyBody == ForceManyBody" (Force.ForceManyBody == Force.ForceManyBody)
  assert' "ForceCenter == ForceCenter" (Force.ForceCenter == Force.ForceCenter)
  assert' "ForceManyBody /= ForceCenter" (Force.ForceManyBody /= Force.ForceCenter)
  assert' "ForceCollide /= ForceLink" (Force.ForceCollide /= Force.ForceLink)

  log "    All force types have correct equality"
  log "  ✓ ForceType equality works correctly"
