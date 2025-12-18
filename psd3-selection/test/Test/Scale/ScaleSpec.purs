-- | Tests for Scale Functions
-- |
-- | Tests the pure computation aspects of scales:
-- | - Linear scale domain/range mapping
-- | - Scale inversion
-- | - Tick generation
-- | - Scale combinators (andThen, contramap, map)
-- | - Scale FP module (sampling, modifiers)
module Test.Scale.ScaleSpec where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

-- Scale imports
import PSD3.Scale (linear, pow, sqrt, domain, range, applyScale, invert, ticks, clamp, nice, exponent, andThen, contramap, map)
import PSD3.Scale.FP (sample, sampleRange, tickPositions, niceModifier, clampModifier, combineModifiers, normalize, scaleExtent, scaleMidpoint, scaleInRange)

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Scale Tests ---"

  testLinearScale
  testScaleInversion
  testTickGeneration
  testScaleModifiers
  testPowerScales
  testScaleCombinators
  testScaleFP

-- | Test: Linear scale basic operations
testLinearScale :: Effect Unit
testLinearScale = do
  log "\n  Linear Scale:"

  -- Create a linear scale: domain [0, 100] → range [0, 500]
  let scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]

  -- Test midpoint mapping
  let mid = applyScale scale 50.0
  assert' ("Expected 250.0 at midpoint, got " <> show mid) (mid == 250.0)
  log $ "    50.0 → " <> show mid <> " (midpoint)"

  -- Test lower bound
  let lower = applyScale scale 0.0
  assert' ("Expected 0.0 at lower bound, got " <> show lower) (lower == 0.0)
  log $ "    0.0 → " <> show lower <> " (lower bound)"

  -- Test upper bound
  let upper = applyScale scale 100.0
  assert' ("Expected 500.0 at upper bound, got " <> show upper) (upper == 500.0)
  log $ "    100.0 → " <> show upper <> " (upper bound)"

  -- Test interpolation
  let quarter = applyScale scale 25.0
  assert' ("Expected 125.0 at 25%, got " <> show quarter) (quarter == 125.0)
  log $ "    25.0 → " <> show quarter <> " (quarter)"

  log "  ✓ Linear scale works correctly"

-- | Test: Scale inversion
testScaleInversion :: Effect Unit
testScaleInversion = do
  log "\n  Scale Inversion:"

  let scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]

  -- Invert: 250 → 50
  case invert scale 250.0 of
    Just inverted -> do
      assert' ("Expected invert 250.0 = 50.0, got " <> show inverted) (inverted == 50.0)
      log $ "    invert 250.0 = " <> show inverted
    Nothing ->
      assert' "Invert returned Nothing unexpectedly" false

  -- Invert: 0 → 0
  case invert scale 0.0 of
    Just inverted -> do
      assert' ("Expected invert 0.0 = 0.0, got " <> show inverted) (inverted == 0.0)
      log $ "    invert 0.0 = " <> show inverted
    Nothing ->
      assert' "Invert returned Nothing unexpectedly" false

  -- Invert: 500 → 100
  case invert scale 500.0 of
    Just inverted -> do
      assert' ("Expected invert 500.0 = 100.0, got " <> show inverted) (inverted == 100.0)
      log $ "    invert 500.0 = " <> show inverted
    Nothing ->
      assert' "Invert returned Nothing unexpectedly" false

  log "  ✓ Scale inversion works correctly"

-- | Test: Tick generation
testTickGeneration :: Effect Unit
testTickGeneration = do
  log "\n  Tick Generation:"

  let scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]

  -- Request 5 ticks
  let tickVals = ticks 5 scale
  log $ "    ticks 5 = " <> show tickVals

  -- Should have reasonable number of ticks
  assert' ("Expected 4-7 ticks, got " <> show (Array.length tickVals))
    (Array.length tickVals >= 4 && Array.length tickVals <= 7)

  -- First tick should be at or near 0
  case Array.head tickVals of
    Just first ->
      assert' ("First tick should be near 0, got " <> show first) (first >= -10.0 && first <= 10.0)
    Nothing ->
      assert' "No ticks generated" false

  -- Last tick should be at or near 100
  case Array.last tickVals of
    Just lastTick ->
      assert' ("Last tick should be near 100, got " <> show lastTick) (lastTick >= 90.0 && lastTick <= 110.0)
    Nothing ->
      assert' "No ticks generated" false

  log "  ✓ Tick generation works correctly"

-- | Test: Scale modifiers (nice, clamp)
testScaleModifiers :: Effect Unit
testScaleModifiers = do
  log "\n  Scale Modifiers:"

  -- Nice modifier extends domain to nice round values
  let rawScale = linear # domain [3.0, 97.0] # range [0.0, 500.0]
  let niceScale = rawScale # nice

  -- Test that nice scale still maps reasonably
  let rawMid = applyScale rawScale 50.0
  let niceMid = applyScale niceScale 50.0
  log $ "    raw scale(50) = " <> show rawMid
  log $ "    nice scale(50) = " <> show niceMid
  log "    (nice extends domain to round values)"

  -- Clamp modifier constrains output to range
  let clampedScale = linear # domain [0.0, 100.0] # range [0.0, 500.0] # clamp true

  -- Out-of-domain value should be clamped
  let outOfBounds = applyScale clampedScale 150.0
  assert' ("Clamped value should be <= 500, got " <> show outOfBounds) (outOfBounds <= 500.0)
  log $ "    clamped scale(150) = " <> show outOfBounds <> " (clamped to range)"

  log "  ✓ Scale modifiers work correctly"

-- | Test: Power scales (pow, sqrt)
testPowerScales :: Effect Unit
testPowerScales = do
  log "\n  Power Scales:"

  -- Square scale (exponent 2)
  let squareScale = pow # exponent 2.0 # domain [0.0, 10.0] # range [0.0, 100.0]
  let sq5 = applyScale squareScale 5.0
  -- 5^2 / 10^2 * 100 = 25
  assert' ("Expected square(5) = 25, got " <> show sq5) (sq5 == 25.0)
  log $ "    square scale(5) = " <> show sq5 <> " (x^2 mapping)"

  -- Square root scale
  let sqrtScale = sqrt # domain [0.0, 100.0] # range [0.0, 10.0]
  let sqrt25 = applyScale sqrtScale 25.0
  -- sqrt(25) / sqrt(100) * 10 = 5
  assert' ("Expected sqrt(25) = 5, got " <> show sqrt25) (sqrt25 == 5.0)
  log $ "    sqrt scale(25) = " <> show sqrt25 <> " (sqrt mapping)"

  log "  ✓ Power scales work correctly"

-- | Test: Scale combinators (andThen, contramap, map)
testScaleCombinators :: Effect Unit
testScaleCombinators = do
  log "\n  Scale Combinators:"

  let scale1 = linear # domain [0.0, 100.0] # range [0.0, 1.0]
  let scale2 = linear # domain [0.0, 1.0] # range [0.0, 500.0]

  -- andThen: compose two scales
  let combined = scale1 `andThen` scale2
  let result = combined 50.0
  -- 50 → 0.5 → 250
  assert' ("Expected andThen(50) = 250, got " <> show result) (result == 250.0)
  log $ "    scale1 `andThen` scale2 at 50 = " <> show result

  -- contramap: transform input
  let fahrenheitScale = linear # domain [32.0, 212.0] # range [0.0, 100.0]
  let celsiusToFahrenheit = \c -> c * 9.0 / 5.0 + 32.0
  let celsiusScale = contramap celsiusToFahrenheit fahrenheitScale
  let celsiusResult = celsiusScale 0.0  -- 0°C = 32°F → 0%
  assert' ("Expected celsius(0) = 0, got " <> show celsiusResult)
    (celsiusResult >= -1.0 && celsiusResult <= 1.0)
  log $ "    contramap celsius(0°C) = " <> show celsiusResult <> " (freezing point)"

  -- map: transform output
  let baseScale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
  let offsetScale = map (_ + 50.0) baseScale
  let offsetResult = offsetScale 0.0
  -- 0 → 0 → 50
  assert' ("Expected map(0) = 50, got " <> show offsetResult) (offsetResult == 50.0)
  log $ "    map (+50) scale(0) = " <> show offsetResult

  log "  ✓ Scale combinators work correctly"

-- | Test: Scale FP module
testScaleFP :: Effect Unit
testScaleFP = do
  log "\n  Scale FP Module:"

  let scale = linear # domain [0.0, 1.0] # range [0.0, 100.0]

  -- sample: evenly spaced points
  let samples = sample 5 scale
  log $ "    sample 5 = " <> show samples
  assert' "Sample should have 5 points" (Array.length samples == 5)
  case Array.head samples of
    Just first -> assert' ("First sample should be 0, got " <> show first) (first == 0.0)
    Nothing -> assert' "No samples" false
  case Array.last samples of
    Just lastSample -> assert' ("Last sample should be 100, got " <> show lastSample) (lastSample == 100.0)
    Nothing -> assert' "No samples" false

  -- sampleRange: specific range
  let rangedSamples = sampleRange 3 0.25 0.75 scale
  log $ "    sampleRange 3 0.25 0.75 = " <> show rangedSamples
  assert' "Ranged sample should have 3 points" (Array.length rangedSamples == 3)

  -- normalize: create normalizing scale
  let normalizer = normalize 0.0 100.0
  let normalized = applyScale normalizer 50.0
  assert' ("normalize(50) should be 0.5, got " <> show normalized) (normalized == 0.5)
  log $ "    normalize 0 100: 50 → " <> show normalized

  -- scaleExtent
  let extent = scaleExtent scale
  log $ "    extent = {min: " <> show extent.min <> ", max: " <> show extent.max <> "}"
  assert' "Extent min should be 0" (extent.min == 0.0)
  assert' "Extent max should be 100" (extent.max == 100.0)

  -- scaleMidpoint
  let midpoint = scaleMidpoint scale
  log $ "    midpoint = " <> show midpoint
  assert' ("Midpoint should be 50, got " <> show midpoint) (midpoint == 50.0)

  -- scaleInRange
  let inRange50 = scaleInRange scale 50.0
  let inRange150 = scaleInRange scale 150.0
  log $ "    inRange 50 = " <> show inRange50 <> ", inRange 150 = " <> show inRange150
  assert' "50 should be in range" inRange50
  assert' "150 should NOT be in range" (not inRange150)

  -- combineModifiers
  let mods = combineModifiers [niceModifier, clampModifier]
  let modifiedScale = mods scale
  log "    combineModifiers [nice, clamp] applied"

  log "  ✓ Scale FP module works correctly"
