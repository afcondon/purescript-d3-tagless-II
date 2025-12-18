-- | Tests for Tick-Driven Transitions
-- |
-- | Tests the pure computation aspects of tick-based transitions:
-- | - Linear interpolation (lerp, lerpClamped, lerpInt)
-- | - Easing functions (linear, quadratic, cubic)
-- | - Progress map operations
-- | - Transitioning array operations
-- | - Duration calculations
module Test.Transition.TickSpec where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

-- Import the module under test
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Tick Transition Tests ---"

  testLinearInterpolation
  testClampedInterpolation
  testIntegerInterpolation
  testLinearEasing
  testQuadraticEasing
  testCubicEasing
  testProgressMapOperations
  testTransitioningArrayOperations
  testTicksForDuration
  testWithEasing

-- | Test: Linear interpolation between numbers
testLinearInterpolation :: Effect Unit
testLinearInterpolation = do
  log "\n  Linear Interpolation (lerp):"

  -- Midpoint
  let mid = Tick.lerp 0.0 100.0 0.5
  assert' ("lerp 0 100 0.5 expected 50.0, got " <> show mid) (mid == 50.0)
  log $ "    lerp 0 100 0.5 = " <> show mid

  -- Start
  let start = Tick.lerp 0.0 100.0 0.0
  assert' ("lerp 0 100 0.0 expected 0.0, got " <> show start) (start == 0.0)
  log $ "    lerp 0 100 0.0 = " <> show start

  -- End
  let end = Tick.lerp 0.0 100.0 1.0
  assert' ("lerp 0 100 1.0 expected 100.0, got " <> show end) (end == 100.0)
  log $ "    lerp 0 100 1.0 = " <> show end

  -- Shrink from 20 to 5
  let shrunk = Tick.lerp 20.0 5.0 1.0
  assert' ("lerp 20 5 1.0 expected 5.0, got " <> show shrunk) (shrunk == 5.0)
  log $ "    lerp 20 5 1.0 = " <> show shrunk <> " (shrink)"

  -- Quarter way
  let quarter = Tick.lerp 0.0 100.0 0.25
  assert' ("lerp 0 100 0.25 expected 25.0, got " <> show quarter) (quarter == 25.0)
  log $ "    lerp 0 100 0.25 = " <> show quarter

  log "  ✓ Linear interpolation works correctly"

-- | Test: Clamped interpolation
testClampedInterpolation :: Effect Unit
testClampedInterpolation = do
  log "\n  Clamped Interpolation (lerpClamped):"

  -- Over 1.0 should clamp to max
  let over = Tick.lerpClamped 0.0 100.0 1.5
  assert' ("lerpClamped 0 100 1.5 expected 100.0, got " <> show over) (over == 100.0)
  log $ "    lerpClamped 0 100 1.5 = " <> show over <> " (clamped to max)"

  -- Under 0.0 should clamp to min
  let under = Tick.lerpClamped 0.0 100.0 (-0.5)
  assert' ("lerpClamped 0 100 -0.5 expected 0.0, got " <> show under) (under == 0.0)
  log $ "    lerpClamped 0 100 -0.5 = " <> show under <> " (clamped to min)"

  -- Normal value should work as expected
  let normal = Tick.lerpClamped 0.0 100.0 0.5
  assert' ("lerpClamped 0 100 0.5 expected 50.0, got " <> show normal) (normal == 50.0)
  log $ "    lerpClamped 0 100 0.5 = " <> show normal

  log "  ✓ Clamped interpolation works correctly"

-- | Test: Integer interpolation
testIntegerInterpolation :: Effect Unit
testIntegerInterpolation = do
  log "\n  Integer Interpolation (lerpInt):"

  let mid = Tick.lerpInt 0 100 0.5
  assert' ("lerpInt 0 100 0.5 expected 50, got " <> show mid) (mid == 50)
  log $ "    lerpInt 0 100 0.5 = " <> show mid

  let start = Tick.lerpInt 0 100 0.0
  assert' ("lerpInt 0 100 0.0 expected 0, got " <> show start) (start == 0)
  log $ "    lerpInt 0 100 0.0 = " <> show start

  let end = Tick.lerpInt 0 100 1.0
  assert' ("lerpInt 0 100 1.0 expected 100, got " <> show end) (end == 100)
  log $ "    lerpInt 0 100 1.0 = " <> show end

  -- Rounds appropriately
  let rounded = Tick.lerpInt 0 10 0.33
  log $ "    lerpInt 0 10 0.33 = " <> show rounded <> " (rounded)"

  log "  ✓ Integer interpolation works correctly"

-- | Test: Linear easing (identity function)
testLinearEasing :: Effect Unit
testLinearEasing = do
  log "\n  Linear Easing:"

  let val1 = Tick.linear 0.0
  assert' ("linear 0.0 expected 0.0, got " <> show val1) (val1 == 0.0)

  let val2 = Tick.linear 0.5
  assert' ("linear 0.5 expected 0.5, got " <> show val2) (val2 == 0.5)

  let val3 = Tick.linear 1.0
  assert' ("linear 1.0 expected 1.0, got " <> show val3) (val3 == 1.0)

  log $ "    linear: 0.0 → " <> show val1 <> ", 0.5 → " <> show val2 <> ", 1.0 → " <> show val3

  log "  ✓ Linear easing is identity function"

-- | Test: Quadratic easing functions
testQuadraticEasing :: Effect Unit
testQuadraticEasing = do
  log "\n  Quadratic Easing:"

  -- easeInQuad: t² - slow start
  let inQuad = Tick.easeInQuad 0.5
  assert' ("easeInQuad 0.5 expected 0.25, got " <> show inQuad) (inQuad == 0.25)
  log $ "    easeInQuad 0.5 = " <> show inQuad <> " (t²)"

  -- easeOutQuad: 1-(1-t)² - fast start
  let outQuad = Tick.easeOutQuad 0.5
  assert' ("easeOutQuad 0.5 expected 0.75, got " <> show outQuad) (outQuad == 0.75)
  log $ "    easeOutQuad 0.5 = " <> show outQuad <> " (1-(1-t)²)"

  -- easeInOutQuad: smooth transition
  let inOutQuadMid = Tick.easeInOutQuad 0.5
  assert' ("easeInOutQuad 0.5 expected 0.5, got " <> show inOutQuadMid) (inOutQuadMid == 0.5)
  log $ "    easeInOutQuad 0.5 = " <> show inOutQuadMid

  -- Verify easeIn and easeOut are aliases for quad versions
  let easeInVal = Tick.easeIn 0.5
  assert' ("easeIn 0.5 should equal easeInQuad 0.5") (easeInVal == inQuad)

  let easeOutVal = Tick.easeOut 0.5
  assert' ("easeOut 0.5 should equal easeOutQuad 0.5") (easeOutVal == outQuad)

  log "  ✓ Quadratic easing functions work correctly"

-- | Test: Cubic easing functions
testCubicEasing :: Effect Unit
testCubicEasing = do
  log "\n  Cubic Easing:"

  -- easeInCubic: t³
  let inCubic = Tick.easeInCubic 0.5
  assert' ("easeInCubic 0.5 expected 0.125, got " <> show inCubic) (inCubic == 0.125)
  log $ "    easeInCubic 0.5 = " <> show inCubic <> " (t³)"

  -- easeOutCubic: 1-(1-t)³
  let outCubic = Tick.easeOutCubic 0.5
  assert' ("easeOutCubic 0.5 expected 0.875, got " <> show outCubic) (outCubic == 0.875)
  log $ "    easeOutCubic 0.5 = " <> show outCubic <> " (1-(1-t)³)"

  -- easeInOutCubic at midpoint
  let inOutCubicMid = Tick.easeInOutCubic 0.5
  assert' ("easeInOutCubic 0.5 expected 0.5, got " <> show inOutCubicMid) (inOutCubicMid == 0.5)
  log $ "    easeInOutCubic 0.5 = " <> show inOutCubicMid

  -- Boundaries should always be 0 and 1
  assert' "easeInCubic 0.0 should be 0.0" (Tick.easeInCubic 0.0 == 0.0)
  assert' "easeInCubic 1.0 should be 1.0" (Tick.easeInCubic 1.0 == 1.0)
  assert' "easeOutCubic 0.0 should be 0.0" (Tick.easeOutCubic 0.0 == 0.0)
  assert' "easeOutCubic 1.0 should be 1.0" (Tick.easeOutCubic 1.0 == 1.0)

  log "  ✓ Cubic easing functions work correctly"

-- | Test: Progress map operations
testProgressMapOperations :: Effect Unit
testProgressMapOperations = do
  log "\n  Progress Map Operations:"

  -- Start with empty map
  let initial = Map.empty

  -- Add some entries
  let withEntries = Tick.startProgress ["a", "b", "c"] initial
  assert' "Should have 3 entries after startProgress" (Map.size withEntries == 3)
  log $ "    startProgress [a, b, c] creates " <> show (Map.size withEntries) <> " entries at 0.0"

  -- startProgressFrom with custom initial value
  let fromMid = Tick.startProgressFrom 0.5 ["x", "y"] Map.empty
  case Map.lookup "x" fromMid of
    Just p -> do
      assert' ("startProgressFrom 0.5 should set to 0.5, got " <> show p) (p == 0.5)
      log $ "    startProgressFrom 0.5 creates entries at " <> show p
    Nothing ->
      assert' "Entry should exist" false

  -- Tick the progress
  let { active, completed } = Tick.tickProgressMap 0.3 withEntries
  log $ "    tickProgressMap 0.3: " <> show (Map.size active) <> " active, " <> show (Array.length completed) <> " completed"

  -- After large tick, entries should complete
  let almostDone = Tick.startProgressFrom 0.9 ["done"] Map.empty
  let { active: stillActive, completed: nowComplete } = Tick.tickProgressMap 0.2 almostDone
  assert' "Entry at 0.9 + 0.2 should complete" (Array.length nowComplete == 1)
  assert' "No entries should remain active" (Map.size stillActive == 0)
  log $ "    Entry at 0.9 + 0.2 tick → completed: " <> show nowComplete

  log "  ✓ Progress map operations work correctly"

-- | Test: Transitioning array operations
testTransitioningArrayOperations :: Effect Unit
testTransitioningArrayOperations = do
  log "\n  Transitioning Array Operations:"

  -- Start transitions
  let items = ["node1", "node2", "node3"]
  let transitions = Tick.startTransitions items
  assert' "Should have 3 transitions" (Array.length transitions == 3)
  log $ "    startTransitions creates " <> show (Array.length transitions) <> " transitions at progress 0.0"

  -- Check initial progress
  case Array.head transitions of
    Just t -> do
      assert' ("Initial progress should be 0.0, got " <> show t.progress) (t.progress == 0.0)
    Nothing ->
      assert' "Should have at least one transition" false

  -- Start from specific progress
  let fromMid = Tick.startTransitionsFrom 0.5 ["item"]
  case Array.head fromMid of
    Just t -> do
      assert' ("startTransitionsFrom 0.5 should set progress to 0.5, got " <> show t.progress) (t.progress == 0.5)
      log $ "    startTransitionsFrom 0.5 creates transitions at progress " <> show t.progress
    Nothing ->
      assert' "Should have at least one transition" false

  -- Tick transitions
  let { active, completed } = Tick.tickTransitions 0.3 transitions
  log $ "    tickTransitions 0.3: " <> show (Array.length active) <> " active, " <> show (Array.length completed) <> " completed"

  -- Complete a transition
  let almostDone = Tick.startTransitionsFrom 0.9 ["finishing"]
  let { active: stillActive, completed: nowComplete } = Tick.tickTransitions 0.2 almostDone
  assert' "Transition at 0.9 + 0.2 should complete" (Array.length nowComplete == 1)
  assert' "No transitions should remain active" (Array.length stillActive == 0)
  log $ "    Transition at 0.9 + 0.2 tick → completed: " <> show nowComplete

  log "  ✓ Transitioning array operations work correctly"

-- | Test: ticksForDuration calculation
testTicksForDuration :: Effect Unit
testTicksForDuration = do
  log "\n  Ticks for Duration:"

  -- 1 second at 60fps should give ~60 ticks, so delta ~= 1/60 ~= 0.0167
  let delta1000 = Tick.ticksForDuration 1000
  assert' ("1000ms should give delta ~0.0167, got " <> show delta1000)
    (delta1000 > 0.015 && delta1000 < 0.02)
  log $ "    ticksForDuration 1000ms = " <> show delta1000 <> " (delta per tick)"

  -- 500ms should be roughly twice the delta
  let delta500 = Tick.ticksForDuration 500
  assert' ("500ms should give delta ~0.033, got " <> show delta500)
    (delta500 > 0.03 && delta500 < 0.04)
  log $ "    ticksForDuration 500ms = " <> show delta500

  -- 2000ms should be half the delta of 1000ms
  let delta2000 = Tick.ticksForDuration 2000
  assert' ("2000ms delta should be roughly half of 1000ms delta")
    (delta2000 > delta1000 * 0.4 && delta2000 < delta1000 * 0.6)
  log $ "    ticksForDuration 2000ms = " <> show delta2000

  log "  ✓ Duration calculations work correctly"

-- | Test: withEasing combinator
testWithEasing :: Effect Unit
testWithEasing = do
  log "\n  withEasing Combinator:"

  -- Apply ease-out to a lerp
  let easeOutLerp = Tick.withEasing Tick.easeOut (Tick.lerp 0.0 100.0)

  -- At t=0.5, easeOut gives 0.75, so lerp should give 75
  let val = easeOutLerp 0.5
  assert' ("withEasing easeOut (lerp 0 100) 0.5 expected 75.0, got " <> show val) (val == 75.0)
  log $ "    withEasing easeOut (lerp 0 100) 0.5 = " <> show val

  -- At t=0.0, should still be 0
  let startVal = easeOutLerp 0.0
  assert' ("withEasing easeOut (lerp 0 100) 0.0 expected 0.0, got " <> show startVal) (startVal == 0.0)

  -- At t=1.0, should be 100
  let endVal = easeOutLerp 1.0
  assert' ("withEasing easeOut (lerp 0 100) 1.0 expected 100.0, got " <> show endVal) (endVal == 100.0)

  log "  ✓ withEasing combinator works correctly"
