-- | Tick-Driven Transitions
-- |
-- | Primitives for animating transitions using simulation ticks rather than CSS.
-- | This is the simulation-aware analog to d3-transition.
-- |
-- | Key insight: Force simulations already have a tick loop. We can use that
-- | same loop to drive enter/exit/update animations, giving us:
-- | - Predictable, debuggable behavior
-- | - No CSS timing coordination
-- | - Pure PureScript interpolation
-- |
-- | Usage:
-- | ```purescript
-- | import PSD3.Transition.Tick as T
-- |
-- | -- In your state
-- | type State = { entering :: Map String Progress, exiting :: Array (Transitioning Node) }
-- |
-- | -- In tick handler
-- | onTick state = do
-- |   let { active: stillEntering } = T.tickProgressMap 0.025 state.entering
-- |   let { active: stillExiting } = T.tickTransitions 0.025 state.exiting
-- |   -- render with interpolated values
-- |
-- | -- In render
-- | radius = case enterProgress of
-- |   Just p -> T.lerp 20.0 5.0 (T.easeOut p)
-- |   Nothing -> 5.0
-- | ```
module PSD3.Transition.Tick
  ( -- * Types
    Progress
  , TickDelta
  , Transitioning
  , Easing
    -- * Progress Map operations (for key-based tracking)
  , tickProgressMap
  , startProgress
  , startProgressFrom
    -- * Transitioning Array operations (for item-based tracking)
  , tickTransitions
  , startTransitions
  , startTransitionsFrom
    -- * Interpolation
  , lerp
  , lerpClamped
  , lerpInt
    -- * Easing functions
  , linear
  , easeIn
  , easeOut
  , easeInOut
  , easeInQuad
  , easeOutQuad
  , easeInOutQuad
  , easeInCubic
  , easeOutCubic
  , easeInOutCubic
    -- * Combinators
  , withEasing
  , ticksForDuration
  ) where

import Prelude

import Data.Array as Array
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set

-- =============================================================================
-- Types
-- =============================================================================

-- | Progress from 0.0 (start) to 1.0 (complete)
type Progress = Number

-- | Amount to advance progress each tick
-- | At 60fps: 0.025 ≈ 40 ticks ≈ 0.67 seconds
-- | At 60fps: 0.020 ≈ 50 ticks ≈ 0.83 seconds
-- | At 60fps: 0.015 ≈ 67 ticks ≈ 1.1 seconds
type TickDelta = Number

-- | An item in transition, carrying its state and progress
type Transitioning a = { item :: a, progress :: Progress }

-- | Easing function: maps linear progress to eased progress
type Easing = Progress -> Progress

-- =============================================================================
-- Progress Map Operations
-- =============================================================================

-- | Advance all progress values in a Map, partitioning into active and completed
-- |
-- | ```purescript
-- | let { active, completed } = tickProgressMap 0.025 enteringNodes
-- | -- active: nodes still animating
-- | -- completed: keys that just finished (for cleanup, callbacks, etc.)
-- | ```
tickProgressMap
  :: forall k
   . Ord k
  => TickDelta
  -> Map k Progress
  -> { active :: Map k Progress, completed :: Array k }
tickProgressMap delta progressMap =
  let
    advanced = map (\p -> min 1.0 (p + delta)) progressMap
    active = Map.filter (\p -> p < 1.0) advanced
    completed = Set.toUnfoldable $ Map.keys $ Map.filter (\p -> p >= 1.0) advanced
  in
    { active, completed }

-- | Start tracking progress for new keys (from 0.0)
startProgress :: forall k. Ord k => Array k -> Map k Progress -> Map k Progress
startProgress keys existing =
  Array.foldl (\m k -> Map.insert k 0.0 m) existing keys

-- | Start tracking progress for new keys from a specific value
startProgressFrom :: forall k. Ord k => Progress -> Array k -> Map k Progress -> Map k Progress
startProgressFrom initial keys existing =
  Array.foldl (\m k -> Map.insert k initial m) existing keys

-- =============================================================================
-- Transitioning Array Operations
-- =============================================================================

-- | Advance all transitions, partitioning into active and completed
-- |
-- | ```purescript
-- | let { active, completed } = tickTransitions 0.025 exitingNodes
-- | -- active: items still animating out
-- | -- completed: items that finished (now safe to remove from DOM)
-- | ```
tickTransitions
  :: forall a
   . TickDelta
  -> Array (Transitioning a)
  -> { active :: Array (Transitioning a), completed :: Array a }
tickTransitions delta transitions =
  let
    advanced = map (\t -> t { progress = min 1.0 (t.progress + delta) }) transitions
    active = Array.filter (\t -> t.progress < 1.0) advanced
    completed = map _.item $ Array.filter (\t -> t.progress >= 1.0) advanced
  in
    { active, completed }

-- | Wrap items as transitions starting at progress 0.0
startTransitions :: forall a. Array a -> Array (Transitioning a)
startTransitions = map (\item -> { item, progress: 0.0 })

-- | Wrap items as transitions starting at a specific progress
startTransitionsFrom :: forall a. Progress -> Array a -> Array (Transitioning a)
startTransitionsFrom initial = map (\item -> { item, progress: initial })

-- =============================================================================
-- Interpolation
-- =============================================================================

-- | Linear interpolation between two numbers
-- |
-- | ```purescript
-- | lerp 0.0 100.0 0.5  -- 50.0
-- | lerp 20.0 5.0 1.0   -- 5.0 (shrink from 20 to 5)
-- | ```
lerp :: Number -> Number -> Progress -> Number
lerp start end t = start + (end - start) * t

-- | Linear interpolation with progress clamped to [0, 1]
lerpClamped :: Number -> Number -> Progress -> Number
lerpClamped start end t = lerp start end (clamp 0.0 1.0 t)

-- | Linear interpolation for integers
lerpInt :: Int -> Int -> Progress -> Int
lerpInt start end t = round $ lerp (toNumber start) (toNumber end) t

-- =============================================================================
-- Easing Functions
-- =============================================================================

-- | No easing (linear)
linear :: Easing
linear t = t

-- | Ease in (slow start, fast end) - alias for easeInQuad
easeIn :: Easing
easeIn = easeInQuad

-- | Ease out (fast start, slow end) - alias for easeOutQuad
easeOut :: Easing
easeOut = easeOutQuad

-- | Ease in-out (slow start and end) - alias for easeInOutQuad
easeInOut :: Easing
easeInOut = easeInOutQuad

-- | Quadratic ease in: t²
easeInQuad :: Easing
easeInQuad t = t * t

-- | Quadratic ease out: 1 - (1-t)²
easeOutQuad :: Easing
easeOutQuad t = 1.0 - (1.0 - t) * (1.0 - t)

-- | Quadratic ease in-out
easeInOutQuad :: Easing
easeInOutQuad t =
  if t < 0.5
    then 2.0 * t * t
    else 1.0 - ((-2.0 * t + 2.0) * (-2.0 * t + 2.0)) / 2.0

-- | Cubic ease in: t³
easeInCubic :: Easing
easeInCubic t = t * t * t

-- | Cubic ease out: 1 - (1-t)³
easeOutCubic :: Easing
easeOutCubic t = 1.0 - (1.0 - t) * (1.0 - t) * (1.0 - t)

-- | Cubic ease in-out
easeInOutCubic :: Easing
easeInOutCubic t =
  if t < 0.5
    then 4.0 * t * t * t
    else 1.0 - ((-2.0 * t + 2.0) * (-2.0 * t + 2.0) * (-2.0 * t + 2.0)) / 2.0

-- =============================================================================
-- Combinators
-- =============================================================================

-- | Apply easing to an interpolation function
-- |
-- | ```purescript
-- | -- Ease-out shrink from 20 to 5
-- | radius = withEasing easeOut (lerp 20.0 5.0) progress
-- | ```
withEasing :: forall a. Easing -> (Progress -> a) -> Progress -> a
withEasing ease f t = f (ease t)

-- | Calculate tick delta for a desired duration at assumed 60fps
-- |
-- | ```purescript
-- | ticksForDuration 1000  -- 0.0167 (1 second at 60fps)
-- | ticksForDuration 500   -- 0.0333 (0.5 seconds)
-- | ```
ticksForDuration :: Int -> TickDelta
ticksForDuration milliseconds =
  let ticks = toNumber milliseconds / 16.67  -- ~60fps
  in 1.0 / ticks

-- Note: Uses Prelude's clamp function
