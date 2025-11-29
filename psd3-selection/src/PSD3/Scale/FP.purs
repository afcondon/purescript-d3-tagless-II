-- | PSD3.Scale.FP - Functional Programming Abstractions for Scales
-- |
-- | This module provides higher-level functional programming idioms
-- | built on top of the core Scale module.
-- |
-- | ## Profunctor-like Operations
-- |
-- | Scales are naturally profunctorial: contravariant in domain, covariant in range.
-- |
-- | ```purescript
-- | -- Pre-process domain values
-- | normalizedScale = rawScale # contramapDomain normalize
-- |
-- | -- Post-process range values
-- | offsetScale = pixelScale # mapRange (_ + margin)
-- |
-- | -- Both at once
-- | transformedScale = scale # dimapScale preprocess postprocess
-- | ```
-- |
-- | ## Scale as Semigroup/Monoid (Modifiers)
-- |
-- | Scale modifiers compose with `<>`:
-- |
-- | ```purescript
-- | niceAndClamped = niceModifier <> clampModifier
-- | myScale = linear # niceAndClamped # domain [...] # range [...]
-- | ```
-- |
-- | ## Tick Generation as Unfold
-- |
-- | Generate ticks lazily using unfold semantics:
-- |
-- | ```purescript
-- | import Data.Unfoldable (unfoldr)
-- |
-- | -- Custom tick generation
-- | logTicks = ticksUnfold logScale  -- Lazy stream of tick values
-- | ```
-- |
-- | ## Scale Sampling
-- |
-- | Sample a scale at regular intervals (useful for gradients, paths):
-- |
-- | ```purescript
-- | -- Sample 100 points along a color scale
-- | gradient = sample 100 viridisScale
-- | ```
module PSD3.Scale.FP
  ( -- * Scale Modifiers (Endo-like)
    ScaleModifier
  , niceModifier
  , clampModifier
  , roundModifier
  , combineModifiers

    -- * Sampling
  , sample
  , sampleRange
  , sampleWithDomain

    -- * Tick Operations
  , tickPositions
  , tickLabels
  , ticksWithLabels

    -- * Interpolation Combinators
  , blendInterpolators
  , reverseInterpolator
  , clampInterpolator
  , cycleInterpolator

    -- * Scale Transformations
  , normalize
  , quantize
  , threshold

    -- * Useful Combinators
  , scaleExtent
  , scaleMidpoint
  , scaleInRange
  ) where

import Prelude hiding (clamp)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PSD3.Scale (Scale, Continuous, ContinuousScale, Interpolator)
import PSD3.Scale (applyScale, clamp, domain, nice, range, round, tickFormat, ticks) as Scale

-- ============================================================================
-- SCALE MODIFIERS (Endo-like composition)
-- ============================================================================

-- | A scale modifier transforms a scale while preserving its type
-- | Modifiers form a Semigroup under composition
type ScaleModifier d r k = Scale d r k -> Scale d r k

-- | Modifier that makes the domain nice (rounds to clean values)
niceModifier :: forall r k. ScaleModifier Number r k
niceModifier = Scale.nice

-- | Modifier that enables clamping
clampModifier :: forall d r k. ScaleModifier d r k
clampModifier = Scale.clamp true

-- | Modifier that enables rounding
roundModifier :: forall d r k. ScaleModifier d r k
roundModifier = Scale.round true

-- | Combine multiple modifiers (apply left to right)
-- |
-- | ```purescript
-- | combined = combineModifiers [niceModifier, clampModifier, roundModifier]
-- | myScale = linear # combined # domain [...] # range [...]
-- | ```
combineModifiers :: forall d r k. Array (ScaleModifier d r k) -> ScaleModifier d r k
combineModifiers mods = \scale -> Array.foldl (\s m -> m s) scale mods

-- ============================================================================
-- SAMPLING
-- ============================================================================

-- | Sample a scale at n evenly-spaced points in [0, 1]
-- |
-- | Useful for generating color gradients, paths, etc.
-- |
-- | ```purescript
-- | -- Generate 256 colors for a gradient
-- | colors = sample 256 viridisColorScale
-- | ```
sample :: forall r. Int -> Scale Number r Continuous -> Array r
sample n scale =
  let
    step = if n <= 1 then 0.0 else 1.0 / (toNumber (n - 1))
    ts = Array.range 0 (n - 1) <#> \i -> toNumber i * step
  in
    ts <#> Scale.applyScale scale

-- | Sample within a specific range
-- |
-- | ```purescript
-- | -- Sample 50 points between 0.2 and 0.8
-- | samples = sampleRange 50 0.2 0.8 scale
-- | ```
sampleRange :: forall r. Int -> Number -> Number -> Scale Number r Continuous -> Array r
sampleRange n start end scale =
  let
    step = if n <= 1 then 0.0 else (end - start) / (toNumber (n - 1))
    ts = Array.range 0 (n - 1) <#> \i -> start + toNumber i * step
  in
    ts <#> Scale.applyScale scale

-- | Sample and return both domain and range values
-- |
-- | ```purescript
-- | -- Get (input, output) pairs
-- | pairs = sampleWithDomain 10 scale
-- | ```
sampleWithDomain :: forall r. Int -> Scale Number r Continuous -> Array (Tuple Number r)
sampleWithDomain n scale =
  let
    step = if n <= 1 then 0.0 else 1.0 / (toNumber (n - 1))
    ts = Array.range 0 (n - 1) <#> \i -> toNumber i * step
  in
    ts <#> \t -> Tuple t (Scale.applyScale scale t)

-- ============================================================================
-- TICK OPERATIONS
-- ============================================================================

-- | Get tick positions as pixel coordinates
-- |
-- | ```purescript
-- | -- Get where to draw tick marks
-- | positions = tickPositions 10 xScale
-- | ```
tickPositions :: forall r k. Int -> Scale Number r k -> Array r
tickPositions count scale =
  Scale.ticks count scale <#> Scale.applyScale scale

-- | Get formatted tick labels
tickLabels :: forall r k. Int -> String -> Scale Number r k -> Array String
tickLabels count specifier scale =
  let
    formatter = Scale.tickFormat count specifier scale
    tickVals = Scale.ticks count scale
  in
    tickVals <#> formatter

-- | Get ticks with both position and label
-- |
-- | ```purescript
-- | -- For rendering axis ticks
-- | tickData = ticksWithLabels 10 ".0f" xScale
-- | -- Returns: [{ position: 0.0, label: "0" }, ...]
-- | ```
ticksWithLabels :: forall r k. Int -> String -> Scale Number r k -> Array { position :: r, label :: String }
ticksWithLabels count specifier scale =
  let
    formatter = Scale.tickFormat count specifier scale
    tickVals = Scale.ticks count scale
  in
    tickVals <#> \t -> { position: Scale.applyScale scale t, label: formatter t }

-- ============================================================================
-- INTERPOLATION COMBINATORS
-- ============================================================================

-- | Blend two interpolators together
-- |
-- | ```purescript
-- | -- 50% viridis, 50% plasma
-- | blended = blendInterpolators 0.5 interpolateViridis interpolatePlasma
-- | ```
blendInterpolators :: Number -> Interpolator String -> Interpolator String -> Interpolator String
blendInterpolators mix i1 i2 = \t ->
  -- Note: This is a simple implementation - proper color blending would need RGB interpolation
  if t < mix then i1 (t / mix) else i2 ((t - mix) / (1.0 - mix))

-- | Reverse an interpolator (1-t instead of t)
-- |
-- | ```purescript
-- | reversed = reverseInterpolator interpolateViridis
-- | ```
reverseInterpolator :: forall a. Interpolator a -> Interpolator a
reverseInterpolator interp = \t -> interp (1.0 - t)

-- | Clamp interpolator input to [0, 1]
clampInterpolator :: forall a. Interpolator a -> Interpolator a
clampInterpolator interp = \t -> interp (max 0.0 (min 1.0 t))

-- | Make an interpolator cycle (values outside [0,1] wrap)
cycleInterpolator :: forall a. Interpolator a -> Interpolator a
cycleInterpolator interp = \t ->
  let t' = t - toNumber (floor t)
  in interp (if t' < 0.0 then t' + 1.0 else t')

-- ============================================================================
-- SCALE TRANSFORMATIONS
-- ============================================================================

-- | Create a normalizing scale (maps domain to [0, 1])
-- |
-- | ```purescript
-- | normalizer = normalize 0.0 100.0
-- | normalizer 50.0  -- Returns 0.5
-- | ```
normalize :: Number -> Number -> ContinuousScale
normalize minVal maxVal =
  Scale.domain [minVal, maxVal] $ Scale.range [0.0, 1.0] createLinear

-- | Create a quantizing scale (continuous → discrete buckets)
-- |
-- | ```purescript
-- | colorBuckets = quantize ["low", "medium", "high"]
-- | colorBuckets 0.0 100.0 33.0  -- Returns "low"
-- | colorBuckets 0.0 100.0 66.0  -- Returns "medium"
-- | ```
quantize :: forall a. Array a -> Number -> Number -> Number -> a
quantize buckets minVal maxVal value =
  let
    n = Array.length buckets
    normalized = (value - minVal) / (maxVal - minVal)
    idx = min (n - 1) (max 0 (floor (normalized * toNumber n)))
  in
    case Array.index buckets idx of
      Just b -> b
      Nothing -> case Array.head buckets of
        Just b -> b
        Nothing -> unsafeCoerce unit  -- Should never happen if buckets is non-empty

-- | Create a threshold scale with custom breakpoints
-- |
-- | ```purescript
-- | rating = threshold [0.0, 60.0, 80.0, 90.0] ["F", "D", "C", "B", "A"]
-- | rating 75.0  -- Returns "C"
-- | ```
threshold :: forall a. Array Number -> Array a -> Number -> a
threshold thresholds values value =
  let
    idx = Array.length (Array.filter (_ <= value) thresholds)
  in
    case Array.index values (min idx (Array.length values - 1)) of
      Just v -> v
      Nothing -> case Array.head values of
        Just v -> v
        Nothing -> unsafeCoerce unit

-- ============================================================================
-- USEFUL COMBINATORS
-- ============================================================================

-- | Get the extent (min, max) of a scale's output for inputs in [0, 1]
scaleExtent :: ContinuousScale -> { min :: Number, max :: Number }
scaleExtent scale =
  let
    v0 = Scale.applyScale scale 0.0
    v1 = Scale.applyScale scale 1.0
  in
    { min: min v0 v1, max: max v0 v1 }

-- | Get the midpoint of a scale
scaleMidpoint :: ContinuousScale -> Number
scaleMidpoint scale = Scale.applyScale scale 0.5

-- | Check if a value is within a scale's output range
scaleInRange :: ContinuousScale -> Number -> Boolean
scaleInRange scale value =
  let
    ext = scaleExtent scale
  in
    value >= ext.min && value <= ext.max

-- ============================================================================
-- INTERNAL HELPERS
-- ============================================================================

foreign import createLinear :: ContinuousScale

foreign import unsafeCoerceScale :: forall a b c d e f. Scale a b c -> Scale d e f

toNumber :: Int -> Number
toNumber = unsafeCoerce

floor :: Number -> Int
floor = unsafeCoerce <<< floorImpl

foreign import floorImpl :: Number -> Number

foreign import unsafeCoerce :: forall a b. a -> b
