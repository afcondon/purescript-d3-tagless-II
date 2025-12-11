-- | PSD3.Scale - Type-safe scales with functional programming idioms
-- |
-- | This module provides D3-compatible scales with PureScript type safety
-- | and functional programming abstractions. Uses d3-scale-chromatic.js
-- | via FFI. 
-- |
-- | ## Basic Usage
-- |
-- | ```purescript
-- | import PSD3.Scale (linear, domain, range, apply, ticks)
-- |
-- | -- Create a linear scale
-- | myScale = linear # domain [0.0, 100.0] # range [0.0, 800.0]
-- |
-- | -- Apply the scale
-- | pixelX = apply myScale 50.0  -- Returns 400.0
-- |
-- | -- Get tick marks
-- | tickValues = ticks 10 myScale  -- Returns nice tick values
-- | ```
-- |
-- | ## Functional Idioms
-- |
-- | Scales can be composed and transformed:
-- |
-- | ```purescript
-- | -- Compose scales
-- | timeToPixel = timeScale `andThen` positionScale
-- |
-- | -- Transform with Profunctor-like operations
-- | celsiusScale = fahrenheitScale # contramap fahrenheitToCelsius
-- | ```
module PSD3.Scale
  ( -- * Scale Types
    Scale
  , Continuous
  , Ordinal
  , Band
  , Point
  , Time
  , ContinuousScale
  , OrdinalScale
  , BandScale
  , TimeScale

  -- * Continuous Scale Constructors
  , linear
  , log
  , pow
  , sqrt
  , symlog

  -- * Time Scale Constructors
  , JSDate
  , scaleTime
  , scaleUtc

  -- * Sequential/Diverging Scale Constructors
  , Sequential
  , Diverging
  , SequentialScale
  , DivergingScale
  , sequential
  , sequentialLog
  , sequentialSqrt
  , sequentialSymlog
  , diverging
  , divergingLog
  , divergingSqrt
  , divergingSymlog

  -- * Quantize/Quantile Scale Constructors
  , Quantize
  , Quantile
  , QuantizeScale
  , QuantileScale
  , scaleQuantize
  , scaleQuantile

  -- * Ordinal Scale Constructors
  , ordinal
  , band
  , point

  -- * Scale Configuration
  , domain
  , range
  , clamp
  , nice
  , niceCount
  , padding
  , paddingInner
  , paddingOuter
  , align
  , round
  , base
  , exponent
  , constant

  -- * Scale Operations
  , applyScale
  , invert
  , ticks
  , tickFormat
  , bandwidth
  , step
  , copy

  -- * Functional Combinators
  , andThen
  , contramap
  , map
  , dimap

  -- * Interpolators (for color scales)
  , Interpolator
  , interpolateNumber
  , interpolateRgb
  , interpolateHsl

  -- * Color Schemes (Categorical)
  , schemeCategory10
  , schemeCategory10At
  , schemeTableau10
  , schemeTableau10At
  , schemePaired
  , schemePairedAt
  , schemeSet1
  , schemeSet2
  , schemeSet3
  , schemeAccent
  , schemeDark2
  , schemePastel1
  , schemePastel2

  -- * Sequential Interpolators (Single-Hue)
  , interpolateBlues
  , interpolateGreens
  , interpolateGreys
  , interpolateOranges
  , interpolatePurples
  , interpolateReds

  -- * Sequential Interpolators (Multi-Hue)
  , interpolateViridis
  , interpolatePlasma
  , interpolateInferno
  , interpolateMagma
  , interpolateTurbo
  , interpolateWarm
  , interpolateCool
  , interpolateRainbow
  , interpolateCividis
  , interpolateCubehelixDefault
  , interpolateBuGn
  , interpolateBuPu
  , interpolateGnBu
  , interpolateOrRd
  , interpolatePuBuGn
  , interpolatePuBu
  , interpolatePuRd
  , interpolateRdPu
  , interpolateYlGnBu
  , interpolateYlGn
  , interpolateYlOrBr
  , interpolateYlOrRd

  -- * Diverging Interpolators
  , interpolateRdYlGn
  , interpolateRdBu
  , interpolatePiYG
  , interpolateBrBG
  , interpolatePRGn
  , interpolateSpectral
  , interpolateRdGy
  , interpolateRdYlBu

  -- * Cyclical Interpolators
  , interpolateSinebow

  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

-- ============================================================================
-- SCALE TYPES
-- ============================================================================

-- | A scale maps values from a domain to a range
-- | The phantom types track:
-- |   - `domain` - the input type
-- |   - `range` - the output type
-- |   - `kind` - the scale kind (Continuous, Ordinal, Band, etc.)
foreign import data Scale :: Type -> Type -> Type -> Type

-- | Phantom type for continuous scales (Linear, Log, Pow, etc.)
data Continuous

-- | Phantom type for ordinal scales
data Ordinal

-- | Phantom type for band scales
data Band

-- | Phantom type for point scales
data Point

-- | Phantom type for time scales
data Time

-- | Phantom type for sequential scales (color gradients)
data Sequential

-- | Phantom type for diverging scales (two-directional color gradients)
data Diverging

-- | Phantom type for quantize scales (continuous → discrete buckets)
data Quantize

-- | Phantom type for quantile scales (continuous → discrete based on quantiles)
data Quantile

-- | Convenient type aliases
type ContinuousScale = Scale Number Number Continuous
type OrdinalScale domain range = Scale domain range Ordinal
type BandScale domain = Scale domain Number Band
type TimeScale = Scale JSDate Number Time
type SequentialScale = Scale Number String Sequential
type DivergingScale = Scale Number String Diverging
type QuantizeScale r = Scale Number r Quantize
type QuantileScale r = Scale Number r Quantile

-- | JavaScript Date object for time scales
-- | Compatible with native JS Date, can be created via FFI or js-date library
foreign import data JSDate :: Type

-- ============================================================================
-- CONTINUOUS SCALE CONSTRUCTORS
-- ============================================================================

-- | Create a linear scale
-- | Maps domain values linearly to range values
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | apply scale 50.0  -- Returns 250.0
-- | ```
foreign import linear :: ContinuousScale

-- | Create a logarithmic scale
-- | Uses log transformation - domain must not include zero
-- |
-- | ```purescript
-- | scale = log # domain [1.0, 1000.0] # range [0.0, 300.0]
-- | ```
foreign import log :: ContinuousScale

-- | Create a power scale with configurable exponent
-- |
-- | ```purescript
-- | scale = pow # exponent 2.0 # domain [0.0, 100.0] # range [0.0, 500.0]
-- | ```
foreign import pow :: ContinuousScale

-- | Create a square root scale (pow with exponent 0.5)
-- |
-- | Useful for sizing circles by area
-- |
-- | ```purescript
-- | radiusScale = sqrt # domain [0.0, maxValue] # range [0.0, 50.0]
-- | ```
foreign import sqrt :: ContinuousScale

-- | Create a symlog scale (symmetric log)
-- | Handles negative values and zero gracefully
-- |
-- | ```purescript
-- | scale = symlog # constant 1.0 # domain [-100.0, 100.0] # range [0.0, 500.0]
-- | ```
foreign import symlog :: ContinuousScale

-- ============================================================================
-- TIME SCALE CONSTRUCTORS
-- ============================================================================

-- | Create a time scale for local time
-- | Like linear but domain is Date objects with time-appropriate ticks
-- |
-- | ```purescript
-- | timeScale = scaleTime
-- |   # domain [startDate, endDate]
-- |   # range [0.0, width]
-- | ```
foreign import scaleTime_ :: TimeScale

-- | Create a time scale for local time
scaleTime :: TimeScale
scaleTime = scaleTime_

-- | Create a UTC time scale
-- | Like scaleTime but uses UTC for formatting
-- |
-- | ```purescript
-- | utcScale = scaleUtc
-- |   # domain [startDate, endDate]
-- |   # range [0.0, width]
-- | ```
foreign import scaleUtc_ :: TimeScale

-- | Create a UTC time scale
scaleUtc :: TimeScale
scaleUtc = scaleUtc_

-- ============================================================================
-- SEQUENTIAL/DIVERGING SCALE CONSTRUCTORS
-- ============================================================================

-- | Create a sequential scale with an interpolator
-- | Maps domain [0, 1] to colors via the interpolator
-- |
-- | ```purescript
-- | colorScale = sequential interpolateViridis
-- |   # domain [0.0, 100.0]
-- | ```
foreign import sequential_ :: Interpolator String -> SequentialScale

sequential :: Interpolator String -> SequentialScale
sequential = sequential_

-- | Sequential scale with logarithmic transform
foreign import sequentialLog_ :: Interpolator String -> SequentialScale

sequentialLog :: Interpolator String -> SequentialScale
sequentialLog = sequentialLog_

-- | Sequential scale with square root transform
foreign import sequentialSqrt_ :: Interpolator String -> SequentialScale

sequentialSqrt :: Interpolator String -> SequentialScale
sequentialSqrt = sequentialSqrt_

-- | Sequential scale with symlog transform
foreign import sequentialSymlog_ :: Interpolator String -> SequentialScale

sequentialSymlog :: Interpolator String -> SequentialScale
sequentialSymlog = sequentialSymlog_

-- | Create a diverging scale with an interpolator
-- | Maps domain with midpoint to colors via the interpolator
-- |
-- | ```purescript
-- | colorScale = diverging interpolateRdBu
-- |   # domain [-1.0, 0.0, 1.0]
-- | ```
foreign import diverging_ :: Interpolator String -> DivergingScale

diverging :: Interpolator String -> DivergingScale
diverging = diverging_

-- | Diverging scale with logarithmic transform
foreign import divergingLog_ :: Interpolator String -> DivergingScale

divergingLog :: Interpolator String -> DivergingScale
divergingLog = divergingLog_

-- | Diverging scale with square root transform
foreign import divergingSqrt_ :: Interpolator String -> DivergingScale

divergingSqrt :: Interpolator String -> DivergingScale
divergingSqrt = divergingSqrt_

-- | Diverging scale with symlog transform
foreign import divergingSymlog_ :: Interpolator String -> DivergingScale

divergingSymlog :: Interpolator String -> DivergingScale
divergingSymlog = divergingSymlog_

-- ============================================================================
-- QUANTIZE/QUANTILE SCALE CONSTRUCTORS
-- ============================================================================

-- | Create a quantize scale
-- | Maps continuous domain to discrete range (uniform intervals)
-- |
-- | ```purescript
-- | colorScale = scaleQuantize
-- |   # domain [0.0, 100.0]
-- |   # range ["low", "medium", "high"]
-- | ```
foreign import scaleQuantize :: forall r. QuantizeScale r

-- | Create a quantile scale
-- | Maps continuous domain to discrete range based on quantiles
-- |
-- | ```purescript
-- | colorScale = scaleQuantile
-- |   # domain [0.0, 25.0, 50.0, 75.0, 100.0]
-- |   # range ["Q1", "Q2", "Q3", "Q4"]
-- | ```
foreign import scaleQuantile :: forall r. QuantileScale r

-- ============================================================================
-- ORDINAL SCALE CONSTRUCTORS
-- ============================================================================

-- | Create an ordinal scale
-- | Maps discrete domain values to discrete range values
-- |
-- | ```purescript
-- | colorScale = ordinal
-- |   # domain ["a", "b", "c"]
-- |   # range ["red", "green", "blue"]
-- | ```
foreign import ordinal :: forall d r. OrdinalScale d r

-- | Create a band scale
-- | Maps discrete domain values to continuous bands with configurable padding
-- |
-- | ```purescript
-- | xScale = band
-- |   # domain ["Mon", "Tue", "Wed", "Thu", "Fri"]
-- |   # range [0.0, 500.0]
-- |   # padding 0.1
-- | ```
foreign import band :: forall d. BandScale d

-- | Create a point scale
-- | Like band but with zero bandwidth - just points
-- |
-- | ```purescript
-- | xScale = point
-- |   # domain ["A", "B", "C"]
-- |   # range [0.0, 500.0]
-- | ```
foreign import point :: forall d. BandScale d

-- ============================================================================
-- SCALE CONFIGURATION
-- ============================================================================

-- | Set the domain (input extent) of a scale
-- |
-- | For continuous scales: `[min, max]` (can include intermediate values for piecewise)
-- | For ordinal/band: array of discrete values
foreign import domain :: forall d r k. Array d -> Scale d r k -> Scale d r k

-- | Set the range (output extent) of a scale
-- |
-- | For continuous scales: `[min, max]`
-- | For band scales: `[start, end]` of the band space
foreign import range :: forall d r k. Array r -> Scale d r k -> Scale d r k

-- | Enable or disable clamping
-- | When enabled, output is constrained to the range even for out-of-domain inputs
foreign import clamp :: forall d r k. Boolean -> Scale d r k -> Scale d r k

-- | Extend the domain to nice round values
foreign import nice :: forall r k. Scale Number r k -> Scale Number r k

-- | Extend the domain to nice round values with specified tick count hint
foreign import niceCount :: forall r k. Int -> Scale Number r k -> Scale Number r k

-- | Set padding for band scales (both inner and outer)
foreign import padding :: forall d. Number -> BandScale d -> BandScale d

-- | Set inner padding (between bands) for band scales
foreign import paddingInner :: forall d. Number -> BandScale d -> BandScale d

-- | Set outer padding (before first and after last band) for band scales
foreign import paddingOuter :: forall d. Number -> BandScale d -> BandScale d

-- | Set alignment for band scales (0 = left, 0.5 = center, 1 = right)
foreign import align :: forall d. Number -> BandScale d -> BandScale d

-- | Enable rounding of output values
foreign import round :: forall d r k. Boolean -> Scale d r k -> Scale d r k

-- | Set the base for logarithmic scales (default 10)
foreign import base :: forall r. Number -> Scale Number r Continuous -> Scale Number r Continuous

-- | Set the exponent for power scales
foreign import exponent :: forall r. Number -> Scale Number r Continuous -> Scale Number r Continuous

-- | Set the constant for symlog scales
foreign import constant :: forall r. Number -> Scale Number r Continuous -> Scale Number r Continuous

-- ============================================================================
-- SCALE OPERATIONS
-- ============================================================================

-- | Apply a scale to a value
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | applyScale scale 50.0  -- Returns 250.0
-- | ```
foreign import applyScale :: forall d r k. Scale d r k -> d -> r

-- | Invert a continuous scale (range value → domain value)
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | invert scale 250.0  -- Returns Just 50.0
-- | ```
invert :: forall r. Scale Number r Continuous -> r -> Maybe Number
invert scale value = toMaybe (invertImpl scale value)

foreign import invertImpl :: forall r. Scale Number r Continuous -> r -> Nullable Number

-- | Generate tick values for a continuous scale
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | ticks 5 scale  -- Returns [0.0, 20.0, 40.0, 60.0, 80.0, 100.0]
-- | ```
foreign import ticks :: forall r k. Int -> Scale Number r k -> Array Number

-- | Get a tick formatter function
foreign import tickFormat :: forall r k. Int -> String -> Scale Number r k -> (Number -> String)

-- | Get the bandwidth of a band scale
foreign import bandwidth :: forall d. BandScale d -> Number

-- | Get the step size of a band scale (bandwidth + padding)
foreign import step :: forall d. BandScale d -> Number

-- | Create a copy of a scale
foreign import copy :: forall d r k. Scale d r k -> Scale d r k

-- ============================================================================
-- FUNCTIONAL COMBINATORS
-- ============================================================================

-- | Compose two scales: apply first, then second
-- |
-- | ```purescript
-- | -- Time → normalized → pixels
-- | combined = timeScale `andThen` pixelScale
-- | ```
andThen :: forall a b c k1 k2. Scale a b k1 -> Scale b c k2 -> (a -> c)
andThen s1 s2 = applyScale s2 <<< applyScale s1

-- | Transform the domain type (contravariant)
-- |
-- | ```purescript
-- | -- Convert Celsius input to a Fahrenheit scale
-- | celsiusScale = fahrenheitScale # contramap celsiusToFahrenheit
-- | ```
contramap :: forall a a' b k. (a' -> a) -> Scale a b k -> (a' -> b)
contramap f scale = applyScale scale <<< f

-- | Transform the range type (covariant/functor-like)
-- |
-- | ```purescript
-- | -- Add offset to all positions
-- | offsetScale = positionScale # map (_ + 50.0)
-- | ```
map :: forall a b b' k. (b -> b') -> Scale a b k -> (a -> b')
map f scale = f <<< applyScale scale

-- | Transform both domain and range (profunctor-like)
-- |
-- | ```purescript
-- | transformed = scale # dimap preprocess postprocess
-- | ```
dimap :: forall a a' b b' k. (a' -> a) -> (b -> b') -> Scale a b k -> (a' -> b')
dimap pre post scale = post <<< applyScale scale <<< pre

-- ============================================================================
-- INTERPOLATORS
-- ============================================================================

-- | An interpolator maps [0, 1] to a value
type Interpolator a = Number -> a

-- | Linear number interpolation
interpolateNumber :: Number -> Number -> Interpolator Number
interpolateNumber a b t = a + (b - a) * t

-- | RGB color interpolation
foreign import interpolateRgb_ :: String -> String -> Interpolator String

-- | RGB color interpolation
interpolateRgb :: String -> String -> Interpolator String
interpolateRgb = interpolateRgb_

-- | HSL color interpolation (better for perceptual uniformity)
foreign import interpolateHsl_ :: String -> String -> Interpolator String

interpolateHsl :: String -> String -> Interpolator String
interpolateHsl = interpolateHsl_

-- ============================================================================
-- COLOR SCHEMES (Categorical)
-- ============================================================================

-- | Category10 color scheme (10 colors)
foreign import schemeCategory10 :: Array String

-- | Get a Category10 color by index (wraps around modularly)
-- | Useful for coloring nodes by group number
-- |
-- | ```purescript
-- | fill = schemeCategory10At node.group
-- | ```
foreign import schemeCategory10At :: Int -> String

-- | Tableau10 color scheme (10 colors)
-- | Modern, accessible categorical palette
foreign import schemeTableau10 :: Array String

-- | Get a Tableau10 color by index (wraps around modularly)
-- | Useful for coloring categories in visualizations
-- |
-- | ```purescript
-- | fill = schemeTableau10At categoryIndex
-- | ```
foreign import schemeTableau10At :: Int -> String

-- | Paired color scheme (12 colors)
foreign import schemePaired :: Array String

-- | Get a Paired color by index (wraps around modularly)
foreign import schemePairedAt :: Int -> String

-- | Set1 color scheme (9 colors)
foreign import schemeSet1 :: Array String

-- | Set2 color scheme (8 colors)
foreign import schemeSet2 :: Array String

-- | Set3 color scheme (12 colors)
foreign import schemeSet3 :: Array String

-- | Accent color scheme (8 colors)
foreign import schemeAccent :: Array String

-- | Dark2 color scheme (8 colors)
foreign import schemeDark2 :: Array String

-- | Pastel1 color scheme (9 colors)
foreign import schemePastel1 :: Array String

-- | Pastel2 color scheme (8 colors)
foreign import schemePastel2 :: Array String

-- ============================================================================
-- SEQUENTIAL INTERPOLATORS (Single-Hue)
-- ============================================================================

-- | Blues single-hue sequential
foreign import interpolateBlues :: Interpolator String

-- | Greens single-hue sequential
foreign import interpolateGreens :: Interpolator String

-- | Greys single-hue sequential
foreign import interpolateGreys :: Interpolator String

-- | Oranges single-hue sequential
foreign import interpolateOranges :: Interpolator String

-- | Purples single-hue sequential
foreign import interpolatePurples :: Interpolator String

-- | Reds single-hue sequential
foreign import interpolateReds :: Interpolator String

-- ============================================================================
-- SEQUENTIAL INTERPOLATORS (Multi-Hue)
-- ============================================================================

-- | Viridis perceptually-uniform colormap
foreign import interpolateViridis :: Interpolator String

-- | Plasma colormap
foreign import interpolatePlasma :: Interpolator String

-- | Inferno colormap
foreign import interpolateInferno :: Interpolator String

-- | Magma colormap
foreign import interpolateMagma :: Interpolator String

-- | Turbo colormap (rainbow-like but more perceptual)
foreign import interpolateTurbo :: Interpolator String

-- | Warm colormap (red to yellow)
foreign import interpolateWarm :: Interpolator String

-- | Cool colormap (cyan to purple)
foreign import interpolateCool :: Interpolator String

-- | Rainbow colormap
foreign import interpolateRainbow :: Interpolator String

-- | Cividis colormap (colorblind-friendly)
foreign import interpolateCividis :: Interpolator String

-- | Cubehelix default colormap
foreign import interpolateCubehelixDefault :: Interpolator String

-- | Blue-Green sequential
foreign import interpolateBuGn :: Interpolator String

-- | Blue-Purple sequential
foreign import interpolateBuPu :: Interpolator String

-- | Green-Blue sequential
foreign import interpolateGnBu :: Interpolator String

-- | Orange-Red sequential
foreign import interpolateOrRd :: Interpolator String

-- | Purple-Blue-Green sequential
foreign import interpolatePuBuGn :: Interpolator String

-- | Purple-Blue sequential
foreign import interpolatePuBu :: Interpolator String

-- | Purple-Red sequential
foreign import interpolatePuRd :: Interpolator String

-- | Red-Purple sequential
foreign import interpolateRdPu :: Interpolator String

-- | Yellow-Green-Blue sequential
foreign import interpolateYlGnBu :: Interpolator String

-- | Yellow-Green sequential
foreign import interpolateYlGn :: Interpolator String

-- | Yellow-Orange-Brown sequential
foreign import interpolateYlOrBr :: Interpolator String

-- | Yellow-Orange-Red sequential
foreign import interpolateYlOrRd :: Interpolator String

-- ============================================================================
-- DIVERGING INTERPOLATORS
-- ============================================================================

-- | Red-Yellow-Green diverging
foreign import interpolateRdYlGn :: Interpolator String

-- | Red-Blue diverging
foreign import interpolateRdBu :: Interpolator String

-- | Pink-Yellow-Green diverging
foreign import interpolatePiYG :: Interpolator String

-- | Brown-Blue-Green diverging
foreign import interpolateBrBG :: Interpolator String

-- | Purple-Red-Green diverging
foreign import interpolatePRGn :: Interpolator String

-- | Spectral diverging
foreign import interpolateSpectral :: Interpolator String

-- | Red-Grey diverging
foreign import interpolateRdGy :: Interpolator String

-- | Red-Yellow-Blue diverging
foreign import interpolateRdYlBu :: Interpolator String

-- ============================================================================
-- CYCLICAL INTERPOLATORS
-- ============================================================================

-- | Sinebow cyclical colormap
foreign import interpolateSinebow :: Interpolator String
