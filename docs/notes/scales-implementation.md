# PSD3 Scale Implementation Notes

## Overview

The PSD3 Scale module provides a comprehensive, type-safe implementation of D3's scale functionality with additional functional programming idioms.

## Module Structure

### PSD3.Scale (Core)

Full D3-compatible scale implementation:

**Scale Types (with phantom types for safety):**
- `ContinuousScale` - linear, log, pow, sqrt, symlog
- `BandScale` - band, point
- `OrdinalScale` - ordinal

**Configuration (immutable API - returns new scale):**
- `domain`, `range`, `clamp`, `nice`, `round`
- `padding`, `paddingInner`, `paddingOuter`, `align`
- `base` (log), `exponent` (pow), `constant` (symlog)

**Operations:**
- `applyScale` - apply scale to value
- `invert` - reverse lookup (returns `Maybe`)
- `ticks`, `tickFormat` - for axis rendering
- `bandwidth`, `step` - for band scales
- `copy` - clone a scale

**Functional Combinators:**
- `andThen` - compose scales
- `contramap` - transform domain (contravariant)
- `map` - transform range (covariant)
- `dimap` - transform both (profunctor-like)

**Color Schemes & Interpolators:**
- Categorical: `schemeCategory10`, `schemePaired`, `schemeSet1/2/3`
- Sequential: `interpolateViridis`, `interpolatePlasma`, `interpolateInferno`, etc.
- Diverging: `interpolateRdYlGn`, `interpolateRdBu`, `interpolatePiYG`, etc.

### PSD3.Scale.FP (Functional Programming Abstractions)

**Scale Modifiers (Endo-like):**
```purescript
combined = combineModifiers [niceModifier, clampModifier, roundModifier]
myScale = linear # combined # domain [...] # range [...]
```

**Sampling:**
```purescript
-- Generate 256 colors for a gradient
colors = sample 256 viridisColorScale

-- Sample with domain values
pairs = sampleWithDomain 10 scale  -- Array (Tuple Number r)
```

**Tick Operations:**
```purescript
tickData = ticksWithLabels 10 ".0f" xScale
-- Returns: [{ position: 0.0, label: "0" }, ...]
```

**Interpolation Combinators:**
```purescript
reversed = reverseInterpolator interpolateViridis
clamped = clampInterpolator myInterpolator
cycling = cycleInterpolator myInterpolator
```

**Utility Functions:**
```purescript
normalizer = normalize 0.0 100.0  -- Maps to [0,1]
rating = threshold [60.0, 80.0, 90.0] ["F", "D", "C", "B", "A"]
buckets = quantize ["low", "medium", "high"]
```

## FP Idioms

### Scales as Profunctors

Scales are naturally profunctorial:
- **Domain** is contravariant (you consume domain values)
- **Range** is covariant (you produce range values)

```purescript
-- Transform domain preprocessing
celsiusScale = fahrenheitScale # contramap celsiusToFahrenheit

-- Transform range postprocessing
offsetScale = pixelScale # map (_ + margin)

-- Both at once
transformed = scale # dimap preprocess postprocess
```

### Scale Composition

Scales compose naturally:

```purescript
-- Chain scales: data → normalize → color
tempToColor = normalize (-10.0) 40.0 `andThen` interpolateRdYlGn
```

### Modifiers as Monoid

Scale modifiers form a monoid under composition:

```purescript
type ScaleModifier d r k = Scale d r k -> Scale d r k

combined = niceModifier <> clampModifier <> roundModifier
```

## Future FP Explorations

1. **Profunctor instance** - Make Scale a proper Profunctor typeclass instance
2. **Ticks as Unfoldable** - Lazy tick generation
3. **Scale Semigroup** - Combine scales meaningfully (e.g., piecewise)
4. **Lens-based configuration** - Use lenses for scale properties
5. **Free monad for scale DSL** - Compose scale configurations declaratively

## Usage Examples

### Basic Linear Scale
```purescript
xScale = linear # domain [0.0, 100.0] # range [0.0, 800.0]
pixelX = applyScale xScale 50.0  -- 400.0
```

### Color Scale for Heatmap
```purescript
colorScale = \value -> interpolateViridis (value / maxValue)
fill = colorScale <<< _.value
```

### Band Scale for Bar Chart
```purescript
xScale = band
  # domain ["Mon", "Tue", "Wed", "Thu", "Fri"]
  # range [0.0, 500.0]
  # padding 0.1

barX = applyScale xScale "Wed"
barWidth = bandwidth xScale
```

### Composed Pipeline
```purescript
-- Temperature → Normalized → Color
tempToColor = normalize (-10.0) 40.0 `andThen` interpolateRdYlGn

-- Apply to data
colors = temperatures <#> tempToColor
```

## Related Files

- `psd3-selection/src/PSD3/Scale.purs` - Core module
- `psd3-selection/src/PSD3/Scale.js` - FFI bindings
- `psd3-selection/src/PSD3/Scale/FP.purs` - FP abstractions
- `demo-website/src/Viz/FPFTW/ScalesDemo.purs` - Visual demo
- `demo-website/src/Component/HowTo/HowtoAxesScales.purs` - Tutorial page
