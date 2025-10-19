# Syntax and API Improvements for D3 PureScript Charts

## Analysis of Chart Examples

This document outlines opportunities to improve the ergonomics and consistency of the D3 PureScript library based on a review of the Line Chart, Bar Chart, Scatter Plot, Bubble Chart, and Chord Diagram examples.

---

## 1. Inconsistencies in Naming and Structure

### Dimensions Pattern
- **Line/Bar/Scatter charts**: Use `defaultDimensions`, `innerWidth`, `innerHeight` pattern with margins
- **Bubble/Chord charts**: Inline dimension records without margin handling
- **Issue**: No consistent approach to sizing and margins
- **Recommendation**: Standardize on a single dimensions pattern across all charts

### Variable Naming
- **Group containers**: `chartGroup` vs `centerGroup`
- **Drawing functions**: `addPoint`, `addBar` vs `drawBubble`, `drawRibbon`, `drawArc`
  - Mixing "add" and "draw" verbs inconsistently
- **Recommendation**: Standardize on one naming pattern (suggest "draw" prefix for clarity)

### CSS Classes
- Inconsistent hyphenation: `"line-chart"`, `"bar-chart"` vs `"bubble-chart"`, `"chord-diagram"`
- Generally consistent, but worth documenting the pattern
- **Recommendation**: Document and enforce consistent naming convention

---

## 2. Major Verbosity Issue: Generator Configuration

This is the **biggest pain point** compared to D3 JavaScript.

### Current PureScript (3 lines):
```purescript
let arcGen0 = arcGenerator_ unit
let arcGen1 = setArcInnerRadius_ arcGen0 innerR
let arcGen = setArcOuterRadius_ arcGen1 outerR
```

### D3 JavaScript (2 lines):
```javascript
const arcGen = d3.arc()
  .innerRadius(innerR)
  .outerRadius(outerR)
```

### Proposed Solutions:

#### Option A: Pipe-forward operator (#)
```purescript
let arcGen = arcGenerator_ unit
           # setArcInnerRadius_ innerR
           # setArcOuterRadius_ outerR
```

**Implementation**: Flip FFI function parameter order:
```purescript
-- Change from:
setArcInnerRadius_ :: ArcGenerator_ -> Number -> ArcGenerator_

-- To:
setArcInnerRadius_ :: Number -> ArcGenerator_ -> ArcGenerator_
```

#### Option B: Configuration record
```purescript
let arcGen = createArcGenerator { innerRadius: innerR, outerRadius: outerR }
```

#### Option C: Backwards composition (<<<)
```purescript
let arcGen = (setArcOuterRadius_ outerR <<< setArcInnerRadius_ innerR <<< arcGenerator_) unit
```

**Recommendation**: Option A is closest to D3's style while maintaining type safety.

---

## 3. Repeated Boilerplate: Extent Calculation

All 2D charts repeat this pattern:

### Current PureScript:
```purescript
let xValues = map _.x dataPoints
let yValues = map _.y dataPoints
let minX = fromMaybe 0.0 $ minimum xValues
let maxX = fromMaybe 100.0 $ maximum xValues
let minY = fromMaybe 0.0 $ minimum yValues
let maxY = fromMaybe 100.0 $ maximum yValues
```

### D3 JavaScript equivalent:
```javascript
const xExtent = d3.extent(data, d => d.x)
const yExtent = d3.extent(data, d => d.y)
```

### Proposed helper:
```purescript
-- Returns [min, max] like D3
extent :: forall a. Ord a => (DataPoint -> a) -> Array DataPoint -> Array a
extent accessor data =
  let values = map accessor data
  in [fromMaybe 0.0 $ minimum values, fromMaybe 100.0 $ maximum values]

-- Usage:
let [minX, maxX] = extent _.x dataPoints
let [minY, maxY] = extent _.y dataPoints
```

**Alternative**: Use a record instead of array for better type safety:
```purescript
extent :: forall a. Ord a => (DataPoint -> a) -> Array DataPoint -> { min :: a, max :: a }
extent accessor data =
  let values = map accessor data
  in { min: fromMaybe 0.0 $ minimum values
     , max: fromMaybe 100.0 $ maximum values
     }

-- Usage:
let xExtent = extent _.x dataPoints
let yExtent = extent _.y dataPoints
xScale <- liftEffect $ createLinearScale {
  domain: [xExtent.min, xExtent.max],
  range: [0.0, iWidth]
}
```

---

## 4. Transform String Construction

Manual transform building is error-prone:

### Current:
```purescript
transform [ \_ -> "translate(" <> show x <> "," <> show y <> ")" ]
```

### Proposed helper:
```purescript
translateTransform :: Number -> Number -> Datum_ -> String
translateTransform x y = \_ -> "translate(" <> show x <> "," <> show y <> ")"

-- Usage:
transform [ translateTransform dims.margin.left dims.margin.top ]
```

### Even better - integrate with existing transform system:
```purescript
-- If this already exists, document it and use consistently
translate :: Number -> Number -> TransformFunction

-- Usage:
transform [ translate dims.margin.left dims.margin.top ]
```

---

## 5. The `let _ <- ...` Pattern

This appears frequently and is confusing for beginners:

### Current:
```purescript
_ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
_ <- liftEffect $ callAxis yAxisGroup (axisLeft yScale)
```

### Issues:
- Not obvious why we ignore the result
- Mixing `let _ <-` and `_ <-` inconsistently

### Recommendations:
1. Be consistent (use `_ <-` without `let`)
2. Add comments explaining why results are discarded
3. Consider if axes should return useful values for chaining

### Example with documentation:
```purescript
-- Add axes (returns selections but we don't need them here)
_ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
_ <- liftEffect $ callAxis yAxisGroup (axisLeft yScale)
```

---

## 6. Missing D3 Conveniences

### Color Scales
**Current**: Manual case statements
```purescript
let color = case colors !! idx of
              Just c -> c
              Nothing -> "#999999"
```

**D3 JavaScript**:
```javascript
const color = d3.scaleOrdinal(d3.schemeCategory10)
color(idx)
```

**Proposed**: Wrap D3's color scales
```purescript
colorScale <- liftEffect $ createOrdinalColorScale schemeCategory10
let color = applyColorScale colorScale idx
```

### Axis Configuration
**Current**: Imperative
```purescript
xAxisGroup <- appendTo chartGroup Group [
    classed "x-axis"
  , transform [ \_ -> "translate(0," <> show iHeight <> ")" ]
  ]
_ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
```

**Could be**:
```purescript
_ <- addAxis chartGroup (axisBottom xScale) {
  position: Bottom,
  transform: "translate(0," <> show iHeight <> ")",
  class: "x-axis"
}
```

---

## 7. Specific Recommendations

### Short-term improvements (no breaking changes):

#### 1. Standardize helper naming
- Use `draw` prefix consistently: `drawPoint`, `drawBar`, `drawBubble`
- Use `chartGroup` for all container groups
- Document naming conventions

#### 2. Add pipe-forward support for generator configuration
Change FFI signatures to enable piping:
```purescript
-- Change from:
setArcInnerRadius_ :: ArcGenerator_ -> Number -> ArcGenerator_
setArcOuterRadius_ :: ArcGenerator_ -> Number -> ArcGenerator_

-- To:
setArcInnerRadius_ :: Number -> ArcGenerator_ -> ArcGenerator_
setArcOuterRadius_ :: Number -> ArcGenerator_ -> ArcGenerator_

-- Similarly for all generator setters:
setRibbonRadius_ :: Number -> RibbonGenerator_ -> RibbonGenerator_
packSetSize_ :: Number -> Number -> PackLayout_ -> PackLayout_
packSetPadding_ :: Number -> PackLayout_ -> PackLayout_
```

**Usage**:
```purescript
let arcGen = arcGenerator_ unit
           # setArcInnerRadius_ innerR
           # setArcOuterRadius_ outerR

let ribbonGen = ribbonGenerator_ unit
              # setRibbonRadius_ innerR

let packLayout = packLayout_ unit
               # packSetSize_ dims.width dims.height
               # packSetPadding_ 2.0
```

#### 3. Create extent helper
Add to a new module `D3.Utilities` or `D3.Data.Helpers`:
```purescript
-- Option 1: Array return (matches D3)
extent :: forall a. Ord a => (DataPoint -> a) -> Array DataPoint -> Array a

-- Option 2: Record return (more type-safe)
extent :: forall a. Ord a => (DataPoint -> a) -> Array DataPoint -> { min :: a, max :: a }
```

#### 4. Document the `_ <-` pattern
Add comments in examples explaining when and why we discard results:
```purescript
-- Axis calls return selections but we already have references to these groups,
-- so we discard the return value
_ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
```

#### 5. Extract common chart setup
Create a helper to reduce boilerplate:
```purescript
type ChartSetup = {
  svg :: D3Selection_,
  chartGroup :: D3Selection_,
  dims :: ChartDimensions,
  iWidth :: Number,
  iHeight :: Number
}

setupChart :: forall m. MonadEffect m => SelectionM D3Selection_ m =>
  Selector D3Selection_ -> String -> ChartDimensions -> m ChartSetup
setupChart selector className dims = do
  let iWidth = innerWidth dims
  let iHeight = innerHeight dims

  root <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed className
    , width dims.width
    , height dims.height
    ]

  chartGroup <- appendTo svg Group [
      transform [ translateTransform dims.margin.left dims.margin.top ]
    ]

  pure { svg, chartGroup, dims, iWidth, iHeight }

-- Usage:
setup <- setupChart selector "line-chart" defaultDimensions
-- Now use setup.chartGroup, setup.iWidth, etc.
```

---

### Medium-term improvements (worth considering):

#### 1. Unified dimensions API
All charts should use consistent margin/sizing:
```purescript
-- Standardize on ChartDimensions record
type ChartDimensions = {
  width :: Number,
  height :: Number,
  margin :: { top :: Number, right :: Number, bottom :: Number, left :: Number }
}

-- Even for charts without traditional margins (Chord, Bubble)
-- use margin: { top: 0, right: 0, bottom: 0, left: 0 }
```

#### 2. Transform helpers library
Make transform construction type-safe:
```purescript
data Transform
  = Translate Number Number
  | Rotate Number
  | Scale Number Number
  | Custom String

renderTransform :: Transform -> String
applyTransforms :: Array Transform -> SelectionAttribute
```

#### 3. Color scale API
Wrap D3's color scales properly:
```purescript
-- Create module D3.Scales.Color
foreign import data ColorScale :: Type
foreign import createOrdinalColorScale :: Array String -> Effect ColorScale
foreign import applyColorScale :: ColorScale -> Int -> String

-- Predefined schemes
schemeCategory10 :: Array String
schemeTableau10 :: Array String
```

#### 4. Declarative axis API
Reduce boilerplate for common axis patterns:
```purescript
type AxisConfig = {
  scale :: LinearScale,
  orientation :: AxisOrientation,
  position :: { x :: Number, y :: Number },
  className :: String
}

data AxisOrientation = Top | Bottom | Left | Right

addConfiguredAxis :: forall m.
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  D3Selection_ -> AxisConfig -> m D3Selection_
```

---

## Summary

### Most Impactful Improvements (Priority Order):

1. **Fix generator configuration verbosity** - Enable `#` operator for chaining
   - Flip parameter order in all generator setter functions
   - Highest priority - affects all examples

2. **Add `extent` helper** - Eliminate repeated boilerplate
   - Simple utility function
   - Immediate readability improvement

3. **Standardize naming** - Use consistent verb prefixes and variable names
   - Document conventions
   - Update examples to follow conventions

4. **Transform helpers** - Make string construction safer
   - Either document existing helpers or create new ones
   - Reduce error-prone manual string concatenation

### Key Insight

D3's method chaining can be approximated with PureScript's pipe operators (`#`), but our FFI function signatures need to support this pattern by taking the object-to-modify as the **last** parameter instead of the first.

This single change would dramatically improve the ergonomics of generator configuration, bringing the PureScript API much closer to D3's familiar style while preserving full type safety.

---

## Implementation Plan (Suggested)

### Phase 1: Low-hanging fruit
1. Flip parameter order for generator setters
2. Add `extent` helper function
3. Document naming conventions
4. Update all examples to use consistent naming

### Phase 2: Helper functions
1. Create `setupChart` helper
2. Add transform helpers
3. Add color scale wrappers
4. Update examples to use helpers

### Phase 3: API improvements
1. Consider declarative axis API
2. Evaluate need for more sophisticated transform system
3. Gather user feedback on ergonomics

---

*Analysis completed: 2025-10-16*
