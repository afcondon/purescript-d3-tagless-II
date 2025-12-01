# Visualization Modules Catalog

## Overview
This document catalogs all visualization modules in the codebase, assessing them for quality as examples, clarity, redundancy, and PureScript/FP idiomatic expression. We distinguish between two categories:

1. **Simple D3-like Charts** - Static or minimally interactive visualizations demonstrating D3 patterns
2. **Interactive Apps** - Stateful, event-driven applications using data visualization techniques

The PureScript idiomatic considerations matter more as we move toward the "app" end of the spectrum.

---

## 📊 Simple D3-like Charts (Pedagogical Examples)

### ⭐ Three Little Circles
**File**: `ThreeLittleCircles/ThreeLittleCircles.purs`
**Type**: Simple Chart (Pure D3 pattern)
**Quality**: ⭐⭐⭐⭐⭐ Excellent

**Assessment**:
- Perfect pedagogical example - the "Hello World" of data binding
- Very clean, minimal (~28 lines)
- Shows basic `simpleJoin` pattern clearly
- Has explicit Snippet markers for extraction

**FP/PureScript Idioms**: ✅ Good
- Monadic composition is clear
- Type signatures explicit

**Improvements Needed**: None - this is a teaching example and should stay simple

---

### ⭐ Parabola (Datum_ Accessor Pattern)
**File**: `Parabola/Parabola.purs`
**Type**: Simple Chart
**Quality**: ⭐⭐⭐⭐⭐ Excellent

**Assessment**:
- Shows the `datum_` accessor pattern very clearly
- Demonstrates color scales and coordinate transformations
- Good separation of concerns (accessor record vs draw function)
- Has snippet markers

**FP/PureScript Idioms**: ✅ Good
- Accessor record is a nice FP pattern
- Clear functional transformations

**Improvements Needed**: None - good teaching example

---

### ⭐ General Update Pattern (GUP)
**File**: `GUP.purs`
**Type**: Interactive Chart (Enter/Update/Exit pattern)
**Quality**: ⭐⭐⭐⭐ Very Good

**Assessment**:
- Critical D3 pattern demonstration
- Shows enter/update/exit with transitions
- Returns update function (Halogen-friendly pattern)
- Good use of `where` clause for separation

**FP/PureScript Idioms**: ✅ Good
- Higher-order function pattern (returns update function)
- Accessor record for type safety

**Improvements Needed**:
- Could benefit from comments explaining the three-phase update
- The `andThen` infix operator might be confusing to newcomers

---

### ⭐ Bar Chart
**File**: `Charts/BarChart.purs`
**Type**: Simple Chart
**Quality**: ⭐⭐⭐⭐ Very Good (Recently Improved)

**Assessment**:
- Clean standard chart example
- Uses D3 axes properly
- NOW uses shared `ChartDimensions` module ✅
- Has snippet markers

**FP/PureScript Idioms**: ✅ Good
- Monadic composition for SVG building
- `traverse_` for iteration

**Improvements Needed**:
- Could use `zoomableSVG` utility for consistency
- Manual scale creation could be abstracted

---

### ⭐ Line Chart
**File**: `Charts/LineChart.purs`
**Type**: Simple Chart
**Quality**: ⭐⭐⭐⭐ Very Good (Recently Improved)

**Assessment**:
- Standard line chart with D3 line generator
- Uses D3 axes
- NOW uses shared `ChartDimensions` module ✅
- Has snippet markers

**FP/PureScript Idioms**: ✅ Good
- Clean monadic flow

**Improvements Needed**:
- Could use `zoomableSVG` utility
- Line generator setup could be clearer

---

### ⭐ Scatter Plot
**File**: `Charts/ScatterPlot.purs`
**Type**: Simple Chart
**Quality**: ⭐⭐⭐⭐ Very Good (Recently Improved)

**Assessment**:
- Demonstrates Anscombe's Quartet (excellent pedagogical choice)
- Shows multiple datasets clearly
- NOW uses shared `ChartDimensions` module ✅
- Has snippet markers

**FP/PureScript Idioms**: ✅ Good
- Good use of higher-order functions for rendering multiple plots

**Improvements Needed**:
- Could use `zoomableSVG` utility
- Quartet-specific code could be better separated from basic scatter plot

---

### ⚠️ Grouped Bar Chart
**File**: `Charts/GroupedBarChart.purs`
**Type**: Simple Chart (Complex)
**Quality**: ⭐⭐⭐ Decent (Needs Work)

**Assessment**:
- Implements grouped bars manually (no D3 band scale FFI)
- Has extensive CSV parsing code mixed with viz code
- Manual scale calculations throughout
- 250+ lines - quite complex for a "simple" chart

**FP/PureScript Idioms**: ⚠️ Mixed
- Good use of `nub`, `groupBy`, `filter`
- BUT: Lots of manual calculations that should use D3 scales
- CSV parsing should be in separate module

**Improvements Needed**:
- **HIGH PRIORITY**: Extract CSV parsing to utility module
- Should use D3 band scales via FFI instead of manual calculations
- Could use shared `ChartDimensions` pattern
- Consider simplifying to use hardcoded data like other examples

---

### ⚠️ Multi-Line Chart
**File**: `Charts/MultiLineChart.purs`
**Type**: Simple Chart (Complex)
**Quality**: ⭐⭐⭐ Decent (Needs Work)

**Assessment**:
- Multiple series with different colors
- Manual scale implementations
- Similar issues to GroupedBarChart

**FP/PureScript Idioms**: ⚠️ Mixed
- Good functional data transformations
- BUT: Manual scale math instead of D3

**Improvements Needed**:
- Use D3 color scales
- Could share more code with basic LineChart
- Consider using shared utilities

---

### ⚠️ Radial Stacked Bar
**File**: `Charts/RadialStackedBar.purs`
**Type**: Simple Chart (Advanced)
**Quality**: ⭐⭐ Needs Work

**Assessment**:
- Very specialized visualization
- Lots of manual trigonometry
- Unclear if this is the best example to showcase

**FP/PureScript Idioms**: ⚠️ Mixed
- Complex coordinate transformations
- Could benefit from more helper functions

**Improvements Needed**:
- Question: Is this necessary? Consider if it adds value
- If keeping: Add extensive comments explaining radial math
- Consider extracting coordinate transformation utilities

---

## 🌳 Hierarchical Layouts

### ⭐ Horizontal Tree
**File**: `Tree/HorizontalTree.purs`
**Type**: Simple Chart (Hierarchical)
**Quality**: ⭐⭐⭐⭐⭐ Excellent (Recently Refactored)

**Assessment**:
- Clean, standalone implementation
- Parametrized by `TreeType` (Tidy vs Dendrogram)
- Uses shared `TreeHelpers` module ✅
- Uses `zoomableSVG` utility ✅
- ~140 lines, very readable

**FP/PureScript Idioms**: ✅ Excellent
- Good separation of concerns
- Type-safe tree node accessors
- Clean monadic composition

**Improvements Needed**: None - this is now exemplary

---

### ⭐ Vertical Tree
**File**: `Tree/VerticalTree.purs`
**Type**: Simple Chart (Hierarchical)
**Quality**: ⭐⭐⭐⭐⭐ Excellent (Recently Refactored)

**Assessment**:
- Same quality as HorizontalTree
- Consistent structure across tree modules
- Uses shared utilities ✅

**FP/PureScript Idioms**: ✅ Excellent

**Improvements Needed**: None

---

### ⭐ Radial Tree
**File**: `Tree/RadialTree.purs`
**Type**: Simple Chart (Hierarchical)
**Quality**: ⭐⭐⭐⭐⭐ Excellent (Recently Refactored)

**Assessment**:
- Polar coordinate transforms well-encapsulated
- Helper functions for radial positioning
- Consistent with other tree modules

**FP/PureScript Idioms**: ✅ Excellent

**Improvements Needed**: None

---

### ⭐ Icicle (Partition Layout)
**File**: `Icicle/Icicle.purs`
**Type**: Simple Chart (Hierarchical)
**Quality**: ⭐⭐⭐⭐ Very Good

**Assessment**:
- Clean partition layout implementation
- Uses D3 hierarchy functions properly

**FP/PureScript Idioms**: ✅ Good

**Improvements Needed**:
- Could use `zoomableSVG` utility
- Consider extracting hierarchy accessor pattern like `treeDatum_`

---

### ⭐ Treemap
**File**: `Treemap/Treemap.purs`
**Type**: Simple Chart (Hierarchical)
**Quality**: ⭐⭐⭐⭐ Very Good

**Assessment**:
- Good treemap implementation
- Clean rectangle rendering

**FP/PureScript Idioms**: ✅ Good

**Improvements Needed**:
- Could use `zoomableSVG` utility
- Similar to Icicle suggestions

---

### ⭐ Bubble Chart (Circle Packing)
**File**: `BubbleChart/BubbleChart.purs`
**Type**: Simple Chart (Hierarchical)
**Quality**: ⭐⭐⭐ Decent

**Assessment**:
- Implements circle packing
- Has TODO about extracting JSON loading ✅ (good self-awareness)
- Hardcoded fallback handling

**FP/PureScript Idioms**: ⚠️ Mixed
- Good use of `traverse_`
- BUT: `unsafeCoerce` for data access could be typed better

**Improvements Needed**:
- Extract `loadFlareData` to shared utility
- Create proper types for hierarchy node data
- Consider using same accessor pattern as tree modules

---

## 🔗 Flow & Network Diagrams

### ⭐ Sankey Diagram
**File**: `Sankey/SankeyDiagram.purs`
**Type**: Simple Chart (Flow)
**Quality**: ⭐⭐⭐⭐ Very Good

**Assessment**:
- Has its own capability (`SankeyM` typeclass)
- Good accessor records (`node_`, `link_`)
- Uses proper D3 Sankey layout

**FP/PureScript Idioms**: ✅ Excellent
- Typeclass abstraction is very PureScript
- Accessor records are clean

**Improvements Needed**:
- Could use `zoomableSVG` utility
- Documentation about what Sankey diagrams are good for

---

### ⭐ Chord Diagram
**File**: `Chord/ChordDiagram.purs`
**Type**: Simple Chart (Relational)
**Quality**: ⭐⭐⭐⭐ Very Good

**Assessment**:
- Clean chord diagram with ribbons
- Example data is programming concepts (nice choice)
- Uses D3 chord layout properly

**FP/PureScript Idioms**: ✅ Good

**Improvements Needed**:
- Some `unsafeCoerce` usage could be typed better
- Consider accessor record pattern for chord data

---

## 🎯 Interactive Applications

### ⭐ Les Misérables Force Graph
**File**: `LesMis/LesMiserables.purs`
**Type**: Interactive App (Force Simulation)
**Quality**: ⭐⭐⭐⭐ Very Good

**Assessment**:
- Uses `SimulationM2` capability
- Proper tick-based updates
- Drag behavior integration
- Good separation of init vs update

**FP/PureScript Idioms**: ✅ Excellent
- Typeclass abstraction for simulation
- Stateful updates handled properly
- Good use of Maybe for selections

**Improvements Needed**:
- Could use `zoomableSVG` utility
- Documentation about simulation parameters

---

### ⭐⭐ Force Navigator (Site Navigation)
**File**: `ForceNavigator/Draw.purs`, `ForceNavigator/Forces.purs`, `ForceNavigator/Data.purs`
**Type**: Interactive App (Complex)
**Quality**: ⭐⭐⭐⭐⭐ Excellent

**Assessment**:
- Most complex and app-like visualization
- Proper separation: Model, Forces, Draw, Data
- Uses simulation with drag interactions
- Init/update pattern (Elm Architecture-like)
- Zoom capability
- Color coding by node type
- This is NOT a simple chart - it's a full application

**FP/PureScript Idioms**: ✅ Excellent
- Module separation is exemplary
- Type-safe node accessors
- Proper use of `Maybe` for stateful selections
- Drag behavior properly integrated

**Improvements Needed**:
- This is actually a good model! Could document as architectural pattern
- Could use `zoomableSVG` if it fits the zoom behavior

---

### ⚠️ Wealth & Health (Gapminder-style)
**File**: `WealthHealth/Draw.purs`
**Type**: Interactive App
**Quality**: ⭐⭐⭐ Decent (Needs Improvement)

**Assessment**:
- Animated scatter plot (Gapminder-style)
- **PROBLEM**: Implements scales manually in PureScript (!)
- Has TODOs about using proper D3 scales ✅
- Complex update logic
- Manual tick generation

**FP/PureScript Idioms**: ⚠️ Mixed
- Good separation of scale functions
- BUT: Reimplementing D3 scales defeats the purpose

**Improvements Needed**:
- **HIGH PRIORITY**: Use D3 scales via FFI instead of manual implementations
- Extract `scaleX`, `scaleY`, `scaleRadius` to use D3
- The scale functions have hardcoded domains - should be data-driven
- Consider if this should be a simpler example first

---

### ⚠️ Spago Dependency Visualizer
**Files**: `Spago/*.purs` (multiple)
**Type**: Interactive App (Complex)
**Quality**: ⭐⭐⭐ Decent (Needs Review)

**Assessment**:
- Visualizes Spago dependencies as tree
- Multiple modules: Tree, Draw, Attributes, Files, Model
- Likely similar complexity to ForceNavigator
- Haven't fully reviewed

**FP/PureScript Idioms**: Unknown - needs review

**Improvements Needed**:
- Full review needed
- Check for redundancy with other tree visualizations
- Consider if this adds unique value or duplicates other examples

---

## 🎨 Three Little Dimensions (D3 Core Concepts)

**File**: `ThreeLittleDimensions/ThreeLittleDimensions.purs`
**Type**: Pedagogical Demo
**Quality**: ⭐⭐⭐⭐ Very Good

**Assessment**:
- Teaches the three dimensions of D3: Data, Document, Driven
- Two examples: basic and with nested selections
- Has snippet markers
- Good conceptual demonstration

**FP/PureScript Idioms**: ✅ Good

**Improvements Needed**: None for pedagogical purpose

---

## 📈 Summary Statistics

### By Category:
- **Simple Charts**: 10 modules
  - Excellent: 5 (TLC, Parabola, Bar, Line, Scatter)
  - Very Good: 2 (GUP, basic charts)
  - Needs Work: 3 (GroupedBar, MultiLine, RadialStackedBar)

- **Hierarchical Layouts**: 6 modules
  - Excellent: 3 (all tree modules)
  - Very Good: 2 (Icicle, Treemap)
  - Decent: 1 (BubbleChart)

- **Flow Diagrams**: 2 modules
  - Very Good: 2 (Sankey, Chord)

- **Interactive Apps**: 4 modules
  - Excellent: 1 (ForceNavigator)
  - Very Good: 1 (Les Mis)
  - Needs Work: 2 (WealthHealth, Spago - needs review)

### Common Patterns to Extract:
✅ DONE:
- Chart dimensions and margins → `ChartDimensions` module
- Tree FFI helpers → `TreeHelpers` module
- Zoomable SVG → `ZoomableViewbox` module

🔄 IN PROGRESS:
- Apply `zoomableSVG` to remaining visualizations

📋 TODO:
- Hierarchy node accessor pattern (like `treeDatum_`)
- JSON data loading utilities
- CSV parsing utilities
- Manual scale implementations → use D3 scales

### FP/PureScript Idioms Assessment:

**Strong Patterns** (keep these):
- ✅ Accessor records (e.g., `datum_`, `node_`, `link_`)
- ✅ Typeclass capabilities (`SelectionM`, `SimulationM2`, `SankeyM`)
- ✅ Init/update separation (Elm Architecture)
- ✅ Higher-order functions (returning update functions)
- ✅ Module separation for complex apps

**Anti-Patterns** (fix these):
- ❌ Manual scale implementations (WealthHealth)
- ❌ Mixed concerns (CSV parsing in viz modules)
- ❌ Excessive `unsafeCoerce` where types could be proper
- ❌ Hardcoded dimensions (should use shared utilities)

### Tension: Simple Charts vs Interactive Apps

**Observation**: The tension is REAL and should be embraced:

1. **Simple D3-like Charts** (Bar, Line, Scatter, Trees)
   - Should be close to D3 idioms
   - Minimize PureScript abstractions
   - Focus: teaching D3 concepts
   - Keep them < 150 lines
   - Direct FFI to D3, minimal wrappers

2. **Interactive Apps** (ForceNavigator, WealthHealth, Spago)
   - Should use PureScript idioms heavily
   - Proper types, no unsafe operations
   - Module separation
   - Stateful updates via capabilities
   - This is where FP shines

**Recommendation**: Document this distinction clearly. The simple charts are "transliterated D3" while the apps are "idiomatic PureScript applications that happen to use D3".

---

## 🎯 Priority Improvements

### High Priority:
1. **WealthHealth**: Replace manual scales with D3 scale FFI
2. **GroupedBarChart**: Extract CSV parsing, use D3 band scales
3. **BubbleChart**: Extract JSON loading, type the hierarchy accessors
4. **Apply zoomableSVG**: to charts that would benefit (Bar, Line, Scatter, Icicle, Treemap, Sankey, Chord)

### Medium Priority:
5. **Spago viz**: Full review and assessment
6. **MultiLineChart**: Use D3 scales and color scales
7. **RadialStackedBar**: Evaluate if it should stay, add extensive comments if so
8. **Create hierarchy accessor utility**: Extract pattern from tree modules

### Low Priority:
9. Documentation: Add architectural guide explaining Simple vs App distinction
10. Consider: More snippet markers for code extraction
