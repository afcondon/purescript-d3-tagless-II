# Chimeric Visualization Architecture Analysis

## Current State Assessment

### What's Already in Libraries ✅

**psd3-selection** (Core library):
- ✅ `PSD3.AST` - **ConditionalRender** constructor
- ✅ `PSD3.Interpreter.D3` - D3 interpreter handles ConditionalRender
- ✅ `PSD3.Interpreter.Mermaid` - Mermaid interpreter handles ConditionalRender
- ✅ `PSD3.Interpreter.English` - English interpreter handles ConditionalRender
- ✅ `PSD3.Interpreter.MetaAST` - Meta interpreter handles ConditionalRender
- ✅ `PSD3.Interpreter.SemiQuine` - Code generation handles ConditionalRender
- ✅ All core selection/rendering operations

**psd3-layout** (Layout algorithms library):
- ✅ `DataViz.Layout.Hierarchy.Tree` - Tree layout algorithm
- ✅ `DataViz.Layout.Hierarchy.Partition` - Partition layout (basis for sunburst)
- ✅ `DataViz.Layout.Hierarchy.Treemap` - Treemap layout
- ✅ `DataViz.Layout.Hierarchy.Core` - Hierarchy core types
- ✅ `DataViz.Layout.Hierarchy.Link` - Link path generation
- ✅ `DataViz.Layout.Sankey` - Sankey diagram layouts

**Status:** The **foundation is solid**. All core chimeric infrastructure is in libraries.

---

### What's in Demo Code (demo-website)

**Domain-Agnostic Examples:**
- `Viz/SimpleChimera.purs` - Minimal chimeric test (circles)
- `Component/SimpleChimera.purs` - Halogen wrapper

**Tidal-Specific Code:**
- `Component/PatternTree.purs` - PatternTree data type
- `Viz/PatternTree/CombinatorTree.purs` - **Chimeric combinator forest** (uses ConditionalRender!)
- `Viz/PatternTree/Sunburst.purs` - Sunburst rendering for patterns
- `Viz/PatternTree/Types.purs` - Shared types (ZoomTransform, etc.)
- `Viz/PatternTree/Tree.purs` - Tree layout for patterns
- `Viz/PatternTree/Radial.purs` - Radial tree layout
- `Viz/PatternTree/Isometric.purs` - Isometric 3D layout
- `Viz/PatternTree/Mixed.purs` - Mixed layout modes
- `Viz/PatternTree/Layout.purs` - Layout utilities
- `Viz/PatternTree/Euclidean.purs` - Euclidean rhythm viz
- `Viz/PatternTreeViz.purs` - Main pattern tree viz module
- `Component/AlgoraveViz.purs` - Live algorave visualizer

**TreeBuilder (Emmet-style) Code:**
- `TreeBuilder/App.purs` - Interactive tree builder
- `TreeBuilder3/App.purs` - DSL tree builder
- `TreeBuilder3/Converter.purs`, `TypePropagation.purs`, etc. - Tree builder infrastructure

**Status:** Demo code is **correctly placed**. It's domain-specific or educational.

---

## Architectural Analysis

### Current Package Responsibilities

```
psd3-selection/
├── AST & Interpreters         ← ConditionalRender lives here ✅
├── Selection operations        ← Core D3 wrapping ✅
├── Attributes & Behaviors      ← Type-safe attributes ✅
└── Rendering infrastructure    ← renderTree, etc. ✅

psd3-layout/
├── Hierarchy layouts           ← Tree, Partition, Treemap ✅
├── Sankey layouts              ← Flow diagrams ✅
└── Edge bundling               ← Hierarchical edge routing ✅

psd3-simulation/
├── Force simulation            ← Physics-based layouts ✅
└── Force configuration         ← Immutable force config ✅

psd3-tidal/
├── Mini-notation parser        ← Tidal pattern syntax ✅
├── TPat AST                    ← Tidal pattern AST ✅
└── Pretty printer              ← AST → string ✅

demo-website/
├── Examples & tutorials        ← Educational code ✅
├── Domain-specific viz         ← Tidal, code explorer ✅
└── Interactive components      ← Halogen wrappers ✅
```

**Assessment:** Package boundaries are **well-designed and appropriate**.

---

## Key Questions

### Q1: Should chimeric pattern code move to a library?

**Answer: NO** - The current architecture is correct.

**Reasoning:**

1. **Foundation in library** ✅
   - ConditionalRender is in `psd3-selection` (core AST)
   - All interpreters handle it
   - This is the right layer of abstraction

2. **Domain-specific implementations in demos** ✅
   - CombinatorTree is Tidal-specific (uses PatternTree, PatternMetrics)
   - SimpleChimera is a minimal example (belongs in demos)
   - These are **applications** of the library, not library code

3. **No new abstraction needed**
   - ConditionalRender is the abstraction
   - Different domains use it differently (Tidal patterns vs generic data)
   - No common pattern to extract

**Analogy:** ConditionalRender is like `map` - it's in the library. CombinatorTree is like "map over user records to format names" - that's application code.

---

### Q2: Should there be a `psd3-chimeras` package?

**Answer: NO** - Not needed.

**Reasoning:**

1. **Wrong level of abstraction**
   - "Chimeric" describes a *pattern of use*, not a reusable component
   - Like asking "should we have a psd3-conditionals package?" - no, `if` is in the language

2. **No shared code to extract**
   - CombinatorTree: Tidal patterns + tree layout + sunburst leaves
   - SimpleChimera: Generic data + simple shapes
   - **Nothing in common** except both use ConditionalRender (which is in psd3-selection)

3. **Future chimeras will be domain-specific**
   - Each new chimeric viz will be tied to its domain
   - Network graph with mixed node types → specific to that domain
   - Geographic map with mixed region representations → specific to that domain

**Better approach:** Keep domain-specific chimeric vizzes in their respective packages/demos.

---

### Q3: Is anything in the wrong place?

**Answer: ONE MINOR THING** - Sunburst utilities could move to psd3-layout.

#### Current State:

**demo-website:**
```purescript
-- Viz/PatternTree/Sunburst.purs
patternToHierarchy :: PatternTree -> HierarchyData HierarchyNodeData
sunburstColor :: String -> String
sunburstArcPath :: Number -> Number -> Number -> Number -> Number -> String
flattenPartition :: forall a. PartitionNode a -> Array (PartitionNode a)
fixParallelLayout :: PartitionNode HierarchyNodeData -> PartitionNode HierarchyNodeData
```

**Analysis:**

- `patternToHierarchy` - **Tidal-specific** (converts PatternTree) ✓ demo-website
- `sunburstColor` - **Tidal-specific** (colors for pattern node types) ✓ demo-website
- `sunburstArcPath` - **Generic utility** (partition coordinates → SVG path) → psd3-layout?
- `flattenPartition` - **Generic utility** (flatten partition tree) → psd3-layout?
- `fixParallelLayout` - **Generic utility** (fix parallel node stacking) → psd3-layout?

#### Recommendation:

**Move to psd3-layout:**
```purescript
-- psd3-layout/src/DataViz/Layout/Hierarchy/Partition.purs
-- Add these utilities:

sunburstArcPath :: Number -> Number -> Number -> Number -> Number -> String
flattenPartition :: forall a. PartitionNode a -> Array (PartitionNode a)
fixParallelLayout :: forall a. (a -> String) -> PartitionNode a -> PartitionNode a
```

**Keep in demo-website:**
```purescript
-- demo-website/src/Viz/PatternTree/Sunburst.purs
patternToHierarchy :: PatternTree -> HierarchyData HierarchyNodeData
sunburstColor :: String -> String
```

**Benefits:**
- Generic sunburst utilities reusable for other domains
- Partition layout module is more complete
- Tidal-specific logic stays in demo

---

### Q4: What about the two-step rendering pattern?

**Current:** Every chimeric example manually does:

```purescript
-- Step 1: Render scaffolding (Tree Unit)
_ <- renderTree container scaffoldTree

-- Step 2: Render chimeric tree (Tree Datum)
nodesGroup <- select "#nodes"
_ <- renderTree nodesGroup chimericTree
```

**Should this be in the library?**

**Answer: MAYBE** - Could add a helper, but not essential.

**Option A: Helper function (library)**

```purescript
-- psd3-selection/src/PSD3/Render.purs
renderChimeric
  :: forall m datum
   . SelectionM m
  => m (Selection SEmpty Element Unit)  -- Container
  -> Tree Unit                           -- Scaffolding
  -> String                              -- Placeholder group ID
  -> Tree datum                          -- Chimeric tree
  -> m (Map String ...)
renderChimeric container scaffolding groupId chimericTree = do
  _ <- renderTree container scaffolding
  group <- select ("#" <> groupId)
  renderTree group chimericTree
```

**Usage:**
```purescript
renderChimeric container scaffoldTree "nodes" chimericTree
```

**Option B: Keep it explicit (current)**

Developers manually do two steps. More verbose but:
- Clearer what's happening
- More flexible (can do things between steps)
- Not much boilerplate to justify abstraction

**Recommendation:** Keep it explicit for now. Add helper later if pattern proves universal.

---

## Recommendations

### Short-term (This Session)

**1. Extract generic sunburst utilities to psd3-layout** ✅ Recommended

Move from `demo-website/src/Viz/PatternTree/Sunburst.purs`:
- `sunburstArcPath` → `psd3-layout/src/DataViz/Layout/Hierarchy/Partition.purs`
- `flattenPartition` → `psd3-layout/src/DataViz/Layout/Hierarchy/Partition.purs`
- `fixParallelLayout` → `psd3-layout/src/DataViz/Layout/Hierarchy/Partition.purs`

Update `Sunburst.purs` to import from psd3-layout.

**2. Add examples to library docs**

Add to `psd3-selection/README.md`:
- Section on "Chimeric Visualizations with ConditionalRender"
- Link to SimpleChimera and CombinatorTree as examples
- Explain two-step rendering pattern

**3. Update CHIMERIC_VIZ_FOR_TIDAL.md**

Add section:
```markdown
## Library vs Application Code

- **Library:** ConditionalRender AST constructor + interpreters
- **Application:** Domain-specific template functions + visualization logic
- **Pattern:** Two-step rendering (scaffolding, then chimeric tree)
```

---

### Medium-term (Next Few Weeks)

**4. Add more chimeric examples**

Show ConditionalRender versatility:
- **NetworkChimera** - Network graph with mixed node types (circle, rectangle, image)
- **GeoChimera** - Map with different region representations (choropleth, pie, bar)
- **CalendarChimera** - Calendar heatmap with event details on hover (text vs icons)

**5. Consider renderChimeric helper**

If three+ examples all use same pattern, add helper to psd3-selection.

**6. Document pattern in PSD3 guide**

Add to library documentation:
- "Building Chimeric Visualizations" tutorial
- ConditionalRender API reference
- Template function best practices

---

### Long-term (When Publishing)

**7. Package structure for publishing**

Current structure is **publication-ready**:

```
psd3-selection    → Publish to registry
psd3-layout       → Publish to registry
psd3-simulation   → Publish to registry
psd3-tidal        → Publish to registry (or separate tidal-mininotation)
demo-website      → Keep as examples repo
```

**8. Separate Tidal viz into standalone package?**

**Option:** `psd3-tidal-viz` package
- Depends on: psd3-selection, psd3-layout, psd3-tidal
- Contains: CombinatorTree, Sunburst helpers, PatternTree viz
- Published separately for Tidal-specific use cases

**Decision:** Wait until there's demand. For now, examples in demo-website are fine.

---

## Conclusion

### ✅ Current Architecture is Correct

1. **ConditionalRender in psd3-selection** - Right place ✅
2. **Layout algorithms in psd3-layout** - Right place ✅
3. **Domain examples in demo-website** - Right place ✅
4. **No need for psd3-chimeras** - Pattern, not component ✅

### 🔧 One Small Fix Needed

**Extract generic sunburst utilities:**
- `sunburstArcPath`, `flattenPartition`, `fixParallelLayout` → psd3-layout

### 📚 Documentation Improvements

- Add ConditionalRender section to library README
- Document two-step rendering pattern
- Add more chimeric examples

---

## Action Items

**If proceeding with extraction:**

1. [ ] Add `sunburstArcPath` to `psd3-layout/src/DataViz/Layout/Hierarchy/Partition.purs`
2. [ ] Add `flattenPartition` to same file
3. [ ] Add generic `fixParallelLayout` (parameterize node type field access)
4. [ ] Update `demo-website/src/Viz/PatternTree/Sunburst.purs` to import from psd3-layout
5. [ ] Update `CombinatorTree.purs` imports
6. [ ] Test that everything still builds
7. [ ] Commit with message: "Extract generic sunburst utilities to psd3-layout"

**If keeping as-is:**

1. [ ] Document current architecture in `notes/ARCHITECTURE.md`
2. [ ] Add examples section to psd3-selection README
3. [ ] Update CHIMERIC_VIZ_FOR_TIDAL.md with library/app distinction

---

**Document version:** 1.0
**Date:** 2025-12-25
**Status:** Architecture analysis complete, awaiting decision on sunburst utilities extraction
