# PSD3 Functional Programming Showcase - Ideas & Analysis

**Date:** 2025-11-01
**Status:** Research phase - brainstorming examples that showcase PSD3's unique FP capabilities

## Core Question
What examples can we build that showcase things only possible (or only easily possible) with PSD3's functional programming approach?

## Key Findings: PSD3's Unique Strengths

### 1. **Foldable Data Structures (⭐⭐⭐⭐⭐ Highest Impact)**

**The Gap:** Your `nestedJoin` accepts ANY `Foldable` type, but all current examples only use Arrays!

**Implementation Details:**
- Location: `/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Capabilities/Selection.purs` (lines 230-231)
- Signature: `nestedJoin :: ∀ f datum. Foldable f => selection -> Element -> (Datum_ -> f datum) -> (Datum_ -> Index_) -> m selection`
- Current usage: Only `Array` types in practice

**What's Possible:**
- Visualize directly from `Map.values`, `Set`, `List`, or custom Foldable structures
- Show the SAME visualization code working with different container types
- This is literally impossible in vanilla D3 (only works with arrays)

**Example Ideas:**
- **"Polyglot Data Structures"**: Same bar chart consuming Array, List, Map values, and Set, demonstrating true container polymorphism
- **"Map Visualization"**: Show `Map String Number` as bar chart using `nestedJoin` with `Map.values`
- **"Set Operations"**: Visualize set intersections/unions with Venn diagrams
- **"Custom Foldable"**: Binary search tree visualization using Tree's Foldable instance

**Code Sketch:**
```purescript
-- Using Map values directly in join
visualizeMapData :: forall m. SelectionM D3Selection_ m => Map String Number -> m Unit
visualizeMapData dataMap = do
  svg <- attach "#chart" >>= appendTo _ Svg [...]
  -- Extract values as Foldable, use directly in nestedJoin
  bars <- nestedJoin svg Rect (\_ -> Map.values dataMap) keyFn
  setAttributes bars [height (\d -> d), ...]
```

### 2. **Finally Tagless Interpreters (⭐⭐⭐⭐⭐)**

**What Exists:**
- Three interpreters implemented:
  - **D3 Interpreter** (`/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Interpreter/D3.purs`): Executes actual D3.js operations
  - **String Interpreter** (`/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Interpreter/String.purs`): Generates D3 JavaScript code as strings
  - **MetaTree Interpreter** (`/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Interpreter/MetaTree.purs`): Builds an AST for meta-visualization
- Current demo: `/Users/andrew/work/PureScript Tagless D3/src/website/Component/Understanding/InterpretersDemo.purs`

**The Gap:** The demo is limited - could show more dramatic use cases

**Unique Capability:** Write a visualization once, run it in D3, generate its code, AND visualize its own structure as a tree. This has no equivalent in vanilla D3.

**Key Code Pattern:**
```purescript
-- From InterpretersDemo.purs
scatterPlotD3 :: forall m. SelectionM D3Selection_ m => m Unit
scatterPlotString :: D3PrinterM String
scatterPlotMeta :: D3MetaTreeM NodeID

-- Same visualization code, three different interpreters!
```

**Example Ideas:**
- **"Viz Code Generator"**: Build a viz interactively in UI, generate the PureScript code in real-time using String interpreter
- **"Interpreter Comparison Matrix"**: Multiple visualizations switching between all three interpreters live
- **"Self-Documenting Viz"**: Use MetaTree to auto-generate documentation/flowcharts from viz code
- **"Teaching Tool"**: Show students the code, the viz, and the execution tree simultaneously

### 3. **Graph Algorithms (⭐⭐⭐⭐)**

**What Exists:**
- `getReachableNodes` in `/Users/andrew/work/PureScript Tagless D3/src/lib/Data/DependencyGraph.purs`
- Used in Spago viz for spanning tree computation (lines 41)
- Returns reachable nodes AND redundant links

**The Gap:** No pathfinding, centrality, or other classic graph algorithms visualized

**Why It Matters:** Shows how pure functional graph algorithms integrate seamlessly with visualization

**Example Ideas:**
- **"A* Pathfinding Visualizer"**: Click two nodes, watch algorithm explore graph with animation showing open/closed sets
- **"Centrality Measures"**: Compute betweenness/closeness centrality, size nodes accordingly, show algorithm steps
- **"Community Detection"**: Pure functional clustering algorithms + colored visualization
- **"Graph Algorithm Playground"**: Interactive graph with Dijkstra's, BFS, DFS, MST, etc.

**Code Sketch:**
```purescript
-- Compute shortest path, then visualize
shortestPath :: NodeID -> NodeID -> Graph NodeID _ -> Maybe (Array NodeID)
shortestPath = -- ... pure graph algorithm (A*, Dijkstra's, etc.)

visualizeShortestPath :: NodeID -> NodeID -> Graph _ -> m Unit
visualizeShortestPath start end graph = do
  case shortestPath start end graph of
    Nothing -> pure unit
    Just path -> do
      edges <- simpleJoin svg Line (pathToEdges path) keyFn
      setAttributes edges [strokeColor "red", strokeWidth 3.0]
```

### 4. **Lens-Based Transformations (⭐⭐⭐⭐)**

**What Exists:**
- Extensive lens system in `/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Internal/Simulation/Types.purs` (lines 11-268)
- Prisms, lenses, profunctor optics throughout
- Used internally for state management

**The Gap:** No user-facing examples showing lens-powered data access/transformation

**Why It Matters:** Makes data transformation purely functional, composable, type-safe

**Key Lens Patterns from Codebase:**
```purescript
-- Lines 61-62: Prism for Nullable
_nullable :: forall a. Prism' (Nullable a) a
_nullable = prism' notNull N.toMaybe

-- Lines 64-65: Lens for handle
_handle :: forall r. Lens' { simulation :: D3SimulationState_ | r } D3Simulation_
_handle = _d3Simulation <<< _Newtype <<< prop (Proxy :: Proxy "handle_")

-- Lines 161-162: Composed lens using profunctors
_filterLabel :: forall p row. ... => p Label Label -> p Force Force
_filterLabel = _Newtype <<< prop (Proxy :: Proxy "filter") <<< _Just <<< _forceFilterLabel
```

**Example Ideas:**
- **"Lens Dashboard"**: Complex nested data structure, multiple coordinated views each using lens composition to extract different slices
- **"Zoom Lens"**: Use prisms and lenses to drill into hierarchical data interactively
- **"Lens-Based Data Filtering"**: Interactive filters using lens composition
- **"Profunctor Optics Tour"**: Show composed lens chains for deep data access

**Code Sketch:**
```purescript
-- Using lens to extract nested data
type Company = { name :: String, financials :: { revenue :: Number, profit :: Number } }
_revenue :: Lens' Company Number
_revenue = prop (Proxy :: _ "financials") <<< prop (Proxy :: _ "revenue")

visualizeCompanies :: Array Company -> m Unit
visualizeCompanies companies = do
  bars <- simpleJoin svg Rect companies keyFn
  setAttributes bars [
    height (view _revenue),  -- Lens extracts nested revenue
    fill (if view _revenue > 1000000.0 then "green" else "red")
  ]
```

### 5. **Type-Safe Scales & Coordinates (⭐⭐⭐)**

**The Gap:** Could use phantom types to prevent mixing continuous/discrete scales or incompatible coordinate systems

**Example Ideas:**
- **Phantom Types for Scales**: Type-safe scale domains (Continuous vs Discrete)
- **Type-Level Dimensions**: Ensure x/y attributes match coordinate system
- **GADTs for Viz Specs**: Typed visualization specifications

### 6. **Algebraic Data Types for Domain Modeling**

**What Exists:**
- **Tree with Foldable/Traversable** (`/Users/andrew/work/PureScript Tagless D3/src/lib/Data/Tree.purs`)
- **Graph ADT** (`/Users/andrew/work/PureScript Tagless D3/src/lib/Data/DependencyGraph.purs`)
- **Force System ADTs** (Simulation/Types.purs lines 112-127)

**Unique Capability:** Total pattern matching, impossible states unrepresentable, rich type-driven development

**Example Ideas:** Could showcase more ADT-driven design patterns

## Other Functional Patterns in Use

### Compositional Patterns
**Location:** `/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Interpreter/D3.purs` (lines 132-171)
- Order-independent configuration
- Composable force libraries
- Used in `/Users/andrew/work/PureScript Tagless D3/src/website/Component/Understanding/LesMiserables.purs`

### Row Polymorphism
**Location:** Throughout simulation code
- `SimulationM D3Selection_ (D3SimM row D3Selection_)` allows extending state with custom fields
- Could showcase more advanced type-level programming

## Summary Table: Feature Impact & Gaps

| Functional Feature | Implementation | Current Usage | Gap | Demo Potential |
|-------------------|---------------|---------------|-----|----------------|
| **Finally Tagless** | Interpreters/*.purs | ✅ InterpretersDemo (limited) | Could show more dramatic use cases | ⭐⭐⭐⭐⭐ |
| **Foldable Join** | Selection.purs:230 | ❌ Only Arrays used | No Map/Set/List examples | ⭐⭐⭐⭐⭐ |
| **Lenses** | Simulation/Types.purs | ✅ Internal use only | Not user-facing | ⭐⭐⭐⭐ |
| **ADTs** | Data/*.purs | ✅ Tree, Graph | Could show more patterns | ⭐⭐⭐ |
| **Graph Algorithms** | DependencyGraph.purs | ✅ Spanning tree only | No pathfinding/centrality | ⭐⭐⭐⭐ |
| **Type-Level** | Row polymorphism | ✅ SimulationM rows | No phantom types | ⭐⭐⭐ |

**Legend:**
- ✅ = Currently demonstrated
- ❌ = Not demonstrated
- ⭐ = Impact potential (5 = highest)

## Next Steps

Before building, need to decide:
1. **Which capability to showcase first?** (Foldable, Graph Algorithms, Enhanced Interpreter, Lens Dashboard)
2. **Scope:** Simple & focused (~100-200 lines) vs comprehensive showcase (~500+ lines)
3. **Data source:** Use existing data, create new dataset, or generate synthetic data

## Open Questions for Discussion

1. Should we prioritize pedagogical value (teaching FP concepts) or "wow factor" (impressive demos)?
2. Do we want examples that compare PSD3 vs vanilla D3 side-by-side?
3. Should examples build on each other (series) or be standalone?
4. Target audience: FP experts, D3 users learning PureScript, or general developers?
