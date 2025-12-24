# Chimeric Visualizations: Hybrid Data Viz Showcase

## Meta-Pattern: Structural Exception Handling

**Core Insight**: Most real-world data has a dominant structure with exceptional regions that violate the primary pattern. Traditional visualizations force you to choose one representation, losing clarity where the structure shifts. Chimeric visualizations use the optimal representation for each region.

**Structural Transitions**:
- Mostly hierarchical → occasional merger (DAG)
- Mostly DAG → occasional feedback loop (cyclic/network)
- Mostly sparse → occasional dense cluster
- Mostly local connections → occasional long-range links
- Mostly static → occasional temporal variation

This has a "Grammar of Graphics 2.0" quality: composing not just aesthetic mappings but structural primitives.

---

## 1. Sankey-Network Chimera
**Flow with Cyclic Regions**

### Use Case: Software Dependency Resolution
- **Primary structure**: Acyclic dependency flow (packages depending on packages)
- **Exception**: Circular dependency groups (A→B→C→A)
- **Solution**: Main flow renders as Sankey; circular dependency groups collapse into "super-nodes" rendered as small force-directed networks showing the cycle
- **Real scenario**: npm/cargo dependency graphs, build systems with circular module references, Maven dependency trees

### Prototyping Analysis

**PSD3 Requirements**:
- Detect cycles in DAG structure (graph algorithm)
- Collapse subgraphs into composite nodes
- Render different viz types at different nodes
- Handle zoom/interaction at node level (click to expand force network)

**Potential PSD3 Shortcomings**:
1. **Nested coordinate systems**: Sankey has its own layout; force network has its own. Need ability to:
   - Define a local coordinate space for each super-node
   - Transform between global (Sankey) and local (network) coordinates
   - Currently PSD3 selections work in a single coordinate space

2. **Heterogeneous node rendering**:
   - Some nodes are simple shapes
   - Some nodes are entire embedded visualizations
   - Need a "node renderer" abstraction that can be swapped
   - Current approach: selection → data bind → visual property
   - Needed: selection → data bind → **renderer strategy** → visual properties

3. **Interactive state management**:
   - Expanded vs collapsed state per super-node
   - Transition animations between states
   - Propagating events from nested viz to parent

**Prototype Steps**:
1. Extend Sankey layout to mark "cyclic super-nodes"
2. Create `EmbeddedViz` abstraction for nested renderers
3. Implement force layout in local coordinate space
4. Add interaction handlers for expand/collapse
5. Test with real npm dependency data (parse package.json/lock files)

**Reusability Factor**: HIGH
- `CyclicSankey` component
- Generic `SuperNode` abstraction useful for many chimeras
- Could generalize to any flow + exception pattern

---

## 2. Tree-Sunburst-Treemap Cascade
**Progressive Spatial Optimization**

### Use Case: File System Visualization at Scale
- **Top levels**: Tree (navigable structure, low fanout)
- **Middle levels**: Sunburst (radial space efficiency as breadth increases)
- **Leaf directories**: Treemap (maximize screen real estate for size comparison)
- **Why**: Each level uses the viz that best handles its fanout/depth characteristics

### Prototyping Analysis

**PSD3 Requirements**:
- Switch rendering strategy based on depth and/or fanout
- Three different layout algorithms on same data structure
- Seamless transitions between regions
- Unified interaction model (click to zoom works across all three types)

**Potential PSD3 Shortcomings**:
1. **Layout polymorphism**:
   - Need to run different layout algorithms on different subtrees
   - Current layout approach: single algorithm for entire structure
   - Need: `layout :: DataStructure -> LayoutStrategy -> LayoutResult`
   - LayoutStrategy could vary per node based on heuristics

2. **Rendering strategy selection**:
   ```purescript
   chooseRenderer :: TreeNode -> RendererType
   chooseRenderer node
     | depth node < 3 = TreeRenderer
     | depth node < 6 && fanout node > 10 = SunburstRenderer
     | isLeaf node = TreemapRenderer
     | otherwise = TreeRenderer
   ```
   - Need conditional rendering in PSD3 DSL
   - Currently: single declarative pipeline
   - Need: strategy pattern with predicates

3. **Coordinate system transitions**:
   - Tree: Cartesian (x, y)
   - Sunburst: Polar (angle, radius)
   - Treemap: Nested Cartesian (relative to parent bounds)
   - Need coordinate transform utilities

**Prototype Steps**:
1. Implement heuristic for choosing renderer (depth/fanout thresholds)
2. Create unified layout interface for all three types
3. Build coordinate transformation layer
4. Implement smooth transitions between rendering types
5. Test with real file system data (e.g., node_modules directory)

**Reusability Factor**: MEDIUM-HIGH
- `AdaptiveHierarchy` component
- Useful for any large hierarchical data
- Pattern applicable beyond these three specific viz types

**Library Insight**: Reveals need for **strategy-based rendering** in PSD3

---

## 3. DAG with Matrix Heatmap Regions
**Sparse Graph with Dense Subgraphs**

### Use Case: Academic Citation Networks
- **Primary structure**: Sparse citation DAG (paper A cites B, C, D)
- **Exception**: "Literature clusters" are fully connected (everyone cites everyone)
- **Solution**: Collapse dense clusters (>50% connectivity) into matrix/heatmap nodes
- **Real scenario**: Citation analysis, especially survey papers that cite everything in a subfield

### Prototyping Analysis

**PSD3 Requirements**:
- Detect dense subgraphs (community detection, clique finding)
- Collapse subgraph into single visual unit
- Render adjacency matrix as heatmap within node
- Handle edges connecting to/from the cluster

**Potential PSD3 Shortcomings**:
1. **Graph analysis integration**:
   - Need community detection algorithms
   - Density calculation for subgraphs
   - Currently: layouts assume pre-processed data
   - Need: graph analysis utilities in library

2. **Dual-mode edges**:
   - Edges between normal nodes: curves/arrows
   - Edges into matrix region: connect to matrix row/column
   - Need edge routing that understands node type
   - Currently: edges assume point-to-point

3. **Hierarchical abstraction levels**:
   - Overview: show collapsed matrix nodes
   - Detail: expand to see individual papers and citations
   - Need LOD (level of detail) system
   - Currently: single detail level

**Prototype Steps**:
1. Implement density detection (% of possible edges present)
2. Create matrix layout for dense clusters
3. Implement edge routing to matrix boundaries
4. Add expand/collapse interaction
5. Test with real citation data (could use arXiv or DBLP)

**Reusability Factor**: MEDIUM
- Specific to graph data with clustering
- But `DenseClusterMatrix` node type could be reused
- Edge routing logic valuable for other chimeras

**Library Insight**: Need for **graph analysis utilities** and **smart edge routing**

---

## 4. Force Network with Embedded Hierarchies
**Flat Relationships, Hierarchical Entities**

### Use Case: Corporate Partnerships and M&A Activity
- **Entities**: Companies (which have internal structure: divisions, subsidiaries)
- **Relationships**: Partnerships, contracts, supply chains
- **Solution**: Companies are force-network nodes that can expand to show internal tree structure
- **Real scenario**: Supply chain analysis where vendors have complex internal org structures

### Prototyping Analysis

**PSD3 Requirements**:
- Force simulation for inter-company relationships
- Tree layout within each company node
- Expandable/collapsible nodes
- Edges connect at appropriate granularity (company-level or division-level)

**Potential PSD3 Shortcomings**:
1. **Nested simulations**:
   - Outer force simulation positions companies
   - Inner layout positions divisions within company
   - When company moves, all divisions move with it
   - Need parent-child coordinate transformation

2. **Dynamic edge granularity**:
   ```
   Collapsed: CompanyA ←→ CompanyB
   Expanded: CompanyA.DivisionX ←→ CompanyB.DivisionY
   ```
   - Edges need to retarget when nodes expand
   - Need edge "anchoring" system

3. **Bounding box constraints**:
   - Expanded company needs enough space for tree
   - Force simulation needs to respect minimum node sizes
   - Need dynamic collision detection based on expanded state

**Prototype Steps**:
1. Create expandable node abstraction
2. Implement tree layout in local coordinate space
3. Add force simulation with dynamic node sizes
4. Implement edge retargeting on expand/collapse
5. Test with corporate structure data (could scrape from public filings)

**Reusability Factor**: HIGH
- `ExpandableNode` pattern useful for many scenarios
- Generic "entity with internal structure" abstraction
- Applicable to: org charts, biological cells, software modules

**Library Insight**: Need for **nested coordinate spaces** and **stateful nodes**

---

## 5. Hierarchical Edge Bundling with Sankey-styled Edges
**Hierarchy + Weighted Flow**

### Use Case: Organizational Communication Patterns
- **Structure**: Org chart hierarchy
- **Data**: Email volume/communication flow between units
- **Standard HEB**: Shows connections but not volume
- **Chimera**: Edge thickness varies like Sankey to show flow magnitude
- **Real scenario**: Finding communication bottlenecks in large organizations

### Prototyping Analysis

**PSD3 Requirements**:
- Hierarchical edge bundling layout
- Variable edge width based on data (flow volume)
- Possibly directional flow (asymmetric communication)
- Color encoding for additional dimensions (sentiment, response time)

**Potential PSD3 Shortcomings**:
1. **Bundled edge styling**:
   - HEB creates bezier curves through hierarchy
   - Need to vary stroke-width along the bundle
   - SVG path width is constant; might need gradient meshes
   - Could use multiple parallel paths of varying width

2. **Flow directionality in bundles**:
   - Sankey shows direction with clear flow
   - Bundled edges curve through hierarchy
   - Need visual cue for direction (animated particles? gradient?)

3. **Edge attribute encoding**:
   - Width = volume
   - Color = type or sentiment
   - Animation = active/recent?
   - Need multi-channel edge encoding

**Prototype Steps**:
1. Implement basic HEB layout
2. Add edge width calculation from data
3. Experiment with direction indicators (particles or gradients)
4. Add interaction (hover edge to see volume, click to filter)
5. Test with simulated email/Slack data

**Reusability Factor**: MEDIUM
- Specific to hierarchical data with weighted relationships
- But edge encoding techniques broadly applicable
- Could generalize to any HEB use case

**Library Insight**: Need for **advanced edge styling** and **multi-channel encoding**

---

## 6. Treemap with Embedded Bar Chart Cells
**Nested Quantification with Temporal Dimension**

### Use Case: Budget Allocation with Time Series
- **Structure**: Hierarchical budget (departments → projects)
- **Size**: Total budget allocation (treemap area)
- **Trend**: Spend over time (sparkline/bar chart in each cell)
- **Why**: See both "how much" and "trending up/down" simultaneously
- **Real scenario**: Financial dashboards, cloud cost analysis by service over time

### Prototyping Analysis

**PSD3 Requirements**:
- Treemap layout for hierarchy
- Small multiple charts within each rect
- Responsive sizing (charts scale with cell size)
- Coordinated interaction (brush time range affects all charts)

**Potential PSD3 Shortcomings**:
1. **Micro-visualizations**:
   - Each treemap cell contains a tiny chart
   - Need to render legibly at small sizes
   - Need intelligent simplification (fewer ticks, no labels)
   - Currently: no special handling for small multiples

2. **Dual data binding**:
   - Treemap bound to hierarchical structure
   - Charts bound to time series data
   - Need to join two data sources
   - Currently: single data bind per selection

3. **Coordinated brushing**:
   - Select time range → all charts update
   - Hover cell → highlight in time series
   - Need inter-chart communication
   - Currently: independent chart state

**Prototype Steps**:
1. Implement basic treemap
2. Create minimal sparkline renderer
3. Bind time series data to treemap nodes
4. Add responsive sizing (chart adapts to cell size)
5. Implement coordinated time brushing
6. Test with AWS billing data or similar

**Reusability Factor**: VERY HIGH
- `TreemapWithTimeSeries` incredibly useful for real dashboards
- Pattern applicable to any hierarchical data with temporal aspect
- Small multiple infrastructure reusable across many viz types

**Library Insight**: Need for **multi-source data binding** and **small multiples support**

---

## 7. Parallel Coordinates with Network Clusters
**Multivariate + Relational**

### Use Case: Patient Similarity Analysis
- **Each patient**: Line through parallel coordinates (age, BP, cholesterol, glucose, etc.)
- **Similarity**: Patients with similar values have converging lines
- **Chimera**: Where lines converge tightly, render as a network node showing patient-patient similarity subgraph
- **Real scenario**: Precision medicine, finding patient cohorts, clinical trial recruitment

### Prototyping Analysis

**PSD3 Requirements**:
- Parallel coordinates layout
- Similarity detection (lines that stay close together)
- Collapse similar patients into cluster node
- Network diagram showing within-cluster relationships
- Expand/collapse clusters

**Potential PSD3 Shortcomings**:
1. **Similarity computation**:
   - Need distance metrics between polylines
   - Real-time clustering as user adjusts axes
   - Computationally intensive for large datasets
   - May need WebWorker or WASM for performance

2. **Hybrid rendering**:
   - Some entities render as lines
   - Some entities render as cluster nodes
   - Transitions between states
   - Need dynamic entity representation

3. **Dimensionality reduction**:
   - Network node needs to show cluster summary
   - Could show: centroid, range, internal variance
   - Need data aggregation utilities

**Prototype Steps**:
1. Implement parallel coordinates
2. Add line similarity detection (Hausdorff distance or similar)
3. Create cluster detection with adjustable threshold
4. Implement network view of cluster internals
5. Add expand/collapse transitions
6. Test with public health dataset (UCI ML repository)

**Reusability Factor**: MEDIUM
- Specific to multivariate data with clustering
- But clustering infrastructure reusable
- Parallel coordinates alone are valuable

**Library Insight**: Need for **performance optimization** and **online clustering algorithms**

---

## 8. Radial Tree with Chord Diagram at Leaves
**Hierarchy Terminating in Cycles**

### Use Case: Phylogenetic Tree with Horizontal Gene Transfer
- **Main structure**: Evolutionary tree (hierarchical, species divergence)
- **Exception**: Bacteria exchange genes horizontally (cyclic relationships)
- **Solution**: Leaf clades that exchange genes render as chord diagram showing transfers
- **Real scenario**: Microbial evolution, antibiotic resistance gene spread

### Prototyping Analysis

**PSD3 Requirements**:
- Radial tree layout
- Chord diagram for leaf clusters
- Mixed hierarchy + graph data structure
- Visual distinction between vertical (tree) and horizontal (transfer) relationships

**Potential PSD3 Shortcomings**:
1. **Data model complexity**:
   - Need both tree edges and graph edges
   - Tree edges: parent-child
   - Chord edges: peer-to-peer
   - Currently: single edge type per viz
   - Need: multi-relational data model

2. **Layout integration**:
   - Radial tree positions nodes in circle
   - Chord diagram also uses circle layout
   - Need to align chord arc positions with tree leaf positions
   - Shared angular coordinate system

3. **Visual hierarchy**:
   - Tree edges: thin, hierarchical
   - Chord edges: thick, prominent
   - Need to style edge types differently
   - Layer ordering to prevent occlusion

**Prototype Steps**:
1. Implement radial tree layout
2. Identify leaf clusters with cross-links
3. Create chord layout aligned with tree leaves
4. Style edge types distinctly
5. Add interaction (hover gene to see transfers)
6. Test with microbial genomics data (public databases available)

**Reusability Factor**: MEDIUM-LOW
- Fairly specific to phylogenetics
- But pattern useful for any "tree with cross-links at leaves"
- Could apply to: family trees with marriages, organizational mergers, etc.

**Library Insight**: Need for **multi-relational graph support** and **layout alignment**

---

## 9. Nested Sankeys
**Hierarchical Flows**

### Use Case: Energy/Material Flow at Different Scales
- **Top level**: Country-level energy flows (oil → transport, coal → electricity)
- **Drill down**: Click "transport" → see fuel → cars/trucks/planes/ships
- **Deeper**: Click "cars" → see manufacturing → materials → suppliers
- **Each node can expand** to show its internal flow structure
- **Real scenario**: Sustainability analysis, supply chain carbon accounting, circular economy

### Prototyping Analysis

**PSD3 Requirements**:
- Sankey layout at multiple scales
- Node expansion to reveal internal flows
- Consistent flow conservation across scales
- Zoom/pan navigation through hierarchy

**Potential PSD3 Shortcomings**:
1. **Recursive layout**:
   - Each node is itself a Sankey diagram
   - Need recursive rendering infrastructure
   - Currently: layouts are flat, single-level
   - Need: composable layout primitives

2. **Flow conservation verification**:
   - Flow into node = sum of internal flows
   - Need data validation
   - Automatic flow balancing
   - Error visualization if flows don't balance

3. **Navigation state**:
   - Track which nodes are expanded
   - Breadcrumb trail (Country > Transport > Cars)
   - Zoom to focus on expanded region
   - Need navigation state management

**Prototype Steps**:
1. Create expandable Sankey node type
2. Implement recursive layout algorithm
3. Add flow conservation checking
4. Build navigation UI (breadcrumbs, zoom)
5. Add smooth transitions on expand/collapse
6. Test with energy flow data (IEA provides open data)

**Reusability Factor**: VERY HIGH
- `HierarchicalSankey` applicable to many domains
- Any flow with nested sub-flows benefits
- Manufacturing, finance, ecology, epidemiology

**Library Insight**: Need for **recursive rendering** and **composable layouts**

---

## 10. Arc Diagram with Bar Chart Nodes
**Sequence + Multivariate**

### Use Case: Software Execution Trace with Performance Metrics
- **Sequence**: Function calls over time (arc diagram showing call relationships)
- **Each function node**: Small stacked bar showing CPU/memory/I/O breakdown
- **Why**: See both control flow and resource consumption
- **Real scenario**: Performance profiling, finding bottlenecks, optimizing hot paths

### Prototyping Analysis

**PSD3 Requirements**:
- Arc diagram layout (nodes on line, arcs above)
- Bar chart in place of each node
- Coordinated highlighting (hover arc highlights corresponding bars)
- Timeline scrubbing (see state at different times)

**Potential PSD3 Shortcomings**:
1. **Composite glyphs**:
   - Node is not a circle, it's a bar chart
   - Need custom node renderers
   - Currently: nodes are simple shapes
   - Need: "glyph" abstraction for complex node visuals

2. **Temporal coordination**:
   - Arc represents call at specific time
   - Bar chart shows aggregated metrics
   - Need to link instance (arc) to aggregate (bar)
   - Time brushing to show subset of calls

3. **Visual density**:
   - Many function calls → many arcs
   - Can become cluttered quickly
   - Need arc bundling or filtering
   - Need LOD based on zoom level

**Prototype Steps**:
1. Implement arc diagram layout
2. Create stacked bar glyph for nodes
3. Bind performance data to nodes
4. Add arc-to-node highlighting
5. Implement time filtering/brushing
6. Test with profiler output (Chrome DevTools, perf, etc.)

**Reusability Factor**: MEDIUM-HIGH
- Useful for any sequential data with node attributes
- Could apply to: process flows, user journeys, biological sequences
- `GlyphNode` pattern broadly applicable

**Library Insight**: Need for **custom node glyphs** and **temporal coordination**

---

## Cross-Cutting PSD3 Shortcomings & Opportunities

### 1. **Nested Coordinate Systems**
**Needed for**: 1, 2, 4, 6, 9

Currently PSD3 works in a single coordinate space. Many chimeras need:
- Local coordinate space within a node
- Transform between parent and child spaces
- Coordinate system metadata (Cartesian vs Polar vs Log)

**Potential solution**:
```purescript
type CoordSpace =
  { transform :: Matrix3x3  -- parent-to-local
  , inverse :: Matrix3x3    -- local-to-parent
  , type :: CartesianSpace | PolarSpace | ...
  }

-- Attach to selections
selection
  # coordSpace localSpace
  # ... (all positions now in local coords)
```

### 2. **Heterogeneous Node Rendering**
**Needed for**: 1, 3, 4, 6, 10

Nodes are not all the same type. Need:
- Strategy pattern for node rendering
- Ability to swap renderer per node based on data
- Composable node types

**Potential solution**:
```purescript
data NodeRenderer
  = CircleNode CircleConfig
  | RectNode RectConfig
  | EmbeddedViz VizSpec
  | CustomGlyph (Data -> SVGElement)

selection
  # nodes
  # renderWith chooseNodeRenderer
  where
    chooseNodeRenderer datum
      | isCluster datum = EmbeddedViz networkSpec
      | otherwise = CircleNode defaultConfig
```

### 3. **Multi-Source Data Binding**
**Needed for**: 6, 7, 10

Current model: one selection, one data array. Some chimeras need:
- Primary data structure (hierarchy, graph)
- Secondary data (time series, attributes)
- Joining multiple data sources

**Potential solution**:
```purescript
selection
  # dataJoin primaryData _.id
  # secondaryData timeSeriesData (_.id)
  # enter \primary secondary ->
      -- render using both data sources
```

### 4. **Graph Analysis Utilities**
**Needed for**: 1, 3, 7, 8

Currently: layouts assume pre-processed data. Need built-in:
- Cycle detection
- Community detection / clustering
- Density calculation
- Centrality metrics
- Path finding

**Potential solution**: `PSD3.Analysis` module with pure graph algorithms

### 5. **Recursive/Composable Layouts**
**Needed for**: 9, and generally valuable

Currently: layouts are monolithic. Need:
- Layouts that can invoke other layouts
- Composable layout primitives
- Layout strategies that can vary by subtree

**Potential solution**:
```purescript
type Layout = DataStructure -> LayoutResult

-- Layouts are first-class, composable
nestedLayout :: Layout -> (Data -> Boolean) -> Layout -> Layout
nestedLayout outer shouldNest inner = \data ->
  let outerResult = outer data
      nodeResults = map processNode outerResult.nodes
  in outerResult { nodes = nodeResults }
  where
    processNode node
      | shouldNest node.data =
          -- apply inner layout in node's local space
          inner node.children
      | otherwise = node
```

### 6. **Stateful Interactions**
**Needed for**: 1, 3, 4, 9

Many chimeras have:
- Expand/collapse state
- Drill-down navigation
- LOD (level of detail) based on zoom
- Animated transitions between states

Currently: PSD3 is fairly declarative/functional. Need:
- State management patterns
- Transition system
- Event handling that modifies state

**Potential solution**: Integration with Halogen or similar
- PSD3 generates visual output
- Halogen manages state and interaction
- Already somewhat in place, but could formalize

### 7. **Performance Optimization**
**Needed for**: 7, and large datasets generally

- WebWorker support for expensive computations
- Progressive rendering for large datasets
- Spatial indexing for hit detection
- Memoization of layout calculations

**Potential solution**: `PSD3.Performance` module with:
- `webWorkerLayout :: Layout -> WorkerLayout`
- `spatialIndex :: [Node] -> QuadTree`
- `memoize :: Layout -> MemoizedLayout`

---

## Prototyping Priority

### Tier 1: High Impact, Demonstrates Core Gaps
1. **Sankey-Network Chimera**: Clearest use case, reveals nested coord systems & heterogeneous nodes
2. **Treemap with Bar Charts**: Immediately useful, reveals multi-source data binding
3. **Nested Sankeys**: Reveals recursive layout needs

### Tier 2: High Reusability
4. **Force Network with Embedded Hierarchies**: Demonstrates expandable nodes
5. **Tree-Sunburst-Treemap Cascade**: Demonstrates strategy-based rendering

### Tier 3: Specialized but Novel
6. **DAG with Matrix Regions**: Needs graph analysis
7. **HEB with Flow Width**: Advanced edge styling
8. **Arc Diagram with Bar Nodes**: Custom glyphs
9. **Parallel Coords with Clusters**: Performance & clustering
10. **Radial Tree with Chords**: Multi-relational model

---

## Implementation Roadmap

### Phase 1: Foundation (Library Enhancements)
- Nested coordinate system support
- Heterogeneous node rendering abstraction
- Multi-source data binding
- Basic graph analysis utilities

### Phase 2: First Chimeras
- Sankey-Network (showcases nested coords + heterogeneous nodes)
- Treemap-Timeline (showcases multi-source binding)

### Phase 3: Expand Capabilities
- Recursive layout support → Nested Sankeys
- Custom glyph system → Arc-Bar diagram
- Strategy-based rendering → Adaptive hierarchy

### Phase 4: Advanced Chimeras
- Remaining visualizations based on user feedback
- Generalize patterns into reusable components

---

## Expected Library Evolution

Building these chimeras would likely drive PSD3 toward:

1. **More composable primitives**: Layouts, renderers, coordinate systems as first-class, mixable components
2. **Richer data model**: Support for multi-relational graphs, hierarchies with cross-links, joined datasets
3. **State management integration**: Formalized patterns for interactive, stateful visualizations
4. **Performance tier**: Tools for handling large datasets and expensive computations
5. **Analysis toolkit**: Built-in graph algorithms and statistical utilities

The result: **PSD3 as not just a D3 binding, but a true visualization composition framework** where complex, novel visualizations are built by combining orthogonal concerns (layout + rendering + interaction + data model).

---

## Open Questions

1. **How much should be in PSD3 core vs. add-on packages?**
   - Core: coordinate systems, node rendering abstraction, data binding
   - Add-ons: specific chimeras, graph analysis, domain-specific layouts?

2. **How to balance declarative API with imperative needs?**
   - Pure functional where possible
   - Escape hatches for imperative (D3 force simulation, animations)

3. **How to handle WebGL for performance?**
   - SVG works for hundreds of elements
   - Need Canvas or WebGL for thousands
   - Different rendering backend?

4. **Should chimeras be "smart" or "dumb"?**
   - Smart: auto-detect when to switch representations
   - Dumb: developer explicitly configures thresholds
   - Likely: smart defaults, but configurable

5. **How to test complex, interactive visualizations?**
   - Unit tests for layouts and algorithms
   - Visual regression tests for rendering
   - Interaction testing?

---

## Conclusion

These chimeric visualizations are not just cool demos—they're a structured exploration of PSD3's architectural boundaries. Each one stress-tests a different aspect of the library and reveals opportunities for principled generalization.

The meta-pattern of "structural exception handling" suggests a deeper truth about data visualization: **real data is messy, and the best visualizations adapt their representation to the local structure rather than forcing everything into a single template.**

PSD3 is well-positioned to support this vision, but would benefit from:
- More first-class support for composition
- Richer abstractions for nodes, edges, and coordinate spaces
- Better integration of data transformation and layout computation

Building even 2-3 of these chimeras would provide invaluable feedback for the library's evolution.
