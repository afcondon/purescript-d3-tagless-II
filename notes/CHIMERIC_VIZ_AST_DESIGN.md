# Declarative Chimeric Visualizations via AST Composition

## Core Insight

The PSD3 AST already provides compositional building blocks for visualizations. Chimeric viz could be **declaratively specified** by extending the AST with:

1. **Conditional rendering** - different viz specs based on data predicates
2. **Nested specs** - embedding one visualization inside another
3. **Strategy combinators** - choosing layouts/renderers dynamically
4. **Coordinate transformations** - as first-class AST nodes

This makes chimeras **compositional** rather than requiring imperative logic.

---

## Example 1: Sankey-Network Chimera

### Current Approach (Imperative)
```purescript
-- Pseudo-code for imperative implementation
renderSankeyNetwork data = do
  let cycles = detectCycles data
  let collapsedData = collapseIntoClusters cycles data

  -- Render main Sankey
  sankeyLayout collapsedData

  -- For each cluster, render network
  for_ clusters \cluster ->
    let networkData = expandCluster cluster
    forceLayout networkData
    renderInLocalCoordSpace cluster.bounds
```

### AST Approach (Declarative)

```purescript
sankeyNetworkSpec :: VizSpec
sankeyNetworkSpec =
  Select "svg"
    # DataJoin sankeyData
    # ConditionalRender
        [ { predicate: isCyclicCluster
          , spec: embeddedNetworkSpec
          }
        , { predicate: isNormalNode
          , spec: normalSankeyNodeSpec
          }
        ]
    # SankeyLayout defaultConfig

-- Embedded network is just another spec
embeddedNetworkSpec :: VizSpec
embeddedNetworkSpec =
  LocalCoordSpace  -- Creates nested coordinate system
    # Select "g.cluster"
    # DataJoin (\cluster -> cluster.nodes)
    # ForceLayout forceConfig
    # Nodes
        # Circle
        # Attr "r" (\d -> d.radius)
    # Edges
        # Line
        # Attr "stroke" "red"

normalSankeyNodeSpec :: VizSpec
normalSankeyNodeSpec =
  Rect
    # Attr "width" (\d -> d.width)
    # Attr "height" (\d -> d.height)
    # Attr "fill" (\d -> d.color)
```

### Key AST Extensions

#### 1. ConditionalRender
```purescript
-- In the AST
data VizAST
  = ... existing constructors
  | ConditionalRender (Array { predicate :: Data -> Boolean, spec :: VizSpec })

-- Usage
ConditionalRender
  [ { predicate: \d -> d.type == "cluster", spec: clusterSpec }
  , { predicate: \d -> d.type == "node", spec: nodeSpec }
  , { predicate: \_ -> true, spec: fallbackSpec }  -- default case
  ]
```

#### 2. LocalCoordSpace
```purescript
-- Creates a nested coordinate system
data VizAST
  = ...
  | LocalCoordSpace CoordConfig VizSpec

-- Automatically handles parent-child transforms
LocalCoordSpace
  { bounds: \d -> d.boundingBox  -- compute from data
  , scaleX: \width -> scale 0.0 width 0.0 100.0  -- local coords
  , scaleY: \height -> scale 0.0 height 0.0 100.0
  }
  networkSpec  -- this spec operates in local coords
```

#### 3. EmbedViz
```purescript
-- Embed an entire viz as a node renderer
data VizAST
  = ...
  | EmbedViz VizSpec

-- Instead of rendering a circle or rect, render a whole viz
Nodes
  # EmbedViz miniChartSpec
```

---

## Example 2: Treemap with Time Series Cells

### AST Approach

```purescript
treemapWithTimeSeriesSpec :: VizSpec
treemapWithTimeSeriesSpec =
  Select "svg"
    # DataJoin hierarchicalBudgetData
    # TreemapLayout
    # Nodes
        # Rect  -- The cell background
            # Attr "fill" (\d -> d.color)
        # EmbedViz timeSeriesSpec  -- Embed sparkline in each cell
            # SecondaryData (\d -> d.timeSeries)  -- Join second data source
            # ScaleToParent  -- Auto-scale to fit parent rect

timeSeriesSpec :: VizSpec
timeSeriesSpec =
  Select "g.sparkline"
    # Line
    # Attr "d" lineGenerator
    # Attr "stroke" "black"
    # Attr "stroke-width" 1
```

### Key AST Extension

#### SecondaryData
```purescript
-- Join a second data source to an already-bound selection
data VizAST
  = ...
  | SecondaryData (PrimaryData -> SecondaryDataArray)

-- In interpreter, this enriches the bound data
-- Before: [{id: "dept1", budget: 1000}]
-- After: [{id: "dept1", budget: 1000, timeSeries: [...]}]
```

---

## Example 3: Tree-Sunburst-Treemap Cascade

### AST Approach

```purescript
adaptiveHierarchySpec :: VizSpec
adaptiveHierarchySpec =
  Select "svg"
    # DataJoin hierarchyData
    # StrategyLayout
        [ { predicate: \d -> depth d < 3
          , layout: TreeLayout treeConfig
          , renderer: treeNodeRenderer
          }
        , { predicate: \d -> depth d >= 3 && depth d < 6
          , layout: SunburstLayout sunburstConfig
          , renderer: sunburstArcRenderer
          }
        , { predicate: \d -> depth d >= 6
          , layout: TreemapLayout treemapConfig
          , renderer: treemapRectRenderer
          }
        ]
```

### Key AST Extension

#### StrategyLayout
```purescript
-- Apply different layouts to different subtrees
data LayoutStrategy =
  { predicate :: Data -> Boolean
  , layout :: LayoutAlgorithm
  , renderer :: VizSpec
  }

data VizAST
  = ...
  | StrategyLayout (Array LayoutStrategy)

-- Interpreter partitions data based on predicates,
-- applies appropriate layout to each partition,
-- then renders with appropriate spec
```

---

## Example 4: Nested Sankeys

### AST Approach

```purescript
nestedSankeySpec :: VizSpec
nestedSankeySpec =
  Select "svg"
    # DataJoin topLevelFlowData
    # SankeyLayout
    # Nodes
        # Expandable  -- Stateful node that can expand
            { collapsed: collapsedNodeSpec
            , expanded:
                LocalCoordSpace
                  # EmbedViz nestedSankeySpec  -- RECURSIVE!
                  # DataJoin (\parent -> parent.children)
            }

collapsedNodeSpec :: VizSpec
collapsedNodeSpec =
  Rect
    # Attr "width" (\d -> d.width)
    # OnClick ToggleExpanded  -- State transition
```

### Key AST Extensions

#### Expandable (Stateful Nodes)
```purescript
data VizAST
  = ...
  | Expandable
      { collapsed :: VizSpec
      , expanded :: VizSpec
      , state :: ExpandState  -- Initially Collapsed
      }

-- Interpreter maintains state and renders appropriate spec
-- Transitions between states can be animated
```

#### Recursive Specs
```purescript
-- AST is naturally recursive, so this just works!
-- But interpreter needs to handle recursion carefully:
-- - Depth limiting
-- - Lazy evaluation
-- - Memoization
```

---

## Compositional Building Blocks

### 1. Conditional Combinators

```purescript
-- Choose renderer based on data
cond :: Array (Data -> Boolean, VizSpec) -> VizSpec -> VizSpec
cond cases fallback = ConditionalRender (cases <> [(\_ -> true, fallback)])

-- Example
nodeRenderer = cond
  [ (isCyclicCluster, networkSpec)
  , (isDenseCluster, matrixSpec)
  ]
  defaultCircleSpec
```

### 2. Nesting Combinators

```purescript
-- Embed one viz inside another
embed :: VizSpec -> VizSpec -> VizSpec
embed parent child =
  parent # EmbedViz (LocalCoordSpace child)

-- Example
treemapWithCharts =
  treemapSpec `embed` sparklineSpec
```

### 3. Strategy Combinators

```purescript
-- Apply different layouts based on heuristics
strategy :: Array (Data -> Boolean, Layout, VizSpec) -> VizSpec
strategy = StrategyLayout

-- Example
adaptiveTree = strategy
  [ (\d -> fanout d < 5, treeLayout, treeRenderer)
  , (\d -> fanout d >= 5, sunburstLayout, sunburstRenderer)
  ]
```

### 4. Coordinate Transform Combinators

```purescript
-- Transform coordinate space
inLocalSpace :: (BoundingBox -> CoordTransform) -> VizSpec -> VizSpec
inLocalSpace transformer spec =
  LocalCoordSpace { transform: transformer } spec

-- Example
miniNetwork = inLocalSpace
  (\box -> scaleToFit box 0 100 0 100)
  forceNetworkSpec
```

---

## AST Interpreter Implications

### Current Interpreter
```purescript
interpret :: VizAST -> D3Selection -> Effect Unit
interpret ast selection = case ast of
  Select selector -> d3Select selector
  Attr name value -> selection # attr name value
  -- etc
```

### Extended Interpreter

```purescript
interpret :: VizAST -> D3Selection -> VizState -> Effect Unit
interpret ast selection state = case ast of
  -- Existing cases
  Select selector -> d3Select selector
  Attr name value -> selection # attr name value

  -- New cases for chimeric viz
  ConditionalRender cases -> do
    -- Partition data based on predicates
    let partitioned = partitionData selection cases
    -- Interpret each partition with appropriate spec
    for_ partitioned \{data, spec} ->
      interpret spec (selection # filterData data) state

  LocalCoordSpace config childSpec -> do
    -- Create nested coordinate system
    let childSelection = selection
          # append "g"
          # attr "transform" (computeTransform config)
    -- Interpret child in local coords
    interpret childSpec childSelection state

  EmbedViz childSpec -> do
    -- Create container for embedded viz
    let container = selection # append "g" # classed "embedded" true
    -- Interpret child spec
    interpret childSpec container state

  StrategyLayout strategies -> do
    -- Partition data based on strategies
    for_ strategies \{predicate, layout, renderer} ->
      let matchingData = filterByPredicate predicate
          layoutResult = applyLayout layout matchingData
      interpret renderer (bindData layoutResult) state

  Expandable {collapsed, expanded, state: expState} -> do
    case expState of
      Collapsed -> interpret collapsed selection state
      Expanded -> interpret expanded selection state

  SecondaryData accessor -> do
    -- Enrich bound data with secondary data
    let enriched = enrichData selection accessor
    pure unit  -- Data is now enriched for subsequent operations
```

### State Management

```purescript
-- Viz state for interactive chimeras
type VizState =
  { expandedNodes :: Set NodeId  -- Which nodes are expanded
  , selectedNodes :: Set NodeId  -- Which nodes are selected
  , hoverNode :: Maybe NodeId    -- Current hover
  , zoomTransform :: Transform   -- Current zoom/pan
  , customState :: Map String Any  -- Extension point
  }

-- State transitions
data StateTransition
  = ToggleExpanded NodeId
  | SelectNode NodeId
  | SetHover (Maybe NodeId)
  | UpdateZoom Transform

-- State reducer
updateState :: StateTransition -> VizState -> VizState
updateState transition state = case transition of
  ToggleExpanded nodeId ->
    state { expandedNodes = toggle nodeId state.expandedNodes }
  -- etc
```

---

## Advantages of AST Approach

### 1. Declarative Composition
- **Before**: Imperative code with nested loops and conditionals
- **After**: Declarative spec that composes building blocks

### 2. Reusability
- Chimeric patterns become first-class, reusable specs
- `sankeyNetworkSpec` can be imported and used anywhere
- Combinators are reusable across different chimeras

### 3. Analyzability
- AST can be inspected before execution
- Can extract: required data shape, coordinate systems used, interaction patterns
- Could generate documentation from AST

### 4. Optimization
- AST can be transformed before interpretation
- Example: merge multiple attribute sets, eliminate redundant selections
- Could compile to optimized D3 code

### 5. Serialization
- AST can be serialized to JSON
- Specs can be stored, transmitted, versioned
- Enables server-side spec generation

### 6. Testing
- Test specs independently of DOM
- Mock interpreter to verify AST structure
- Property testing for AST transformations

---

## Further Componentization

### Current Model
```
Data → Layout → Rendering (single viz type)
```

### Chimeric Model
```
Data → Analysis (detect structure)
     → Strategy Selection (choose viz types)
     → Layout (potentially different per region)
     → Rendering (heterogeneous)
```

### AST Componentization

Each phase becomes a first-class AST concept:

#### 1. Analysis Layer
```purescript
data AnalysisAST
  = DetectCycles
  | ComputeDensity
  | ClusterByDistance (Data -> Number)
  | ClassifyByDepth (Int -> VizType)

-- Produces metadata that feeds strategy selection
```

#### 2. Strategy Layer
```purescript
data StrategyAST
  = ConditionalStrategy (Array (Predicate, VizSpec))
  | AdaptiveStrategy (Metric -> VizSpec)
  | HierarchicalStrategy (Depth -> VizSpec)

-- Produces a heterogeneous rendering plan
```

#### 3. Layout Layer
```purescript
data LayoutAST
  = SingleLayout LayoutAlgorithm
  | PartitionedLayout (Data -> LayoutAlgorithm)
  | RecursiveLayout LayoutAST  -- Layouts can nest
  | CompositeLayout (Array LayoutAST)  -- Multiple layouts combined

-- Produces positioned elements
```

#### 4. Rendering Layer
```purescript
data RenderAST
  = PrimitiveRender Shape
  | EmbeddedRender VizSpec
  | ConditionalRender (Array (Predicate, RenderAST))

-- Produces SVG/Canvas/WebGL output
```

### Unified Chimeric Spec

```purescript
type ChimericSpec =
  { data :: DataSource
  , analysis :: AnalysisAST  -- What structure exists?
  , strategy :: StrategyAST  -- How to visualize each structure?
  , layout :: LayoutAST      -- How to position elements?
  , render :: RenderAST      -- How to draw elements?
  , interaction :: InteractionAST  -- How to respond to user?
  }

-- Example: Sankey-Network
sankeyNetworkChimeric :: ChimericSpec
sankeyNetworkChimeric =
  { data: flowGraphData
  , analysis: DetectCycles  -- Finds cyclic subgraphs
  , strategy: ConditionalStrategy
      [ (isCyclic, networkStrategy)
      , (otherwise, sankeyStrategy)
      ]
  , layout: PartitionedLayout \d ->
      if isCyclic d
      then ForceLayout forceConfig
      else SankeyLayout sankeyConfig
  , render: ConditionalRender
      [ (isCyclic, embeddedNetworkRender)
      , (otherwise, sankeyNodeRender)
      ]
  , interaction: ExpandableInteraction
  }
```

---

## Open Questions

### 1. How much should the AST "know" about chimeric patterns?

**Option A**: Generic primitives (Conditional, Embed, LocalSpace)
- Chimeras are user-composed from these primitives
- More flexible, less opinionated
- Example: `ConditionalRender` works for any conditional viz

**Option B**: Specialized chimera nodes
- AST has `SankeyNetworkNode`, `TreemapTimeSeriesNode`, etc.
- Less flexible, more guided
- Interpreter can optimize for specific patterns

**Likely answer**: Start with A, add B for common patterns

### 2. How to handle performance-critical chimeras?

Some chimeras need:
- WebGL for large datasets
- WebWorkers for expensive analysis
- Streaming for real-time data

Can AST accommodate these?

**Possible approach**: Backend selection in AST
```purescript
data RenderBackend = SVG | Canvas | WebGL

chimericSpec
  # RenderWith WebGL  -- Hint to interpreter
```

### 3. How to handle animations and transitions?

Chimeras often transition between states:
- Expanding a node
- Switching from tree to sunburst layout
- Revealing embedded viz

**Possible approach**: Transition AST node
```purescript
data VizAST
  = ...
  | Transition
      { from :: VizSpec
      , to :: VizSpec
      , duration :: Milliseconds
      , easing :: EasingFunction
      }

-- Interpreter interpolates between specs
```

### 4. How to type heterogeneous data?

Different regions may need different data shapes:
- Sankey nodes: `{id, value}`
- Network nodes: `{id, x, y, vx, vy}`

**Possible approach**: Typed data contexts
```purescript
type DataContext node edge =
  { nodes :: Array node
  , edges :: Array edge
  , metadata :: Metadata
  }

-- Different specs constrain different data types
sankeySpec :: VizSpec (DataContext SankeyNode SankeyEdge)
networkSpec :: VizSpec (DataContext NetworkNode NetworkEdge)

-- Chimera must transform data when switching specs
```

### 5. Can we generate chimeric specs from data?

**Auto-chimera**: Analyze data structure, automatically choose optimal viz combo

```purescript
autoChimera :: DataStructure -> ChimericSpec
autoChimera data =
  let analysis = analyzeStructure data
      strategies = recommendStrategies analysis
  in generateSpec strategies
```

This would be the ultimate expression of the "structural exception handling" meta-pattern.

---

## Next Steps

### 1. Prototype ConditionalRender
- Extend AST with conditional node
- Implement in interpreter
- Test with simple example (color by category)
- Generalize to heterogeneous rendering

### 2. Prototype LocalCoordSpace
- Extend AST with coordinate transform
- Implement transform stack in interpreter
- Test with embedded chart
- Handle interactions across coord spaces

### 3. Prototype EmbedViz
- Test recursive interpretation
- Handle data binding for embedded viz
- Test with Treemap-Timeline example

### 4. Build First Complete Chimera
- Sankey-Network using new AST features
- Document pattern for future chimeras
- Extract reusable combinators

### 5. Generalize Patterns
- Identify common chimeric patterns
- Create combinator library
- Write guide for building custom chimeras

---

## Conclusion

The PSD3 AST is **perfectly positioned** to make chimeric visualizations declarative and compositional. By extending the AST with:

- **ConditionalRender** - different specs for different data
- **LocalCoordSpace** - nested coordinate systems
- **EmbedViz** - recursive embedding
- **StrategyLayout** - dynamic layout selection
- **Expandable** - stateful interactions

We can express complex chimeric visualizations as **compositions of simple, reusable parts**.

This aligns perfectly with PSD3's philosophy: **tagless final for ultimate composability**. Chimeras become first-class citizens, not special cases requiring imperative code.

The further componentization into Analysis → Strategy → Layout → Render layers suggests a deeper architecture where each concern is a separate, composable AST. This would be a true "Grammar of Graphics 2.0" - not just mapping data to aesthetics, but **mapping structural patterns to optimal visual representations**.

This is exciting! Let's prototype one of these to validate the approach.
