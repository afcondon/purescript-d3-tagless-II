# Semantic Zoom Design Notes

## Motivation

Dense visualizations (like module graphs with 100+ nodes) become cluttered at certain zoom levels. We need a declarative way to specify how visual attributes change based on zoom scale, similar to how map applications show different detail levels at different zoom levels.

## Proposed API

### Lambda-based Zoom-Aware Attributes

```purescript
T.elem Text
  [ x d.x
  , y d.y
  , fontSizeZoom (\scale -> if scale < 0.5 then 36.0 else if scale < 2.0 then 12.0 else 2.0)
  , opacityZoom (\scale -> if scale < 0.5 then 0.0 else 1.0)
  , strokeWidthZoom (\scale -> scale * 2.0)  -- Linear scaling
  ]
```

### Use Cases Beyond Text

1. **Adaptive Detail Levels**
   - Show simplified shapes at far zoom, detailed at close zoom
   - Hide/show elements based on zoom
   - Aggregate/disaggregate data (like clustering at different scales)

2. **Performance Optimization**
   - Reduce geometry complexity at far zoom
   - LOD (Level of Detail) for expensive elements
   - Conditional rendering based on visibility

3. **Visual Hierarchy**
   - Emphasize important elements at overview scale
   - Show details only when zoomed in
   - Progressive disclosure of information

4. **Interaction Affordances**
   - Larger click targets at small scales
   - Show labels only on hover at far zoom
   - Hide interactive elements when too small to use

## Design Challenges

### 1. Type System Integration

How do we type zoom-aware attributes alongside regular attributes?

```purescript
-- Regular attribute
fontSize :: forall datum. Number -> Attribute datum

-- Zoom-aware attribute
fontSizeZoom :: forall datum. (Number -> Number) -> Attribute datum

-- Unified?
fontSize :: forall datum a. ToAttr Number a datum => a -> Attribute datum
-- where a could be Number OR (Number -> Number)
```

### 2. Zoom State Management

Who provides the zoom scale to the lambda?

**Option A: Interpreter tracks zoom state**
- Pros: Clean API, automatic
- Cons: Tight coupling between zoom behavior and rendering

**Option B: Explicit zoom state parameter**
```purescript
renderTree :: ZoomState -> Selection -> Tree datum -> ...
```
- Pros: Explicit, testable
- Cons: More ceremony, needs threading through

**Option C: Reader monad / environment**
```purescript
type RenderEnv = { zoomScale :: Number, ... }
type TreeM a = ReaderT RenderEnv Effect a
```

### 3. Update Strategy

When zoom changes, how do we update attributes?

**Option A: Re-render entire tree**
- Simple but potentially expensive

**Option B: Track zoom-dependent attributes**
- Only update elements with zoom-aware attrs
- Requires attribute dependency tracking

**Option C: D3 zoom event handler**
```purescript
onZoom :: (Number -> Effect Unit) -> Behavior
-- User manually updates zoom-dependent elements
```

### 4. Interaction with Transitions

Should zoom-scale changes trigger transitions?

```purescript
-- Instant update (performance)
fontSizeZoom (\scale -> ...)

-- With transition
fontSizeZoomT (transition 200.0) (\scale -> ...)
```

### 5. Composition with Data-Driven Attributes

How do zoom-aware attributes compose with data-driven ones?

```purescript
T.elem Circle
  [ radius (\d -> baseRadius d)  -- Data-driven
  , strokeWidthZoom (\scale -> scale * 2.0)  -- Zoom-driven
  , fillZoom (\scale -> if scale < 1.0 then categoryColor d else detailColor d)  -- Both!
  ]
```

Need access to both `scale` and datum `d` in the lambda.

### 6. Alternative: Scale-Datum Tuples

```purescript
-- Attribute gets (scale, datum) tuple
type ZoomAttr a datum = (Number, datum) -> a

fontSizeZoom :: forall datum. ZoomAttr Number datum -> Attribute datum
fontSizeZoom f = ...

-- Usage
T.elem Text
  [ fontSizeZoom (\(scale, d) ->
      if scale < 0.5
        then 36.0
        else if d.importance > 0.8
          then 16.0
          else 12.0
    )
  ]
```

## Implementation Strategy

### Phase 1: Prototype (Validation)
- Manual zoom event handler approach
- Update specific attributes on zoom
- Test performance and UX
- Validate the concept before API commitment

### Phase 2: Type-level Design
- Design attribute type system to support zoom-aware attrs
- Determine how to thread zoom state through rendering
- Prototype with a few attributes (fontSize, opacity)

### Phase 3: Interpreter Support
- Modify D3 interpreter to track zoom state
- Implement attribute evaluation with zoom scale
- Handle update/transition strategies

### Phase 4: Full Implementation
- Support all relevant attributes
- Optimize update strategy
- Add helpers (zoom levels, interpolation)
- Documentation and examples

## Open Questions

1. **Should zoom-aware attributes be opt-in per element or global?**
   - Per element: More flexible but verbose
   - Global: Simpler but less control

2. **How granular should updates be?**
   - Per-frame during zoom (smooth but expensive)
   - On zoom end (janky but performant)
   - Throttled updates (compromise)

3. **Should we support pan-aware attributes too?**
   - Position-based LOD (render detail only in viewport)
   - Could generalize to "view-aware" attributes

4. **Integration with existing zoom behavior?**
   - Current: `Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#zoom-group"`
   - Enhanced: Needs to provide scale to render loop

5. **Interpolation support?**
   ```purescript
   fontSizeZoom (interpolate
     [ (0.0, 36.0)   -- at scale 0, font size 36
     , (1.0, 12.0)   -- at scale 1, font size 12
     , (10.0, 2.0)   -- at scale 10, font size 2
     ])
   ```

## Related Concepts

- **D3 Zoom**: https://github.com/d3/d3-zoom
- **Mapbox GL Zoom Functions**: https://docs.mapbox.com/style-spec/reference/expressions/#zoom
- **Observable Plot scales**: https://observablehq.com/plot/features/scales
- **Semantic Zooming** (academic): Different representations at different scales

## Examples to Implement

1. **Module Graph** (current need)
   - Hide labels at far zoom, show on hover
   - Increase label size at far zoom
   - Simplify to cluster view at very far zoom

2. **Geographic Map**
   - Country labels → City labels → Street labels
   - Simplified borders → Detailed borders

3. **Timeline**
   - Decade view → Year view → Month view → Day view
   - Aggregate events → Individual events

4. **Network Graph**
   - Show clusters → Show individual nodes
   - Hide low-importance edges at far zoom

## Advanced Use Cases: Multi-Scale Hierarchical Exploration

### Source Code Explorer
**Vision**: Navigate through layers of abstraction using zoom

- **Far zoom (Cluster level)**:
  - Modules as colored dots clustered by namespace
  - Force layout positions top-level modules
  - Only show namespace-level dependencies

- **Medium zoom (Module level)**:
  - Module names become visible
  - Dependency arrows appear
  - Circles sized by module complexity

- **Close zoom (Export level)**:
  - Each module expands to bubble pack
  - Shows individual exports/functions as nested circles
  - Links show cross-module function calls

- **Very close zoom (Implementation level)**:
  - Function details: type signatures, documentation
  - Internal call graph
  - Complexity metrics, performance data

**Implementation Pattern**:
```purescript
renderModule scale node =
  T.elem Circle
    [ cx node.x
    , cy node.y
    , radiusZoom (\s -> if s > 2.0 then 5.0 else 20.0)
    , fillZoom (\s -> if s > 2.0 then node.categoryColor else node.detailColor)
    ]
    `T.withChildren`
      (if scale > 2.0
        then [] -- collapsed: just the module circle
        else map (renderExport scale) node.exports -- expanded: show internals
      )
```

### Kubernetes Cluster Explorer
**Vision**: Understand cluster health and topology through spatial navigation

- **Far zoom (Cluster overview)**:
  - Namespaces as large circles
  - Colored by aggregate health status
  - Force layout separates concerns

- **Medium zoom (Namespace detail)**:
  - Namespace expands → pods appear as nested bubbles
  - Bubble size = resource allocation
  - Color = health (green/yellow/red)
  - Links show service dependencies

- **Close zoom (Pod internals)**:
  - Pod expands → containers visible inside
  - Container metrics overlaid
  - Init containers vs. running containers distinguished

- **Very close zoom (Container detail)**:
  - Process tree visualization
  - Real-time resource graphs (CPU, memory)
  - Log stream preview
  - Volume mounts

**Performance Optimization**:
- Out-of-viewport pruning: Only render visible namespaces/pods
- Aggregate metrics at far zoom: Don't compute pod details until zoomed in
- Progressive loading: Fetch details on-demand as user zooms

### AWS Infrastructure Explorer
**Vision**: Make sense of complex cloud architectures

- **Far zoom (Global view)**:
  - AWS regions as colored circles
  - Geographic layout or force-directed
  - Inter-region traffic flows

- **Medium zoom (Region detail)**:
  - Region → VPCs and availability zones appear
  - Subnets as nested areas
  - Cross-AZ redundancy visible

- **Close zoom (VPC internals)**:
  - Security groups as boundaries
  - Resources (EC2, RDS, Lambda) as nodes
  - Network topology becomes clear
  - Resource dependencies shown as links

- **Very close zoom (Resource detail)**:
  - Configuration viewer
  - CloudWatch metrics embedded
  - Recent logs
  - Cost allocation tags
  - IAM permissions graph

**Interactive Features**:
- Click security group → highlight affected resources
- Click resource → show all dependencies
- Filter by tag, cost center, or team
- Time-travel: Replay infrastructure changes

## The Composability Advantage

What makes this approach powerful is **composition of transformations**:

1. **Force Layout** positions top-level elements (modules, namespaces, regions)
2. **Bubble Pack** positions nested children (exports, pods, resources)
3. **Semantic Zoom** controls visibility and detail level
4. **Tree API** provides uniform interface for all scales

```purescript
-- Same declarative API works for all scales!
hierarchicalViz scale data =
  T.named SVG "svg" [...]
    `T.withChild`
      (T.named Group "zoom-group" []
        `T.withChildren` (map (renderNode scale) (forceLayout data))
      )

renderNode scale node =
  T.elem Circle [...]  -- Force-positioned
    `T.withChildren`
      (if scale > threshold
        then []  -- Collapsed
        else bubblePackLayout node.children  -- Expanded with bubble pack
      )
```

## Key Design Principles

1. **Spatial = Structural**: Physical layout mirrors information architecture
2. **Zoom = Navigation**: Moving through detail levels feels like exploring a space
3. **Progressive Disclosure**: Show overview first, details on demand
4. **Performance by Design**: Only render what's visible and relevant
5. **Uniform API**: Same Tree API regardless of scale or domain

## Beyond Visualization: Exploration Interfaces

This isn't just about "seeing" data - it's about **exploring** complex systems:

- **DevOps**: "Why is this pod unhealthy?" → Zoom in to see container logs
- **Architecture**: "What depends on this service?" → See incoming/outgoing links
- **Cost Optimization**: "Where are we spending?" → Color by cost, zoom to detail
- **Security**: "What has access to this resource?" → IAM permission graph
- **Debugging**: "Where is this function called?" → Call graph at function level

The combination of semantic zoom + hierarchical layout + force simulation creates a **spatial interface to abstract concepts** - making the invisible visible and explorable.

## Notes from Discussion (2025-01-16)

- User strongly prefers lambda/callback approach: `fontSizeZoom (\scale -> ...)`
- Recognition that there are "quite some challenges and interactions to think about"
- Desire to note this for future implementation rather than rush it
- This could be transformative for the Tree API beyond just text labels
- Need to validate concept before committing to specific API

## Next Steps

- [ ] Create working prototype using manual zoom event handlers
- [ ] Test with module graph example
- [ ] Measure performance characteristics
- [ ] Gather more use cases
- [ ] Design type-level approach
- [ ] Propose RFC for community feedback
