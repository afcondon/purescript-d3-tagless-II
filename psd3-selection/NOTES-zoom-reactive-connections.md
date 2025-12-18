# Design Note: Zoom-Reactive Connections

## Problem

When building visualizations with fixed UI elements (palettes, legends, toolbars) alongside zoomable content (graphs, trees, maps), we often need visual connections between them. For example:
- Type annotations connecting to nodes
- Tooltips pointing to data points
- Legend items linked to chart elements

The challenge: fixed elements exist in SVG coordinate space, while zoomable elements transform with `translate(x, y) scale(k)`. A naive static connection breaks as soon as the user zooms or pans.

## Current Solution (TreeBuilder3 Demo)

### FFI Additions

```javascript
// Zoom behavior with callback on each zoom event
attachZoomWithCallback_(element, scaleMin, scaleMax, targetSelector, initialTransform, onZoom)

// Imperative DOM update by selector
updateAttr_(selector, attr, value)
```

### Pattern

1. **Setup**: Use `attachZoomWithCallback_` which calls a PureScript callback on each zoom event
2. **State**: Store zoom transform `{ k, x, y }` in Halogen state via `ZoomChanged` action
3. **Update**: On zoom change, recalculate connection coordinates and update DOM imperatively

### Coordinate Transform

For a point at `(baseX, baseY)` inside a zoom group:
```
screenX = baseX * k + zoomX
screenY = baseY * k + zoomY
```

## Future Library Design

### Anchor Types

```purescript
data Anchor
  = FixedAnchor Number Number              -- Static SVG coordinates
  | ZoomedAnchor Number Number String      -- Base coords + group selector
  | SimulationAnchor NodeId String         -- Simulation node + sim ID

-- Connection definition
type Connection =
  { from :: Anchor
  , to :: Anchor
  , pathStyle :: PathStyle  -- Bezier, straight, elbow, etc.
  }
```

### ZoomRegistry

A central registry tracking transforms for named zoom groups:

```purescript
type ZoomRegistry = Map String ZoomTransform

-- Register a zoom group
registerZoomGroup :: String -> Effect Unit

-- Update transform (called from zoom callback)
updateZoomTransform :: String -> ZoomTransform -> Effect Unit

-- Query current transform
getZoomTransform :: String -> Effect ZoomTransform
```

### Connection Manager

Manages connections and auto-updates on zoom:

```purescript
-- Create a connection that auto-updates
createConnection :: Connection -> Effect ConnectionId

-- Remove a connection
removeConnection :: ConnectionId -> Effect Unit

-- Manual refresh (e.g., when anchors move)
refreshConnection :: ConnectionId -> Effect Unit
```

### Declarative Integration

Ideally integrate with the tree rendering DSL:

```purescript
T.connection
  [ F.from (F.fixedAnchor 100.0 50.0)
  , F.to (F.zoomedAnchor nodeX nodeY ".zoom-group")
  , F.pathStyle F.bezierHorizontal
  , F.stroke (F.color Theme.arrowColor)
  ]
```

## Considerations

1. **Multiple zoom groups**: Different parts of the viz may zoom independently
2. **Simulation nodes**: Force simulations update positions on tick, not just zoom
3. **Performance**: Many connections updating on every zoom frame
4. **Cleanup**: Connections must be removed when elements are removed
5. **Declarative vs Imperative**: Balance between tree DSL purity and update efficiency

## Use Cases

- Type annotations (TreeBuilder3)
- Tooltip arrows
- Legend highlighting
- Cross-visualization linking
- Annotation layers
- Node relationship indicators

## Status

**Demonstrated** in TreeBuilder3 with manual implementation.
**Not yet generalized** to library - needs design for simulation support and multi-group scenarios.
