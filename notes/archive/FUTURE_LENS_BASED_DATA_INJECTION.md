# Future Idea: Lens-Based Data Injection

## The Concept

Use lenses to "reach into" a tree structure and inject data at specific points.

```purescript
-- Define tree with unfilled data holes
baseTree :: Tree Unit
baseTree =
  named "svg" SVG [...] `withChildren`
    [ dataHole "links"  -- Placeholder, no data yet
    , dataHole "nodes"  -- Placeholder, no data yet
    ]

-- Define lenses for each hole
_linksData :: Lens' VisualizationTree (Array LinkDatum)
_nodesData :: Lens' VisualizationTree (Array NodeDatum)

-- Fill in data using lenses
myViz = baseTree
  # set _linksData linkData
  # set _nodesData nodeData

-- Or partially fill:
partialViz = baseTree # set _nodesData nodeData
-- Still has unfilled _linksData hole

-- Or update data:
updatedViz = myViz # over _nodesData (filter _.visible)
```

## Benefits

✅ **Separation of structure and data** - Define tree once, fill data multiple times
✅ **Composability** - Lenses compose naturally
✅ **Type safety** - Lens tracks what type of data goes in each hole
✅ **Incremental** - Can partially fill a tree
✅ **Transformations** - Use `over` to transform data before injecting

## Implementation Sketch

```purescript
-- Tree with labeled holes
data TreeWithHoles holes datum
  = Node ...
  | Hole (Proxy holeName) ElementType Template  -- Named hole
  | Filled datum (Tree datum)  -- Hole with data filled in

-- Lens to fill a specific hole
dataAt :: forall holeName datum. IsSymbol holeName => Proxy holeName -> Lens' (TreeWithHoles holes Unit) (Maybe (Array datum))
```

This would enable really powerful composition patterns, but it's a refinement of the current approach. Let's get the basic data joins working first, then we can explore this!

**Status**: Future enhancement, don't implement yet
**Filed**: 2025-01-15
