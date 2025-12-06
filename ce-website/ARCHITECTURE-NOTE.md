# Architecture Note: Type-Safe Scene Routing

**Date:** 2025-12-05 (review tomorrow morning)

## The Design Smell

`setViewState` in Explorer.purs has a case expression where `pure unit` means two different things:
- For layout views (Treemap, TreeLayout, ForceLayout): triggers scene transition
- For detail views (Neighborhood, FunctionCalls): intentionally skips scene transition

The bug we fixed (Treemap → Tree → Force → Treemap not working) happened because Treemap had `pure unit` instead of `goToScene TreemapForm`. The compiler couldn't distinguish "forgot to add scene" from "intentionally no scene".

## Proposed Solution: Split ViewState ADT

```purescript
-- Layout affects node positions - REQUIRES scene transition
data LayoutView
  = TreemapLayout ScopeFilter
  | TreeLayout ScopeFilter String
  | ForceLayout ScopeFilter String

-- Detail views filter/zoom - handled separately from scenes
data DetailView
  = NeighborhoodDetail String
  | FunctionCallsDetail String

-- Combined view state
data ViewState
  = Overview LayoutView           -- Layout-only, all nodes
  | Detail LayoutView DetailView  -- Layout preserved, detail overlaid

-- TOTAL function - compiler enforces exhaustiveness
layoutToScene :: LayoutView -> SceneId
layoutToScene (TreemapLayout _) = TreemapForm
layoutToScene (TreeLayout _ _) = TreeForm
layoutToScene (ForceLayout _ _) = TreeRun
```

## Why This Helps

1. `layoutToScene` is **total** - adding a new LayoutView constructor forces you to update the function
2. The distinction between "needs scene" and "doesn't need scene" is encoded in the type, not just comments
3. `setViewState` becomes: always call `goToScene (layoutToScene layout)` for layout changes

## Tradeoffs

- Breaking change to ViewState (touches multiple files)
- More explicit but more verbose
- Alternative: `toMaybeScene :: ViewState -> Maybe SceneId` is less invasive but still has the "Nothing looks the same" problem

## Files to Update (if implementing)

- `Engine/ViewState.purs` - new ADT structure
- `Engine/Explorer.purs` - setViewState, restoreToView, navigation
- `Component/SpagoGridApp.purs` - ViewState handling
- `Component/NarrativePanel.purs` - rendering based on ViewState

## Decision

Think about whether the type safety is worth the refactoring cost, or if there's a lighter-weight approach.

---

## Issue 2: Package Color Restoration Bug

When navigating back from Neighborhood view to Treemap, packages show 10% opacity instead of solid fill.

- `getNodeFill` in ColorPalette.purs returns solid color only for `PackageNode + Treemap`, 10% opacity for all other views
- `restoreFullView` calls `renderNodesOnly` which should set correct colors, but they're not applying
- Added `updateNodeColors targetView` as belt-and-suspenders fix - needs testing

Root cause unclear - may be D3 data binding issue with `appendData` not updating existing elements properly.

---

## Issue 3: Package Color Key Design

The color key showing all packages is problematic:
- With many packages (20+), the key becomes uselessly long
- But showing no key leaves users confused about what colors mean

Options to consider:
1. **Threshold approach**: If >10 packages, show explanatory text instead of full key ("Packages are colored to distinguish their modules in other views")
2. **Collapse/expand**: Show top N packages with "show more" option
3. **On-demand legend**: Don't show package colors by default, reveal on hover/click of a package
4. **Rethink the color strategy**: Maybe packages shouldn't have arbitrary colors - use a more meaningful color scheme?

The real question: What information does the user need from the color key? If the answer is "which package is which color," a scrolling legend doesn't help with 30 packages anyway. If it's "what do colors mean conceptually," explanatory text is better.
