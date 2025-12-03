# Data.Tree Triage

## Current State (RESOLVED)

Tree type is now consolidated in `psd3-tree` package:

```
psd3-tree/
├── src/Data/Tree.purs        -- Core type: Tree(..), leaf, node
└── src/Data/Tree/Functions.purs  -- Utilities (currently unused)
```

Dependency graph:
```
psd3-tree  <--  psd3-selection
           <--  psd3-layout
```

Both define:
```purescript
data Tree a = Node a (List (Tree a))
```

## Active Usage (non-archived code)

### Packages using `Data.Tree` (psd3-selection)
| File | Usage |
|------|-------|
| psd3-selection/src/Data/DependencyGraph.purs | `import Data.Tree (Tree)` |
| psd3-selection/src/PSD3/Data/Graph/Algorithms.purs | `import Data.Tree (Tree(..))` |
| psd3-selection/src/PSD3/Data/Tree.purs | `import Data.Tree (Tree(..))` |

### Packages using `D3.Layout.Tree` (psd3-layout)
| File | Usage |
|------|-------|
| psd3-layout/src/D3/Layout/Hierarchy/Tree4.purs | `import D3.Layout.Tree (Tree(..))` |
| psd3-layout/src/D3/Layout/Hierarchy/Cluster4.purs | `import D3.Layout.Tree (Tree(..))` |
| demo-website/src/Viz/TreeAPI/*.purs (6 files) | `import D3.Layout.Tree (Tree(..))` |
| demo-website/src/Viz/Hierarchy/AnimatedTree4Cluster4v2.purs | `import D3.Layout.Tree (Tree(..))` |
| demo-website/src/Viz/Spago/Tree.purs | `import D3.Layout.Tree (Tree(..))` |
| demo-website/src/Shared/Data.purs | `import D3.Layout.Tree (Tree(..))` |
| demo-website/src/Component/HowTo/HowtoTreeExplorer.purs | `import D3.Layout.Tree (Tree(..))` |
| ce-website/src/Data/Loader.purs | `import D3.Layout.Tree (Tree(..))` |

## Utility Function Usage

| Function | Used in Active Code? | Notes |
|----------|---------------------|-------|
| `Node` constructor | YES | Core type, widely used |
| `treeMapDeep` | NO | Only in archived `TreeViz3/Tree3.purs` |
| `treeMapOverChildren` | NO | Never used outside definition |
| `hasChildren` | NO | Different FFI `hasChildren` exists in FlareData |
| `subTree` | NO | Never used outside definition |
| `filterTransformToList` | NO | Never used outside definition |
| `filterTransformToListRecursive` | NO | Never used outside definition |
| `leaf` | NO | Never used outside definition |
| `node` | NO | Never used outside definition |

## Registry Alternative: `tree-rose`

Package `purescript-tree-rose` (v4.0.2) uses:
```purescript
type Tree a = Cofree List a  -- type alias over Cofree comonad
```

Different API - uses `mkTree` instead of `Node` constructor.

## Recommendations

### Option 1: Create `psd3-tree` foundational package (RECOMMENDED)
- Move `Data.Tree` to new minimal package
- Both `psd3-selection` and `psd3-layout` depend on it
- Eliminates duplication while keeping API stable
- Clean dependency graph: `psd3-tree` <- `psd3-selection`, `psd3-layout`

### Option 2: Keep current duplication
- Works but code smell
- Two identical files to maintain

### Option 3: Migrate to `tree-rose`
- Would require API changes (Node -> mkTree)
- Could contribute our utility functions as PR
- More principled (Cofree comonad)
- **Future consideration** after Option 1 stabilizes

## Cleanup Opportunity

Since utility functions are unused, we could:
1. Remove them from the Tree module
2. Keep only: `Tree(..)`, `leaf`, `node` (if convenient)
3. Or keep them for potential future use / PR to tree-rose

---
*Generated: 2025-12-03*
