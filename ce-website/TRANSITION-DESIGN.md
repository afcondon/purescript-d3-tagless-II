# Transition Design: Four-View Architecture

**Date:** 2025-12-06

## Overview

Replace TangleJS-based text controls with four icon buttons representing the four canonical views. Each view has explicit enter/exit/update transitions. Detail views (Neighborhood, FunctionCalls) are overlays - the icons act as "cancel" buttons returning to the chosen overview.

This is essentially an elaborate version of the LesMisGUP demo: nodes not used in a view are **removed from the DOM entirely**, eliminating any possibility of stale CSS or accidental selection.

## The Four Views

| View | Nodes Shown | Layout | Simulation |
|------|-------------|--------|------------|
| **Treemap** | All packages + all modules | Packages at grid positions, modules clustered on their package | On (clustering forces) |
| **Tree** | Used modules only | Vertical tree descending from Main | Off (pinned positions) |
| **Force** | Used modules only | Radial tree with Main at center | On (link forces) |
| **Topo** | Packages only | Topologically sorted DAG | Off (pinned positions) |

## Transition Matrix

### Direct Transitions (8 total)

```
         Treemap    Tree       Force      Topo
Treemap    -         T→Tr      T→F        T→To
Tree      Tr→T        -        Tr→F       (via T)
Force     F→T       F→Tr        -         (via T)
Topo      To→T      (via T)   (via T)      -
```

### Treemap ↔ Topo

**Treemap → Topo:**
1. All modules fade out and exit (remove from DOM)
2. All packages transition to topo graph positions
3. Topo links enter (fade in)
4. Simulation: OFF throughout

**Topo → Treemap:**
1. Topo links exit instantly
2. All packages transition to grid positions
3. All modules enter at package's grid position (fade in)
4. Simulation: ON after modules enter

### Treemap ↔ Tree

**Treemap → Tree:**
1. Packages fade out and exit
2. Unused modules fade out and exit
3. Used modules transition to tree positions
4. Tree links enter (bonus: animate growing from root)
5. Simulation: OFF

**Tree → Treemap:**
1. Tree links fade out and exit
2. Packages enter at grid positions (fade in)
3. Unused modules enter at package's grid position (fade in)
4. Used modules transition to package positions
5. Simulation: ON after all nodes in place

### Treemap ↔ Force

**Treemap → Force:**
1. Packages fade out and exit
2. Unused modules fade out and exit
3. Used modules transition to tree positions (starting point)
4. Force links enter
5. Simulation: ON (unpins and arranges radially)

**Force → Treemap:**
1. Force links exit instantly
2. Packages enter at grid positions (fade in)
3. Unused modules enter at package's grid position (fade in)
4. Used modules transition toward package positions
5. Simulation: ON throughout (clustering forces take over)

### Tree ↔ Force

**Tree → Force:**
1. Tree links exit instantly
2. Force links enter instantly
3. Modules unpinned
4. Simulation: ON

**Force → Tree:**
1. Simulation: STOP
2. Force links exit instantly
3. Tree links enter instantly
4. Modules transition to tree positions and pin
5. Simulation: OFF

### Indirect Transitions (via Treemap)

For simplicity, transitions to/from Topo that don't involve Treemap go through Treemap:

- **Tree → Topo:** Tree → Treemap → Topo
- **Topo → Tree:** Topo → Treemap → Tree
- **Force → Topo:** Force → Treemap → Topo
- **Topo → Force:** Topo → Treemap → Force

**Bonus:** If we can concatenate transitions (à la `<>`), these become single animated sequences rather than two separate transitions.

## Implementation Notes

### Node Lifecycle

```purescript
-- Each view defines which nodes it needs
data ViewNodes
  = AllNodes                    -- Treemap: packages + all modules
  | UsedModulesOnly             -- Tree, Force: modules in spanning tree
  | PackagesOnly                -- Topo: just packages

-- Transition computes enter/update/exit sets
computeTransition :: ViewNodes -> ViewNodes -> Array SimNode
                  -> { enter :: Array SimNode, update :: Array SimNode, exit :: Array SimNode }
```

### Link Lifecycle

```purescript
data ViewLinks
  = NoLinks                     -- Treemap: clustering forces, no visible links
  | TreeLinks                   -- Tree: parent-child edges
  | ForceLinks                  -- Force: same edges, different styling?
  | TopoLinks                   -- Topo: package dependency edges
```

### Transition Monad (aspirational)

```purescript
-- Composable transitions
newtype Transition a = Transition (Aff a)

instance Semigroup (Transition Unit) where
  append t1 t2 = t1 *> t2  -- sequence

-- Then:
treeToTopo :: Transition Unit
treeToTopo = treeToTreemap <> treemapToTopo
```

## UI Design

Four icons in the narrative panel, horizontally arranged:

```
[ Grid ] [ Tree ] [ Radial ] [ Topo ]
   ▲
  active
```

- Active view has highlighted/selected state
- Clicking active view: no-op (or could reset zoom)
- Clicking inactive view: triggers transition
- In detail view (Neighborhood): any icon click exits detail and goes to that overview

Below icons: description text changes based on view
- "Treemap: All modules clustered by package"
- "Tree: Import hierarchy from Main"
- "Force: Radial import layout"
- "Topo: Package dependencies"

## Files to Modify

1. **Engine/ViewState.purs** - Simplify to four views (detail views become overlays)
2. **Engine/Scenes.purs** - Define enter/exit/update for each view
3. **Engine/Explorer.purs** - Transition logic, DOM management
4. **Component/NarrativePanel.purs** - Replace Tangle text with icon buttons
5. **Data/ColorPalette.purs** - Simplify (no view-dependent opacity hacks)
6. **public/styles.css** - Icon button styling

## Migration from TangleJS

TangleJS code (Tangle.Core, Tangle.Parser) should be extracted to a separate package or moved to demo-website where it can shine in a more appropriate context.
