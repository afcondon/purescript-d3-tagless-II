# TangleJS View System Design

## Overview

A curated, semantic view description system where users explore visualizations through natural language descriptions with interactive (TangleJS-style) controls.

## Key Insight

**Two layers:**
- **User-facing**: Semantic view descriptions with meaningful parameters
- **Hidden**: Scene configs, forces, transitions, mechanics

Users never see forces or transition sequences. They see:
```
"[Radial tree] of [project modules], rooted at [Main]"
```

## ViewState ADT

```purescript
data ViewState
  = PackageGrid ScopeFilter
  | RadialTree ScopeFilter TreeRoot
  | DependencyGraph ScopeFilter
  | FunctionCalls NeighborhoodScope Ordering
  | Treemap ScopeFilter SizeMetric
  | Neighborhood ModuleName

data ScopeFilter
  = ProjectOnly
  | ProjectAndLibraries
  | SinglePackage String

data TreeRoot = FromMain | FromModule String
data Ordering = Topological | BySize | Alphabetical
data SizeMetric = ModuleCount | LineCount | DependencyCount
```

## Description Generator

Each `ViewState` generates natural text with bracketed TangleJS controls:

| ViewState | Description |
|-----------|-------------|
| `PackageGrid ProjectOnly` | "**[Grid]** of **[project]** packages" |
| `RadialTree ProjectAndLibraries FromMain` | "**[Radial tree]** of **[all modules]**, rooted at **[Main]**" |
| `FunctionCalls (InNeighborhood "Effect.Aff") Topological` | "**[Function calls]** in **[Effect.Aff neighborhood]**, **[topologically sorted]**" |

Clicking a bracketed item reveals alternatives available from this state.

## State Machine

Transitions are curated—not all combinations exist:

```
PackageGrid ──→ RadialTree, Treemap, DependencyGraph
RadialTree ──→ PackageGrid, Neighborhood (on click)
Neighborhood ──→ FunctionCalls, RadialTree (zoomed)
```

Internally, `PackageGrid → RadialTree` might execute `GridForm → OrbitForm → TreeForm → TreeRun`, but users only see the end state description.

## Design Principles

1. **Curated but not obviously curated** — feels like natural exploration
2. **Semantic parameters** — "project modules" not "nodeFilter: ProjectOnly"
3. **Hidden mechanics** — forces, transitions, scene configs are implementation details
4. **TangleJS controls** — inline, interactive parameter editing
5. **State machine validity** — only offer transitions that make sense
