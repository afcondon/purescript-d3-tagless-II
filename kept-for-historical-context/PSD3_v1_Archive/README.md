# PSD3 v1 Library Archive

**Archived**: 2025-11-15

This directory contains the **OLD PSD3 v1 library** modules that have been superseded by **PSD3v2**.

## What Was Archived

### Core OLD Architecture
- **Capabilities/** - Old capability typeclasses (Selection, Simulation, Sankey)
- **Interpreter/** - Old interpreters (D3, MermaidAST, MetaTree, String)
- **Attributes.purs** - Old attribute system
- **Types.purs** - Old type definitions
- **Component/** - Old component helpers (SimulationState)

### OLD Internal Modules
- **Internal/Selection/** - Old selection types and functions
- **Internal/Attributes/** - Old attribute instances and sugar
- **Internal/Hierarchical.purs** - Old hierarchical helpers
- **Simulation/** - Old simulation helpers (RunSimulation, Scene, SceneTransition, Update)

## Why Archived

These modules represented the **v1 architecture**:

**Problems with v1**:
- SelectionM monad made composition difficult
- No phantom types = runtime errors possible
- datum_/Datum_ pattern was confusing
- Manual DOM manipulation (append/select chains)
- No declarative structure (Tree API)
- Old simulation management

## New Architecture (PSD3v2)

**PSD3v2 Solutions**:
- ✅ Phantom types track selection state at compile time
- ✅ Tree API for declarative DOM structure
- ✅ SimulationM2 with init/update/start/stop
- ✅ General Update Pattern (GUP) built-in
- ✅ Type-safe behaviors (drag, zoom)
- ✅ Smooth transitions with caching

### Module Mapping

| OLD (v1) | NEW (v2) |
|----------|----------|
| `PSD3.Capabilities.Selection` | `PSD3v2.Capabilities.Selection` |
| `PSD3.Capabilities.Simulation` | `PSD3v2.Capabilities.Simulation` |
| `PSD3.Interpreter.D3` | `PSD3v2.Interpreter.D3v2` |
| `PSD3.Internal.Selection.*` | `PSD3v2.Selection.*` |
| `PSD3.Attributes` | `PSD3v2.Attribute.Types` |
| Manual `append`/`select` | Tree API (`renderTree`) |

### Code Comparison

**OLD (v1)**:
```purescript
import PSD3.Interpreter.D3 (runD3, D3M)
import PSD3.Capabilities.Selection (select, append, attr)

example :: D3M Unit
example = do
  body <- select "body"
  svg <- append SVG body
  attr Width 800.0 svg
  attr Height 600.0 svg
  g <- append Group svg
  circles <- selectAll Circle g
  ...
```

**NEW (v2)**:
```purescript
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.VizTree.Tree as T

example :: D3v2M SUnbound Unit
example = do
  container <- select "body"
  let tree =
        T.elem SVG [width 800.0, height 600.0]
          `T.withChild`
            T.elem Group []
              `T.withChild`
                T.joinData "circles" "circle" data \d ->
                  T.elem Circle [cx d.x, cy d.y, radius 5.0]
  renderTree container tree
```

## What's Still Used

These modules are **SHARED** between v1 and v2:

**Data Structures** (still in `PSD3/Data/`):
- Node.purs, Tree.purs, Graph.purs - Universal data structures

**Layouts** (still in `PSD3/Layout/`):
- Hierarchy layouts (Tree, Cluster, Pack, Treemap, Partition)
- Sankey layout
- These are pure layout algorithms, work with any renderer

**Simulation Core** (still in `PSD3/Internal/Simulation/`):
- Types, Config, Forces, Functions
- Used by both v1 and v2

**FFI & Utilities** (still in `PSD3/Internal/`):
- FFI.purs, Types.purs, Utility.purs, Zoom.purs
- Axes.purs, Scales/, Generators/
- Low-level D3 bindings

## Migration Guide

1. Replace imports:
   - `PSD3.Interpreter.D3` → `PSD3v2.Interpreter.D3v2`
   - `PSD3.Capabilities.*` → `PSD3v2.Capabilities.*`

2. Use Tree API:
   - Replace manual `append`/`select` chains with declarative trees
   - Use `renderTree` instead of imperative DOM manipulation

3. Use phantom types:
   - Selection types now track state: `D3v2Selection_ SUnbound`
   - Compiler prevents invalid operations

4. Use SimulationM2:
   - `init` → initialize simulation
   - `update` → modify nodes/links/forces
   - `start`/`stop` → control simulation
   - `reheat` → adjust alpha

5. Use GUP:
   - `joinData` in Tree API automatically handles enter/update/exit
   - No manual data joins needed

## References

- Cleanup plan: `../../../CLEANUP_PLAN.md`
- Working examples: `src/website/Component/LesMisGUPTree.purs`
- PSD3v2 modules: `../PSD3v2/`
