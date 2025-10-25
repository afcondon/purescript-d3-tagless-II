# Module Migration Plan - Phase 1

**Branch:** `refactor/module-structure`
**Status:** Planning

## Design Principles

1. **Clear top-level namespace:** `PSD3` is the public API
2. **Predictable structure:** Users can guess where things are
3. **Separation of concerns:** Type classes, interpreters, types, and utilities are distinct
4. **Single import for common use:** `import PSD3` should work for 90% of cases

---

## New Module Hierarchy

```
PSD3                                  -- Main public API (re-exports)
├── PSD3.Attributes                   -- Re-exports all attribute functions
├── PSD3.Types                        -- Re-exports common types
├── PSD3.Interpreters                 -- Re-exports common interpreters
│
PSD3.Capabilities                     -- Type classes (was D3Tagless.Capabilities)
├── PSD3.Capabilities.Selection       -- SelectionM type class
├── PSD3.Capabilities.Simulation      -- SimulationM type class
├── PSD3.Capabilities.Sankey          -- SankeyM type class
│
PSD3.Interpreter                      -- Interpreter implementations
├── PSD3.Interpreter.D3               -- Main D3 interpreter (D3M monad)
├── PSD3.Interpreter.String           -- String interpreter
├── PSD3.Interpreter.MetaTree         -- MetaTree interpreter
│
PSD3.Internal                         -- Internal implementation (not for users)
├── PSD3.Internal.Attributes          -- Attribute system
├── PSD3.Internal.Selection           -- Selection operations
├── PSD3.Internal.Simulation          -- Simulation operations
├── PSD3.Internal.Sankey              -- Sankey operations
├── PSD3.Internal.Scales              -- Scale operations
├── PSD3.Internal.Axes                -- Axes operations
├── PSD3.Internal.FFI                 -- All FFI functions
├── PSD3.Internal.Types               -- Core types
├── PSD3.Internal.Zoom                -- Zoom behavior
├── PSD3.Internal.Generators          -- Path generators etc
│
PSD3.Data                             -- Data types and utilities
├── PSD3.Data.Tree                    -- Tree data structures
├── PSD3.Data.Graph                   -- Graph data structures
├── PSD3.Data.Node                    -- Node types
```

---

## Migration Mapping

### Current → New

#### Type Classes (Capabilities)
```
D3Tagless.Capabilities
  → PSD3.Capabilities (single file with all three classes)
  OR
  → PSD3.Capabilities.Selection (SelectionM only)
  → PSD3.Capabilities.Simulation (SimulationM only)
  → PSD3.Capabilities.Sankey (SankeyM only)
```

**Decision needed:** Single file or split? I recommend **split** for clarity.

#### Interpreters
```
D3Tagless.Instance.Selection  → PSD3.Interpreter.D3 (main interpreter)
D3Tagless.Instance.Simulation → (merged into PSD3.Interpreter.D3)
D3Tagless.Instance.Sankey     → (merged into PSD3.Interpreter.D3)
D3Tagless.Utility             → PSD3.Internal.Utility

D3Tagless.Capabilities.String   → PSD3.Interpreter.String
D3Tagless.Capabilities.MetaTree → PSD3.Interpreter.MetaTree
```

**Rationale:** The D3 interpreter instances are all part of the same monad (D3M), so they should live together.

#### Attributes
```
D3.Attributes.Instances → PSD3.Internal.Attributes.Instances
D3.Attributes.Sugar     → PSD3.Internal.Attributes.Sugar

PSD3.Attributes (NEW)   → Re-exports all attribute functions
```

#### Selection
```
D3.Selection           → PSD3.Internal.Selection.Types
D3.Selection.Functions → PSD3.Internal.Selection.Functions
D3.Selection.Zoom      → PSD3.Internal.Zoom
```

#### Scales & Axes
```
D3.Scales        → PSD3.Internal.Scales.Types
D3.Scales.Linear → PSD3.Internal.Scales.Linear
D3.Axes          → PSD3.Internal.Axes
```

#### Simulation
```
D3.Simulation.Types     → PSD3.Internal.Simulation.Types
D3.Simulation.Config    → PSD3.Internal.Simulation.Config
D3.Simulation.Forces    → PSD3.Internal.Simulation.Forces
D3.Simulation.Functions → PSD3.Internal.Simulation.Functions
```

#### Sankey
```
D3.Layouts.Sankey.Types     → PSD3.Internal.Sankey.Types
D3.Layouts.Sankey.Functions → PSD3.Internal.Sankey.Functions
```

#### Hierarchical
```
D3.Layouts.Hierarchical → PSD3.Internal.Hierarchical
```

#### Generators
```
D3.Generators.Line → PSD3.Internal.Generators.Line
```

#### Data Types
```
D3.Data.Types   → PSD3.Internal.Types (core types)
D3.Data.Tree    → PSD3.Data.Tree
D3.Data.Graph   → PSD3.Data.Graph
D3.Data.Node    → PSD3.Data.Node
D3.Data.Utility → PSD3.Data.Utility

Data.Tree → (keep as is - not part of D3 library)
```

#### FFI
```
D3.FFI → PSD3.Internal.FFI
```

---

## Public API Module Design

### PSD3.purs (Main Entry Point)

```purescript
module PSD3
  ( -- Type Classes
    class SelectionM(..)
  , class SimulationM(..)
  , class SankeyM(..)

    -- Main Interpreter
  , D3M
  , eval_D3M
  , runD3M
  , exec_D3M

    -- Common Types
  , D3Selection_
  , Element(..)
  , Selector
  , SelectionAttribute
  , Behavior(..)

    -- Attributes (re-export common ones)
  , module Attrs

    -- Data Types
  , module Data
  ) where

import PSD3.Capabilities.Selection (class SelectionM(..))
import PSD3.Capabilities.Simulation (class SimulationM(..))
import PSD3.Capabilities.Sankey (class SankeyM(..))

import PSD3.Interpreter.D3 (D3M, eval_D3M, runD3M, exec_D3M)

import PSD3.Internal.Types (D3Selection_, Element(..), Selector, SelectionAttribute, Behavior(..))

import PSD3.Attributes (fill, stroke, opacity, x, y, width, height, text) as Attrs

import PSD3.Data (TreeNode, GraphNode) as Data
```

**Users import:** `import PSD3`

### PSD3.Attributes.purs (All Attributes)

```purescript
module PSD3.Attributes
  ( -- Geometry
    x, y, cx, cy, r, width, height

    -- Colors
  , fill, fillOpacity
  , stroke, strokeWidth, strokeOpacity
  , backgroundColor

    -- Text
  , text, fontSize, fontFamily

    -- Transformations
  , translate, rotate, scale

    -- Classes and styles
  , classed, id

    -- SVG specific
  , viewBox, preserveAspectRatio
  , d, pathD

    -- Transitions
  , transition, transitionWithDuration
  , andThen, to

    -- Events
  , onClick, onMouseOver, onMouseOut

    -- Ordering
  , order, raise, lower

    -- Special
  , remove
  ) where

import PSD3.Internal.Attributes.Sugar
```

### PSD3.Types.purs (Common Types)

```purescript
module PSD3.Types
  ( -- Core Selection Types
    D3Selection_
  , Element(..)
  , Selector
  , SelectionAttribute
  , Behavior(..)

    -- Data Types
  , Datum_
  , Index_

    -- Simulation Types
  , D3Simulation_
  , SimulationNode
  , SimulationLink

    -- Transition Types
  , Transition
  , EasingFunction(..)
  ) where

import PSD3.Internal.Types
```

---

## Migration Steps

### Step 1: Create New Directory Structure
```bash
mkdir -p src/lib/PSD3/Capabilities
mkdir -p src/lib/PSD3/Interpreter
mkdir -p src/lib/PSD3/Internal/Attributes
mkdir -p src/lib/PSD3/Internal/Selection
mkdir -p src/lib/PSD3/Internal/Simulation
mkdir -p src/lib/PSD3/Internal/Sankey
mkdir -p src/lib/PSD3/Internal/Scales
mkdir -p src/lib/PSD3/Data
```

### Step 2: Move and Rename Files (in order)

**Phase 2a: Move Internal Implementation**
1. Move `D3/FFI/FFI.purs` → `PSD3/Internal/FFI.purs`
2. Move `D3/Data/Types.purs` → `PSD3/Internal/Types.purs`
3. Move `D3/Attributes/*` → `PSD3/Internal/Attributes/`
4. Move `D3/Selection/*` → `PSD3/Internal/Selection/`
5. Move `D3/Scales/*` → `PSD3/Internal/Scales/`
6. Move `D3/Axes/*` → `PSD3/Internal/Axes/`
7. Move `D3/Simulation/*` → `PSD3/Internal/Simulation/`
8. Move `D3/Layouts/Sankey/*` → `PSD3/Internal/Sankey/`
9. Move `D3/Layouts/Hierarchical/*` → `PSD3/Internal/Hierarchical/`
10. Move `D3/Generators/*` → `PSD3/Internal/Generators/`
11. Move `D3/Selection/Zoom.purs` → `PSD3/Internal/Zoom.purs`

**Phase 2b: Move Type Classes**
1. Move `Interpreters/Capabilities.purs` → Split into:
   - `PSD3/Capabilities/Selection.purs`
   - `PSD3/Capabilities/Simulation.purs`
   - `PSD3/Capabilities/Sankey.purs`

**Phase 2c: Move Interpreters**
1. Merge `Interpreters/D3/*.purs` → `PSD3/Interpreter/D3.purs`
2. Move `Interpreters/String/String.purs` → `PSD3/Interpreter/String.purs`
3. Move `Interpreters/MetaTree/Meta.purs` → `PSD3/Interpreter/MetaTree.purs`

**Phase 2d: Move Data Types**
1. Move `D3/Data/Tree.purs` → `PSD3/Data/Tree.purs`
2. Move `D3/Data/Graph.purs` → `PSD3/Data/Graph.purs`
3. Move `D3/Data/Node.purs` → `PSD3/Data/Node.purs`
4. Move `D3/Data/Utility.purs` → `PSD3/Data/Utility.purs`

**Phase 2e: Create Public API Modules**
1. Create `PSD3.purs` (main entry point)
2. Create `PSD3/Attributes.purs` (attribute re-exports)
3. Create `PSD3/Types.purs` (type re-exports)

### Step 3: Update Module Declarations

For each moved file, update:
- `module X where` declaration
- All `import` statements
- Re-export lists

### Step 4: Update Website/Examples

Update all imports in:
- `src/website/**/*.purs`
- Example code snippets
- Documentation

### Step 5: Test

1. Run `npm run build`
2. Run `npm run bundle`
3. Test all visualizations in browser
4. Fix any import errors

---

## Breaking Changes

This is a **major breaking change**. All imports will need updating.

### Before:
```purescript
import D3Tagless.Capabilities (class SelectionM(..))
import D3Tagless.Instance.Selection (D3M, eval_D3M)
import D3.Attributes.Sugar (fill, stroke, x, y)
import D3.Selection (SelectionAttribute)
```

### After:
```purescript
import PSD3
-- OR for explicit imports:
import PSD3 (class SelectionM(..), D3M, eval_D3M)
import PSD3.Attributes (fill, stroke, x, y)
```

---

## File Move Script

We can automate much of this with git mv:

```bash
# Move FFI
git mv src/lib/D3/FFI/FFI.purs src/lib/PSD3/Internal/FFI.purs
git mv src/lib/D3/FFI/FFI.js src/lib/PSD3/Internal/FFI.js

# Move Types
git mv src/lib/D3/Data/Types.purs src/lib/PSD3/Internal/Types.purs

# ... etc
```

---

## Open Questions

1. **Split Capabilities?**
   - Single `PSD3.Capabilities` with all three type classes?
   - Or split into `Selection`, `Simulation`, `Sankey`?
   - **Recommendation:** Split for clarity

2. **Merge D3 Interpreters?**
   - Currently 3 files: Selection, Simulation, Sankey instances
   - All for the same `D3M` monad
   - **Recommendation:** Merge into single `PSD3.Interpreter.D3`

3. **Internal vs Public Data types?**
   - Are `Tree`, `Graph`, `Node` public API or internal?
   - **Recommendation:** Public in `PSD3.Data.*`

4. **Keep Data.Tree?**
   - `src/lib/Data/Tree.purs` is a general tree structure
   - Not D3-specific
   - **Recommendation:** Keep separate, or move to `PSD3.Data.Tree`?

---

## Next Steps

1. Get approval on this structure
2. Create directory structure
3. Start moving files (automated where possible)
4. Update module declarations
5. Update imports throughout codebase
6. Test thoroughly

**Estimated time:** 2-3 sessions
