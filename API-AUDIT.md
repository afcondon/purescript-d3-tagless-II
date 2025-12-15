# API Generation Audit

## Status: Phase 3 Complete (Full Migration)

### Completed Actions
- [x] Created `PSD3.AST` - Canonical source for visualization AST (renamed "TreeAPI" to "AST")
- [x] Created `PSD3.Render` - Clean facade for D3 rendering (`runD3`, `select`, `renderTree`)
- [x] Updated `PSD3` umbrella module with clean exports and updated documentation
- [x] Migrated all demo-website, ce2-website, ce-website imports from `PSD3v2.VizTree.Tree` to `PSD3.AST`
- [x] Migrated all internal imports to use `PSD3.AST`
- [x] **Deleted `PSD3v2.VizTree.Tree`** - code moved into `PSD3.AST`
- [x] All packages build successfully

---

## Current State

### Module Namespaces

**psd3-selection:**
| Namespace | Purpose | Status |
|-----------|---------|--------|
| `PSD3` | Umbrella re-exports | Updated with clean exports |
| `PSD3.AST` | Declarative DOM AST (was "TreeAPI") | **NEW** - Public API |
| `PSD3.Render` | D3 rendering facade | **NEW** - Public API |
| `PSD3v2.VizTree.Tree` | Legacy "TreeAPI" | **DELETED** - code moved to `PSD3.AST` |
| `PSD3v2.Selection.*` | Selection system (lower-level) | Keep but hide |
| `PSD3v2.Interpreter.*` | D3, Mermaid, English interpreters | Keep |
| `PSD3v2.Attribute.Types` | Type-safe attributes | Keep |
| `PSD3v2.Behavior.*` | Zoom, drag, click | Keep |
| `PSD3v2.Transition.*` | Transitions | Keep |
| `PSD3v2.Capabilities.*` | Selection/Transition type classes | Keep (internal) |
| `PSD3v3.*` | Finally tagless expressions | Keep |
| `PSD3v3.Integration` | Bridge v3→v2 | Keep |
| `PSD3.Data.*` | Node, Tree, Graph types | Keep |
| `PSD3.Scale.*` | D3 scales | Keep |
| `PSD3.Internal.*` | FFI, internal types | Keep (internal) |

**psd3-simulation:**
| Namespace | Purpose | Status |
|-----------|---------|--------|
| `PSD3.ForceEngine.*` | Simulation engine | Keep |
| `PSD3.ForceEngine.Setup` | Declarative force setup | **Promote** |
| `PSD3.Scene.*` | Scene management | Review |
| `PSD3.Config.*` | Configuration DSL | Review |

---

## Problems

### 1. "TreeAPI" Name Confusion

Current: `PSD3v2.VizTree.Tree`

The name "Tree" conflicts with:
- `PSD3.Data.Tree` - Tree data structures
- `PSD3.Layout.Hierarchy.Tree` - Tree layouts (psd3-layout)
- Tree layout examples in `demo-website/src/Viz/TreeAPI/TreeViz.purs`

Users see "TreeAPI" and think "tree visualization" not "declarative DOM structure".

### 2. Version Suffixes (v2, v3) Are Implementation Details

Users see:
```purescript
import PSD3v2.VizTree.Tree as T
import PSD3v3.Integration (v3Attr)
import PSD3v2.Interpreter.D3v2 (render)
```

These versions are meaningless to users. They're internal evolution markers.

### 3. Selection API Exposure

`PSD3v2.Selection.Operations` is low-level DOM manipulation. Now that VizTree works with simulations, most users shouldn't need direct Selection access.

---

## Proposed Changes

### Rename "TreeAPI" → "AST"

**Decision: `PSD3.AST`**

Rationale:
- It IS an abstract syntax tree that gets interpreted
- Short and accurate
- Fits the interpreter pattern narrative
- Common in PL contexts, appropriate for PureScript community

```purescript
-- Before
import PSD3v2.VizTree.Tree as T

-- After
import PSD3.AST as A
```

### Remove Version Suffixes from Public API

Create clean re-export modules:

```
PSD3.Viz          -- re-exports PSD3v2.VizTree.Tree
PSD3.Interpreter  -- re-exports PSD3v2.Interpreter.*
PSD3.Attr         -- re-exports PSD3v3 expressions + v2 attrs
PSD3.Selection    -- re-exports PSD3v2.Selection (for advanced use)
```

### Hide Low-Level Selection API

Move `PSD3v2.Selection.Operations` to `PSD3.Internal.Selection.Operations` or simply don't re-export from `PSD3` umbrella module.

Most users should use:
- `PSD3.Viz` for declarative structures
- `PSD3.ForceEngine.Render` for simulation updates

---

## Migration Path

### Phase 1: Add New Names (Non-Breaking)

1. Create `PSD3.Viz` that re-exports `PSD3v2.VizTree.Tree`
2. Create `PSD3.Interpreter` that re-exports interpreters
3. Update docs to use new names

### Phase 2: Deprecate Old Names

1. Add deprecation comments to `PSD3v2.VizTree.Tree`
2. Update all demo-website imports
3. Update all documentation

### Phase 3: Remove Old Names (Breaking)

1. Remove `PSD3v2.VizTree` module
2. Version bump to indicate breaking change

---

## Public API Surface (Post-Cleanup)

### For Users

```purescript
-- Main visualization API
import PSD3.Viz as V           -- Declarative viz specs
import PSD3.Attr as A          -- Attributes (v3 expressions + v2 types)
import PSD3.Interpreter.D3 as D3      -- DOM rendering
import PSD3.Interpreter.Mermaid as M  -- Debugging
import PSD3.Scale              -- Scales

-- For simulations
import PSD3.ForceEngine as FE         -- Simulation engine
import PSD3.ForceEngine.Setup as Setup -- Declarative forces
import PSD3.ForceEngine.Halogen       -- Halogen integration

-- Data types
import PSD3.Data.Node          -- SimulationNode, D3Link
import PSD3.Data.Graph         -- Graph utilities
```

### Hidden/Internal

```purescript
-- Internal (not re-exported from PSD3)
PSD3v2.Selection.Operations    -- Low-level DOM
PSD3v2.Selection.Types         -- Selection state machine
PSD3v3.Interpreter.Eval        -- Expression evaluation
PSD3.Internal.*                -- FFI internals
```

---

## Files to Update

### psd3-selection

| File | Change |
|------|--------|
| `src/PSD3.purs` | Update re-exports to use new names |
| `src/PSD3/Viz.purs` | **NEW** - re-export VizTree.Tree |
| `src/PSD3/Interpreter.purs` | **NEW** - re-export all interpreters |
| `src/PSD3/Attr.purs` | **NEW** - unified attribute exports |

### demo-website

~60 files importing `PSD3v2.VizTree.Tree` → `PSD3.Viz`

### Documentation

- README.md
- DOCUMENTATION.md
- Getting Started examples
- All HowTo guides

---

## Questions to Resolve

1. **Keep "TreeAPI" folder name for examples?**
   - Current: `demo-website/src/Viz/TreeAPI/*.purs`
   - Could rename to `Viz/Examples/` or `Viz/VizSpec/`

2. **Keep v2/v3 modules or delete entirely?**
   - Option A: Re-export only, keep v2/v3 modules as implementation
   - Option B: Move files to new locations

3. **What to do with `PSD3.Data.Tree`?**
   - This is a data structure, not the viz API
   - Name is fine, but may still confuse near `PSD3.Viz`
   - Consider: `PSD3.Data.TreeStructure` or leave as-is
