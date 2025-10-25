# Library Refactoring Plan

**Date:** 2025-10-25
**Status:** Planning
**Goal:** Clean up library code before writing comprehensive reference documentation

## Executive Summary

The library's core design is sound: the Finally Tagless approach, type class hierarchy, and FFI integration work well. However, module organization, naming consistency, and API surface clarity need improvement. These are primarily **refactoring** issues, not design problems.

---

## Issues Identified

### 1. Module Naming Chaos ⚠️ **CRITICAL**

**Problem:** Inconsistent module naming makes the library difficult to navigate and import.

**Examples:**
- `D3.Selection` vs `D3.Selection.Functions` - unclear which to import
- `D3.Node` (module name) vs `D3/Data/Node.purs` (file path)
- `D3.Data.Tree` but `D3.Simulation.Config` - inconsistent nesting levels
- Two top-level prefixes: `D3Tagless.*` vs `D3.*`
- `D3Tagless.Instance.Selection` (interpreter) vs `D3Tagless.Capabilities` (type class)

**Impact:**
- Users can't intuitively find what to import
- No clear mental model of library structure
- Documentation will be confusing if modules don't follow predictable patterns

**Proposed Solution:**
1. Establish clear module hierarchy:
   ```
   PSD3.* - Public API (re-exports)
   D3Tagless.Capabilities.* - Type classes
   D3Tagless.Interpreters.* - Implementations
   D3.* - Core types and utilities
   ```

2. Consistent naming rules:
   - Module name should match primary export
   - Nested modules under clear categories
   - Implementation details in `.Internal` when needed

**Files Affected:** ~28 module files

---

### 2. Unclear Public API Surface ⚠️ **HIGH PRIORITY**

**Problem:** No clear entry point or "blessed" imports for users.

**Current State:**
- Users must discover which of 28 modules to import
- No way to distinguish public API from implementation details
- Examples show ad-hoc imports from various modules

**Impact:**
- Steep learning curve
- Examples don't teach good import patterns
- Breaking changes are unpredictable (no clear API contract)

**Proposed Solution:**
Create a single public API module that re-exports everything users need:

```purescript
module PSD3
  ( -- Core type classes
    class SelectionM(..)
  , class SimulationM(..)
  , class SankeyM(..)
    -- Main interpreter
  , D3M
  , eval_D3M
  , runD3M
  , exec_D3M
    -- Attributes
  , module Attributes
    -- etc
  ) where

import D3Tagless.Capabilities as Capabilities
import D3.Attributes.Sugar as Attributes
-- ... selective re-exports
```

**Benefits:**
- Single import for most use cases: `import PSD3`
- Clear API contract
- Implementation can change without breaking user code
- Documentation has a clear starting point

**Files to Create:**
- `src/lib/PSD3.purs` - main entry point
- `src/lib/PSD3/Attributes.purs` - attribute re-exports
- `src/lib/PSD3/Simulation.purs` - simulation re-exports
- etc.

---

### 3. Minimal Documentation **HIGH PRIORITY**

**Problem:** Library code has almost no documentation comments.

**Current State:**
- Type signatures with no explanation
- TODO comments scattered throughout
- No module-level documentation
- No usage examples in comments

**Examples of Missing Docs:**
```purescript
-- What does this do? When should I use it?
openSelection :: selection -> Selector selection -> m selection

-- What's the difference between these?
simpleJoin :: ...
updateJoin :: ...

-- What are the semantics of this?
actualizeForces :: Set Label -> m Unit
```

**Proposed Solution:**
Add comprehensive doc comments:
1. Module-level overviews
2. Type class documentation with usage examples
3. Function documentation explaining when/why/how
4. Link to relevant D3.js docs where applicable

**Example:**
```purescript
-- | SelectionM provides the core D3 selection operations.
-- |
-- | This type class abstracts over D3's selection API, allowing
-- | multiple interpreters (D3, String, MetaTree).
-- |
-- | ## Basic Usage
-- |
-- | ```purescript
-- | myVisualization = do
-- |   root <- attach "#chart"
-- |   svg <- appendTo root Svg [width 800.0, height 600.0]
-- |   circles <- appendTo svg Circle [...]
-- |   pure circles
-- | ```
class (Monad m) <= SelectionM selection m where
  -- | Attach to an existing DOM element using a CSS selector.
  -- | This is typically the first operation in any visualization.
  attach :: Selector selection -> m selection

  -- | Create and append a new element to the selection.
  -- | Returns the newly created selection.
  appendTo :: selection -> Element -> Array SelectionAttribute -> m selection
```

---

### 4. Unsafe Coercions Everywhere **MEDIUM PRIORITY**

**Problem:** Users must write `unsafeCoerce` in every example.

**Current State:**
```purescript
-- Users write this in EVERY example:
datumIsChar :: Datum_ -> Char
datumIsChar = unsafeCoerce

indexIsNumber :: Index_ -> Number
indexIsNumber = unsafeCoerce

keyFunction :: Datum_ -> Index_
keyFunction = unsafeCoerce
```

**Why This Happens:**
The JS/PureScript boundary - D3 uses untyped data, PureScript wants types.

**Impact:**
- Type safety breaks down at the boundary
- Boilerplate in every example
- Users might not understand why this is necessary
- Scary for newcomers

**Proposed Solutions (Pick One or Combine):**

**Option A: Embrace It**
- Provide blessed helper functions in library:
  ```purescript
  module PSD3.Unsafe where

  unsafeDatum :: forall a. Datum_ -> a
  unsafeIndex :: forall a. Index_ -> a
  unsafeKey :: forall a. a -> Index_
  ```
- Document clearly why this is necessary
- Show safe patterns in examples

**Option B: Type-Safe Builders**
- Provide newtype wrappers:
  ```purescript
  newtype Datum a = Datum Datum_
  newtype Index = Index Index_

  -- Safe extraction
  fromDatum :: forall a. Datum a -> a
  toIndex :: forall a. Show a => a -> Index
  ```

**Option C: Do Both**
- Default to type-safe wrappers
- Provide unsafe escape hatches for advanced users
- Mark unsafe module clearly

**Decision Needed:** Which approach fits the library philosophy?

---

### 5. Inconsistent Naming Conventions **MEDIUM PRIORITY**

**Problem:** No consistent naming pattern across the codebase.

**Examples:**
- FFI functions: `d3SelectAllInDOM_`, `d3Append_`, `selectionAttach`
- Types: `D3Selection_` vs `D3Selection` (which is which?)
- Functions: mix of camelCase starting points
- Underscores used inconsistently

**Current Patterns:**
1. Some FFI ends with `_`: `d3SelectAllInDOM_`
2. Some FFI has `d3` prefix: `d3Append_`
3. Some has neither: `autoBox_`
4. Some wrapper functions match: `selectionAttach`
5. Types from JS have `_`: `D3Selection_`

**Proposed Convention:**

**FFI Functions (from .js files):**
- Always end with `_`
- Use descriptive names
- Don't prefix with `d3` unless disambiguating
- Example: `selectAllInDOM_`, `append_`, `setAttr_`

**FFI Types (opaque JS types):**
- Always end with `_`
- Use full descriptive names
- Example: `D3Selection_`, `D3Simulation_`, `Datum_`

**Public API Functions:**
- No underscores
- Clear, descriptive names
- Example: `attach`, `appendTo`, `setAttributes`

**Public API Types:**
- No underscores
- Newtype wrappers around `_` types when needed
- Example: `Selection`, `Simulation`

**Files Affected:** All modules, ~2000 lines

---

### 6. FFI Organization **LOW PRIORITY**

**Problem:** Large `D3.FFI` module appears to be a dumping ground.

**Current State:**
- `D3/FFI/FFI.purs` - 300+ lines
- Mix of selection, scale, axis, zoom, etc. operations
- Unclear organization
- Some FFI in dedicated modules, some in FFI.purs

**Impact:**
- Hard to find functions
- Difficult to maintain
- Not clear which operations are implemented

**Proposed Solution:**
1. Split FFI into logical modules:
   ```
   D3/FFI/Selection.purs + .js
   D3/FFI/Scales.purs + .js
   D3/FFI/Axes.purs + .js
   D3/FFI/Simulation.purs + .js
   ```

2. Keep `D3.FFI` as re-export module for internal use

**Decision:** Lower priority - doesn't affect user-facing API.

---

### 7. Mixed Concerns in Capabilities **MEDIUM PRIORITY**

**Problem:** Some type class methods feel too specific.

**Examples:**
```purescript
-- Very specific signature for one use case:
mergeNewDataWithSim :: forall d r id. (Eq id) =>
  selection ->
  (Datum_ -> Index_) ->
  selection ->
  (Datum_ -> Index_) ->
  RawData d r id ->
  m { links :: Array ..., nodes :: Array ... }

-- Comment admits it's problematic:
-- "in particular, it could be good to have Simulation do it's join
--  function by putting nodes / links into both DOM and Simulation
--  for example (and current implementation is gross and wrong)"
```

**Impact:**
- Type class feels less general
- Hard to document what these operations mean semantically
- May be solving implementation problems, not API problems

**Proposed Solution:**
1. Review SimulationM capabilities
2. Separate "core" operations from "convenience" operations
3. Consider helper functions instead of type class methods
4. Clean up TODOs or mark as experimental

**Decision Needed:** How much do we want to change SimulationM?

---

### 8. Incomplete/Experimental Features **LOW PRIORITY**

**Problem:** TODO comments and incomplete implementations.

**Examples:**
```purescript
-- TODO see whether it can be useful to extend the interpreter here

-- TODO need to provide the simpler, non-simulation version here

-- the following versions are less type-safe but they are necessary
-- for updating simulations

selectionOn selection (Drag drag) = do
-- TODO need to provide the simpler, non-simulation version here
  pure unit  -- Not implemented!
```

**Impact:**
- Unclear what's production-ready
- Can't document incomplete features
- Users might rely on non-working code

**Proposed Solution:**
1. Mark experimental features clearly
2. Remove TODOs or convert to issues
3. Document known limitations
4. Decide: implement, defer, or remove?

---

## Refactoring Phases

### Phase 1: Module Reorganization (Feature Branch: `refactor/module-structure`)
- [ ] Design final module hierarchy
- [ ] Create migration plan
- [ ] Move files to new structure
- [ ] Update all imports in library
- [ ] Update all imports in examples/website
- [ ] Create `PSD3` public API module

**Estimated Impact:** High effort, high value

### Phase 2: Naming Consistency (Feature Branch: `refactor/naming-conventions`)
- [ ] Document naming conventions
- [ ] Rename FFI functions consistently
- [ ] Rename types consistently
- [ ] Update all references
- [ ] Update examples

**Estimated Impact:** Medium effort, medium value

### Phase 3: Documentation (Feature Branch: `refactor/add-documentation`)
- [ ] Add module-level docs
- [ ] Document all type classes
- [ ] Document all public functions
- [ ] Add usage examples in comments
- [ ] Link to D3.js documentation

**Estimated Impact:** High effort, high value

### Phase 4: Unsafe Coercion Strategy (Feature Branch: `refactor/datum-safety`)
- [ ] Decide on approach (A, B, or C above)
- [ ] Implement chosen solution
- [ ] Update all examples
- [ ] Document the pattern

**Estimated Impact:** Medium effort, high value

### Phase 5: SimulationM Review (Feature Branch: `refactor/simulation-cleanup`)
- [ ] Review all SimulationM operations
- [ ] Resolve TODOs
- [ ] Simplify or mark experimental
- [ ] Document limitations

**Estimated Impact:** Low effort (if just documenting), high value

### Phase 6: FFI Organization (Feature Branch: `refactor/ffi-split`)
- [ ] Split D3.FFI into modules
- [ ] Update imports
- [ ] Test thoroughly

**Estimated Impact:** Medium effort, low value (internal only)

---

## What We're NOT Changing

### Core Design ✅
- Finally Tagless approach - works well
- Type class hierarchy - sound design
- Multiple interpreter pattern - demonstrates flexibility
- Monad transformer stack - appropriate

### API Semantics ✅
- Selection operations - match D3 well
- Attribute system - elegant
- Join operations - correct
- Transition handling - works

### FFI Implementation ✅
- JavaScript FFI code - mostly fine
- D3 integration - works correctly
- Effect handling - appropriate

---

## Success Criteria

Before we write reference documentation, we should have:

1. **Clear Import Story**
   - Single `import PSD3` works for 90% of use cases
   - Users know where to import specialized features

2. **Consistent Naming**
   - Predictable patterns throughout
   - Easy to guess function/module names

3. **Documented Code**
   - Every public function has doc comment
   - Type classes explain usage patterns
   - Examples in comments

4. **Safe Patterns**
   - Clear guidance on `unsafeCoerce`
   - Blessed helper functions or safe wrappers
   - Examples show best practices

5. **Production Ready**
   - No TODO comments in public API
   - Experimental features marked clearly
   - Known limitations documented

---

## Notes for Documentation Phase

Once refactoring is complete, the reference documentation should cover:

1. **Getting Started Guide** (Tutorial category)
   - Installation
   - First visualization
   - Understanding the monad

2. **API Reference** (Reference category)
   - Type class documentation
   - Function reference
   - Type reference
   - Module guide

3. **Patterns & Practices** (Understanding category)
   - When to use which type class
   - Working with data at the boundary
   - Interpreter selection
   - Performance considerations

4. **Cookbook** (How-to category)
   - Common visualization patterns
   - Data binding patterns
   - Animation patterns
   - Interaction patterns

---

## Questions to Resolve

1. **Module naming:** What should the top-level namespace be? `PSD3`? `D3.PS`? `PureScriptD3`?

2. **Unsafe datum handling:** Options A, B, or C? Or something else?

3. **SimulationM:** Fix the TODOs or mark as experimental?

4. **FFI split:** Worth the effort? Or keep as-is since it's internal?

5. **Breaking changes:** Are we willing to break existing code for better API?

6. **Backward compatibility:** Do we need to maintain old module names temporarily?

---

## Timeline Estimate

- Phase 1 (Module reorg): 2-3 sessions
- Phase 2 (Naming): 1-2 sessions
- Phase 3 (Documentation): 3-4 sessions
- Phase 4 (Datum safety): 1-2 sessions
- Phase 5 (Simulation review): 1 session
- Phase 6 (FFI split): 1-2 sessions (if doing it)

**Total: ~10-14 sessions before documentation**

---

## Approval Checklist

Before starting each phase:
- [ ] Agree on approach
- [ ] Create feature branch
- [ ] Document any deviations from this plan
- [ ] Test thoroughly
- [ ] Merge to main

---

*This document is a living plan and will be updated as we make decisions and progress through the refactoring.*
