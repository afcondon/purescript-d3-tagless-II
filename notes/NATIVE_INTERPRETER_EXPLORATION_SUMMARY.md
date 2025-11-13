# Native DOM Interpreter - Exploration Summary

**Exploration Branch:** `feature/native-dom-interpreter`
**Branch Hash:** `6e15574`
**Status:** Experimental - Proof of concept successful, architectural insights documented

## Quick Summary

Successfully demonstrated that a **pure PureScript interpreter** for PSD3 is feasible using native browser DOM APIs (no D3.js dependency). More importantly, this exploration revealed fundamental architectural insights about Selection type design that apply to the entire PSD3 library.

## Key Finding: Selection is a Leaky Abstraction

The most important discovery: **The current `Selection d` type conflates different states** that should be distinguished at the type level.

### The Problem

```purescript
-- All have type "Selection d" but represent different states:
svg :: Selection Unit          -- No data
circles :: Selection Int       -- Data-bound
enterSel :: Selection Char     -- Enter (has data + needs parents)
updateSel :: Selection Char    -- Update (has data + has elements)
```

This leads to:
- Runtime checks instead of compile-time guarantees
- Functions needing parameters they shouldn't (e.g., `appendTo` needing `keyFn` even for non-data cases)
- Implicit state management

### Proposed Solution

Use ADT to make states explicit:

```purescript
data NativeSelection d
  = EmptySelection Document (Array Element) Int
  | DataSelection Document (Array Element) (Array d) KeyFn Int
  | EnterSelection Document (Array Element) (Array d) KeyFn Int
```

**Benefits:**
- Impossible states become unrepresentable
- Pattern matching makes intent explicit
- KeyFn stored with data-bound selections only
- Compiler enforces correct usage

## What Was Implemented

### Core Components

1. **NativeDOMM Interpreter** (`src/lib/PSD3/Interpreter/NativeDOM.purs`)
   - Uses native browser APIs instead of D3.js
   - Working: element creation, data binding, joins, attributes
   - Missing: transitions, events (straightforward to add)

2. **Pure Data Join Algorithm** (`src/lib/PSD3/Internal/NativeDOM/DataJoin.purs`)
   - Implements D3's enter/update/exit pattern in pure PureScript
   - O(n + m) time complexity
   - Type-safe with `Ord` and `Show` constraints on keys

3. **Element Identity Management** (`src/lib/PSD3/Internal/ElementIdentity.purs`)
   - Unique ElementID for each created element
   - Stored in `data-element-id` attribute
   - Prepares for unified D3M/NativeDOMM architecture

### Working Examples

- **ThreeLittleCirclesNative** - ✅ Fully working
- **GUP Native** - 🔨 Needs testing with new architecture

## Architectural Recommendations

### For Selection Type Design

Consider encoding selection state in types:

```purescript
-- Option 1: ADT (simpler)
data Selection d = Empty ... | Data ... | Enter ...

-- Option 2: Phantom types (more powerful)
data SelectionState = Empty | HasData | Enter | Update | Exit
newtype Selection (state :: SelectionState) d = ...
```

### For Join Operations

Current (two separate operations):
```purescript
simpleJoin :: ... -> m selection
updateJoin :: ... -> m { enter, update, exit }
```

Future (single unified operation):
```purescript
join :: ... -> m { enter :: Selection Enter d
                  , update :: Selection Update d
                  , exit :: Selection Exit d }
```

### For Key Management

Instead of requiring `keyFn` on `appendTo`, store it with data-bound selections:

```purescript
-- Enter selection carries its key function
EnterSelection { data :: Array d, keyFn :: d -> String, ... }

-- appendTo no longer needs keyFn parameter!
appendTo :: Selection Enter d -> Element -> ... -> Selection Data d
```

## Lessons for Main PSD3 Architecture

1. **Type-driven design reveals abstraction leaks** - Working in pure PureScript exposes issues hidden by JavaScript wrapper

2. **Finally Tagless enables gradual improvement** - Can experiment with NativeSelection ADT while keeping D3Selection_ opaque

3. **Keys should be `Ord + Show`** - Stricter than D3 but safer and more compositional

4. **FFI should be minimal** - Most DOM operations available in purescript-web-dom

## Full Documentation

See `NATIVE_DOM_INTERPRETER_EXPLORATION.md` for complete details including:
- Full implementation notes
- Data join semantics reference
- Comparison with D3.js approach
- Step-by-step refactoring recommendations
- Philosophical reflections on type-driven API design

## Next Steps (If Pursuing Further)

1. Test GUP Native with key function architecture
2. Implement ADT-based NativeSelection
3. Add missing features (transitions, events)
4. Consider applying insights to main PSD3 Selection type
5. Explore phantom type states for compile-time correctness

---

**Note:** This branch preserves valuable architectural insights even if the implementation doesn't merge. The learnings about Selection type design apply to the entire PSD3 library.
