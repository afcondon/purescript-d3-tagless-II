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

1. ✅ Test GUP Native with key function architecture
2. ✅ Implement ADT-based NativeSelection
3. ✅ Add missing features (transitions, events)
4. ✅ Consider applying insights to main PSD3 Selection type
5. ✅ Explore phantom type states for compile-time correctness

---

## Update: PSD3v2 Implementation (2025-11-14)

**Status:** ✅ **Recommendations Successfully Implemented in PSD3v2**

The architectural insights from this exploration were validated and implemented in the PSD3v2 library:

### 1. Phantom Type States ✅
Implemented exactly as recommended:
```purescript
-- PSD3v2 Selection with phantom type states
newtype Selection (state :: Type) (parent :: Type) (datum :: Type)
  = Selection (SelectionImpl parent datum)

data SEmpty    -- No data bound
data SBound    -- Data bound to elements
data SPending  -- Enter selection
data SExiting  -- Exit selection
```

### 2. Type-Safe Join Results ✅
```purescript
data JoinResult sel parent datum = JoinResult
  { enter  :: sel SPending parent datum
  , update :: sel SBound Element datum
  , exit   :: sel SExiting Element datum
  }
```

### 3. Index Preservation for Positioning ✅
**Critical Discovery:** When implementing GUP, we found that selections must preserve **logical indices** in sorted data, not just array positions:

```purescript
| BoundSelection
    { elements :: Array Element
    , data :: Array datum
    , indices :: Maybe (Array Int)  -- Just for join results
    , document :: Document
    }
| PendingSelection
    { parentElements :: Array Element
    , pendingData :: Array datum
    , indices :: Maybe (Array Int)  -- Just for join results
    , document :: Document
    }
```

**Why This Matters:** When data is sorted for display (e.g., alphabetically sorted letters in GUP), the position in the enter/update arrays (0,1,2,3...) differs from the position in the final sorted data. IndexedAttr functions like `x (\_ i -> 50.0 + toNumber i * 48.0)` must use the logical position, not the array position, to avoid overlapping elements.

**Solution:** The join algorithm tracks `newIndex` for both enter and update data:
```purescript
type EnterBinding datum =
  { datum :: datum
  , newIndex :: Int  -- Position in the new data array
  }

type UpdateBinding datum =
  { element :: Element
  , oldDatum :: datum
  , newDatum :: datum
  , newIndex :: Int  -- Position in the new data array
  }
```

### 4. Working Examples ✅
- Three Little Circles (static)
- Three Circles with Transitions (animated)
- GUP (General Update Pattern with enter/update/exit)

### 5. Pure PureScript Join Algorithm ✅
Implemented in `src/lib/PSD3v2/Selection/Join.purs`:
- O(n × m) worst case (good enough in practice)
- Handles duplicate datums correctly via order-preserving array matching
- Identity via `Eq` constraint (no separate key functions needed for simple cases)

### Documentation
See `PURESCRIPT_SELECTION_2_DESIGN.md` for complete design and implementation notes.

---

**Note:** This exploration's architectural insights were proven correct and are now the foundation of PSD3v2. The phantom type approach successfully eliminates entire classes of runtime errors while maintaining ergonomic APIs.
