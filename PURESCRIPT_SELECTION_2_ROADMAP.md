# PureScript Selection 2.0 - Implementation Roadmap

**Branch:** `purescript-selection-2`
**Status:** Phase 1 complete, ready for Phase 2
**Target:** Proof of concept with Three Little Circles

## Architecture Overview

PSD3v2 is the **implementation layer** for interpreters, not a replacement for tagless final:

```
User Viz Code (uses type class constraints)
    ↓
SelectionM, HierarchicalM, etc. (capabilities)
    ↓
Interpreter Instances (D3v2, String, Meta, Music)
    ↓
PSD3v2.Selection.Operations (renderData, joinData, etc.)
    ↓
DOM / String / Meta structure
```

**Key insight:** We're building the primitives that the D3v2 interpreter will delegate to. Other interpreters (String, Meta) will be reworked to align with these patterns.

## Phase 1: Core Types ✅ COMPLETE

### 1.1 Selection Types with Phantom States ✅
**File:** `src/lib/PSD3v2/Selection/Types.purs`

**Status:** Implemented and building cleanly

```purescript
-- Selection state phantom types (uninhabited types)
data SEmpty    -- No data bound
data SBound    -- Data bound to elements
data SPending  -- Data waiting for elements (enter)
data SExiting  -- Elements to be removed (exit)

-- Main selection type
newtype Selection (state :: Type) (parent :: Type) (datum :: Type)
  = Selection (SelectionImpl parent datum)

-- Internal implementation (not exported)
data SelectionImpl parent datum
  = EmptySelection
      { parentElements :: Array Element
      , document :: Document
      }
  | BoundSelection
      { elements :: Array Element
      , data :: Array datum
      , document :: Document
      }
  | PendingSelection
      { parentElements :: Array Element
      , pendingData :: Array datum
      , document :: Document
      }
  | ExitingSelection
      { elements :: Array Element
      , document :: Document
      }

-- Join result type
data JoinResult parent datum = JoinResult
  { enter  :: Selection Pending parent datum
  , update :: Selection Bound Element datum
  , exit   :: Selection Exiting Element datum
  }
```

### 1.2 Pure Join Algorithm ✅
**File:** `src/lib/PSD3v2/Selection/Join.purs`

**Status:** Implemented with O(n+m) complexity using Map

```purescript
-- Core join algorithm (pure!)
computeJoin
  :: forall datum
   . Ord datum
  => Array datum                                      -- New data
  -> Array { element :: Element, datum :: datum }     -- Old bindings
  -> { enter :: Array datum
     , update :: Array { element :: Element, oldDatum :: datum, newDatum :: datum }
     , exit :: Array { element :: Element, datum :: datum }
     }

-- Implementation uses Map for O(n+m) performance
-- Keys derived from datum Ord instance
```

**Tests:** Write QuickCheck properties FIRST:
- Property: enter ++ update ++ exit covers all data and elements
- Property: No element appears in multiple sets
- Property: Keys match correctly
- Property: Order preservation

### 1.3 Attribute Types ✅
**File:** `src/lib/PSD3v2/Attribute/Types.purs`

**Status:** Implemented with 20+ smart constructors

```purescript
-- Attribute with datum phantom type
data Attribute (datum :: Type) where
  StaticAttr :: AttributeName -> AttributeValue -> Attribute datum
  DataAttr :: AttributeName -> (datum -> AttributeValue) -> Attribute datum
  IndexedAttr :: AttributeName -> (datum -> Int -> AttributeValue) -> Attribute datum

-- Attribute value ADT
data AttributeValue
  = StringValue String
  | NumberValue Number
  | BooleanValue Boolean

-- Smart constructors
fill :: forall datum. String -> Attribute datum
cx :: forall datum. (datum -> Number) -> Attribute datum
cy :: forall datum. (datum -> Number) -> Attribute datum
-- ... etc
```

## Phase 2: DOM Operations (Minimal FFI)

### 2.1 Selection Operations
**File:** `src/lib/PSD3v2/Selection/Operations.purs`

```purescript
-- Core operations (type-safe!)
select
  :: forall m
   . MonadEffect m
  => String
  -> m (Selection Empty Element Unit)

selectAll
  :: forall parent datum m
   . MonadEffect m
  => String
  -> Selection state parent datum_
  -> m (Selection Empty Element datum)

-- Low-level join (power users)
joinData
  :: forall f parent datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  -> f datum
  -> String
  -> Selection Empty parent datum_
  -> m (JoinResult parent datum)

-- High-level render (most users)
renderData
  :: forall f parent datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  => ElementType
  -> f datum
  -> String
  -> Selection Empty parent datum_
  -> Maybe (datum -> Array (Attribute datum))  -- Enter
  -> Maybe (datum -> Array (Attribute datum))  -- Update
  -> Maybe (datum -> Array (Attribute datum))  -- Exit
  -> m (Selection Bound Element datum)

-- Operations on typed selections
append
  :: forall parent datum m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datum)
  -> Selection Pending parent datum
  -> m (Selection Bound Element datum)

setAttrs
  :: forall datum m
   . MonadEffect m
  => Array (Attribute datum)
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)

remove
  :: forall datum m
   . MonadEffect m
  => Selection Exiting Element datum
  -> m Unit

merge
  :: forall datum m
   . MonadEffect m
  => Selection Bound Element datum
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)
```

### 2.2 FFI (Minimal)
**File:** `src/lib/PSD3v2/Selection/Operations.js`

Use purescript-web-dom for most operations. Only FFI for:
- Querying DOM: `querySelectorAll`
- Data binding: `element.__data__`
- Attribute setting: `element.setAttribute`

## Phase 3: Three Little Circles Proof of Concept

### 3.1 Port Original Example
**File:** `src/website/Viz/ThreeLittleCirclesV2.purs`

```purescript
draw :: forall m. MonadEffect m => Array Char -> m Unit
draw letters = do
  -- Select parent
  svg <- select "svg"

  -- Render data (high-level API)
  circles <- renderData
    Circle
    letters
    "circle"
    svg
    (Just $ const
      [ cx (\d i -> 50.0 + fromInt i * 100.0)
      , cy 60.0
      , radius 40.0
      , fill "green"
      ])
    (Just $ const
      [ fill "orange"
      ])
    (Just $ const
      [ fill "red"
      ])

  pure unit
```

### 3.2 Compare with Current Implementation
- Type safety: Can we express illegal states?
- Ergonomics: Is it more concise?
- Performance: Benchmark vs D3 version
- Bundle size: Is it smaller due to tree-shaking?

## Phase 4: Transitions (If time permits)

### 4.1 Transition Attributes
```purescript
transitionTo
  :: forall datum
   . AttributeName
  -> (datum -> Number)
  -> TransitionConfig
  -> Attribute datum
```

### 4.2 Port Three Little Circles Transition

## Key Design Decisions Locked In

1. ✅ **Phantom types for states** - compile-time safety
2. ✅ **`renderData` for common case** - no sequencing errors
3. ✅ **`joinData` for power users** - fine-grained control
4. ✅ **Ord datum for identity** - Eq instance controls semantics
5. ✅ **Foldable input** - works with Array, List, Set, etc.
6. ✅ **Events via purescript-web-dom** - reuse ecosystem types
7. ✅ **Merge follows D3** - document order concatenation
8. ✅ **FRP optional** - imperative core, reactive wrapper possible

## Success Criteria

- [ ] All code compiles with no `unsafeCoerce`
- [ ] Three Little Circles works correctly
- [ ] Type system prevents misuse (test with invalid code)
- [ ] Performance within 20% of D3 version
- [ ] Code is more concise than current PSD3
- [ ] QuickCheck properties pass for join algorithm

## Implementation Notes

### Start with Types, Not FFI
Write types and pure functions first. Add FFI only when needed for DOM interaction.

### Test-Driven for Join Algorithm
Write QuickCheck properties before implementing `computeJoin`. This is pure code - test thoroughly!

### Keep It Minimal
Don't implement everything. Prove the concept with:
- Basic shapes (Circle, Rect, Path)
- Basic attributes (cx, cy, radius, fill, stroke)
- Basic join (enter/update/exit)
- Three Little Circles example

### Document Everything
Each type should have Pursuit-ready documentation explaining:
- What it represents
- Why it has phantom types
- How users should interact with it

## References

- **Design Doc:** `PURESCRIPT_SELECTION_2_DESIGN.md`
- **D3 Join Semantics:** `D3_DATA_JOIN_SEMANTICS.md`
- **Native DOM Exploration:** `NATIVE_DOM_INTERPRETER_EXPLORATION.md` (in git history)
- **Current Selection:** `src/lib/PSD3/Capabilities/Selection.purs`
- **Current D3 Interpreter:** `src/lib/PSD3/Interpreter/D3.purs`

## Phase 4: Tagless Final Integration

### 4.1 Revise Capability Type Classes
**File:** `src/lib/PSD3v2/Capabilities/Selection.purs`

Redesign `SelectionM` to align with PSD3v2 patterns:

```purescript
class SelectionM sel m | m -> sel where
  -- High-level user-friendly API
  renderData
    :: forall f parent datum
     . Foldable f
    => Ord datum
    => ElementType
    -> f datum
    -> String
    -> sel SEmpty parent datum
    -> Maybe (datum -> Array (Attribute datum))  -- Enter
    -> Maybe (datum -> Array (Attribute datum))  -- Update
    -> Maybe (datum -> Array (Attribute datum))  -- Exit
    -> m (sel SBound Element datum)

  -- Low-level power user API
  joinData
    :: forall f parent datum
     . Foldable f
    => Ord datum
    -> f datum
    -> String
    -> sel SEmpty parent datum
    -> m (JoinResult parent datum)

  append
    :: forall parent datum
     . ElementType
    -> Array (Attribute datum)
    -> sel SPending parent datum
    -> m (sel SBound Element datum)

  setAttrs
    :: forall datum
     . Array (Attribute datum)
    -> sel SBound Element datum
    -> m (sel SBound Element datum)

  remove
    :: forall datum
     . sel SExiting Element datum
    -> m Unit

  merge
    :: forall datum
     . sel SBound Element datum
    -> sel SBound Element datum
    -> m (sel SBound Element datum)
```

### 4.2 Implement D3v2 Interpreter
**File:** `src/lib/PSD3v2/Interpreter/D3v2.purs`

Create interpreter that delegates to PSD3v2.Selection.Operations:

```purescript
newtype D3v2Selection_ (state :: Type) (parent :: Type) (datum :: Type)
  = D3v2Selection_ (Selection state parent datum)

instance SelectionM D3v2Selection_ Effect where
  renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs = do
    -- Unwrap, delegate to PSD3v2 primitive, re-wrap
    let Selection impl = unwrap emptySelection
    result <- PSD3v2Ops.renderData elemType foldableData selector (Selection impl) enterAttrs updateAttrs exitAttrs
    pure $ D3v2Selection_ result

  -- ... similar for other operations
```

### 4.3 Port String Interpreter
**File:** `src/lib/PSD3v2/Interpreter/String.purs`

Rework to align with new patterns:
- `renderData` generates HTML strings directly from data
- Phantom types ensure type safety even in string generation
- No DOM operations needed

### 4.4 Port Meta Interpreter
**File:** `src/lib/PSD3v2/Interpreter/Meta.purs`

Rework to track operations:
- Record what operations were called
- Track phantom type transitions
- Enable visualization of the data flow

## Phase 5: Migration and Examples

### 5.1 Port Three Little Circles
**File:** `src/website/Viz/ThreeLittleCirclesV2.purs`

Use D3v2 interpreter via type class constraints.

### 5.2 Port GUP Example
**File:** `src/website/Viz/GUPv2.purs`

Test the full enter-update-exit cycle with real interactions.

### 5.3 Create Migration Guide
**File:** `MIGRATION_TO_V2.md`

Document:
- What changed and why
- How to update existing code
- Side-by-side examples (old vs new)
- Benefits of the new approach

## Next Steps After Phase 1-5

1. Get feedback on API ergonomics
2. Port more examples (bar chart, hierarchies)
3. Add transitions (Phase 6)
4. Add event handling (Phase 7)
5. FRP integration for composition
6. Performance optimization
7. Comprehensive documentation
8. Decide on deprecation timeline for PSD3 v1

---

**Remember:** This is a clean-slate design. Don't worry about backward compatibility. Focus on making the API as good as it can be.
