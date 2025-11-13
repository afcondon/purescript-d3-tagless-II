# PureScript Selection 2.0 - Ideal Design Exploration

**Branch:** `purescript-selection-2`
**Date:** 2025-11-13
**Goal:** Design a strongly-typed, no-compromise, fully declarative selection/attribute/transition API that captures D3 semantics in "cannot express an illegal state" PureScript

## Core Philosophical Principles

### 1. Make Illegal States Unrepresentable
- Use ADTs and phantom types to encode selection states
- Compiler should prevent impossible operations (e.g., can't append to exit selection)
- No runtime checks for things the type system can verify

### 2. Explicit Over Implicit
- Key functions are always explicit (no hidden state)
- Parent context is explicit in types
- Data binding is explicit in the type
- No "magic" behavior - everything visible in the types

### 3. Pure PureScript Core, FFI at Boundaries
- Data join algorithm: pure PureScript
- Attribute value computation: pure PureScript
- DOM mutation: minimal FFI (leverage purescript-web-dom)
- Transition timing: pure PureScript (leverage purescript-behaviors/FRP)

### 4. Declarative, Not Imperative
- Describe WHAT the DOM should look like
- Let the system figure out HOW to get there
- Embrace functional reactive programming for animations

## Type-Level Architecture

### Selection States as Phantom Types

```purescript
-- Selection state captured in the type system
data SelectionState
  = Empty        -- No data, no elements bound
  | Bound        -- Data bound to existing elements
  | Pending      -- Data waiting to be materialized (enter)
  | Exiting      -- Elements to be removed

-- Selection parameterized by state and datum type
newtype Selection (state :: SelectionState) (parent :: Type) (datum :: Type)
  = Selection (SelectionImpl parent datum)

-- Internal representation (not exposed)
data SelectionImpl parent datum
  = EmptySelection
      { parentElements :: Array parent
      , document :: Document
      }
  | BoundSelection
      { elements :: Array Element
      , data :: Array datum
      , keys :: Array String
      , document :: Document
      }
  | PendingSelection
      { parentElements :: Array parent
      , pendingData :: Array datum
      , keys :: Array String
      , document :: Document
      }
  | ExitingSelection
      { elements :: Array Element
      , document :: Document
      }
```

**Key Insight:** The phantom type `state` allows us to control which operations are valid at compile time!

### Type-Safe Operations

```purescript
-- Selecting elements (always returns Empty state initially)
selectAll
  :: forall parent datum m
   . MonadEffect m
  => String  -- CSS selector
  -> Selection Empty parent datum
  -> m (Selection Empty Element datum)

-- Binding data creates a JoinResult with three selections
data JoinResult parent datum = JoinResult
  { enter  :: Selection Pending parent datum
  , update :: Selection Bound Element datum
  , exit   :: Selection Exiting Element datum
  }

-- Data binding: Empty -> JoinResult
bindData
  :: forall parent datum key m
   . MonadEffect m
  => Ord key
  => (datum -> key)                      -- Key function (explicit!)
  -> Array datum                         -- New data
  -> Selection Empty parent datum        -- Empty selection
  -> m (JoinResult parent datum)

-- Data binding: Bound -> JoinResult (rebinding)
rebindData
  :: forall parent datum key m
   . MonadEffect m
  => Ord key
  => (datum -> key)                      -- Key function
  -> Array datum                         -- New data
  -> Selection Bound parent datum        -- Existing bound selection
  -> m (JoinResult Element datum)

-- Appending to enter selection creates real elements
append
  :: forall parent datum m
   . MonadEffect m
  => ElementType                         -- What to create (Svg, Circle, etc.)
  -> Array (Attribute datum)             -- Initial attributes
  -> Selection Pending parent datum      -- Enter selection
  -> m (Selection Bound Element datum)   -- New bound selection

-- Setting attributes on bound selections
setAttrs
  :: forall datum m
   . MonadEffect m
  => Array (Attribute datum)
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)

-- Removing exiting elements
remove
  :: forall m
   . MonadEffect m
  => Selection Exiting Element datum
  -> m Unit

-- Transitions (return a new selection that will animate)
transition
  :: forall datum m
   . MonadEffect m
  => TransitionConfig
  -> Array (Attribute datum)             -- Target attributes
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)
```

**Type Safety Examples:**

```purescript
-- ✅ VALID: Can append to Pending (enter) selection
circles <- append Circle [] enterSelection

-- ❌ INVALID: Cannot append to Bound selection
circles <- append Circle [] updateSelection  -- TYPE ERROR!

-- ❌ INVALID: Cannot set attributes on Pending selection
_ <- setAttrs [fill "red"] enterSelection    -- TYPE ERROR!

-- ✅ VALID: Can set attributes on Bound selection
_ <- setAttrs [fill "red"] updateSelection

-- ❌ INVALID: Cannot rebind data on Empty selection
result <- rebindData keyFn newData emptySelection  -- TYPE ERROR!
```

## Attribute System

### Type-Safe Attributes with Phantom Types

```purescript
-- Attribute knows what datum type it expects
data Attribute (datum :: Type) where
  -- Static attributes (no data dependency)
  StaticAttr :: AttributeName -> AttributeValue -> Attribute datum

  -- Data-driven attributes (function of datum)
  DataAttr :: AttributeName -> (datum -> AttributeValue) -> Attribute datum

  -- Index-driven attributes (function of datum and index)
  IndexedAttr :: AttributeName -> (datum -> Int -> AttributeValue) -> Attribute datum

  -- Transition attributes (from current to target value)
  TransitionAttr :: AttributeName -> (datum -> AttributeValue) -> Duration -> Easing -> Attribute datum

-- Smart constructors
fill :: forall datum. String -> Attribute datum
fill color = StaticAttr "fill" (StringValue color)

cx :: forall datum. (datum -> Number) -> Attribute datum
cx f = DataAttr "cx" (NumberValue <<< f)

d :: forall datum. (datum -> String) -> Attribute datum
d f = DataAttr "d" (StringValue <<< f)

-- Leverage purescript-css for type safety
fillCss :: forall datum. CSS.Color -> Attribute datum
fillCss color = StaticAttr "fill" (StringValue $ CSS.toString color)

-- Leverage purescript-halogen-html for element types
type ElementType = H.ElemName

-- Use TagName for type-safe element selection
type Selector = String  -- Could be refined with CSS selector types
```

### Polymorphic Attributes

```purescript
-- Attributes work for any datum type when static
staticAttrs :: forall datum. Array (Attribute datum)
staticAttrs =
  [ stroke "#555"
  , strokeWidth 1.5
  , fill "none"
  ]

-- Data-dependent attributes require specific datum type
dataAttrs :: Array (Attribute { x :: Number, y :: Number, r :: Number })
dataAttrs =
  [ cx _.x
  , cy _.y
  , radius _.r
  ]

-- Can mix static and data-driven
allAttrs :: Array (Attribute { x :: Number, y :: Number })
allAttrs = staticAttrs <> [cx _.x, cy _.y]
```

## Data Join Algorithm

### Pure PureScript Implementation

```purescript
-- Join algorithm with explicit key function
computeJoin
  :: forall datum key
   . Ord key
  => Show key
  => (datum -> key)                -- Key function
  -> Array datum                   -- New data
  -> Array { element :: Element, datum :: datum, key :: String }  -- Old bindings
  -> { enter :: Array datum
     , update :: Array { element :: Element, oldDatum :: datum, newDatum :: datum }
     , exit :: Array { element :: Element, datum :: datum }
     }
computeJoin keyFn newData oldBindings =
  let
    -- Build map of old keys to bindings
    oldMap :: Map String { element :: Element, datum :: datum }
    oldMap = Map.fromFoldable
      $ oldBindings <#> \b -> Tuple b.key { element: b.element, datum: b.datum }

    -- Build map of new keys to data
    newMap :: Map String datum
    newMap = Map.fromFoldable
      $ newData <#> \d -> Tuple (show $ keyFn d) d

    -- Partition new data: update (has matching old element) vs enter (no match)
    { yes: updateData, no: enterData } = Array.partition
      (\d -> Map.member (show $ keyFn d) oldMap)
      newData

    -- Build update tuples with old and new data
    updates = updateData # Array.mapMaybe \newDatum ->
      let key = show $ keyFn newDatum
      in Map.lookup key oldMap <#> \old ->
        { element: old.element, oldDatum: old.datum, newDatum }

    -- Find exit elements (old elements with no matching new data)
    exits = oldBindings # Array.filter \b ->
      not $ Map.member b.key newMap
      # map \b -> { element: b.element, datum: b.datum }
  in
    { enter: enterData
    , update: updates
    , exit: exits
    }
```

**Key Properties:**
- ✅ Pure function - no side effects
- ✅ O(n + m) time complexity (where n = new data, m = old elements)
- ✅ Type-safe key function
- ✅ Preserves element references for smooth updates
- ✅ Testable without DOM

## Transition System

### FRP-Based Animations

```purescript
-- Transition configuration
type TransitionConfig =
  { duration :: Duration          -- How long (Milliseconds 500)
  , delay :: Duration             -- Start delay (Milliseconds 0)
  , easing :: Easing              -- Easing function
  , name :: Maybe String          -- Named transition for cancellation
  }

-- Easing functions (pure!)
data Easing
  = Linear
  | EaseIn
  | EaseOut
  | EaseInOut
  | Elastic
  | Bounce
  | Custom (Number -> Number)  -- t in [0,1] -> progress in [0,1]

-- Transition attribute
transitionTo
  :: forall datum
   . AttributeName
  -> (datum -> Number)           -- Target value accessor
  -> TransitionConfig
  -> Attribute datum

-- Transition multiple attributes together
transitionGroup
  :: forall datum m
   . MonadEffect m
  => TransitionConfig
  -> Array (Attribute datum)
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)
```

### Declarative Animation Model

**Key Insight:** Use FRP (behaviors/signals) to model time-varying values

```purescript
-- Attribute value over time
type AnimatedAttribute datum = Behavior (Attribute datum)

-- Define animation declaratively
animateCircles
  :: Array { x :: Number, y :: Number, r :: Number }
  -> AnimatedAttribute { x :: Number, y :: Number, r :: Number }
animateCircles data = do
  t <- time  -- Behavior Number (seconds elapsed)
  let phase = t * 2.0 * pi
  pure $
    [ cx (\d -> d.x + 10.0 * sin phase)
    , cy (\d -> d.y + 10.0 * cos phase)
    , radius (const $ 5.0 + 2.0 * sin (phase * 2.0))
    ]
```

## Practical API Design

### Typical Usage Pattern

```purescript
-- Create visualization
createBarChart :: forall m. MonadEffect m => Array Number -> m Unit
createBarChart dataset = do
  -- 1. Attach to container
  svg <- select "#chart"

  -- 2. Open selection (returns Empty selection)
  bars <- selectAll "rect" svg

  -- 3. Bind data (returns JoinResult)
  result <- bindData identity dataset bars

  -- 4. Handle enter: create new elements
  newBars <- append Rect [] result.enter
  entered <- setAttrs
    [ x (\d i -> Number.fromInt i * 25.0)
    , y (\d -> 200.0 - d)
    , width 20.0
    , height (\d -> d)
    , fill "steelblue"
    ] newBars

  -- 5. Handle update: update existing
  updated <- setAttrs
    [ y (\d -> 200.0 - d)
    , height (\d -> d)
    ] result.update

  -- 6. Handle exit: remove old
  remove result.exit

  pure unit

-- Update with transitions
updateBarChart :: forall m. MonadEffect m => Array Number -> m Unit
updateBarChart dataset = do
  svg <- select "#chart"
  bars <- selectAll "rect" svg

  -- Rebind creates new JoinResult
  result <- rebindData identity dataset bars

  -- Enter (same as before)
  newBars <- append Rect [] result.enter
  _ <- setAttrs [x (\d i -> ...), ...] newBars

  -- Update with transition
  _ <- transitionGroup
    { duration: Milliseconds 750
    , delay: Milliseconds 0
    , easing: EaseInOut
    , name: Nothing
    }
    [ y (\d -> 200.0 - d)
    , height (\d -> d)
    , fill "orange"
    ]
    result.update

  -- Exit with transition then remove
  _ <- transitionGroup
    { duration: Milliseconds 375
    , delay: Milliseconds 0
    , easing: EaseOut
    , name: Nothing
    }
    [ opacity 0.0
    , y 200.0
    , height 0.0
    ]
    result.exit

  remove result.exit

  pure unit
```

### General Update Pattern (Idiomatic)

```purescript
-- Encapsulate the enter-update-exit pattern
generalUpdate
  :: forall parent datum key m
   . MonadEffect m
  => Ord key
  => (datum -> key)                                   -- Key function
  -> ElementType                                      -- Element to create
  -> Array (Attribute datum)                          -- Enter attributes
  -> Array (Attribute datum)                          -- Update attributes
  -> (Selection Pending parent datum -> m (Selection Bound Element datum))  -- Enter behavior
  -> (Selection Bound Element datum -> m (Selection Bound Element datum))   -- Update behavior
  -> (Selection Exiting Element datum -> m Unit)                            -- Exit behavior
  -> Array datum                                                             -- Data
  -> Selection Empty parent datum                                            -- Parent selection
  -> m (Selection Bound Element datum)                                       -- Result
generalUpdate keyFn elemType enterAttrs updateAttrs enterFn updateFn exitFn newData selection = do
  result <- bindData keyFn newData selection

  -- Handle enter
  entered <- enterFn result.enter

  -- Handle update
  updated <- updateFn result.update

  -- Handle exit
  exitFn result.exit

  -- Merge enter and update selections
  merge entered updated

-- Usage becomes extremely concise
updateChart :: Array Number -> m (Selection Bound Element Number)
updateChart data = do
  rects <- selectAll "rect" svg
  generalUpdate
    identity
    Rect
    [fill "steelblue", opacity 1.0]
    [fill "orange"]
    (append Rect [] >=> setAttrs [x (\d i -> ...), height (\d -> d), ...])
    (transitionGroup transitionCfg [height (\d -> d * 2.0)])
    (transitionGroup exitCfg [opacity 0.0] >=> remove)
    data
    rects
```

## Advanced: Nested Selections

### Handling Hierarchical Data

```purescript
-- Group selection (multiple parent elements)
type GroupSelection parent datum = Selection Bound parent (Array datum)

-- Nested join: bind array data to each parent
nestedJoin
  :: forall parent datum key m
   . MonadEffect m
  => Ord key
  => (datum -> key)
  -> (parent -> Array datum)                    -- Extract data for each parent
  -> GroupSelection parent (Array datum)
  -> m (Array (JoinResult parent datum))        -- One join result per parent

-- Example: Grouped bar chart
createGroupedChart :: Array { category :: String, values :: Array Number } -> m Unit
createGroupedChart dataset = do
  svg <- select "#chart"

  -- Bind groups
  groups <- selectAll "g" svg
  groupResult <- bindData (_.category) dataset groups

  newGroups <- append Group [] groupResult.enter
  _ <- setAttrs [transform (\d i -> "translate(" <> show (i * 100.0) <> ",0)")] newGroups

  -- Nested selection: for each group, bind bars
  allGroups <- merge newGroups groupResult.update

  forEachSelection allGroups \group d -> do
    bars <- selectAll "rect" group
    barResult <- bindData identity d.values bars

    newBars <- append Rect [] barResult.enter
    _ <- setAttrs
      [ x (\val i -> Number.fromInt i * 20.0)
      , y (\val -> 200.0 - val)
      , width 15.0
      , height _.identity
      , fill "steelblue"
      ] newBars

    pure unit
```

## Comparison with D3.js

### What We Improve

| Aspect | D3.js | PureScript Selection 2.0 |
|--------|-------|---------------------------|
| **Type Safety** | Untyped JavaScript | Fully typed with phantom types |
| **State Encoding** | Runtime checks | Compile-time verification |
| **Key Functions** | Optional, implicit | Required, explicit |
| **Illegal Operations** | Runtime errors | Compile errors |
| **Data Binding** | Mutates selections | Returns new typed selections |
| **Attribute Types** | Strings only | Typed values (Color, Number, etc.) |
| **Transitions** | Imperative API | Declarative + FRP |
| **Composition** | Method chaining | Pure function composition |

### What D3 Does Well (That We Must Preserve)

- ✅ **Performance:** Batch DOM updates, minimize reflows
- ✅ **Flexibility:** Support arbitrary attributes and elements
- ✅ **Intuitive:** Enter-update-exit pattern is clear
- ✅ **Power:** Support complex nested joins and hierarchies
- ✅ **Compatibility:** Work with standard web platform

## Implementation Strategy

### Phase 1: Core Types and Join Algorithm
- [ ] Define `Selection` with phantom types
- [ ] Implement pure `computeJoin` algorithm
- [ ] Create attribute ADT with smart constructors
- [ ] Write comprehensive QuickCheck property tests

### Phase 2: Basic DOM Operations
- [ ] Implement `select` and `selectAll`
- [ ] Implement `bindData` and `rebindData`
- [ ] Implement `append` for enter selections
- [ ] Implement `setAttrs` for bound selections
- [ ] Implement `remove` for exit selections
- [ ] Minimal FFI using purescript-web-dom

### Phase 3: Three Little Circles Proof of Concept
- [ ] Port ThreeLittleCircles to new API
- [ ] Verify enter-update-exit works correctly
- [ ] Compare performance with D3 version
- [ ] Iterate on API ergonomics

### Phase 4: Transitions
- [ ] Design transition system (FRP vs. imperative)
- [ ] Implement easing functions
- [ ] Add `transition` operation
- [ ] Port ThreeLittleCirclesTransition

### Phase 5: Advanced Features
- [ ] Nested selections and grouped data
- [ ] Event handling (onClick, onMouseOver, etc.)
- [ ] Axis generation (inspired by d3-axis)
- [ ] Scale functions (ordinal, linear, time, etc.)
- [ ] Layout algorithms (force, hierarchy, etc.)

### Phase 6: Real-World Example
- [ ] Port Code Explorer to new API
- [ ] Measure bundle size vs current implementation
- [ ] Performance benchmarks
- [ ] Documentation and examples

## Open Questions

### 1. Transition System Architecture

**Option A: FRP/Behaviors**
- Pro: Declarative, composable, testable
- Pro: Natural fit for continuous animations
- Con: Additional dependency (purescript-behaviors)
- Con: Learning curve for users unfamiliar with FRP

**Option B: Imperative with Promises**
- Pro: Familiar model (like D3)
- Pro: Simple mental model
- Con: Less composable
- Con: Harder to reason about timing

**Recommendation:** Start with imperative for familiarity, but design for eventual FRP upgrade.

### 2. Attribute Value Types

**Option A: Closed ADT**
```purescript
data AttributeValue
  = StringValue String
  | NumberValue Number
  | BooleanValue Boolean
```
- Pro: Exhaustive, pattern matchable
- Con: Not extensible

**Option B: Type Class**
```purescript
class ToAttributeValue a where
  toAttributeValue :: a -> String

instance ToAttributeValue String where ...
instance ToAttributeValue Number where ...
instance ToAttributeValue Color where ...
```
- Pro: Extensible by users
- Con: Less type-safe at attribute definition

**Recommendation:** Type class for flexibility, but provide well-typed smart constructors.

### 3. Selection Merging

When we have enter and update selections, how do we merge them into a single bound selection?

```purescript
merge
  :: forall datum m
   . MonadEffect m
  => Selection Bound Element datum
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)
```

**Challenge:** Need to maintain element order and data binding consistency.

**Solution:** Track insertion order via timestamp or explicit ordering field.

### 4. Event Handling

Should events be attributes or separate API?

**Option A: Events as Attributes**
```purescript
onClick :: forall datum. (datum -> Element -> Effect Unit) -> Attribute datum
```

**Option B: Separate Event API**
```purescript
on
  :: forall datum m
   . MonadEffect m
  => EventType
  -> (datum -> Element -> Effect Unit)
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)
```

**Recommendation:** Separate API - events are different from visual attributes.

## Success Criteria

This design will be successful if:

1. ✅ **Type Safety:** Impossible to misuse the API (compile errors for invalid operations)
2. ✅ **Performance:** Comparable to D3.js (within 20%)
3. ✅ **Ergonomics:** More concise than current PSD3, comparable to D3.js
4. ✅ **Power:** Can express Code Explorer and all current examples
5. ✅ **Maintainability:** Pure PureScript core with minimal FFI
6. ✅ **Documentation:** Types are self-documenting
7. ✅ **Testing:** Property-based tests for join algorithm
8. ✅ **Bundle Size:** Smaller than current implementation (tree-shaking)

## Next Steps

1. Create `src/lib/PSD3v2/` directory for new implementation
2. Start with core types in `PSD3v2.Selection.Types`
3. Implement pure join algorithm in `PSD3v2.Selection.Join`
4. Write property tests before any FFI
5. Implement minimal DOM interpreter
6. Port Three Little Circles as first example

## References

- **D3 Data Join:** https://bost.ocks.org/mike/join/
- **D3 Source Code:** https://github.com/d3/d3-selection
- **PureScript Web APIs:** https://pursuit.purescript.org/packages/purescript-web-dom
- **Phantom Types:** https://wiki.haskell.org/Phantom_type
- **Tagless Final:** http://okmij.org/ftp/tagless-final/
- **FRP in PureScript:** https://github.com/purescript-contrib/purescript-behaviors

---

**Author:** Claude (with human guidance)
**Status:** Design Exploration
**Next Review:** After Phase 1 prototype
