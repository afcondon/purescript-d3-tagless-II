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

## Practical API Design - User-Centric Naming

### Philosophy: Describe Intent, Not Mechanism

Instead of exposing "enter/update/exit" which are implementation details, we provide operations that describe what the user wants to accomplish:

```purescript
-- High-level: "Show this data as these elements"
renderData
  :: forall f parent datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  => ElementType                    -- What element to create
  -> f datum                        -- Data to visualize
  -> String                         -- CSS selector
  -> Selection Empty parent datum_  -- Parent selection
  -> Maybe (datum -> Array (Attribute datum))        -- How to create new elements (enter)
  -> Maybe (datum -> Array (Attribute datum))        -- How to update existing (update)
  -> Maybe (datum -> Array (Attribute datum))        -- How to remove old (exit, e.g., fade out)
  -> m (Selection Bound Element datum)

-- Example: Simple bar chart
createBarChart :: forall m. MonadEffect m => Array Number -> m Unit
createBarChart dataset = do
  svg <- select "#chart"

  _ <- renderData
    Rect
    dataset
    "rect"
    svg
    (Just $ const  -- Enter: create with initial attributes
      [ x (\d i -> fromInt i * 25.0)
      , y (\d -> 200.0 - d)
      , width 20.0
      , height (\d -> d)
      , fill "steelblue"
      ])
    (Just $ const  -- Update: change these attributes
      [ y (\d -> 200.0 - d)
      , height (\d -> d)
      ])
    Nothing        -- Exit: just remove (no transition)

  pure unit

-- Update with transitions
updateBarChart :: forall m. MonadEffect m => Array Number -> m Unit
updateBarChart dataset = do
  svg <- select "#chart"

  _ <- renderData
    Rect
    dataset
    "rect"
    svg
    (Just $ const  -- Enter: create with attributes
      [ x (\d i -> fromInt i * 25.0)
      , y (\d -> 200.0 - d)
      , width 20.0
      , height (\d -> d)
      , fill "steelblue"
      ])
    (Just $ \d ->  -- Update: transition to new values
      [ transitionTo "y" (\d -> 200.0 - d)
          { duration: Milliseconds 750, delay: zero, easing: EaseInOut, name: Nothing }
      , transitionTo "height" identity
          { duration: Milliseconds 750, delay: zero, easing: EaseInOut, name: Nothing }
      , fill "orange"
      ])
    (Just $ const  -- Exit: fade out before removing
      [ transitionTo "opacity" (const 0.0)
          { duration: Milliseconds 375, delay: zero, easing: EaseOut, name: Nothing }
      , transitionTo "y" (const 200.0)
          { duration: Milliseconds 375, delay: zero, easing: EaseOut, name: Nothing }
      ])

  pure unit
```

**Key Improvements:**
1. ✅ User specifies what they want, not how to sequence operations
2. ✅ All three phases (enter/update/exit) handled internally
3. ✅ `Nothing` for phases you don't care about (just use defaults)
4. ✅ No risk of forgetting to handle a phase
5. ✅ Type system still prevents misuse

### Alternative: Explicit Join for Power Users

For cases where you need fine-grained control (e.g., complex GUP logic), expose the join:

```purescript
-- Low-level: Explicit enter-update-exit (for advanced use)
data JoinResult parent datum = JoinResult
  { enter  :: Selection Pending parent datum
  , update :: Selection Bound Element datum
  , exit   :: Selection Exiting Element datum
  }

joinData
  :: forall f parent datum m
   . MonadEffect m
  => Foldable f
  => Ord datum                       -- Key function implicit: use datum identity
  -> f datum
  -> String                          -- CSS selector
  -> Selection Empty parent datum_
  -> m (JoinResult parent datum)

-- Power user version: full control
createComplexGUP :: Array Node -> m Unit
createComplexGUP nodes = do
  svg <- select "#graph"
  result <- joinData nodes "circle" svg

  -- Fine-grained control over each phase
  entered <- append Circle [] result.enter
  _ <- setAttrs [customEnterLogic] entered
  _ <- customEnterBehavior entered

  updated <- setAttrs [customUpdateLogic] result.update
  _ <- customUpdateBehavior updated

  _ <- customExitBehavior result.exit
  remove result.exit

  pure unit
```

**Design Principle:**
- `renderData` for 90% of use cases (declarative, foolproof)
- `joinData` for 10% that need custom sequencing (powerful, explicit)
- Both use the same type-safe underlying operations

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

## FRP Composition - Interactive Data-Driven Elements

### Philosophy: Reactive Values, Not Imperative Updates

Instead of imperatively updating the DOM, we can model visualizations as **functions from data streams to visual representations**.

```purescript
-- Behavior: A value that changes over time
type Behavior a = Time -> a

-- Event: A discrete occurrence at a point in time
type Event a = Stream (Time, a)

-- A visualization is a behavior over selections
type Visualization datum = Behavior (Selection Bound Element datum)
```

### Example: Reactive Bar Chart

```purescript
-- Data stream from WebSocket/polling/user interaction
dataStream :: Event (Array Number)

-- Create a behavior that represents the current bar chart
barChartViz :: Event (Array Number) -> Behavior (Selection Bound Element Number)
barChartViz dataEvents = do
  currentData <- stepper [] dataEvents  -- Hold latest data

  pure $ renderData
    Rect
    currentData
    "rect"
    svg
    (Just enterAttrs)
    (Just updateAttrs)
    (Just exitAttrs)

-- Render the behavior to the DOM
main = do
  svg <- select "#chart"
  viz <- barChartViz dataStream

  -- Subscribe: whenever behavior changes, re-render
  subscribe viz \selection -> do
    log "Chart updated!"
```

### Composing Multiple Interactive Elements

```purescript
-- Multiple synchronized visualizations
type Dashboard =
  { barChart :: Visualization Number
  , lineChart :: Visualization { x :: Number, y :: Number }
  , scatterPlot :: Visualization Point
  }

-- They all react to the same underlying data
createDashboard :: Behavior Dataset -> Dashboard
createDashboard datasetBehavior =
  let
    -- Derive different views from the same data
    bars = datasetBehavior <#> _.totals <#> renderBarChart
    lines = datasetBehavior <#> _.timeSeries <#> renderLineChart
    points = datasetBehavior <#> _.distribution <#> renderScatterPlot
  in
    { barChart: bars
    , lineChart: lines
    , scatterPlot: points
    }

-- User interaction creates new events
onClick :: Selection Bound Element datum -> Event datum
onClick selection = -- ... create event stream from click handlers

-- Compose interactions
interactiveDashboard = do
  dataset <- loadData
  dash <- createDashboard (pure dataset)

  -- When user clicks bar, highlight corresponding line and scatter points
  barClicks <- onClick dash.barChart

  let highlightedData = stepper Nothing (Just <$> barClicks)

  -- Update visualizations based on selection
  let updatedLines = renderLineChart <$> (applyHighlight <$> highlightedData <*> dataset)

  pure { barChart: dash.barChart, lineChart: updatedLines, scatterPlot: dash.scatterPlot }
```

### Benefits of FRP Approach

1. ✅ **Composability:** Combine multiple reactive visualizations naturally
2. ✅ **Declarative:** Describe relationships, not imperative steps
3. ✅ **Time-traveling:** Behaviors are pure functions of time (testable!)
4. ✅ **Synchronization:** Multiple views automatically stay in sync
5. ✅ **Derived state:** Compute dependent visualizations automatically

### Integration with Current Design

FRP is **optional** - the core API works without it:

```purescript
-- Without FRP (imperative)
updateChart :: Array Number -> m Unit
updateChart newData = do
  selection <- renderData Rect newData "rect" svg ...
  pure unit

-- With FRP (declarative)
chartViz :: Behavior (Array Number) -> Behavior (Selection Bound Element Number)
chartViz dataBehavior = dataBehavior <#> \data ->
  renderData Rect data "rect" svg ...
```

Users can start imperative and graduate to FRP when they need composition.

## Integration with Tagless Final Architecture

### The Big Picture: PSD3v2 as Interpreter Foundation

**Critical Architectural Decision:** PSD3v2 is NOT a replacement for the tagless final interpreter pattern - it's the **implementation layer** that interpreters delegate to.

```
User Code (Tagless Final DSL)
    ↓
Type Class Constraints (SelectionM, HierarchicalM, etc.)
    ↓
Interpreter Instances (D3v2, String, Meta, Music, etc.)
    ↓
PSD3v2 Primitives (renderData, joinData, setAttrs, etc.)
    ↓
DOM / String / Meta structure
```

### Why Keep Tagless Final?

1. **Multiple Interpreters** - Users can extend with custom interpreters:
   - Music/sound interpreter for unsighted data "visualization"
   - WYSIWYG meta tree for visualization development
   - HTML string generation for SSR
   - Test interpreters that verify behavior without DOM

2. **Backwards Compatibility Path** - Could even write a D3v2 interpreter backed by rewritten D3.js if needed

3. **Separation of Concerns**:
   - User code: High-level visualization intent (via type classes)
   - Interpreter: Translation strategy (how to render)
   - PSD3v2: Safe, typed primitives (what can be rendered)

### Revised Capability Type Classes

The existing type classes will be **reworked** to align with PSD3v2 patterns:

```purescript
-- OLD (current PSD3)
class SelectionM sel m | m -> sel where
  simpleJoin :: forall parent datum
    . parent
    -> Element
    -> Array datum
    -> KeyFunction
    -> m sel

-- NEW (PSD3v2-aligned)
class SelectionM sel m | m -> sel where
  -- High-level: user-friendly, manages sequencing
  renderData :: forall f parent datum
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

  -- Low-level: power users, explicit control
  joinData :: forall f parent datum
    . Foldable f
    => Ord datum
    -> f datum
    -> String
    -> sel SEmpty parent datum
    -> m (JoinResult parent datum)

  -- Operations on typed selections
  append :: forall parent datum
    . ElementType
    -> Array (Attribute datum)
    -> sel SPending parent datum
    -> m (sel SBound Element datum)

  setAttrs :: forall datum
    . Array (Attribute datum)
    -> sel SBound Element datum
    -> m (sel SBound Element datum)

  remove :: forall datum
    . sel SExiting Element datum
    -> m Unit
```

### Example: D3v2 Interpreter Implementation

```purescript
-- The new D3v2 interpreter delegates to PSD3v2 primitives
instance SelectionM D3Selection_ Effect where
  renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs = do
    -- Delegate to PSD3v2.Selection.Operations.renderData
    PSD3v2.renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs

  joinData foldableData selector emptySelection = do
    -- Delegate to PSD3v2.Selection.Operations.joinData
    PSD3v2.joinData foldableData selector emptySelection

  append elemType attrs pendingSelection = do
    -- Delegate to PSD3v2.Selection.Operations.append
    PSD3v2.append elemType attrs pendingSelection

  -- ... etc
```

### Example: String Interpreter (Reworked)

```purescript
-- String interpreter generates HTML without DOM
instance SelectionM StringSelection_ Identity where
  renderData elemType foldableData selector _ enterAttrs _ _ = do
    -- Generate HTML string from data and attributes
    let elements = Array.fromFoldable foldableData
    let html = elements # map \datum ->
          let attrs = fromMaybe (const []) enterAttrs datum
          in renderElementToString elemType attrs datum
    pure $ StringSelection_ (Array.fold html)

  -- joinData not needed for string generation
  joinData _ _ _ =
    pure $ JoinResult { enter: ..., update: ..., exit: ... }
```

### Example: Meta Interpreter (Tracking Operations)

```purescript
-- Meta interpreter records what operations were called
instance SelectionM MetaSelection_ (Writer (Array MetaOp)) where
  renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs = do
    tell [RenderDataOp { elemType, count: length foldableData, selector }]
    -- Could delegate to actual D3v2 or return mock selection
    pure $ MetaSelection_ ...
```

### Benefits of This Architecture

✅ **Type safety** - PSD3v2 phantom types prevent illegal states
✅ **Extensibility** - Users can write custom interpreters
✅ **Testability** - String/Meta interpreters enable testing without DOM
✅ **Flexibility** - Could swap out D3.js backend without changing user code
✅ **Migration path** - Can introduce D3v2 interpreter alongside existing D3 interpreter
✅ **Clean separation** - Core primitives (PSD3v2) vs. interpretation strategy (type class instances)

### Migration Strategy

1. **Phase 1-3:** Build PSD3v2 primitives (current work)
2. **Phase 4:** Design revised capability type classes
3. **Phase 5:** Implement D3v2 interpreter using PSD3v2
4. **Phase 6:** Port String interpreter to new patterns
5. **Phase 7:** Port Meta interpreter to new patterns
6. **Phase 8:** Migrate example visualizations to use D3v2 interpreter
7. **Phase 9:** Deprecate old D3 interpreter (or keep for compatibility)

---

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

### 2. Data Requirements: Ord (and Eq) for Identity

**Decision: Require Ord (which requires Eq)**
```purescript
renderData
  :: Ord datum
  => f datum
  -> ...
```

**Key Insight:** Users control "identity" through Eq/Ord instances!

```purescript
-- Example: Nodes identified by ID field
data Node = Node { id :: Int, x :: Number, y :: Number, label :: String }

-- User defines what "same node" means via Eq
instance Eq Node where
  eq (Node a) (Node b) = a.id == b.id  -- Identity is by ID

-- Ord required for Map-based join algorithm
instance Ord Node where
  compare (Node a) (Node b) = compare a.id b.id

-- Now renderData can match nodes by ID across updates!
renderData circles nodes "circle" svg ...
```

**Benefits:**
- ✅ Simpler API - no key function parameter
- ✅ Natural for simple types (Int, String already have Eq/Ord)
- ✅ Users control identity semantics through Eq instance
- ✅ Works with Sets and Maps automatically
- ✅ More PureScript-idiomatic than passing functions

**For complex types, newtype with derived instances:**
```purescript
-- If you can't modify the original type
newtype NodeByID = NodeByID Node

instance Eq NodeByID where
  eq (NodeByID a) (NodeByID b) = a.id == b.id

derive newtype instance Ord NodeByID
```

**Alternative: Generic deriving for common patterns:**
```purescript
-- Let compiler generate by-field comparison
derive instance Generic Node _
derive instance Eq Node      -- Structural equality
derive instance Ord Node     -- Structural ordering

-- Or manually specify for ID-based identity
instance Eq Node where
  eq = comparing _.id        -- Identity by ID field

instance Ord Node where
  compare = comparing _.id
```

**Why Ord, not just Eq?**
- Join algorithm uses `Map` for O(n+m) performance
- Map requires Ord for balanced tree structure
- Alternative would be HashMap (requires Hashable), but Ord is simpler

### 3. Foldable Data, Not Just Arrays

All functions accept `Foldable f => f datum` instead of `Array datum`:
```purescript
renderData :: Foldable f => f datum -> ...

-- Users can pass:
renderData [1, 2, 3]              -- Array
renderData (List.fromFoldable ...) -- List
renderData (Set.fromFoldable ...)  -- Set (already Ord!)
renderData (Map.values myMap)      -- Map values
```

Internally we convert to Array when needed, but the API is more flexible.

### 4. Event Handling - Leverage purescript-web-dom

```purescript
-- Use standard web event types from purescript-web-dom
import Web.Event.Event (Event, EventType)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

-- Events are separate from attributes
on
  :: forall datum m
   . MonadEffect m
  => EventType                      -- e.g., EventType "click"
  -> (datum -> Event -> Effect Unit)  -- Handler gets datum and event
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)

-- Smart constructors for common events
onClick :: (datum -> MouseEvent -> Effect Unit) -> EventHandler datum
onDoubleClick :: (datum -> MouseEvent -> Effect Unit) -> EventHandler datum
onMouseEnter :: (datum -> MouseEvent -> Effect Unit) -> EventHandler datum
onMouseLeave :: (datum -> MouseEvent -> Effect Unit) -> EventHandler datum
onKeyDown :: (datum -> KeyboardEvent -> Effect Unit) -> EventHandler datum

-- Drag events (may need purescript-web-drag or custom)
onDragStart :: (datum -> DragEvent -> Effect Unit) -> EventHandler datum
onDrag :: (datum -> DragEvent -> Effect Unit) -> EventHandler datum
onDragEnd :: (datum -> DragEvent -> Effect Unit) -> EventHandler datum

-- For zoom/pan, we might create higher-level helpers
onZoom :: (datum -> ZoomState -> Effect Unit) -> EventHandler datum
  where ZoomState = { scale :: Number, translate :: { x :: Number, y :: Number } }
```

**Key Insight:** Reuse web platform types from `purescript-web-*` packages:
- `purescript-web-dom` - Core DOM APIs
- `purescript-web-events` - Event types
- `purescript-web-uievents` - Mouse, keyboard, etc.

This unifies with the broader PureScript ecosystem and avoids reinventing these types.

### 5. Selection Merging - Follow D3's Approach

D3 merges selections by concatenating element arrays while preserving document order:

```purescript
merge
  :: forall datum m
   . MonadEffect m
  => Selection Bound Element datum
  -> Selection Bound Element datum
  -> m (Selection Bound Element datum)

-- Internal implementation follows D3:
-- 1. Concatenate element arrays: entered ++ updated
-- 2. Sort by document order (if needed for consistency)
-- 3. Concatenate data arrays in same order
-- 4. Return new merged selection
```

**D3's behavior:**
```javascript
const merged = enter.merge(update);
// merged contains all elements in document order
```

**Our approach:** Same semantics. If D3's ordering works for millions of visualizations, we adopt it unless there's a principled reason not to.

**Open Question:** Should we auto-merge in `renderData` or require explicit merge?
- Auto-merge: Simpler for users
- Explicit merge: More control for complex cases

**Recommendation:** Auto-merge in `renderData`, expose explicit `merge` for `joinData` users.

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

## Implementation Notes

### Index Preservation for Correct Positioning (2025-11-14)

**Problem Discovered:** When implementing the GUP example with alphabetically sorted letters, we discovered that **both enter and update selections must preserve their logical indices** in the sorted data array, not just their array positions after sorting.

**Why This Matters:**

When transitioning from `['a','g','i','k','m','n','p','r','t','y']` to `['c','d','f','h','q','r','t','u','v','w','x','y','z']`:
- Update letter 'r' should be at position 5 (x=290.0), not position 1
- Enter letter 'r' should also be at position 5 (x=290.0), not position 1

Without index preservation, letters would overlap because `traverseWithIndex` only knows their position in the enter/update arrays (0,1,2,3...), not their position in the final sorted alphabet.

**Solution Implemented:**

1. **Added `indices :: Maybe (Array Int)` field** to both `BoundSelection` and `PendingSelection`:
   ```purescript
   | BoundSelection
       { elements :: Array Element
       , data :: Array datum
       , indices :: Maybe (Array Int)  -- Just for join results, Nothing otherwise
       , document :: Document
       }
   | PendingSelection
       { parentElements :: Array Element
       , pendingData :: Array datum
       , indices :: Maybe (Array Int)  -- Just for join results, Nothing otherwise
       , document :: Document
       }
   ```

2. **Enhanced join result types** to track indices:
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

3. **Updated join algorithm** to populate indices:
   ```purescript
   -- In joinData operation:
   let sortedEnter = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.enter
   let enterSelection = Selection $ PendingSelection
         { parentElements
         , pendingData: sortedEnter <#> _.datum
         , indices: Just (sortedEnter <#> _.newIndex)  -- ← Preserves logical positions
         , document: doc
         }
   ```

4. **Modified attribute application** to use logical indices:
   ```purescript
   -- In append and setAttrs:
   traverseWithIndex_ \arrayIndex datum -> do
     let logicalIndex = case indices of
           Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
           Nothing -> arrayIndex
     -- Use logicalIndex for IndexedAttr functions
   ```

**Key Insight:** When data is sorted/reordered for a join, the position in the enter/update arrays (0,1,2,3...) differs from the position in the final data array. IndexedAttr functions like `x (\_ i -> 50.0 + toNumber i * 48.0)` must use the logical position, not the array position.

**Files Modified:**
- `src/lib/PSD3v2/Selection/Types.purs`: Added `indices` field to BoundSelection and PendingSelection
- `src/lib/PSD3v2/Selection/Join.purs`: Added `EnterBinding` type, track `newIndex` for enter data
- `src/lib/PSD3v2/Selection/Operations.purs`: Populate and use `indices` in append, setAttrs, and joinData
- `src/lib/PSD3v2/Selection/Indexed.purs`: Same changes for indexed monad version
- `src/lib/PSD3v2/Interpreter/D3v2.purs`: Use `indices` field in transitions

**Testing:** Verified with GUP example using random alphabetically-sorted letters. Without this fix, letters would overlap; with it, they smoothly transition to their correct alphabetical positions.

## References

- **D3 Data Join:** https://bost.ocks.org/mike/join/
- **D3 Source Code:** https://github.com/d3/d3-selection
- **PureScript Web APIs:** https://pursuit.purescript.org/packages/purescript-web-dom
- **Phantom Types:** https://wiki.haskell.org/Phantom_type
- **Tagless Final:** http://okmij.org/ftp/tagless-final/
- **FRP in PureScript:** https://github.com/purescript-contrib/purescript-behaviors

---

**Author:** Claude (with human guidance)
**Status:** ✅ **Implemented** - Core features working (selections, joins, transitions, GUP)
**Last Updated:** 2025-11-14
**Next Review:** After additional examples (hierarchy layouts, force simulations)
