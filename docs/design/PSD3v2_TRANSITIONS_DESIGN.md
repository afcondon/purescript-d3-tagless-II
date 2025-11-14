# PSD3v2 Transitions Design

## Goals

1. **Type Safety**: Use phantom types to prevent invalid transition usage
2. **Ergonomic DSL**: Clean, composable API for common use cases
3. **Leverage Libraries**: Use purescript-easings for easing functions
4. **Minimal Custom FFI**: Use D3's transition engine, avoid reimplementing

## D3 Transition Semantics

From D3 documentation:
- `selection.transition()` creates a transition from a selection
- Transitions smoothly interpolate DOM from current to target state
- Duration, delay, and easing control timing
- Structural changes (append/remove) must happen BEFORE transition
- Attribute/style changes AFTER `.transition()` are animated

## API Design

### Option 1: Separate Transition Type (Rejected)

```purescript
-- Create separate Transition type
transition :: forall m sel datum
   . SelectionM sel m
  => sel SBound Element datum
  -> m (Transition datum)

-- Apply attributes to transition
setTransitionAttrs
  :: Duration
  -> Array (Attribute datum)
  -> Transition datum
  -> m Unit
```

**Problems**:
- Breaks selection-like interface
- Awkward to chain operations
- Doesn't match D3 mental model

### Option 2: Transition as Method (Rejected)

```purescript
-- Add transition to SelectionM
class SelectionM sel m where
  ...
  transition :: sel SBound Element datum -> m (sel STransitioning Element datum)
  ```

**Problems**:
- Adds complexity to core Selection API
- Transitioning state may not be universally needed
- Hard to compose with non-transitioning operations

### Option 3: Transition as Separate Capability (CHOSEN)

```purescript
-- New capability class for transitions
class Monad m <= TransitionM sel m | m -> sel where
  -- Create a transition from a bound selection
  withTransition
    :: forall datum
     . TransitionConfig
    -> sel SBound Element datum
    -> Array (Attribute datum)
    -> m Unit

-- TransitionConfig specifies timing
type TransitionConfig =
  { duration :: Milliseconds
  , delay :: Maybe Milliseconds
  , easing :: Maybe Easing
  }

-- Smart constructors
defaultTransition :: Milliseconds -> TransitionConfig
transition :: Milliseconds -> TransitionConfig
```

**Benefits**:
- Separates concerns: Selection vs Transition
- Interpreters can choose whether to support transitions
- Type-safe: only bound selections can transition
- Clean API: `withTransition config selection attrs`

### Usage Example

```purescript
drawThreeCircles :: forall m sel. SelectionM sel m => TransitionM sel m => String -> m Unit
drawThreeCircles selector = do
  container <- select selector
  svg <- appendChild SVG [width 400.0, height 150.0, viewBox "0 0 400 150"] container

  -- Initial render
  circles <- renderData Circle [32, 57, 293] "circle" svg
    (Just \_ -> [fill "green", cx (\_ i -> toNumber i * 100.0 + 50.0), cy 50.0, radius 10.0])
    Nothing
    Nothing

  -- Transition to new state
  withTransition (transition (Milliseconds 1500.0)) circles
    [ fill "orange"
    , radius 20.0
    , cy 100.0
    ]
```

## Implementation Plan

### Phase 1: Core Types
- `TransitionConfig` type with duration/delay/easing
- `TransitionM` capability class
- Smart constructors (transition, defaultTransition)

### Phase 2: D3v2 Interpreter
- Implement TransitionM for D3v2M monad
- FFI: Call D3's `.transition()` and configure duration/delay/easing
- FFI: Apply attributes to transitioned selection

### Phase 3: Easing Support
- Import purescript-easings
- Convert Easing to D3 easing function
- Add easing parameter to TransitionConfig

### Phase 4: Advanced Features (Future)
- Named transitions
- Transition events (start, end, interrupt)
- Chained transitions (.on("end", ...))
- Staggered delays (delay as function of datum/index)

## Type Safety Guarantees

1. **Only Bound Selections**: `withTransition` requires `SBound` state
2. **Datum Type Consistency**: Attributes must match selection's datum type
3. **Effect Tracking**: Transitions wrapped in Effect/Aff for ordering

## Comparison with V1

### V1 Approach:
```purescript
setAttributes circles $
  transitionWithDuration (Milliseconds 1500.0) `to`
    [ fill "orange", radius 20.0 ]
```

### V2 Approach:
```purescript
withTransition (transition (Milliseconds 1500.0)) circles
  [ fill "orange", radius 20.0 ]
```

**Differences**:
- V1: Transition embedded in attribute array
- V2: Transition as separate operation
- V1: Uses `to` combinator
- V2: Explicit `withTransition` function
- V2: Better type safety (phantom types)
- V2: Clearer separation of concerns

## FFI Boundary

### Minimal FFI Needed:
```javascript
// Create a transition from a selection
export function createTransition_(duration, delay, easing, selection) {
  let t = selection.transition()
  if (duration) t = t.duration(duration)
  if (delay) t = t.delay(delay)
  if (easing) t = t.ease(easing)
  return t
}

// Set attribute on transition (reuses existing setAttribute from web-dom)
// D3 transitions duck-type as selections for attr/style
```

### Leverage web-dom:
- Element.setAttribute works on transitioned selections
- No need for separate transition attribute setters

## Decision: Separate Capability Class

Transitions are implemented as a separate `TransitionM` capability class because:

1. **Not all interpreters need transitions**: String, Meta, Music interpreters don't animate
2. **Separation of concerns**: Core selection operations vs timing/animation
3. **Progressive enhancement**: Start with SelectionM, add TransitionM when needed
4. **Type safety**: Only bound selections can transition
5. **Testability**: Can test selection logic without animation complexity

## Next Steps

1. Create `PSD3v2.Transition.Types` module
2. Create `PSD3v2.Capabilities.Transition` module
3. Implement TransitionM instance for D3v2 interpreter
4. Create animated ThreeLittleCircles example
5. Test in browser
