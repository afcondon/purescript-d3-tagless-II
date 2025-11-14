# Phase 3: Three Little Circles - Issues Discovered

## Issue 1: Phantom Type Datum Mismatch

**Problem:** `select` returns `sel SEmpty Element Unit`, but `renderData` expects the datum type to match the data being bound.

```purescript
-- This doesn't type-check:
svg <- select "svg"  -- Returns: sel SEmpty Element Unit
renderData Circle [32, 57, 293] "circle" svg  -- Expects: sel SEmpty Element Int
```

**Root Cause:** The Empty selection's phantom `datum` parameter should probably be polymorphic or contravariant, since it has no data bound yet.

**Workaround:** Use `[unit, unit, unit]` instead of `[32, 57, 293]`.

**Proper Fix Options:**
1. Make `select` return `sel SEmpty Element datum` with polymorphic `datum`
2. Add a type coercion function: `coerceDatum :: sel SEmpty parent a -> sel SEmpty parent b`
3. Change Empty selection to not track datum type at all

## Issue 2: renderData Doesn't Apply Attributes Per-Datum

**Problem:** In `Operations.purs`, the `renderData` function has TODOs for applying attributes:

```purescript
-- Operations.purs:296-301
enterBound <- case enterAttrs of
  Nothing -> append elemType [] enter
  Just _mkAttrs -> do
    -- TODO: Need to apply per-datum attributes
    -- For now, we'll use empty attributes
    append elemType [] enter
```

**Current Behavior:** Attributes are ignored, empty array is passed to `append`.

**What We Need:** Apply the `datum -> Array (Attribute datum)` function for each datum in the selection.

**Challenge:** The `append` function takes a single `Array (Attribute datum)` for all elements, but we have a function that generates different attributes for each datum.

**Solution:** Need to rethink the API - maybe `append` should take `Maybe (datum -> Array (Attribute datum))` or we need a different approach where we iterate over the data.

## Issue 3: Attribute Smart Constructors Are Inconsistent

**Problem:** Some attributes are static (`radius`), some require functions (`cx`, `cy`).

```purescript
radius :: forall datum. Number -> Attribute datum          -- Static
cx :: forall datum. (datum -> Number) -> Attribute datum   -- Data-driven
```

**User Expectation:** Want to write `cx 50.0` for a static position, not `cx (\_ -> 50.0)`.

**Fix Needed:** Add static versions of positional attributes:
```purescript
cx_ :: forall datum. Number -> Attribute datum
cx_ n = StaticAttr (AttributeName "cx") (NumberValue n)

cx :: forall datum. (datum -> Number) -> Attribute datum
cx f = DataAttr (AttributeName "cx") (NumberValue <<< f)
```

Or use overloading via type classes.

## Issue 4: Missing Indexed Attribute Smart Constructors

**Problem:** Had to use raw constructor for index-based positioning:

```purescript
-- Wanted:
cxIndexed (\i -> toNumber i * 30.0 + 20.0)

-- Had to write:
IndexedAttr (AttributeName "cx") (\_ i -> NumberValue (toNumber i * 30.0 + 20.0))
```

**Fix:** Add indexed smart constructors:
```purescript
cxIndexed :: forall datum. (Int -> Number) -> Attribute datum
cxIndexed f = IndexedAttr (AttributeName "cx") (\_ i -> NumberValue (f i))
```

## Priority for Next Steps

1. **Fix renderData** - Most critical, currently doesn't work at all
2. **Fix phantom type mismatch** - Blocker for using real data
3. **Add attribute smart constructors** - Quality of life improvement
4. **Add indexed smart constructors** - Quality of life improvement

## Implementation Notes

For fixing renderData, we likely need to change the approach. Instead of:

```purescript
append :: ElementType -> Array (Attribute datum) -> sel SPending parent datum -> m (sel SBound Element datum)
```

We might need something like:

```purescript
appendWith :: ElementType -> (datum -> Array (Attribute datum)) -> sel SPending parent datum -> m (sel SBound Element datum)
```

Or keep both: `append` for uniform attributes, `appendWith` for per-datum attributes.
