# Phase 3: Three Little Circles - Issues Discovered

## Issue 1: Phantom Type Datum Mismatch ✅ FIXED

**Problem:** `select` returns `sel SEmpty Element Unit`, but `renderData` expects the datum type to match the data being bound.

```purescript
-- This doesn't type-check:
svg <- select "svg"  -- Returns: sel SEmpty Element Unit
renderData Circle [32, 57, 293] "circle" svg  -- Expects: sel SEmpty Element Int
```

**Root Cause:** The Empty selection's phantom `datum` parameter should probably be polymorphic or contravariant, since it has no data bound yet.

**Solution Implemented:** Added `coerceSelection` function that safely coerces phantom datum type:

```purescript
coerceSelection
  :: forall state parent a b
   . sel state parent a
  -> m (sel state parent b)
```

Implementation uses `unsafeCoerce` which is safe because phantom types are erased at runtime and the underlying SelectionImpl doesn't change.

**Usage:**
```purescript
svg <- select "svg"  -- Returns: sel SEmpty Element Unit
svg' <- coerceSelection svg  -- Coerce to: sel SEmpty Element Int
circles <- renderData Circle [32, 57, 293] "circle" svg'
```

**Files Modified:**
- `/src/lib/PSD3v2/Selection/Operations.purs` - Added coerceSelection implementation
- `/src/lib/PSD3v2/Capabilities/Selection.purs` - Added to type class
- `/src/lib/PSD3v2/Interpreter/D3v2.purs` - Implemented in interpreter

## Issue 2: renderData Doesn't Apply Attributes Per-Datum ✅ FIXED

**Problem:** In `Operations.purs`, the `renderData` function had TODOs for applying attributes - attributes were ignored, empty array was passed to `append`.

**Solution Implemented:** Created `applyPerDatumAttrs` helper function that:
1. Takes a `datum -> Array (Attribute datum)` function
2. Zips elements with data
3. Applies generated attributes to each element using `traverseWithIndex_`

```purescript
applyPerDatumAttrs
  :: forall datum m state
   . MonadEffect m
  => (datum -> Array (Attribute datum))
  -> Selection state Element datum
  -> m (Selection state Element datum)
applyPerDatumAttrs mkAttrs (Selection impl) = liftEffect do
  case impl of
    BoundSelection { elements, data: datumArray } -> do
      let paired = Array.zipWith Tuple datumArray elements
      paired # traverseWithIndex_ \index (Tuple datum element) -> do
        let attrs = mkAttrs datum
        applyAttributes element datum index attrs
      pure $ Selection impl
    _ -> pure $ Selection impl
```

**Updated renderData flow:**
1. Append elements with no attributes
2. Apply per-datum attributes if provided
3. Return bound selection

**Files Modified:**
- `/src/lib/PSD3v2/Selection/Operations.purs` - Added applyPerDatumAttrs, updated renderData

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
