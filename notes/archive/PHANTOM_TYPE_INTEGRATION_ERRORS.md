# Phantom Type Integration - Remaining Errors

**Status**: 13 errors remaining after systematic integration work
**Date**: Integration of feature/phantom-type-selection with feature/pure-hierarchy-layouts

## Summary by Category

1. **Simulation Type Parameterization (6 errors)** - KindsDoNotUnify
   - Force and D3SimulationState_ need consistent phantom type handling

2. **Hierarchy Visualization ToAttr (3 errors)** - NoInstanceFound
   - BubbleChart, Icicle, Treemap - typed tree nodes vs Datum_ boundary

3. **Application Type Mismatches (4 errors)** - TypesDoNotUnify
   - Parabola, Spago, WealthHealth, ForceNavigator - phantom type propagation

---

## Category 1: Simulation Type Parameterization (6 errors)

### ERROR 1/13 - SimulationState.purs:132:14

**File**: `src/lib/PSD3/Component/SimulationState.purs`
**Line**: 132, column 14

```purescript
132  initialize :: forall f. Foldable f => f Force -> Map Label Force
                       ^^^^^
```

**Error**:
```
Could not match kind
  Type -> Type
with kind
  Type
while checking that type Force
  has kind Type
```

**Context**: Function signature trying to use `Force` without phantom parameter, but `Force` is now `Force d :: Type -> Type`

**Solution Direction**: Should be `f (Force Unit) -> Map Label (Force Unit)` since forces use Unit phantom type

---

### ERROR 2/13 - Forces.purs:198:53

**File**: `src/lib/PSD3/Internal/Simulation/Forces.purs`
**Line**: 198, column 53

```purescript
198  setForceAttr force_ maybeFilter (AttributeSetter label attr) = do
                                                          ^^^^
```

**Error**:
```
Could not match kind
  Type -> Type
with kind
  Type
while checking that type AttributeSetter
  has kind Type
```

**Context**: `AttributeSetter` is `AttributeSetter d` but being used without phantom parameter

**Solution Direction**: Should be `AttributeSetter Unit` to match force attribute system

---

### ERROR 3/13 - RunSimulation.purs:61:20

**File**: `src/lib/PSD3/Simulation/RunSimulation.purs`
**Line**: 61, column 20

```purescript
61   runSimulation :: forall attrs. Array Force -> attrs -> ...
                             ^^^^^
```

**Error**:
```
Could not match kind
  Type -> Type
with kind
  Type
while checking that type Force
  has kind Type
```

**Context**: Function parameter using `Force` without phantom type

**Solution Direction**: `Array (Force Unit)`

---

### ERROR 4/13 - Update.purs:159:20

**File**: `src/lib/PSD3/Simulation/Update.purs`
**Line**: 159, column 20

```purescript
159    { nodes :: Maybe sel, links :: Maybe sel } ->
                          ^^^
```

**Error**:
```
Could not match kind
  Type -> Type
with kind
  Type
while checking that type sel
  has kind Type
```

**Context**: Type variable `sel` being used incorrectly - likely needs application to a phantom type

**Solution Direction**: Needs investigation of `RenderCallbacks` type and how `sel` is used

---

### ERROR 5/13 - CodeAtlas/State.purs:31:19

**File**: `src/website/Component/CodeAtlas/State.purs`
**Line**: 31, column 19

```purescript
31   , forceLibrary  :: M.Map Label Force
                            ^^^^^
```

**Error**:
```
Could not match kind
  Type -> Type
with kind
  Type
while checking that type Force
  has kind Type
```

**Context**: Record field type using `Force` without phantom parameter

**Solution Direction**: `M.Map Label (Force Unit)`

---

### ERROR 6/13 - ForceNavigator/State.purs:25:22

**File**: `src/website/Component/ForceNavigator/State.purs`
**Line**: 25, column 22

```purescript
25   , allForces :: Array Force
                           ^^^^^
```

**Error**:
```
Could not match kind
  Type -> Type
with kind
  Type
while checking that type Force
  has kind Type
```

**Context**: Record field using `Force` without phantom parameter

**Solution Direction**: `Array (Force Unit)`

---

## Category 2: Hierarchy Visualization ToAttr Issues (3 errors)

### Common Pattern

All three errors (BubbleChart, Icicle, Treemap) follow the same pattern:

**Problem**: Lambda functions like `\d -> node'.x (unsafeCoerce d)` cannot have their type inferred when used with `Datum_` phantom type.

**Root Cause**: The hierarchy layouts return `Array Datum_` from `descendants_`, so selections are `D3Selection_ Datum_`. But the attribute functions need ToAttr instances that can work with this setup.

---

### ERROR 7/13 - BubbleChart.purs:62:7

**File**: `src/website/Viz/BubbleChart/BubbleChart.purs`
**Line**: 62, column 7

```purescript
60   bubbles <- simpleJoin chartGroup Circle nodes keyIsID_
61   setAttributes (unsafeCoerce bubbles :: D3Selection_ Datum_)
62     [ cx (\d -> node'.x (unsafeCoerce d))
```

**Error**:
```
No type class instance was found for
  PSD3.Internal.Attributes.Instances.ToAttr Number
                                            (t2 -> Number)
                                            Datum_
The instance head contains unknown type variables.
```

**Context**:
- `bubbles` inferred as `D3Selection_ (D3_TreeNode (...))` from simpleJoin
- Coerced to `D3Selection_ Datum_`
- Lambda `\d -> ...` has type `t2 -> Number` where `t2` is unknown
- Need `ToAttr Number (Datum_ -> Number) Datum_` instance

**Available Instance**: `ToAttr Number (d -> Number) d` - requires datum type in function to match selection phantom type

**Issue**: Compiler can't see through `unsafeCoerce` inside lambda to connect `t2` with `Datum_`

**Attempted Solutions**:
1. Type annotation `\(d :: Datum_) -> ...` - creates mismatch with selection's actual type
2. Indexed lambda `\d _ -> ...` - wrong arity for ToAttr instance
3. Coercing selection to `Datum_` - loses connection between selection and lambda types

---

### ERROR 9/13 - Icicle.purs:60:7

**File**: `src/website/Viz/Icicle/Icicle.purs`
**Line**: 60, column 7

```purescript
58   partitions <- simpleJoin chartGroup Rect nodes keyIsID_
59   setAttributes (unsafeCoerce partitions :: D3Selection_ Datum_)
60     [ x (\d -> node'.x0 (unsafeCoerce d))
```

**Error**: Same as BubbleChart - `ToAttr Number (t2 -> Number) Datum_` not found

---

### ERROR 12/13 - Treemap.purs:60:7

**File**: `src/website/Viz/Treemap/Treemap.purs`
**Line**: 60, column 7

```purescript
58   tiles <- simpleJoin chartGroup Rect nodes keyIsID_
59   setAttributes (unsafeCoerce tiles :: D3Selection_ Datum_)
60     [ x (\d -> node'.x0 (unsafeCoerce d))
```

**Error**: Same as BubbleChart - `ToAttr Number (t2 -> Number) Datum_` not found

---

## Category 3: Application Type Mismatches (4 errors)

### ERROR 8/13 - ForceNavigator/Draw.purs:80:22

**File**: `src/website/Viz/ForceNavigator/Draw.purs`
**Line**: 80, column 22

```purescript
80   , forceLibrary :: Map Label Force
                               ^^^^^
```

**Error**:
```
Could not match kind
  Type -> Type
with kind
  Type
while checking that type Force
  has kind Type
```

**Context**: Record field in component state

**Solution Direction**: `Map Label (Force Unit)` - same as other simulation errors

---

### ERROR 10/13 - Parabola.purs:34:3

**File**: `src/website/Viz/Parabola/Parabola.purs`
**Line**: 34, column 3

```purescript
33   drawWithData :: forall m. SelectionM D3Selection_ m => ... -> m (D3Selection_ d1)
34   root <- attach selector
```

**Error**:
```
Could not match type
  Int
with type
  d1
while trying to match type t2 Int
  with type D3Selection_ d1
```

**Context**:
- Function signature promises to return `m (D3Selection_ d1)` where `d1` is polymorphic
- But the actual data is `Int` (from the parabola calculation)
- The return type should be `m (D3Selection_ Int)` not polymorphic

**Solution Direction**: Fix function signature to use concrete `Int` type or properly parameterize

---

### ERROR 11/13 - Spago/Attributes.purs:25:5

**File**: `src/website/Viz/Spago/Attributes.purs`
**Line**: 25, column 5

```purescript
24   updateAttrs :: forall d0. Array (SelectionAttribute d0)
25   , transform' datum_.translateNode
```

**Error**:
```
Could not match type
  Datum_
with type
  d0
while trying to match type SelectionAttribute t1
  with type SelectionAttribute d0
```

**Context**:
- Function returns `Array (SelectionAttribute d0)` where `d0` is polymorphic
- But using `datum_.translateNode` which works with `Datum_`
- The `d0` should be concrete `Datum_` not polymorphic

**Solution Direction**: Fix signature to `Array (SelectionAttribute Datum_)` or properly parameterize

---

### ERROR 13/13 - WealthHealth/Draw.purs:198:3

**File**: `src/website/Viz/WealthHealth/Draw.purs`
**Line**: 198, column 3

```purescript
198  (root :: D3Selection_ Unit) <- attach selector
```

**Error**:
```
Could not match type
  Datum_
with type
  { income :: Number, lifeExpectancy :: Number, ... }
while trying to match type Array Datum_
  with type Array { income :: Number, ... }
```

**Context**:
- Annotating root selection as `D3Selection_ Unit`
- But the actual data type is a specific record `{ income :: Number, ... }`
- Later code expects to work with this concrete record type

**Solution Direction**: Change annotation to `(root :: D3Selection_ WealthData)` or remove annotation

---

## Analysis & Next Steps

### Simulation Errors (1-6, 8)

**Pattern**: All using `Force` or `AttributeSetter` without the phantom type parameter.

**Root Cause**: After parameterizing Force/ChainableF with phantom type `d`, all uses need to specify `Force Unit` since forces use Unit as phantom type (they work with opaque Datum_ at runtime).

**Fix Complexity**: **EASY** - Mechanical find-and-replace
- Search for: `Array Force`, `Map Label Force`, `f Force`
- Replace with: `Array (Force Unit)`, `Map Label (Force Unit)`, `f (Force Unit)`

**Estimated**: 6 files, ~10 minutes

---

### Hierarchy Viz Errors (7, 9, 12)

**Pattern**: Lambda type inference failure when using `unsafeCoerce` inside attribute functions.

**Root Cause**: Fundamental mismatch between:
- D3 hierarchy FFI returns `Array Datum_` (opaque type)
- Phantom type system expects typed selections `D3Selection_ (D3_TreeNode row)`
- ToAttr instances need the lambda's datum type to match selection's phantom type
- But `unsafeCoerce` prevents type inference from connecting them

**Possible Solutions**:

1. **Add ToAttr instances for Datum_**: Create instances like `ToAttr Number (Datum_ -> Number) Datum_`
   - Pro: Would make current code compile
   - Con: Defeats the purpose of phantom types for hierarchy

2. **Make hierarchy FFI return typed nodes**: Change `descendants_` to return `Array (D3_TreeNode row)`
   - Pro: Proper phantom type flow from start
   - Con: Requires FFI changes, more complex types

3. **Use helper functions instead of inline lambdas**: Create typed wrappers
   ```purescript
   nodeX :: Datum_ -> Number
   nodeX d = node'.x (unsafeCoerce d)
   ```
   - Pro: Explicit types, clear what's happening
   - Con: Verbose, lots of helpers needed

4. **Don't use phantom types for hierarchy viz**: Accept they work with `Datum_` throughout
   - Pro: Matches reality of D3 hierarchy API
   - Con: Inconsistent with typed data approach

**Fix Complexity**: **MEDIUM-HARD** - Requires architectural decision

**Recommended**: Option 2 (typed FFI) or Option 3 (helper functions) for proper phantom type integration

---

### Application Errors (10, 11, 13)

**Pattern**: Type annotations or signatures using wrong phantom type.

**Root Cause**: Functions/annotations using polymorphic phantom types when they should be concrete, or vice versa.

**Fixes**:

- **Parabola**: Change `m (D3Selection_ d1)` to `m (D3Selection_ Int)` in signature
- **Spago**: Change `Array (SelectionAttribute d0)` to `Array (SelectionAttribute Datum_)`
- **WealthHealth**: Change `D3Selection_ Unit` to `D3Selection_ WealthData` (or remove annotation)

**Fix Complexity**: **EASY** - Simple type signature fixes

**Estimated**: 3 files, ~5 minutes

---

## Conclusion

**Can be solved immediately** (7 errors):
- All 6 simulation errors (ERROR 1-6, 8) - mechanical `Force` → `Force Unit` changes
- Parabola error (ERROR 10) - fix signature to use `Int`

**Needs architectural decision** (3 errors):
- All 3 hierarchy viz errors (ERROR 7, 9, 12) - typed vs untyped boundary

**Can be solved with minor thought** (3 errors):
- Spago error (ERROR 11) - fix signature
- WealthHealth error (ERROR 13) - fix annotation
- Update error (ERROR 4) - needs investigation of RenderCallbacks

**Priority**: Fix the 7 easy ones first, then discuss hierarchy approach.
