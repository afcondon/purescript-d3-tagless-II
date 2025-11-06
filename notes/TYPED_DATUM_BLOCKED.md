# Typed Datum Accessor Solutions - Analysis & Blocker

## The Problem

Current pattern requires 15-30 lines of boilerplate per visualization:

```purescript
datum_ = {
  radius: _.r <<< unboxD3SimNode
  , id: _.id <<< unboxD3SimNode
  , loc: _.loc <<< unboxD3SimNode
  -- ... 30 more lines of identical pattern
}
```

This appears across 8 visualizations (Spago, LesMis, Trees, etc.), totaling 120-240 lines of repetitive accessor code.

## Attempted Solution: HasDatum Typeclass

We implemented a `HasDatum` typeclass approach to enable typed lambda syntax:

```purescript
class HasDatum d where
  fromDatum :: Datum_ -> d
  toDatum :: d -> Datum_

-- Usage would be:
instance hasDatumSpago :: HasDatum SpagoSimNode where
  fromDatum = unsafeCoerce
  toDatum = unsafeCoerce

-- Then in attributes:
setAttributes
  [ radius (\(d :: SpagoSimNode) -> d.r)    -- Type annotation once
  , text (\d -> d.name)                      -- Inferred!
  , x (\d -> d.x)
  ]
```

## ❌ CRITICAL BLOCKER: Row Types Cannot Have Instances

**Discovery**: ALL existing visualizations use row type aliases, not newtypes:

```purescript
// Current pattern in ALL 8 visualizations:
type SpagoSimNode  = D3_SimulationNode ( SpagoNodeRow + D3_XY + D3_VxyFxy + ... )
type SpagoDataRecord = Record SpagoDataRow
type LesMisSimNode = D3_SimulationNode ( LesMisNodeData + D3_XY + D3_VxyFxy + () )
```

**PureScript Constraint**: Instance heads must be proper type constructors, not row types:

```purescript
// ❌ FAILS with error:
// "Type class instance head is invalid due to use of type (name :: String, x :: Number, ...)"
type MyNode = { name :: String, x :: Number }
instance hasDatumMyNode :: HasDatum MyNode where
  fromDatum = unsafeCoerce
  toDatum = unsafeCoerce

// ✅ WORKS - newtype is a proper type constructor:
newtype MyNode = MyNode { name :: String, x :: Number }
instance hasDatumMyNode :: HasDatum MyNode where
  fromDatum = unsafeCoerce
  toDatum = unsafeCoerce
```

## Impact Analysis

- ❌ Cannot create `HasDatum SpagoDataRecord` - it's `Record SpagoDataRow`
- ❌ Cannot create `HasDatum` for ANY existing visualization node types
- ⚠️ Would require converting ALL node types to newtypes (breaking change)
- ⚠️ Existing unboxer functions (`unboxD3SimNode`) already do the coercion work
- ⚠️ Row types are integral to the library's extensible record design pattern

## Attempted Workaround Path

**What We Tried**:
1. Implemented `HasDatum` typeclass in src/lib/PSD3/Internal/Types.purs
2. Removed old `Datum_ -> T` instances from Attributes/Instances.purs
3. Added typed `HasDatum d => (d -> T)` instances
4. Build succeeded (0 errors!)
5. Attempted to convert Spago visualization
6. Hit the row type blocker immediately

**Commits Created** (now reverted):
- `3f4f23f`: Solution 1: Remove old Datum_ instances
- `156d297`: Add HasDatum typeclass (overlapping instances issue)
- `5b2edbb`: Add composition operator and documentation
- `bba2606`: Remove test file

All reverted via `git reset --hard 314ffd6`

## Why Row Types Matter

The current architecture DEPENDS on row types for extensibility:

```purescript
// Allows combining different capability rows:
type SpagoSimNode = D3_SimulationNode
  ( SpagoNodeRow       -- Domain data
  + D3_XY              // Position
  + D3_VxyFxy          -- Velocity
  + D3_FocusXY         -- Custom clustering
  + D3_Radius          -- Size
  + ()
  )
```

Converting to newtypes would:
- Break this extensibility pattern
- Require significant refactoring across 8 visualizations
- Lose the benefits of row type polymorphism
- Still need unwrapper functions (defeating the purpose)

## Alternative Paths Forward

### Option 1: Accept Current Pattern ✅ PRAGMATIC
Keep `datum_` with `unboxD3SimNode` composition - it works and is explicit.

**Pros**:
- No breaking changes
- Clear and explicit
- Already understood by codebase

**Cons**:
- 15-30 lines of boilerplate per visualization
- Repetitive pattern

### Option 2: Explore Codegen 🔧 TOOL-BASED
Auto-generate datum_ accessors from type signatures (see notes/DATUM_ACCESSOR_CODEGEN.md).

**Pros**:
- Eliminates manual boilerplate
- Works with existing row types
- No runtime changes

**Cons**:
- Requires build tooling
- Generated code still in repo
- Maintenance burden

### Option 3: Newtype Refactor ⚠️ BREAKING
Convert all visualization node types to newtypes, enable HasDatum pattern.

**Pros**:
- Clean typed lambda syntax
- Type-safe accessor pattern

**Cons**:
- Breaking change for all 8 visualizations
- Loses row type extensibility
- Still need unwrappers in many places
- Significant implementation cost

### Option 4: Template Haskell-Style Macros 🚫 NOT AVAILABLE
PureScript doesn't have compile-time metaprogramming like Haskell's Template Haskell.

### Option 5: Status Quo + Documentation ✅ MINIMAL
Document the pattern well, accept that boilerplate is explicit and type-safe.

## Recommendation

**Accept Option 1** (current pattern) OR **Implement Option 2** (codegen).

**Reasoning**:
1. The current pattern works and is type-safe
2. Row types are architecturally important
3. Codegen could eliminate boilerplate without breaking changes
4. The newtype refactor cost doesn't justify the benefit

The `datum_` pattern is a pragmatic solution that:
- Isolates all `unsafeCoerce` calls in one place
- Provides clear documentation of available fields
- Works with the existing row type architecture
- Is easily searchable and greppable

## Files for Reference

- `notes/DATUM_ACCESSOR_CODEGEN.md` - Codegen approach details
- `notes/TYPED_DATUM_EXPLORATION.md` - Original design exploration
- `src/website/viz/Spago/Model.purs:72-171` - Example of datum_ pattern
- `src/website/viz/Spago/Unsafe.purs:14-16` - unboxD3SimNode function

## Lessons Learned

1. **Type system constraints matter**: Always check if existing code patterns are compatible with proposed solutions
2. **Row types vs newtypes trade-off**: PureScript's row polymorphism is powerful but limits certain typeclass patterns
3. **Pragmatic solutions win**: Sometimes explicit boilerplate is better than fighting the type system
4. **Test early with real code**: Our test file used type aliases too, missing the real-world blocker
