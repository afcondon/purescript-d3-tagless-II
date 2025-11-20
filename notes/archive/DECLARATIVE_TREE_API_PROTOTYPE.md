# Declarative Tree API Prototype

**Date**: 2025-01-15
**Branch**: `feature/declarative-tree-api`
**Status**: Prototype Complete - Basic Implementation Working

## Summary

Successfully prototyped a declarative tree-building API for PSD3v2 that allows users to specify DOM structure as data rather than imperative sequences of `appendChild` calls.

## The Insight

**The DOM structure IS the type signature of your visualization.**

Just as a type signature tells you what a function does, the DOM structure should tell you what your visualization looks like - **at a glance**.

## What We Built

### 1. Tree Data Structure (`PSD3v2.VizTree.Tree`)

A simple ADT for representing DOM trees:

```purescript
data Tree datum
  = Node (TreeNode datum)
  | Join { name :: Maybe String, key :: String, joinData :: Array datum, template :: Tree datum }

type TreeNode datum =
  { name :: Maybe String
  , elemType :: ElementType
  , attrs :: Array (Attribute datum)
  , children :: Array (Tree datum)
  }
```

**Design Philosophy**: Keep runtime representation simple. Type safety can be added later through builder types.

### 2. Smart Constructors

Ergonomic functions for building trees:

```purescript
-- Named elements (will be in the returned selections map)
named :: String -> ElementType -> Array (Attribute datum) -> Tree datum

-- Anonymous elements (won't be in selections)
elem :: ElementType -> Array (Attribute datum) -> Tree datum

-- Add children
withChild :: Tree datum -> Tree datum -> Tree datum
withChildren :: Tree datum -> Array (Tree datum) -> Tree datum

-- Operators
infixl 6 withChild as >:
infixl 5 beside as +:
```

### 3. SelectionM Extension

Added `renderTree` to the SelectionM typeclass:

```purescript
class Monad m <= SelectionM sel m where
  -- ... existing methods ...

  renderTree
    :: forall parent datum
     . sel SEmpty parent datum
    -> Tree datum
    -> m (Map String (sel SBound Element datum))
```

**Key Design Decision**: Returns a `Map String Selection` rather than trying to derive a typed record. This keeps implementation simple while still providing named access.

### 4. Interpreter Implementation

Implemented `renderTree` in both `D3v2M` and `D3v2SimM` interpreters:

- Walks the tree recursively
- Creates DOM elements using `appendChild`
- Collects named nodes into a Map
- Handles both regular nodes and data joins (joins are stubbed for prototype)

## Example Usage

### Before (Imperative Style)

```purescript
container <- select "#viz"
svg <- appendChild SVG [width 800, height 600] container
zoomGroup <- appendChild Group [class_ "zoom"] svg
linksGroup <- appendChild Group [id_ "links"] zoomGroup
nodesGroup <- appendChild Group [id_ "nodes"] zoomGroup
```

The structure is **scattered** across multiple lines. Hard to see the tree shape.

### After (Declarative Style)

```purescript
container <- select "#viz"

let tree =
      T.named "svg" SVG [width 800, height 600]
        `T.withChildren`
          [ T.named "zoom" Group [class_ "zoom"] `T.withChildren`
              [ T.named "links" Group [id_ "links"]
              , T.named "nodes" Group [id_ "nodes"]
              ]
          ]

selections <- renderTree container tree

-- Access named selections
let svg = Map.lookup "svg" selections
let zoom = Map.lookup "zoom" selections
```

The structure is **immediately visible**. The tree shape is clear.

## What Works

✅ Basic tree structure definition
✅ Named and anonymous nodes
✅ Recursive rendering
✅ Map-based selection return
✅ Compiles and type-checks
✅ Simple example (SimpleTreeExample.purs)

## What Doesn't Work Yet

❌ Data joins (stubbed in Join case)
❌ Emmet-style operators (`>:`, `+:`) - would require complex type-level machinery
❌ Automatic record type derivation - returns Map instead
❌ Enter/update/exit for data joins
❌ Complex nested structures with data at different levels

## Design Tradeoffs

### Chose Simplicity Over Type Safety (For Now)

**Option A**: Type-level machinery to derive record types
- Pros: Perfect type safety, IDE autocomplete
- Cons: Complex implementation, incomprehensible errors, high maintenance burden

**Option B** (Chosen): Simple Map-based return
- Pros: Easy to implement, clear code, good error messages
- Cons: Runtime errors if you misspell a name, no IDE autocomplete

**Reasoning**: Get it working first, then incrementally add type safety if needed.

### Kept Tree Structure Separate From Rendering

The `Tree` type is just data - it doesn't know anything about D3 or rendering. This means:
- ✅ Could render the same tree structure to different targets (SVG, Canvas, Virtual DOM, HTML string)
- ✅ Could inspect/transform trees before rendering
- ✅ Could serialize trees (for debugging, testing, etc.)
- ✅ Clean separation of concerns

### Deferred Data Joins

Data joins are tricky because they change the datum type:

```purescript
-- Parent has datum type A
parent :: Selection SEmpty Element A

-- But children need datum type B
join :: Tree B

-- How to handle this type change?
```

For the prototype, we stubbed this out. A full implementation would need to:
1. Allow heterogeneous trees (different datum types at different levels)
2. Thread type parameters through carefully
3. Handle enter/update/exit phases properly

This is solvable but adds complexity. Prototype proves the core concept works.

## Files Created/Modified

### New Files
- `src/lib/PSD3v2/VizTree/Tree.purs` - Core tree structure and smart constructors
- `src/lib/PSD3v2/VizTree/Types.purs` - Sketch of type-safe version (commented out)
- `src/lib/PSD3v2/VizTree/Core.purs` - Alternative design (not used)
- `src/website/Viz/TreeAPI/SimpleTreeExample.purs` - Working example
- `notes/VIZTREE_PROTOTYPE_DESIGN.md` - Design exploration
- `notes/DECLARATIVE_TREE_API_PROTOTYPE.md` - This file

### Modified Files
- `src/lib/PSD3v2/Capabilities/Selection.purs` - Added `renderTree` method
- `src/lib/PSD3v2/Selection/Operations.purs` - Implemented `renderTree`
- `src/lib/PSD3v2/Interpreter/D3v2.purs` - Added `renderTree` to both instances

## Validation of Final Tagless Pattern

This prototype **beautifully validates** the tagless final approach:

1. **Extended DSL Without Breaking Changes**
   - Added `renderTree` to SelectionM
   - All existing code still works
   - New method available to all interpreters

2. **Clean Layering**
   - High-level declarative API (`renderTree`)
   - Implemented using low-level imperative API (`appendChild`)
   - Both available, user chooses appropriate level

3. **Interpreter Independence**
   - Same implementation works for D3v2M and D3v2SimM
   - Could add more interpreters easily
   - Tree structure is interpreter-agnostic

4. **Incremental Adoption**
   - Can mix imperative and declarative styles
   - No need to rewrite existing code
   - Use `renderTree` where it helps, `appendChild` where it doesn't

## Next Steps (If We Continue This Direction)

### Short Term
1. Fix DrawV2.purs to use the new API (currently has type error)
2. Create NestedMatrix example with actual nested elements
3. Test with more complex structures

### Medium Term
4. Implement data joins properly
5. Handle heterogeneous trees (different datum types)
6. Add helper functions for common patterns

### Long Term
7. Explore type-safe version with record return types
8. Investigate builder pattern for better inference
9. Add validation/linting for tree structures

## Open Questions

1. **Should we add operators?**
   - `>:` and `+:` would make syntax more concise
   - But type inference might be tricky
   - Alternative: Keep named functions, they're clear enough

2. **Map vs Record return type?**
   - Map: simple, runtime errors on typos
   - Record: type-safe, but needs type-level machinery
   - Could we provide both? `renderTree` returns Map, `renderTree'` returns Record?

3. **How to handle data joins elegantly?**
   - Current stub isn't usable
   - Need to support heterogeneous datum types
   - Should joins be special, or just another tree node?

4. **Should we integrate with existing renderData?**
   - `renderData` handles enter/update/exit well
   - Could tree API use `renderData` internally?
   - Or replace it entirely?

5. **Performance implications?**
   - Creating tree structure first, then rendering
   - vs. rendering directly as you build
   - Probably negligible, but worth measuring

## Conclusion

The declarative tree API prototype **works** and demonstrates that:

1. ✅ DOM structures can be specified as data
2. ✅ This is more readable than imperative chains
3. ✅ Tagless final naturally supports both styles
4. ✅ Implementation is straightforward
5. ✅ The pattern is extensible

**Recommendation**: This is promising enough to explore further. The next step is to test it with real examples (code-explorer, nested matrix) to see where the rough edges are.

The fact that it compiles and the SimpleTreeExample works proves the core concept is sound. Data joins and type safety are refinements, not blockers.
