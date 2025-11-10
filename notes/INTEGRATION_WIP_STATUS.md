# Integration WIP Status: Phantom Types + Pure Hierarchy Layouts

## What We've Accomplished

### Successfully Rebased ✅
- Rebased `feature/phantom-type-selection` onto `feature/pure-hierarchy-layouts`
- Created new integration branch: `feature/integrate-phantom-types-and-hierarchy`
- Only conflict was generated `docs/bundle.js` (resolved automatically)

### Core Library Integration ✅
All core library files now use phantom types:

1. **Alternative Interpreters** - Made phantom-type compatible:
   - `MermaidAST.purs`: Created `newtype MermaidSelection d = MermaidSelection NodeID`
   - `MetaTree.purs`: Created `newtype MetaTreeSelection d = MetaTreeSelection NodeID`
   - `String.purs`: Created `newtype StringSelection d = StringSelection String`
   - All interpreters now properly wrap/unwrap selection types

2. **Attributes** - Fixed FFI bridges:
   - `Sugar.purs`: Fixed `transform'`, `autoBox`, `transform` to use `d -> String` instead of `Datum_ -> String`
   - `Hierarchical.purs`: Fixed `verticalLink`, `verticalClusterLink` with `unsafeCoerce` for FFI functions
   - Added `unsafeCoerce` where needed to bridge typed phantom types with untyped FFI

3. **Selection Functions** - Updated for phantom type changes:
   - `FFI.purs`: Fixed `d3DataWithKeyFunction_` signature to properly change phantom type from `D3Selection_ d1` to `D3Selection_ d2`
   - `Selection/Functions.purs`: Updated `selectionUpdateJoin` to use corrected FFI signature

4. **Component Updates**:
   - `InterpretersDemo.purs`: Updated to use `MermaidSelection`, `MetaTreeSelection`, `StringSelection`
   - `MermaidAST.purs` (component): Parameterized all types with phantom `d`

## Current Blockers (29 errors)

### Force/Simulation Type Parameterization

The main remaining issue is that `Force` and related simulation types need phantom type parameters:

**What we did**:
```purescript
-- Before:
newtype Force = Force { attributes :: Array ChainableF, ... }
newtype ChainableF = ForceT AttributeSetter

// After:
newtype Force d = Force { attributes :: Array (ChainableF d), ... }
newtype ChainableF d = ForceT (AttributeSetter d)
```

**Why this causes errors**:
- `Force` is used throughout the simulation system
- All lenses need updating: `Lens' Force Label` → `Lens' (Force d) Label`
- All force constructors need phantom type parameter
- Simulation state types need phantom parameters
- ~29 cascading type errors from this change

**Files affected** (need phantom type updates):
- `src/lib/PSD3/Internal/Simulation/Types.purs` - Lenses, Show instances, force types
- `src/lib/PSD3/Internal/Simulation/Functions.purs` - Force creation functions
- `src/lib/PSD3/Simulation/Update.purs` - Simulation update logic
- `src/lib/PSD3/Capabilities/Simulation.purs` - SimulationM type class
- All visualization files using simulations (Spago, LesMis, etc.)

## Next Steps

### Phase 1: Fix Simulation Types (Current blocker)
1. Parameterize all Force-related types with phantom `d`
2. Update all lenses: `_name`, `_status`, `_filter`, etc.
3. Update force constructor functions
4. Update Show/Eq instances for parameterized types
5. Fix all force creation sites to provide phantom type

### Phase 2: Update Hierarchy Visualizations
Once simulation types compile, update hierarchy viz files:
- `TreeViz.purs`, `TreemapViz.purs`, `PackViz.purs`, `ClusterViz.purs`, `PartitionViz.purs`, `SunburstViz.purs`
- Add phantom type annotations: `root :: D3Selection_ Unit <- attach selector`
- Fix data joins: `circles :: D3Selection_ (TreeNode HierData) <- simpleJoin ...`

### Phase 3: Fix Data Binding Issue
Hierarchy viz currently use `appendTo` in loops instead of `simpleJoin`:
- **Problem**: DOM elements don't have `__data__` attached
- **Impact**: Breaks reselection, transitions, event handlers with datum access
- **Solution**: Convert to `simpleJoin` pattern with `getAllNodes` arrays

See: `notes/HIERARCHY_DATA_BINDING_TODO.md`

### Phase 4: Test & Verify
1. Build succeeds with zero errors
2. All hierarchy examples render in browser
3. Existing simulation examples (LesMis, Spago) still work
4. Type inference works for typed lambdas

### Phase 5: Enhancement (Optional)
Leverage phantom types to eliminate `datum_` boilerplate in hierarchy visualizations:
```purescript
-- Before:
datum_ = { x: _.x <<< unboxTreeNode, y: _.y <<< unboxTreeNode }
setAttributes circles [cx datum_.x, cy datum_.y]

// After (with phantom types):
circles :: D3Selection_ (TreeNode HierData) <- simpleJoin ...
setAttributes circles [
  cx (\(TreeNode n) -> n.x),  -- Type inference just works!
  cy (\(TreeNode n) -> n.y)
]
```

## Key Insights

### What Worked Well
1. **Rebase was clean**: Only one conflict in generated bundle.js
2. **Alternative interpreters**: Phantom wrappers work perfectly - simple pattern
3. **FFI bridges**: `unsafeCoerce` at FFI boundary is the right pattern for untyped JS functions
4. **Type-driven development**: Compiler errors guide us to every location that needs updating

### Challenges
1. **Simulation types are complex**: Force, ChainableF, Simulation all interconnected
2. **Cascading phantom parameters**: Once Force gets `d`, everything using it needs `d`
3. **Lenses need updating**: All `Lens' Force _` become `Lens' (Force d) _`

### Pattern for FFI Functions Returning Datum_ -> T
When FFI function returns `Datum_ -> String` but phantom types need `d -> String`:
```purescript
-- Pattern:
attribute = AttrT $ AttributeSetter "label" $ toAttr (unsafeCoerce ffiFunction_ :: d -> String)
```

This is safe because at runtime D3 guarantees datum correspondence - the phantom type just tracks it at compile time.

## Files Modified This Session

Core library:
- `src/lib/PSD3/Internal/Attributes/Sugar.purs`
- `src/lib/PSD3/Internal/FFI.purs`
- `src/lib/PSD3/Internal/Hierarchical.purs`
- `src/lib/PSD3/Internal/Selection/Functions.purs`
- `src/lib/PSD3/Internal/Simulation/Types.purs`

Interpreters:
- `src/lib/PSD3/Interpreter/MermaidAST.purs`
- `src/lib/PSD3/Interpreter/MetaTree.purs`
- `src/lib/PSD3/Interpreter/String.purs`

Components:
- `src/website/Component/Shared/MermaidAST.purs`
- `src/website/Component/Understanding/InterpretersDemo.purs`

Documentation:
- `notes/PHANTOM_TYPES_HIERARCHY_INTEGRATION.md` (analysis)
- `notes/HIERARCHY_DATA_BINDING_TODO.md` (issue tracker)
- `notes/INTEGRATION_WIP_STATUS.md` (this file)

## Commit Strategy

This WIP commit captures:
1. Successful rebase of phantom types onto hierarchy layouts
2. All alternative interpreter updates (MermaidAST, MetaTree, String)
3. Core attribute/selection function fixes for phantom types
4. Partial simulation type updates (Force/ChainableF parameterized)

Next commit will:
1. Complete simulation type parameterization
2. Fix all 29 remaining type errors
3. Get to a building state

## Estimated Remaining Work

- **Phase 1 (Simulation types)**: 2-3 hours - systematic but mechanical
- **Phase 2 (Hierarchy viz updates)**: 1-2 hours - straightforward type annotations
- **Phase 3 (Data binding fix)**: 2-3 hours - refactor from appendTo loops to simpleJoin
- **Phase 4 (Testing)**: 1-2 hours - verify all examples work
- **Phase 5 (Enhancement)**: Optional, 2-4 hours - leverage typed lambdas

**Total**: 6-10 hours remaining (not counting optional enhancement)

## Success Metrics

When complete, we will have:
- ✅ Pure PureScript hierarchy layouts (Tree, Treemap, Pack, Cluster, Partition, Sankey)
- ✅ Phantom type system tracking datum types through entire DSL
- ✅ Type inference for attribute lambdas (no datum_ boilerplate needed)
- ✅ All interpreters working (D3, MermaidAST, MetaTree, String)
- ✅ Proper data binding with `__data__` for reselection/transitions
- ✅ Zero type errors, all examples rendering correctly
- ✅ Foundation for future cleanup: remove obsolete D3 FFI hierarchy code

This integration brings together two major advances and sets us up for a massive cleanup pass once everything is stable.
