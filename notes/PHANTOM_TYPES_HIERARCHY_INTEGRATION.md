# Phantom Types + Pure PureScript Hierarchy Layouts Integration Analysis

## Current State

We have two feature branches with different advances:

### Branch: `feature/phantom-type-selection` (52bed46)
**Achievement**: Phantom type parameters for typed datum access

**Key changes**:
- `D3Selection_ :: Type` → `D3Selection_ :: Type -> Type`
- Tracks datum type through DSL via phantom type parameter
- Eliminates 15-30 lines of `datum_` boilerplate per visualization
- Type inference works: `circles :: D3Selection_ Int` → compiler infers `\d -> d :: Int`
- All attribute/selection operations parameterized by datum type `d`

**Blocker discovered**: Row types cannot have typeclass instances (documented in TYPED_DATUM_BLOCKED.md)
- Can't use `HasDatum` typeclass with row type aliases
- Not a problem for phantom approach (doesn't need instances)
- Blocker was for a different approach that was abandoned

### Branch: `feature/pure-hierarchy-layouts` (5dc1e88)
**Achievement**: Pure PureScript implementations of D3 hierarchy layouts

**Key additions**:
- `PSD3.Layout.Hierarchy.Tree` - Reingold-Tilford tree layout
- `PSD3.Layout.Hierarchy.Treemap` - Rectangular treemap
- `PSD3.Layout.Hierarchy.Pack` - Circle packing
- `PSD3.Layout.Hierarchy.Cluster` - Dendrogram/cluster layout
- `PSD3.Layout.Hierarchy.Partition` - Icicle/sunburst base
- `PSD3.Layout.Hierarchy.Core` - Shared hierarchy utilities
- `PSD3.Layout.Sankey` - Pure Sankey diagram layout

**All layouts**:
- Match D3's algorithms exactly (verified with instrumented D3 tests)
- No D3 FFI dependencies (pure PureScript)
- Working visualizations in browser examples gallery

### Common Ancestor: `e6ecb2c` (Document typed datum blocker)

## Integration Challenge

The pure hierarchy layouts currently use the OLD type system:
```purescript
-- Current hierarchy viz code (feature/pure-hierarchy-layouts):
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>  -- ← OLD: D3Selection_ :: Type
  HierData -> Selector D3Selection_ -> m Unit
```

They need to be updated to:
```purescript
-- Target with phantom types:
draw :: forall m datum.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>  -- ← NEW: D3Selection_ :: Type -> Type
  HierData -> Selector (D3Selection_ datum) -> m Unit
```

## Key Integration Points

### 1. SelectionM Type Class Signature

**Current (hierarchy branch)**:
```purescript
class (Monad m) <= SelectionM selection m where
  appendTo :: selection -> Element -> Array SelectionAttribute -> m selection
  simpleJoin :: selection -> Element -> Array datum -> (Datum_ -> Index_) -> m selection
```

**Phantom types version**:
```purescript
class (Monad m) <= SelectionM selection m where
  appendTo :: forall d. selection d -> Element -> Array (SelectionAttribute d) -> m (selection d)
  simpleJoin :: forall d datum. selection d -> Element -> Array datum -> (Datum_ -> Index_) -> m (selection datum)
```

Key difference: `simpleJoin` returns `selection datum` (NEW phantom type) not `selection` (OLD type)

### 2. Attribute Types

**Current (hierarchy branch)**:
```purescript
type SelectionAttribute = SelectionAttribute_
-- No parameterization
```

**Phantom types version**:
```purescript
type SelectionAttribute d = SelectionAttribute_ d
-- Parameterized by datum type
```

### 3. Visualization Draw Functions

**Current hierarchy viz pattern**:
```purescript
draw :: forall m.
  SelectionM D3Selection_ m =>
  HierData -> Selector D3Selection_ -> m Unit
draw treeData selector = do
  root <- attach selector
  svg <- appendTo root Svg [width 800.0, height 600.0]
  -- ...
```

**Needs to become**:
```purescript
draw :: forall m.
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw treeData selector = do
  root :: D3Selection_ Unit <- attach selector
  svg :: D3Selection_ Unit <- appendTo root Svg [width 800.0, height 600.0]
  -- ...
```

Or with typed data:
```purescript
draw :: forall m.
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw treeData selector = do
  root :: D3Selection_ Unit <- attach selector
  svg :: D3Selection_ Unit <- appendTo root Svg [...]

  let nodes = getAllNodes treeLayout
  circles :: D3Selection_ (TreeNode HierData) <- simpleJoin svg Circle nodes keyFn
  setAttributes circles [
    cx (\(TreeNode n) -> n.x),  -- ← Typed lambda! No datum_ needed!
    cy (\(TreeNode n) -> n.y),
    fill (\(TreeNode n) -> depthColor n.depth)
  ]
```

## Rebase Strategy

### Option 1: Rebase phantom-type-selection onto feature/pure-hierarchy-layouts ✅ RECOMMENDED

**Approach**:
```bash
git checkout feature/phantom-type-selection
git rebase feature/pure-hierarchy-layouts
```

**Pros**:
- Hierarchy layouts are the "base" - they're self-contained, don't depend on phantom types
- Phantom types are the "enhancement" - they modify the type system
- Natural dependency direction: phantom types BUILD ON TOP OF hierarchy layouts
- Commits stay in logical order: first add features, then enhance type system

**Cons**:
- More conflicts to resolve (phantom changes touch many files)
- Need to update hierarchy viz files to use phantom types after rebase

**Conflict areas**:
1. `src/lib/PSD3/Internal/Types.purs` - `D3Selection_` definition
2. `src/lib/PSD3/Capabilities/Selection.purs` - SelectionM signature
3. `src/lib/PSD3/Internal/Attributes/*.purs` - Attribute parameterization
4. `src/lib/PSD3/Interpreter/D3.purs` - Interpreter implementation
5. All viz files (`src/website/Viz/**/*.purs`) - Need phantom type annotations

### Option 2: Rebase feature-pure-hierarchy-layouts onto phantom-type-selection ❌ NOT RECOMMENDED

**Approach**:
```bash
git checkout feature/pure-hierarchy-layouts
git rebase feature/phantom-type-selection
```

**Pros**:
- Fewer files to update (just the new hierarchy files)
- Hierarchy layouts would be written with phantom types from the start

**Cons**:
- Conceptually backwards: hierarchy layouts don't NEED phantom types, but phantom types CHANGE them
- Harder to review: hierarchy layout logic mixed with phantom type system changes
- If phantom types need refinement, harder to separate concerns

### Option 3: Merge both into main, then create new branch ❌ LOSES HISTORY

**Approach**:
```bash
git checkout main
git merge feature/pure-hierarchy-layouts
git merge feature/phantom-type-selection  # Will have conflicts
```

**Pros**:
- Both features available on main
- Clear integration point

**Cons**:
- Loses clean rebase history
- Conflicts still need resolution
- Harder to bisect if issues arise

## Recommended Integration Steps

### Phase 1: Rebase (Mechanical)
```bash
# Save current work
git checkout feature/pure-hierarchy-layouts
git branch feature/pure-hierarchy-layouts-backup

# Rebase phantom types onto hierarchy layouts
git checkout feature/phantom-type-selection
git rebase feature/pure-hierarchy-layouts

# Expect conflicts in:
# - src/lib/PSD3/Internal/Types.purs
# - src/lib/PSD3/Capabilities/Selection.purs
# - src/lib/PSD3/Internal/Attributes/Instances.purs
# - src/lib/PSD3/Interpreter/D3.purs
# - src/website/Viz/*/*.purs (many files)
```

### Phase 2: Resolve Core Library Conflicts
1. `Types.purs`: Keep phantom type version of `D3Selection_`
2. `Selection.purs`: Keep phantom type parameterization
3. `Attributes/*.purs`: Keep phantom type `ToAttr` instances
4. `Interpreter/D3.purs`: Keep phantom type interpreter implementation

**Principle**: Phantom type versions WIN for all core library files

### Phase 3: Update Hierarchy Visualizations
For each hierarchy viz file (`TreeViz.purs`, `TreemapViz.purs`, etc.):

**Before**:
```purescript
draw :: forall m.
  SelectionM D3Selection_ m =>
  HierData -> Selector D3Selection_ -> m Unit
```

**After**:
```purescript
draw :: forall m.
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
```

Add type annotations where needed:
```purescript
root :: D3Selection_ Unit <- attach selector
svg :: D3Selection_ Unit <- appendTo root Svg [...]
circles :: D3Selection_ (TreeNode HierData) <- simpleJoin svg Circle nodes keyFn
```

### Phase 4: Leverage Phantom Types in Hierarchy Layouts
**OPPORTUNITY**: Eliminate datum_ boilerplate in hierarchy visualizations!

**Current pattern** (some hierarchy viz files use this):
```purescript
datum_ = {
  x: _.x <<< unboxTreeNode,
  y: _.y <<< unboxTreeNode,
  depth: _.depth <<< unboxTreeNode
}
setAttributes circles [cx datum_.x, cy datum_.y]
```

**With phantom types**:
```purescript
-- No datum_ needed!
circles :: D3Selection_ (TreeNode HierData) <- simpleJoin ...
setAttributes circles [
  cx (\(TreeNode n) -> n.x),
  cy (\(TreeNode n) -> n.y),
  fill (\(TreeNode n) -> depthColor n.depth)
]
```

### Phase 5: Test and Verify
1. `spago build` - Ensure all type errors resolved
2. `npm run bundle` - Build browser bundle
3. Test each hierarchy example in browser:
   - Tree (vertical/horizontal/radial)
   - Treemap
   - Pack (circle packing)
   - Cluster (dendrogram)
   - Partition (icicle)
   - Sunburst
   - Sankey
4. Verify typed lambdas work with inference

## Potential Challenges

### Challenge 1: Phantom Type Propagation
**Issue**: Every function that touches selections needs phantom parameter

**Example**:
```purescript
-- Before:
helper :: D3Selection_ -> D3M D3Selection_

-- After:
helper :: forall d. D3Selection_ d -> D3M (D3Selection_ d)
```

**Solution**: Systematic update, use type holes to guide

### Challenge 2: Datum Type Annotations
**Issue**: Type inference may fail in complex cases

**Example**:
```purescript
-- May need explicit annotation:
circles :: D3Selection_ (TreeNode HierData) <- simpleJoin svg Circle nodes keyFn
```

**Solution**: Add annotations where compiler requests them

### Challenge 3: Newtype Wrapping
**Issue**: Hierarchy types use newtypes (`TreeNode`, `PackNode`, etc.)

**Example**:
```purescript
newtype TreeNode a = TreeNode { x :: Number, y :: Number, data_ :: a, ... }
```

**Advantage**: Newtypes WORK with phantom types (unlike row types)!
```purescript
setAttributes circles [
  cx (\(TreeNode n) -> n.x)  -- Pattern match newtype directly
]
```

**Solution**: This is actually IDEAL for phantom types - no issues expected

### Challenge 4: Mixed Datum Types in Single Viz
**Issue**: Some viz have multiple data types (nodes + links)

**Example**:
```purescript
nodes :: D3Selection_ NodeType <- simpleJoin ...
links :: D3Selection_ LinkType <- simpleJoin ...
```

**Solution**: Phantom types handle this naturally - different selections have different phantom types

## Success Criteria

1. ✅ All hierarchy layouts compile with phantom types
2. ✅ All hierarchy examples render correctly in browser
3. ✅ Type inference works for hierarchy datum lambdas
4. ✅ No runtime behavior changes (phantom types are compile-time only)
5. ✅ Hierarchy layouts can leverage typed lambdas (optional enhancement)

## Timeline Estimate

- **Phase 1 (Rebase)**: 30-60 minutes (mechanical, expect conflicts)
- **Phase 2 (Core conflicts)**: 30-60 minutes (accept phantom type versions)
- **Phase 3 (Update viz signatures)**: 60-90 minutes (7 hierarchy viz files + Sankey)
- **Phase 4 (Leverage typed lambdas)**: Optional, 60-120 minutes (cleanup/enhancement)
- **Phase 5 (Test)**: 30-60 minutes (verify all examples work)

**Total**: 3-6 hours depending on complexity of conflicts

## Recommendation

**Proceed with Option 1**: Rebase `feature/phantom-type-selection` onto `feature/pure-hierarchy-layouts`

**Rationale**:
1. Hierarchy layouts are self-contained features (don't depend on phantom types)
2. Phantom types are enhancements to the type system (apply to everything including hierarchies)
3. Natural dependency direction: phantom types enhance hierarchy layouts
4. Cleaner history: features first, then type system enhancement
5. Easier to test: can verify hierarchy layouts work, then add phantom types

**Next Steps**:
1. Create new branch from phantom-type-selection
2. Rebase onto feature/pure-hierarchy-layouts
3. Systematically resolve conflicts (core library: keep phantom versions)
4. Update hierarchy viz signatures with phantom types
5. Test thoroughly
6. Optional: Enhance hierarchy viz to use typed lambdas (eliminate datum_ boilerplate)
