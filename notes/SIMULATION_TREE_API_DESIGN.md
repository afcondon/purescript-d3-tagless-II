# Simulation-Friendly Tree API Design

## What We Achieved

We successfully created LesMisGUPSimple.purs (246 LOC) demonstrating declarative GUP with force simulations:

```purescript
-- Define ONCE how nodes appear/update/exit (40 LOC)
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree scene =
  T.sceneNestedJoin "nodeElements" "circle"
    [scene]
    (_.nodes >>> map KeyedNode)
    (\(KeyedNode node) -> T.elem Circle [...])
    { enterBehavior: Just {...}
    , updateBehavior: Just {...}
    , exitBehavior: Just {...}
    }

-- Update is just 12 LOC!
updateNodes :: Array LesMisSimNode -> (LesMisSimNode -> String) -> D3v2SimM () LesMisSimNode Unit
updateNodes newNodes colorFn = do
  { nodes: updatedNodes } <- update { nodes: Just newNodes, ... }
  nodesGroup <- select "#nodes"
  let scene = SceneData { nodes: updatedNodes, nodeColor: colorFn }
  _ <- renderTree nodesGroup (createNodesTree scene)  -- Magic happens here!
  pure unit
```

**The power**: Just call `renderTree` with new data and SceneNestedJoin handles enter/update/exit automatically!

## Current Pain Points

### 1. Ord Constraint
`renderTree` requires `Ord datum` even though SceneNestedJoin only needs Ord for the decomposed inner data (KeyedNode), not the outer data (SceneData).

**Workaround**: Dummy Ord instance for SceneData
```purescript
instance Ord SceneData where compare _ _ = EQ
```

### 2. Newtype Boilerplate
Need `KeyedNode` wrapper just to provide Ord by ID:
```purescript
newtype KeyedNode = KeyedNode LesMisSimNode
instance Eq KeyedNode where ...
instance Ord KeyedNode where ...
```

### 3. Type Coercion
SceneNestedJoin internally works with KeyedNode, but returns Selection SceneData. Need unsafeCoerce to get tick functions to work:
```purescript
let nodeCirclesKeyed = unsafeCoerce nodeCircles :: _ SBound _ KeyedNode
```

## The Big Question: If We Were Designing For Simulations From Scratch?

### Option A: Simulation-Specific Tree API

Create `SimulationTree` parallel to `Tree` that:
- **Takes a key function** instead of requiring Ord
- **Returns properly-typed selections** (no coercion needed)
- **Has simulation-aware join** that understands node filtering

```purescript
-- Hypothetical API
module PSD3v2.VizTree.SimulationTree where

-- Key function replaces Ord requirement
type KeyFn datum key = datum -> key

-- Simulation-friendly join
simJoin
  :: forall outerDatum innerDatum key
   . Eq key
  => String                                    -- Name
  -> String                                    -- Element type
  -> Array outerDatum                          -- Scene data
  -> (outerDatum -> Array innerDatum)          -- Decompose
  -> KeyFn innerDatum key                      -- Key function (not Ord!)
  -> (innerDatum -> SimulationTree innerDatum) -- Template
  -> { enterBehavior :: ..., updateBehavior :: ..., exitBehavior :: ... }
  -> SimulationTree outerDatum

-- Returns selections with correct datum types
renderSimTree
  :: forall datum key
   . Eq key
  => D3v2Selection_  SEmpty Element Unit
  -> SimulationTree datum
  -> D3v2SimM () datum (Map String (D3v2Selection_ SBound Element datum))
```

**Benefits**:
- No Ord needed - just Eq on keys
- No newtype wrappers - keys extracted by function
- Proper typing - no unsafeCoerce
- Simulation-specific operations can be first-class

**Drawbacks**:
- Code duplication with Tree
- Two parallel APIs to maintain
- Users need to know which to use when

### Option B: Make SelectionM/SimulationM Independent

Don't extend SelectionM with SimulationM. Instead:

```purescript
-- Separate capability stacks
class SelectionM m where ...  -- For static visualizations
class SimulationM m where ... -- For force simulations (includes selection ops)

-- Simulation has its own tree type
data SimTree datum
  = SimNode { ... }
  | SimJoin { keyFn :: datum -> key, ... }  -- Key function built-in
```

**Benefits**:
- Clean separation of concerns
- Each optimized for its use case
- No compromises for backwards compatibility

**Drawbacks**:
- Bigger change - breaks existing code
- Learning curve - users need to understand two systems

### Option C: Enhance Current Tree API

Keep single Tree API but make it more flexible:

```purescript
-- Add optional key function to joins
sceneNestedJoinWithKey
  :: forall outerDatum innerDatum key
   . Eq key
  => String
  -> String
  -> Array outerDatum
  -> (outerDatum -> Array innerDatum)
  -> (innerDatum -> key)                      -- Key function
  -> (innerDatum -> Tree innerDatum)
  -> { enterBehavior :: ..., ... }
  -> Tree outerDatum

-- Make renderTree less demanding
renderTreeWithKey
  :: forall parent parentDatum datum key
   . Eq key  -- Only need Eq, not Ord!
  => (datum -> key)
  -> Selection SEmpty parent parentDatum
  -> Tree datum
  -> Effect (Map String (Selection SBound Element datum))
```

**Benefits**:
- Single unified API
- Backwards compatible (add new functions)
- Addresses key pain points

**Drawbacks**:
- API surface grows
- May not go far enough

### Option D: Library-Provided KeyBy Wrapper

Provide generic keying utility:

```purescript
-- In library
newtype KeyBy key datum = KeyBy datum

keyBy :: forall datum key. Eq key => (datum -> key) -> datum -> KeyBy key datum
keyBy _ d = KeyBy d

instance (Eq key) => Eq (KeyBy key datum) where
  eq (KeyBy a) (KeyBy b) = extractKey a == extractKey b

instance (Ord key) => Ord (KeyBy key datum) where
  compare (KeyBy a) (KeyBy b) = compare (extractKey a) (extractKey b)

-- Usage
createNodesTree scene =
  T.sceneNestedJoin "nodeElements" "circle"
    [scene]
    (_.nodes >>> map (keyBy _.id))  -- No custom newtype needed!
    (\(KeyBy node) -> ...)
    { ... }
```

**Benefits**:
- Minimal library change
- Reusable across visualizations
- Removes boilerplate

**Drawbacks**:
- Still requires Ord on outer data
- Doesn't solve coercion issue

## Recommendation

**Start with Option D + Option C (hybrid approach)**:

1. **Immediate (Option D)**: Add `KeyBy` wrapper to library - removes newtype boilerplate
2. **Near-term (Option C)**: Add `*WithKey` variants that only require `Eq key` instead of `Ord datum`
3. **Long-term (evaluate)**: Monitor if Option A (SimulationTree) becomes necessary

This gives us:
- Quick wins (Option D ready now)
- Path forward (Option C is incremental)
- Room to grow (Option A if needed)

## Key Insight

The 12-line update function proves the declarative pattern works! The friction is **all in the type system**, not the conceptual model. The goal is to make the types match the elegant mental model.

## What Code-Explorer Had

Looking at code-explorer, it likely used:
- **MiseEnScene pattern** - scene configs with data filters
- **Declarative scene switching** - "scene.nodes = filterNodes(...)"
- **Force library management** - declarative force activation/deactivation

The missing piece was TreeAPI + SceneNestedJoin making GUP declarative. We've now closed that loop!

## Next Steps

1. ✅ Implement `KeyBy` wrapper in library
2. ✅ Add `sceneNestedJoinWithKey` variant
3. Test with LesMisGUPSimple refactored to use new APIs
4. Document pattern for other visualizations
5. Consider SimulationTree if patterns emerge that can't be expressed
