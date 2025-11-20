# SimulationM2 Update API: Learnings from Interactive Graph Implementation

**Date**: 2025-10-27
**Context**: Building click-to-filter functionality in Code Atlas Interactive tab

## My Initial Chain of Reasoning (What Went Wrong)

1. **Initial approach**: I saw the onClick handler needed to run in Effect (from `mkEffectFn3`), but the `update` function requires the full monadic context (SimulationM2).

2. **Problem identification**: Type error showed `update` couldn't be called from within Effect - it needs `SimulationM2 t2 Effect` but we're in plain `Effect`.

3. **My "solution"**: Since I couldn't call `update` from Effect, I thought "I'll just create an FFI function that directly manipulates the D3 simulation", following the pattern of `unpinAllNodes_`.

4. **Why this was wrong**: I was working **against** the library design instead of **with** it. The whole point of SimulationM2 is to provide a declarative update API, and I was bypassing it by dropping down to imperative FFI!

## The Correct Pattern (from Spago/CodeExplorer)

Looking at the Spago code in `src/website/Viz/Spago/Draw.purs`, the correct pattern is:

1. **Don't use Effect callbacks at all!** The `update` function IS meant to be called from within the monadic drawing context.

2. **Filter the data arrays BEFORE calling update**, not inside an event handler.

3. **Let the update API handle everything** - it will:
   - Merge the new filtered data with existing simulation state
   - Swizzle links properly
   - Engage/disengage forces as needed
   - Return enhanced data ready for DOM binding

4. **Use the General Update Pattern** with `updateJoin` to handle enter/update/exit transitions on the DOM.

### Code Comparison

```purescript
-- ❌ DON'T: Try to call update from an event handler
onClick = mkEffectFn3 \event datum this -> do
  update { ... }  -- This won't work! Wrong monad!

-- ✅ DO: Event handlers should be simple and trigger re-rendering
-- Filter your data and call your drawing function again
let filteredNodes = Array.filter predicate allNodes
    filteredLinks = Array.filter linkPredicate allLinks

-- Call update from within the monadic context
enhanced <- update
  { nodes: Just filteredNodes
  , links: Just filteredLinks
  , activeForces: Just activeForces
  , config: Nothing
  , keyFn: keyIsID_
  }

-- Then use updateJoin for the General Update Pattern
node' <- updateJoin nodeGroup Group enhanced.nodes keyIsID_
-- Handle enter/update/exit...
```

### The Spago Pattern in Detail

From `D3.Viz.Spago.Draw.updateSimulation`:

```purescript
updateSimulation { nodes: Just nodesGroup, links: Just linksGroup } dataConfig attrs = do
  -- Step 1: Use update API to handle simulation complexity
  enhanced <- update
    { nodes: Just dataConfig.nodes
    , links: Just dataConfig.links
    , activeForces: Just dataConfig.activeForces
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Step 2: Open selections for DOM operations
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- Step 3: Apply General Update Pattern to nodes
  node' <- updateJoin node Group enhanced.nodes keyIsID_

  -- Enter: create new groups with children
  nodeEnter <- appendTo node'.enter Group enterAttrs
  _ <- appendTo nodeEnter Circle attrs.circles
  void $ appendTo nodeEnter Text attrs.labels

  -- Exit: remove old nodes
  setAttributes node'.exit [ remove ]

  -- Update: modify existing nodes
  setAttributes node'.update updateAttrs

  -- Merge enter and update selections
  mergedNodeSelection <- mergeSelections nodeEnter node'.update

  -- Step 4: Similar pattern for links...
  -- Step 5: Set up tick functions with merged selections
  addTickFunction "nodes" $ Step mergedNodeSelection [ transform' datum_.translateNode ]
  addTickFunction "links" $ Step mergedLinksShown [ x1 ..., y1 ..., x2 ..., y2 ... ]
```

### CodeExplorer's State-Based Pattern

CodeExplorer takes it further with a state-based approach:

1. Event callbacks trigger Halogen actions
2. Actions update state filters (e.g., `_chooseNodes .= filterPredicate`)
3. Call `runSimulation` which:
   - Reads current state filters
   - Applies filters to data
   - Calls the drawing function with filtered data
4. Drawing function uses `update` API as above

This separates concerns: state management in Halogen, rendering in D3 monad.

## What Would Make This More Intuitive

### 1. Better Documentation/Examples

Add a "Common Patterns" section showing:
- "How to filter nodes on click"
- "How to add/remove nodes dynamically"
- "How to change forces based on user interaction"

With clear ❌ DON'T and ✅ DO examples.

### 2. Clearer Type Errors

The error `No type class instance was found for SimulationM2 t2 Effect` is technically correct but doesn't guide users toward the solution. Could we:
- Add a custom type error for this common case?
- Suggest in docs that this error means "you're trying to call update from the wrong monad"

### 3. Better Naming

- `SimulationM` vs `SimulationM2` isn't immediately clear
- Consider: `SimulationM_Static` and `SimulationM_Dynamic`?
- Or: `SimulationInit` and `SimulationUpdate`?
- **TODO**: Rename after this feature is complete

### 4. Helper Utilities

Since the simulation config record is "entirely static, pure, manipulable" until fed to the interpreter, we could provide utilities like:

```purescript
-- Utility for filtering simulation data
filterSimulationData :: forall d r id.
  (d -> Boolean) ->        -- node predicate
  (D3Link id r -> Boolean) ->  -- link predicate
  SimulationData d id r ->
  SimulationData d id r

-- Utility for common click patterns
onNodeClick :: forall d id r.
  (NodeId -> SimulationData d id r -> SimulationData d id r) ->
  -- ... generates proper callback that triggers re-render
```

The interpreter hides all the state and sequencing complexity, so we should lean into that!

### 5. Scaffolding for Event Handlers

Provide a standard pattern/helper for "event triggers re-render with modified data":

```purescript
-- Something like:
makeInteractiveSimulation ::
  { initialData :: SimulationData d id r
  , onNodeClick :: Maybe (NodeId -> SimulationData -> SimulationData)
  , onNodeHover :: Maybe (NodeId -> Visual
Effects)
  -- etc
  } ->
  m Unit
```

## Key Insights

1. **The update API is declarative, not imperative**: You describe what data should be shown, it handles the how
2. **Event handlers should be thin**: Just trigger state changes or re-renders, don't manipulate D3 directly
3. **The General Update Pattern is powerful**: enter/update/exit handles all DOM transitions cleanly
4. **Separation of concerns**: Data transformation (pure) → update API (handles simulation) → DOM operations (visual)

## Modeling Swizzled vs Unswizzled Links in the Type System

**Context**: One source of confusion is that links change form during simulation initialization:
- **Unswizzled**: `D3Link id r` where `source` and `target` are IDs (String, Int, etc.)
- **Swizzled**: `D3LinkSwizzled` where `source` and `target` are actual node object references

Currently both are somewhat hidden behind `Datum_` which makes it easy to use the wrong form.

**Proposal**: Make these distinct foreign types:
```purescript
foreign import data D3Link_Unswizzled :: Type
foreign import data D3Link_Swizzled :: Type

-- The update API signature becomes clearer:
update ::
  { nodes :: Maybe (Array D3_SimulationNode)
  , links :: Maybe (Array D3Link_Unswizzled)  -- Input: IDs only
  , ...
  } ->
  m { nodes :: Array D3_SimulationNode
    , links :: Array D3Link_Swizzled         -- Output: with object references
    }
```

**Benefits**:
1. **Type safety**: Can't accidentally pass swizzled links where unswizzled are expected
2. **Self-documenting**: Function signatures make it clear what form is needed
3. **Clearer errors**: "Expected D3Link_Unswizzled but got D3Link_Swizzled" is very clear
4. **Easier to implement**: Since we've removed the complex loading sequence, this should be straightforward

**Implementation notes**:
- Both would still be opaque foreign types
- Conversion happens in the FFI during swizzling
- The PureScript type system ensures you can't mix them up
- Helper functions could convert between forms if needed (e.g., `unswizzle :: D3Link_Swizzled -> D3Link_Unswizzled`)

## Status: Typed Links Implementation (feature/typed-swizzled-links)

### ✅ Completed
1. **Foreign types added**: `D3Link_Unswizzled` and `D3Link_Swizzled` are now distinct opaque types
2. **Backward compatibility**: Old `D3Link`/`D3LinkSwizzled` newtypes remain as deprecated
3. **New API types**: `SimulationConfig2` and `SimulationUpdate2` use typed links
4. **Migration helpers**: `toUnswizzled`, `toSwizzled`, `fromUnswizzled`, `fromSwizzled` for gradual migration
5. **Enhanced documentation**: Clear explanations of swizzling transformation in API docs

### Remaining Limitations

The `Datum_` type remains opaque, so extracting link data still requires `unsafeCoerce`. This is unavoidable given D3's design:
```purescript
-- Still needed for tick functions:
unboxLink :: Datum_ -> { source :: NodeRecord, target :: NodeRecord | r }
unboxLink = unsafeCoerce  -- Unavoidable - Datum_ is opaque
```

However, this is acceptable because:
1. **The unsafe code is localized** - only in extractor functions
2. **The API boundary is type-safe** - can't pass wrong link type to init/update
3. **Clear documentation** - users know what they're getting
4. **Morally safe** - D3 guarantees the data is there, we're just accessing it

### Next Steps for Full Migration

To fully eliminate the old types:
1. Add typed variants of instance methods (e.g., `init2`, `update2`)
2. Migrate all application code to use new types
3. Deprecate old instances in major version
4. Remove old types in next breaking change

For now, the hybrid approach works well - new code can use typed links, old code continues to work.

## Action Items

- [x] **Model Swizzled vs Unswizzled links as distinct foreign types** ⭐ DONE!
- [ ] Rename SimulationM/SimulationM2 to clearer names
- [ ] Add "Common Patterns" cookbook to docs
- [ ] Consider helper utilities for simulation data manipulation
- [ ] Consider scaffolding for common event handler patterns
- [ ] Add custom type error hints for common mistakes
- [ ] Consider extending typed approach to SimulationNode and TreeNode
