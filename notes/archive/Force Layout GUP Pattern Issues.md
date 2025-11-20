# Force Layout General Update Pattern - Design Issues and Improvement Proposals

**Date**: 2025-01-22
**Context**: Issues discovered while implementing ForceNavigator component
**Status**: Working but brittle - needs abstraction design

## Overview

The combination of D3 force simulation + General Update Pattern (enter/exit/update) is extremely powerful for building interactive graph visualizations where nodes and links can appear/disappear dynamically. However, the current implementation pattern is **too complicated and brittle** for general use.

### What Makes It Powerful

- **Interactive force-directed graphs** with smooth physics-based layout
- **Dynamic data**: nodes and links can be added/removed at runtime
- **Preserved state**: existing nodes maintain their positions and velocities
- **Smooth transitions**: physics naturally handles enter/exit without jarring jumps
- **Bidirectional communication**: D3 events (clicks, drags) → Halogen actions

### What Makes It Brittle

The pattern requires precise orchestration of multiple steps, and getting any of them wrong leads to subtle bugs:

1. **State mutation**: D3's internal link swizzling mutates objects in place
2. **Initialization order**: Data must flow through specific functions in exact sequence
3. **Memory management**: Static data structures get corrupted across updates
4. **Implicit dependencies**: Success depends on understanding D3's internal state management
5. **Debugging difficulty**: Failures manifest as "existing nodes lose interactivity" or "links disappear" - symptoms far from root causes

## The Specific Issues We Encountered

### Issue 1: Link Mutation (The Core Problem)

**What happens:**
```purescript
-- Initial state
navigationData.links = [
  D3LinkID { source: "purescript-d3", target: "gallery" }  -- String IDs
]

-- After first render, swizzleLinks_ mutates IN PLACE:
navigationData.links = [
  D3LinkID { source: {id: "purescript-d3", x: 100, ...}, target: {id: "gallery", ...} }  -- Object refs!
]

-- Second render tries to filter:
Set.member link.source visibleIds  -- Fails! Object !== String
```

**Why it happens:**
`mergeNewDataWithSim` → `swizzleLinks_` (FFI) converts link IDs to node object references for D3's force simulation. The FFI mutates the original objects in place because JavaScript is pass-by-reference.

**Current fix:**
Clone all links before passing them anywhere:
```purescript
cloneLink :: D3Link String () -> D3Link String ()
cloneLink (D3LinkID link) = D3LinkID { source: link.source, target: link.target }
```

**Why this is brittle:**
Every call site must remember to clone. Easy to forget. Performance cost of cloning large graphs.

### Issue 2: Initialization Inconsistency

**The problem:**
Initial data setup must go through the EXACT same path as updates, or the simulation's internal state becomes inconsistent with the DOM.

**What we initially tried (BROKEN):**
```purescript
-- Initialize with simpleJoin
draw callback model = do
  linksSelection <- simpleJoin linksGroup Line model.links keyIsID_
  nodesSelection <- simpleJoin nodesGroup Group model.nodes keyIsID_
  setNodesFromSelection nodesSelection
  setLinksFromSelection linksSelection

-- Later updates use mergeNewDataWithSim
update callback model = do
  merged <- mergeNewDataWithSim node keyIsID_ link keyIsID_ rawdata
  -- ...
```

**Why it breaks:**
`simpleJoin` bypasses `d3PreserveSimulationPositions_` and `d3PreserveLinkReferences_`, so the simulation's internal state diverges from the DOM state. On the second update, D3 can't correlate existing elements with data.

**Correct pattern (from Spago):**
```purescript
-- Initialize creates EMPTY structure
initialize = do
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]
  pure { nodes: Just nodesGroup, links: Just linksGroup }

-- ALL data (initial and subsequent) goes through update
update callback model = do
  merged <- mergeNewDataWithSim node keyIsID_ link keyIsID_ rawdata
  -- ... full GUP ...
```

### Issue 3: The Mysterious mergeNewDataWithSim

**What it does (not obvious from the name):**
1. Preserves x, y, vx, vy from existing simulation nodes
2. Validates links (both endpoints must exist in node set)
3. Preserves link object references from simulation
4. **Mutates link objects by swizzling IDs → node references**
5. Returns "merged" data ready for D3

**The problem:**
This function has massive side effects that aren't apparent from its type signature. It's the lynchpin of the entire pattern, but its behavior is implicit knowledge.

### Issue 4: Drag Behavior Loss

**Symptom:**
After expand/collapse, existing nodes become un-draggable.

**Root cause:**
Drag behavior must be reattached to the merged selection (enter + update), not just the enter selection:

```purescript
-- WRONG - only new nodes get drag
nodeEnter <- appendTo node'.enter Group []
_ <- nodeEnter `on` Drag (CustomDrag "navigation" simdrag)

-- CORRECT - both new and existing nodes get drag
mergedNodes <- mergeSelections nodeEnter node'.update
_ <- mergedNodes `on` Drag (CustomDrag "navigation" simdrag)
```

**Why it's brittle:**
The `on` function doesn't return an error - it silently succeeds but applies to the wrong selection. The bug only manifests when you interact with the graph.

## Current Working Pattern (from Spago and ForceNavigator)

### The Full Ceremony

```purescript
-- 1. INITIALIZE (one-time SVG structure, no data)
initialize :: m { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
initialize = do
  svg <- appendTo root Svg [viewBox ...]
  linksGroup <- appendTo svg Group [classed "links"]
  nodesGroup <- appendTo svg Group [classed "nodes"]
  pure { nodes: Just nodesGroup, links: Just linksGroup }

-- 2. UPDATE (for both initial data and all subsequent changes)
update :: SelectionAttribute -> Model -> m Unit
update clickCallback model = do
  -- Re-attach to groups
  linksGroup <- attach "g.links"
  nodesGroup <- attach "g.nodes"

  -- Open selections (BEFORE merging data!)
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- CRITICAL: Merge new data with simulation
  -- This preserves positions, validates links, swizzles
  let rawdata = { nodes: model.nodes, links: cloneLinks model.links }  -- Clone to prevent mutation!
  merged <- mergeNewDataWithSim node keyIsID_ link keyIsID_ rawdata

  -- NODES: General Update Pattern
  node' <- updateJoin node Group merged.nodes keyIsID_

  -- Enter: Create DOM structure for new nodes
  nodeEnter <- appendTo node'.enter Group []
  _ <- appendTo nodeEnter Circle []
  _ <- appendTo nodeEnter Text []

  -- Apply attributes to ENTER children
  enterCircles <- selectUnder nodeEnter (show Circle)
  setAttributes enterCircles [radius ..., fill ..., clickCallback]
  enterLabels <- selectUnder nodeEnter (show Text)
  setAttributes enterLabels [text ..., ...]

  -- Exit: Remove old nodes
  setAttributes node'.exit [remove]

  -- Update: Reapply attributes to EXISTING children
  updateCircles <- selectUnder node'.update (show Circle)
  setAttributes updateCircles [radius ..., fill ..., clickCallback]  -- Must reapply callback!
  updateLabels <- selectUnder node'.update (show Text)
  setAttributes updateLabels [text ..., ...]

  -- Merge enter and update for behaviors and tick functions
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- Apply drag to MERGED selection (critical!)
  _ <- mergedNodes `on` Drag (CustomDrag "name" simdrag)

  -- LINKS: General Update Pattern (similar to nodes)
  link' <- updateJoin link Line merged.links keyIsID_
  linkEnter <- appendTo link'.enter Line []
  setAttributes linkEnter [strokeWidth ...]
  setAttributes link'.exit [remove]
  mergedLinks <- mergeSelections linkEnter link'.update

  -- Put merged selections into simulation
  setNodesFromSelection mergedNodes
  setLinksFromSelection mergedLinks (\_ -> true)

  -- Tick functions for merged selections
  addTickFunction "nodes" $ Step mergedNodes [transform' ...]
  addTickFunction "links" $ Step mergedLinks [x1 ..., y1 ..., x2 ..., y2 ...]

  pure unit

-- 3. COMPONENT INTEGRATION
Initialize -> do
  { emitter, listener } <- liftEffect $ HS.create
  _eventListener .= Just listener

  selections <- evalEffectSimulation initialize
  _openSelections .= Just selections

  -- Add initial data through update (not through initialize!)
  let callback = simulationEvent listener
  evalEffectSimulation $ update callback initialModel

  runSimulation
```

### Critical Invariants

1. **All data flows through `update`** - never bypass `mergeNewDataWithSim`
2. **Clone links before passing** - prevent mutation of static data
3. **Open selections first** - before calling `mergeNewDataWithSim`
4. **Reapply ALL attributes to update selection** - including event handlers
5. **Apply behaviors to MERGED selection** - not just enter
6. **Set simulation data from selections** - after join, not before

## Proposed Improvements

### 1. Immutable Data Wrapper

Create a wrapper type that guarantees immutability:

```purescript
-- Opaque type that forces cloning
newtype ImmutableLinks id r = ImmutableLinks (Array (D3Link id r))

-- Constructor that clones
mkImmutableLinks :: forall id r. Array (D3Link id r) -> ImmutableLinks id r
mkImmutableLinks links = ImmutableLinks (map cloneLink links)

-- Consumer that returns clones
getLinks :: forall id r. ImmutableLinks id r -> Array (D3Link id r)
getLinks (ImmutableLinks links) = map cloneLink links
```

This makes mutation protection explicit in the type system.

### 2. Unified GUP Function

Abstract the entire pattern into a single function:

```purescript
-- High-level function that encapsulates the entire pattern
runForceGUP :: forall d r id m.
  Eq id =>
  MonadEffect m =>
  SimulationM D3Selection_ m =>
  { nodeSelection :: D3Selection_       -- Opened node group
  , linkSelection :: D3Selection_       -- Opened link group
  , newNodes :: Array (D3_SimulationNode d)
  , newLinks :: ImmutableLinks id r
  , nodeKeyFn :: Datum_ -> Index_
  , linkKeyFn :: Datum_ -> Index_
  , nodeAttributes :: NodeGUPAttributes   -- Enter/Update attributes
  , linkAttributes :: LinkGUPAttributes   -- Enter/Update attributes
  , behaviors :: Array Behavior           -- Drag, etc.
  } ->
  m { mergedNodes :: D3Selection_, mergedLinks :: D3Selection_ }
```

This function would:
- Handle `mergeNewDataWithSim` internally
- Apply enter/update/exit pattern correctly
- Reapply attributes to both enter and update
- Apply behaviors to merged selections
- Return merged selections for tick functions

### 3. Type-Safe Staging

Make the initialization→update sequence type-safe:

```purescript
-- Phantom types enforce initialization order
data Uninitialized
data Initialized
data WithData

newtype ForceLayoutHandle s = ForceLayoutHandle { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }

initialize :: m (ForceLayoutHandle Initialized)

-- Can only call addData on Initialized, returns WithData
addData :: ForceLayoutHandle Initialized -> Model -> m (ForceLayoutHandle WithData)

-- Can only call update on WithData
update :: ForceLayoutHandle WithData -> Model -> m (ForceLayoutHandle WithData)
```

This prevents calling functions in the wrong order.

### 4. Simulation State Abstraction

Hide D3's internal state management:

```purescript
-- Manages simulation state internally, immutable from user perspective
class SimulationState s where
  -- Merge new data, returns handle with updated state
  mergeData :: forall m. MonadEffect m => s -> RawData -> m s

  -- Get current nodes/links as selections for rendering
  getCurrentSelections :: forall m. MonadEffect m => s -> m { nodes :: D3Selection_, links :: D3Selection_ }

-- Implementation handles all the preservation/swizzling internally
```

### 5. Recipe DSL

Provide a declarative DSL for common patterns:

```purescript
forceLayout :: Recipe
forceLayout = do
  svg <- container [viewBox auto]
  links <- linkGroup [classed "links", stroke "#999"]
  nodes <- nodeGroup [classed "nodes"]

  onUpdate \model -> do
    -- Declarative updates, mutation protection built-in
    nodes `show` model.nodes `with`
      [ circle [radius datum_.size, fill datum_.color]
      , label [text datum_.label]
      , drag "myDrag" simdrag
      ]

    links `show` model.links `with`
      [ strokeWidth 2.0 ]

  runWith initialModel
```

The DSL would compile to the correct sequence of imperative operations.

## Documentation Needs

Even without new abstractions, we need better docs:

1. **Cookbook**: Step-by-step guide for "interactive force graph with GUP"
2. **Pitfall guide**: Common mistakes and their symptoms
3. **Reference**: What each function does to D3's internal state
4. **Migration guide**: How to convert static force graphs to dynamic ones

## Comparison with Mike Bostock's Examples

**Key insight**: Mike Bostock's D3 examples rarely (if ever?) show force simulation + enter/exit. Most force examples have:
- Fixed node/link sets that never change
- OR completely rebuild the simulation on data change (no GUP)

The combination of **force simulation + GUP** is extremely powerful but was likely not part of the original D3 design conception. We've essentially discovered an advanced pattern that works but isn't officially supported.

## Success Criteria for New Abstraction

A good abstraction should make it **impossible** to:
1. Forget to clone links
2. Call functions in the wrong order
3. Skip `mergeNewDataWithSim`
4. Apply behaviors to only enter/update instead of merged
5. Bypass preservation of node positions

And should make it **easy** to:
1. Add/remove nodes dynamically
2. Filter links based on visible nodes
3. Maintain interactivity (drag, click) across updates
4. Integrate with Halogen or other frameworks
5. Debug when something goes wrong

## Files to Review

### Working Examples
- `src/website/Component/Spago/Spago.purs` - Full MiseEnScene pattern
- `src/website/Viz/Spago/Draw.purs` - Complete updateSimulation implementation
- `src/website/Component/ForceNavigator/ForceNavigator.purs` - Simpler expand/collapse pattern
- `src/website/Viz/ForceNavigator/Draw.purs` - initialize + update pattern

### Core Library Functions
- `src/lib/D3/Layouts/Simulation/Functions.purs:215-227` - `simulationMergeNewData` (the critical function)
- `src/lib/D3/FFI.purs` - FFI functions that mutate state: `swizzleLinks_`, `d3PreserveSimulationPositions_`

### Issue Discovered
- `src/website/Component/ForceNavigator/State.purs:60-63` - `cloneLink` function (workaround for mutation)

## Next Steps

1. **Document current pattern** - Write comprehensive guide for users
2. **Design abstraction** - Prototype one of the proposed approaches
3. **Benchmark** - Measure performance cost of cloning vs other solutions
4. **Prototype** - Build proof-of-concept abstraction
5. **Refactor** - Migrate Spago and ForceNavigator to new abstraction
6. **Release** - Document and publish the pattern

## Open Questions

1. **Should we enforce immutability at the FFI level?** (Clone in JavaScript before swizzling?)
2. **Can we use PureScript's RefST for safer mutation?**
3. **Should mergeNewDataWithSim be split into separate pure and effect functions?**
4. **Is there a way to make D3's mutation explicit in the type system?**
5. **Would a completely different approach (e.g., virtual DOM for SVG) be cleaner?**

---

**Conclusion**: This pattern works and is powerful, but requires expert-level understanding of D3's internals and careful attention to detail. We need better abstractions to make it accessible and maintainable. The current implementation should be considered a working prototype that demonstrates feasibility, not a final API design.
