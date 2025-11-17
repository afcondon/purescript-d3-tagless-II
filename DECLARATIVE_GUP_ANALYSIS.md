# Declarative General Update Pattern Analysis

## Executive Summary

The code-explorer in this branch (main) uses a **fully declarative pattern** that eliminates the 8-9 imperative steps we identified in LesMisGUPV2. This pattern makes force-directed graph updates with the General Update Pattern (GUP) as simple as switching scenes - just provide a SceneConfig and call `runSimulation`.

## Key Components of the Declarative Pattern

### 1. **SceneConfig** - Declarative Scene Specification

**Location:** `src/lib/PSD3/Simulation/Scene.purs`

A scene is a complete, self-contained configuration:

```purescript
type SceneConfig a attrs = {
  -- Data filtering
  chooseNodes :: SimulationNode a -> Boolean     -- Which nodes to display
  linksShown :: D3Link_Unswizzled -> Boolean     -- Which links to render
  linksActive :: Datum_ -> Boolean               -- Which links exert force

  -- Force configuration
  activeForces :: Set Label                      -- Which forces to enable

  -- Visual styling
  cssClass :: String                             -- Root CSS class
  attributes :: attrs                            -- Scene-specific attributes

  -- Node initialization (positioning, pinning)
  nodeInitializerFunctions :: Array (Array (SimulationNode a) -> Array (SimulationNode a))

  -- Transition behavior
  transitionConfig :: Maybe TransitionSpec       -- How scene animates
}
```

**Example Scene** (from `src/website/Component/CodeExplorer/Scenes.purs`):

```purescript
packageGridScene :: SceneConfig
packageGridScene = {
  chooseNodes: allNodes
, linksShown: isM2P_Link
, linksActive: const true
, cssClass: "cluster"
, attributes: clusterSceneAttributes
, activeForces: Set.fromFoldable ["clusterx_P", "clustery_P", ...]
, nodeInitializerFunctions: [unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY]
, transitionConfig: Just smoothTransition
}
```

### 2. **genericUpdateSimulation** - The Core Library Function

**Location:** `src/lib/PSD3/Simulation/Update.purs`

This function handles ALL complexity in one place with an "impossible to mess up" API:

```purescript
genericUpdateSimulation
  { nodes: Just nodesGroup, links: Just linksGroup }  -- DOM selections
  Circle                                              -- Node element type
  Line                                                -- Link element type
  { allNodes: model.nodes                             -- FULL dataset
  , allLinks: model.links                             -- FULL dataset
  , nodeFilter: scene.chooseNodes                     -- Single predicate
  , linkFilter: Just scene.linksShown                 -- Optional visual filter
  , nodeInitializers: scene.nodeInitializerFunctions  -- Positioning functions
  , activeForces: Just scene.activeForces
  , config: Nothing
  }
  keyIsID_                                            -- Node key function
  keyIsID_                                            -- Link key function
  myAttributes
  myRenderCallbacks                                   -- Enter/update/exit behaviors
```

**What it does automatically:**

1. ✅ Applies node filter to full dataset
2. ✅ Runs node initializers on filtered nodes (tree layout, grid, pinning)
3. ✅ Extracts node IDs from initialized nodes
4. ✅ **AUTOMATICALLY filters links** to only include links between visible nodes
5. ✅ Applies optional visual link filter (e.g., hide dev dependencies)
6. ✅ Calls SimulationM2 update API with consistent data
7. ✅ Opens DOM selections
8. ✅ Applies General Update Pattern (enter/update/exit/merge)
9. ✅ **Automatically re-registers tick functions** with merged selections
10. ✅ Handles all state management

**The key insight:** By requiring FULL datasets and ONLY a node filter predicate, the library can automatically ensure link consistency. Users can never accidentally provide filtered nodes with unfiltered links.

### 3. **RenderCallbacks** - Declarative Rendering

**Location:** `src/lib/PSD3/Simulation/Update.purs`

Visualizations provide "what to render", not "when to render it":

```purescript
type RenderCallbacks attrs sel m d = {
  -- Node rendering (library controls when these are called)
  onNodeEnter :: sel d -> attrs -> m (sel d)    -- Create new DOM elements
  onNodeUpdate :: sel d -> attrs -> m Unit      -- Update existing elements
  onNodeExit :: sel d -> m Unit                 -- Remove old elements

  -- Link rendering (same pattern)
  onLinkEnter :: sel D3Link_Swizzled -> attrs -> m (sel D3Link_Swizzled)
  onLinkUpdate :: sel D3Link_Swizzled -> attrs -> m Unit
  onLinkExit :: sel D3Link_Swizzled -> m Unit

  -- Tick attributes (applied to merged selections automatically)
  nodeTickAttrs :: attrs -> Array (SelectionAttribute d)
  linkTickAttrs :: Array (SelectionAttribute D3Link_Swizzled)
}
```

**Example** (from `src/website/Viz/Spago/Render.purs`):

```purescript
spagoRenderCallbacks = {
  onNodeEnter: \enterSel attrs -> do
    nodeEnter <- appendTo enterSel Group enterAttrs
    _ <- appendTo nodeEnter Circle attrs.circles
    _ <- appendTo nodeEnter Text attrs.labels
    _ <- nodeEnter `on` Drag (CustomDrag "spago" simdrag_)
    pure nodeEnter

  , onNodeUpdate: \updateSel attrs -> do
      setAttributes updateSel updateAttrs
      updateCircles <- selectUnder updateSel (show Circle)
      setAttributes updateCircles attrs.circles

  , onNodeExit: \exitSel ->
      setAttributes exitSel [remove]

  , nodeTickAttrs: \attrs ->
      [transform' \(d :: SpagoSimNode) -> translateNode d]

  , linkTickAttrs:
      [x1 (DatumFn \d -> d.source.x), y1 (...), x2 (...), y2 (...)]
}
```

### 4. **runSimulationFromState** - High-Level Orchestrator

**Location:** `src/lib/PSD3/Simulation/RunSimulation.purs`

Bridges Halogen state to D3 rendering with a single function call:

```purescript
runSimulationFromState
  (_.staging.selections)        -- Get selections from state
  (_.scene)                     -- Get scene config from state
  getModelNodes                 -- Get all nodes from state
  getModelLinks                 -- Get all links from state
  (\attrs state -> attrs { tagMap = Just state.tags })  -- Enhance attributes
  Graph.updateSimulation        -- Visualization-specific update function
```

**What it does:**

1. Extracts data from state using provided accessors
2. Calls low-level `runSimulation` pattern
3. Handles declarative scene transitions (if configured)
4. Delegates to visualization-specific `updateSimulation`

### 5. **Transition Matrix** - Declarative Scene Choreography

**Location:** `src/website/Component/CodeExplorer/CodeExplorer.purs` (lines 74-127)

Defines how scenes transition to each other:

```purescript
defaultTransitionMatrix :: TransitionMatrix
defaultTransitionMatrix = Map.fromFoldable
  [ Tuple (Tuple PackageGrid PackageGraph) smoothTransition
  , Tuple (Tuple PackageGraph PackageGrid) smoothTransition
  , Tuple (Tuple PackageGrid (ModuleTree Radial)) smoothTransitionPinned
  -- ... etc
  ]
```

**Benefits:**

- ✅ Fully declarative: No manual sequencing in action handlers
- ✅ Type-safe: Compiler ensures all scenes are valid
- ✅ Maintainable: All choreography in one place
- ✅ Asymmetric: Can have different animations A→B vs B→A

## Comparison: Imperative vs Declarative

### Imperative Approach (LesMisGUPV2 - what we had)

**9 manual steps for EVERY update:**

```purescript
updateForceGraph nodesInSim linksInSim = do
  -- Step 1: Re-select the groups
  linksGroup <- select "#links"
  nodesGroup <- select "#nodes"

  -- Step 2: Wrap data for joins
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim
  let keyedNodes = nodesInSim <#> KeyedNode

  -- Step 3: Build trees for data joins
  let linksTree = T.joinData "linkElements" "line" indexedLinks $ ...
  let nodesTree = T.joinData "nodeElements" "circle" keyedNodes $ ...

  -- Step 4: Re-render trees
  linksSelections <- renderTree linksGroup linksTree
  nodesSelections <- renderTree nodesGroup nodesTree

  -- Step 5: Extract updated selections
  let nodeCircles = case Map.lookup "nodeElements" nodesSelections of ...

  -- Step 6: Re-attach behaviors
  _ <- on (Drag $ simulationDrag "lesmis-gup") nodeCircles

  -- Step 7: Re-register tick functions (CRITICAL!)
  addTickFunction "nodes" $ Step nodeCircles [...]
  addTickFunction "links" $ Step linkLines [...]

  -- Step 8: Reheat and restart
  reheat 0.8
  start
```

**Problems:**

- ❌ User must manually re-select groups
- ❌ User must manually wrap data for joins
- ❌ User must manually extract selections from Tree API results
- ❌ User must manually re-attach behaviors
- ❌ User must manually re-register tick functions (easy to forget!)
- ❌ User must manually manage simulation lifecycle (stop/reheat/start)
- ❌ User can easily provide inconsistent data (filtered nodes, unfiltered links)
- ❌ Complex state management with Refs

### Declarative Approach (code-explorer - what we want)

**1 function call:**

```purescript
-- In action handler:
Scene PackageGrid -> do
  H.modify_ $ applySceneWithTransition PackageGrid packageGridScene
  runSimulation
  H.modify_ _ { currentScene = PackageGrid }
```

**That's it!** The library handles:

- ✅ Data filtering (nodes AND links automatically)
- ✅ Node initialization (tree layouts, grids, pinning)
- ✅ DOM updates (enter/update/exit pattern)
- ✅ Tick function re-registration (automatic with merged selections)
- ✅ Behavior re-attachment (in render callbacks)
- ✅ Simulation lifecycle (stop/transition/start)
- ✅ State management (no Refs needed)
- ✅ Transition choreography (via transition matrix)

## Architecture Overview

```
User Action
    ↓
Scene Config (declarative spec)
    ↓
runSimulationFromState (orchestrator)
    ↓
    ├→ Extract data from state
    ├→ runSimulation (low-level pattern)
    │   ├→ Stop simulation
    │   ├→ Apply node initializers
    │   ├→ updateSimulation (viz-specific)
    │   │   └→ genericUpdateSimulation (library core)
    │   │       ├→ Filter nodes (via nodeFilter)
    │   │       ├→ Run node initializers
    │   │       ├→ AUTO-filter links (IMPOSSIBLE TO MESS UP!)
    │   │       ├→ SimulationM2.update (data merging, swizzling)
    │   │       ├→ General Update Pattern (enter/update/exit)
    │   │       ├→ Render callbacks (viz-specific)
    │   │       └→ AUTO-register tick functions (merged selections)
    │   ├→ Execute scene transition (if configured)
    │   └→ Start simulation
    └→ Done!
```

## Key Files to Port

From the `main` branch to `feature/declarative-tree-api`:

### Core Library Files (should already exist, may need updates)

1. ✅ `src/lib/PSD3/Simulation/Scene.purs` - SceneConfig and TransitionSpec
2. ✅ `src/lib/PSD3/Simulation/Update.purs` - genericUpdateSimulation
3. ✅ `src/lib/PSD3/Simulation/RunSimulation.purs` - runSimulation pattern

### Visualization-Specific Files

4. `src/website/Viz/Spago/Draw.purs` - initialize and updateSimulation wrapper
5. `src/website/Viz/Spago/Render.purs` - spagoRenderCallbacks implementation

### LesMis-Specific Files to Create

6. **NEW**: `src/website/Viz/LesMis/LesMisRenderCallbacks.purs` - Render callbacks for LesMis
7. **NEW**: `src/website/Viz/LesMis/LesMisScenes.purs` - Scene configurations
8. **UPDATE**: `src/website/Viz/LesMis/LesMisGUPV2.purs` - Refactor to use declarative pattern

## Implementation Plan for feature/declarative-tree-api

### Phase 1: Verify Library Support

1. Check if `genericUpdateSimulation` exists in PSD3v2
2. Check if `RenderCallbacks` type exists
3. Check if `SceneConfig` and transition support exists
4. If missing, port from this branch (main)

### Phase 2: Create LesMis Render Callbacks

Create `src/website/Viz/LesMis/LesMisRenderCallbacks.purs`:

```purescript
lesMisRenderCallbacks :: RenderCallbacks LesMisAttributes D3Selection_ m LesMisSimNode
lesMisRenderCallbacks = {
  onNodeEnter: \enterSel attrs -> do
    nodeEnter <- appendTo enterSel Circle [
      cx (\d -> d.x)
    , cy (\d -> d.y)
    , radius 5.0
    , fill (\d -> d3SchemeCategory10N_ (toNumber d.group))
    , stroke "#fff"
    , strokeWidth 2.0
    ]
    _ <- nodeEnter `on` Drag (simulationDrag "lesmis")
    pure nodeEnter

  , onNodeUpdate: \updateSel attrs ->
      setAttributes updateSel [
        fill (\d -> d3SchemeCategory10N_ (toNumber d.group))
      ]

  , onNodeExit: \exitSel ->
      setAttributes exitSel [remove]

  , onLinkEnter: \enterSel attrs ->
      appendTo enterSel Line [
        strokeWidth (\il -> sqrt il.value)
      , stroke (\il -> d3SchemeCategory10N_ (toNumber il.target.group))
      ]

  , onLinkUpdate: \updateSel attrs ->
      pure unit

  , onLinkExit: \exitSel ->
      setAttributes exitSel [remove]

  , nodeTickAttrs: \attrs -> [
      cx (\d -> d.x)
    , cy (\d -> d.y)
    ]

  , linkTickAttrs: [
      x1 (\il -> il.source.x)
    , y1 (\il -> il.source.y)
    , x2 (\il -> il.target.x)
    , y2 (\il -> il.target.y)
    ]
}
```

### Phase 3: Create LesMis Scenes

Create `src/website/Viz/LesMis/LesMisScenes.purs`:

```purescript
-- Full graph with all nodes
fullGraphScene :: SceneConfig LesMisDataRow LesMisAttributes
fullGraphScene = {
  chooseNodes: const true
, linksShown: const true
, linksActive: const true
, cssClass: "full-graph"
, attributes: defaultLesMisAttributes
, activeForces: Set.fromFoldable ["manyBody", "center", "links", "collision"]
, nodeInitializerFunctions: [unpinAllNodes, setPhyllotaxisInitialPositions]
, transitionConfig: Just smoothTransition
}

-- Random subset for auto-cycling demo
randomSubsetScene :: Int -> SceneConfig LesMisDataRow LesMisAttributes
randomSubsetScene seed = {
  chooseNodes: randomFilter seed 0.7  -- Show 70% of nodes
, linksShown: const true
, linksActive: const true
, cssClass: "random-subset"
, attributes: defaultLesMisAttributes
, activeForces: Set.fromFoldable ["manyBody", "center", "links", "collision"]
, nodeInitializerFunctions: [unpinAllNodes, setPhyllotaxisInitialPositions]
, transitionConfig: Just smoothTransition
}

-- Grid layout
gridScene :: SceneConfig LesMisDataRow LesMisAttributes
gridScene = {
  chooseNodes: const true
, linksShown: const true
, linksActive: const false
, cssClass: "grid"
, attributes: defaultLesMisAttributes
, activeForces: Set.empty  -- No forces, purely pinned
, nodeInitializerFunctions: [setGridPositions 50.0]
, transitionConfig: Just smoothTransitionPinned
}
```

### Phase 4: Refactor LesMisGUPV2

Replace the imperative `updateForceGraph` with declarative scene switching:

```purescript
-- OLD (imperative):
filterByGroupWithOriginal minGroup originalGraph = do
  stop
  let filteredNodes = Array.filter (\n -> n.group >= minGroup) originalGraph.nodes
  let filteredLinks = Array.filter (...) originalGraph.links
  { nodes: nodesInSim, links: linksInSim } <- update {...}
  updateForceGraph nodesInSim linksInSim  -- 8 manual steps!

-- NEW (declarative):
filterByGroupWithOriginal minGroup originalGraph = do
  let scene = fullGraphScene {
    chooseNodes = \n -> n.group >= minGroup  -- Just modify the predicate!
  }
  runSimulation selections scene originalGraph.nodes originalGraph.links lesMisUpdateSimulation
```

### Phase 5: Create Auto-Cycling Demo

Use the same pattern as code-explorer scene switching:

```purescript
-- In Halogen action handler:
StartAutoCycle -> do
  H.fork $ forever do
    -- Generate random scene
    seed <- liftEffect randomInt
    H.modify_ $ applyScene (randomSubsetScene seed)
    runSimulation
    liftAff $ delay (Milliseconds 3000.0)
```

## Critical Success Factors

### ✅ What Makes This Pattern "Impossible to Mess Up"

1. **Automatic link filtering:** Library extracts node IDs and filters links automatically
2. **Automatic tick re-registration:** Library merges selections and registers tick functions
3. **Declarative callbacks:** Visualization provides "what", library controls "when"
4. **Full datasets always:** Library filters, user never manually filters
5. **Type safety:** Compiler ensures correct scene transitions

### ❌ What We Were Doing Wrong

1. Manual link filtering (easy to forget, easy to get wrong)
2. Manual tick function re-registration (easy to forget, caused non-moving nodes)
3. Mixing Tree API (declarative) with manual GUP (imperative)
4. Manual state management with Refs
5. Manual selection management and merging

## Next Steps

1. ✅ **Analyze declarative pattern** (this document)
2. ⏳ **Check what exists in feature/declarative-tree-api branch**
   - Does genericUpdateSimulation exist?
   - Does RenderCallbacks exist?
   - Does SceneConfig exist?
3. ⏳ **Port missing pieces** from main branch
4. ⏳ **Create LesMis render callbacks** (pure, reusable)
5. ⏳ **Create LesMis scenes** (declarative specs)
6. ⏳ **Refactor LesMisGUPV2** to use declarative pattern
7. ⏳ **Create auto-cycling demo** using scene switching
8. ⏳ **ONLY THEN** consider Tree API integration

## Key Insight

The declarative pattern makes force-directed graph updates **as simple as static rendering**. The complexity is hidden in the library, and the user just provides:

1. A scene configuration (predicates, forces, styles, initializers)
2. Render callbacks (how to create/update/remove DOM elements)
3. Full datasets (library handles all filtering automatically)

That's it! No manual state management, no manual tick re-registration, no manual link filtering, no manual selection merging. Just declarative scene specs.
