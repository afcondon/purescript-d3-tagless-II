# CodeExplorer Component Architecture: Spotlight Mode & Force Management

## Overview
The CodeExplorer and ExpandableBubbles components demonstrate sophisticated patterns for managing force-directed graph visualizations with multiple scenes, spotlight filtering, and dynamic force switching.

## 1. FORCE LIBRARY SETUP (Forces.purs)

### Location
`/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/Forces.purs`

### Key Pattern: Centralized Force Library
```purescript
forceLibrary :: Map Label Force
forceLibrary = initialize [
    createForce "center"       (RegularForce ForceCenter)   allNodes [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
  , createForce "x"            (RegularForce ForceX)        allNodes [ F.strength 0.05, F.x 0.0 ]
  , createForce "y"            (RegularForce ForceY)        allNodes [ F.strength 0.07, F.y 0.0 ]
  
  , createForce "collide1"     (RegularForce ForceCollide)  allNodes [ F.strength 1.0, F.radius datum_.collideRadius ]
  , createForce "collide2"     (RegularForce ForceCollide)  allNodes [ F.strength 0.7, F.radius datum_.collideRadiusBig ]
  
  , createForce "charge1"      (RegularForce ForceManyBody) allNodes [ F.strength (-50.0), ... ]
  , createForce "charge2"      (RegularForce ForceManyBody) allNodes [ F.strength (-100.0), ... ]
  , createForce "chargetree"   (RegularForce ForceManyBody) treeExceptLeaves [ F.strength (-100.0), ... ]
  
  , createForce "clusterx_M"   (RegularForce ForceX)  modulesOnly [ F.strength 0.2, ... ]
  , createForce "clustery_M"   (RegularForce ForceY)  modulesOnly [ F.strength 0.2, ... ]
  
  , createForce "clusterx_P"   (RegularForce ForceX)  packagesOnly [ F.strength 0.8, ... ]
  , createForce "clustery_P"   (RegularForce ForceY)  packagesOnly [ F.strength 0.8, ... ]
  
  -- Tree forces with node filtering via ForceFilter
  , createForce "htreeNodesX"  (RegularForce ForceX)  (Just $ ForceFilter "tree only" \d -> datum_.connected d)
      [ F.strength 0.4, F.x datum_.treePointX ] 
  , createForce "htreeNodesY"  (RegularForce ForceY)  (Just $ ForceFilter "tree only" \d -> datum_.connected d)
      [ F.strength 0.4, F.y datum_.treePointY ] 
  
  , createForce "packageOrbit" (RegularForce ForceRadial) packagesOnly 
      [ F.strength 0.7, F.x 0.0, F.y 0.0, F.radius 500.0 ]
  , createForce "moduleOrbit"  (RegularForce ForceRadial) usedModulesOnly
      [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 600.0 ]
  
  , createLinkForce Nothing [ F.strength 0.5, F.distance 0.0, F.numKey (toNumber <<< datum_.id) ]
  ]
```

**Key Characteristics:**
- All forces defined in a single map: centralized configuration
- Each force has a unique string label (name) used as a key
- Forces can have **node filters** via `ForceFilter` to apply only to specific nodes
- Forces have **strength, radius, distance** parameters that can be tuned per force
- Different forces used for different scenes (e.g., "chargetree" only for tree layouts)

### Node Filter Types
```purescript
packagesOnly      = Just $ ForceFilter "all packages" datum_.isPackage
modulesOnly       = Just $ ForceFilter "all modules" datum_.isModule
unusedModulesOnly = Just $ ForceFilter "unused modules only" datum_.isUnusedModule
usedModulesOnly   = Just $ ForceFilter "used modules only" datum_.isUsedModule
treeExceptLeaves  = Just $ ForceFilter "tree parent nodes only" datum_.isTreeParent
```

---

## 2. SCENE CONFIGURATION (Scenes.purs & State.purs)

### Location
`/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/Scenes.purs`
`/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/State.purs`

### SceneConfig Type
```purescript
type SceneConfig = {
  -- Data filtering predicates
    chooseNodes     :: (SpagoSimNode -> Boolean)
  , linksShown      :: (SpagoGraphLinkID -> Boolean)
  , linksActive     :: (Datum_ -> Boolean)  -- Which links exert force
  
  -- Force configuration
  , activeForces    :: Set Label           -- Set of force labels to enable
  
  -- Visual styling
  , cssClass        :: String
  , attributes      :: SpagoSceneAttributes
  
  -- Node positioning/initialization
  , nodeInitializerFunctions :: Array (Array SpagoSimNode -> Array SpagoSimNode)
}
```

### Scene Definition Examples

#### Package Grid Scene
```purescript
packageGridScene :: SceneConfig
packageGridScene = {
  chooseNodes: allNodes                              -- Show all nodes
, linksShown: isM2P_Link                             -- Module-to-Package links only
, linksActive: const true                            -- All shown links exert force
, cssClass: "cluster"
, attributes: clusterSceneAttributes
, activeForces: Set.fromFoldable [ 
    "clusterx_P", "clustery_P"      -- Grid forces for packages
  , "clusterx_M", "clustery_M"      -- Container forces for modules
  , "collide2"                       -- Collision to prevent overlap
  , "links"                          -- Link force
  ]
, nodeInitializerFunctions: [ 
    unpinAllNodes                    -- Start with no pinned positions
  , packageNodesToGridXY             -- Position packages in a grid
  , moduleNodesToContainerXY         -- Position modules in grid containers
  ]
}
```

#### Radial Tree Scene
```purescript
radialTreeScene :: SceneConfig
radialTreeScene = {
  chooseNodes: isUsedModule                  -- Only modules in dependency tree
, linksShown: isM2M_Tree_Link               -- Module-to-Module tree links
, linksActive: const true
, cssClass: "tree radial"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ 
    "center"                         -- Pull to center
  , "collide2"                       -- Prevent overlap
  , "chargetree"                     -- Repulsion between tree nodes (not leaves)
  , "charge2"                        -- Additional repulsion
  , "links"                          -- Link attraction
  ]
, nodeInitializerFunctions: [ 
    unpinAllNodes
  , treeNodesToTreeXY_R              -- D3 tree layout (radial)
  , fixNamedNodeTo "PSD3.Main" { x: 0.0, y: 0.0 }  -- Pin root at origin
  ]
}
```

### Key Insight: Scene as Configuration Container
- Scenes encapsulate EVERYTHING needed for a visualization
- Change scene = change filters + forces + styling + initializers in one atomic operation
- Makes scene switching declarative and predictable

---

## 3. SPOTLIGHT/FILTERING PATTERN (CodeExplorer.purs)

### Location
`/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/CodeExplorer.purs` (lines 126-134)

### Simple Spotlight Implementation
```purescript
EventFromVizualization ve -> do
  case ve of
    NodeClick (IsPackage _) id -> handleAction $ ToggleChildrenOfNode id
    NodeClick (IsModule _)  id -> handleAction $ SpotlightNode id

ToggleChildrenOfNode id -> do
  H.modify_ $ setChooseNodes (isPackageOrVisibleModule id)  -- Filter to show only this package's modules
  runSimulation                                             -- Re-run with new filter

UnToggleChildrenOfNode _ -> do
  H.modify_ $ setChooseNodes isPackage                     -- Reset to show all packages
  runSimulation
```

**Pattern:** 
- Spotlight is just a **node filter** stored in `scene.chooseNodes`
- Change filter → call `runSimulation` → D3 re-filters and re-renders

---

## 4. ADVANCED SPOTLIGHT MODE (ExpandableBubbles.purs)

### Location
`/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeAtlas/Tabs/ExpandableBubbles.purs`

### Dual Force Configuration Pattern
ExpandableBubbles has **TWO collision forces** to switch between modes:

```purescript
forces =
  [ createForce "manyBody-compact" (RegularForce ForceManyBody) allNodes 
      [ F.strength (-50.0), F.theta 0.9, F.distanceMin 1.0 ]   -- Weak repulsion
      
  , createForce "manyBody-spotlight" (RegularForce ForceManyBody) allNodes 
      [ F.strength (-150.0), F.theta 0.9, F.distanceMin 1.0 ]  -- Moderate repulsion
      
  , createForce "collision-compact" (RegularForce ForceCollide) allNodes 
      [ F.radius compactCollisionRadius, F.strength 0.9, F.iterations 3.0 ]
      
  , createForce "collision-spotlight" (RegularForce ForceCollide) allNodes 
      [ F.radius spotlightCollisionRadius, F.strength 0.9, F.iterations 3.0 ]
      
  , createForce "center" (RegularForce ForceCenter) allNodes 
      [ F.x 0.0, F.y 0.0, F.strength 0.2 ]
      
  , createLinkForce Nothing [ F.distance 150.0, F.strength 0.3 ]
  ]

activeForces = Set.fromFoldable [ "manyBody-compact", "collision-compact", "center", "links" ]  -- Start compact
```

### Collision Radius Functions
```purescript
compactCollisionRadius :: Datum_ -> Index_ -> Number
compactCollisionRadius datum _ =
  let node = unsafeCoerce datum :: BubbleNodeRecord
      baseRadius = nodeRadius false node.loc  -- Always use collapsed size
  in baseRadius + 2.0  -- Minimal padding

spotlightCollisionRadius :: Datum_ -> Index_ -> Number
spotlightCollisionRadius datum _ =
  let node = unsafeCoerce datum :: BubbleNodeRecord
      baseRadius = nodeRadius node.expanded node.loc
      padding = if node.expanded then baseRadius * 0.3 else 5.0  -- 30% padding for expanded
  in baseRadius + padding
```

### Force Switching via actualizeForcesDirect
```purescript
-- Enter spotlight mode
simSt <- Ref.read simStateRef
let updatedSimState = actualizeForcesDirect 
      (Set.fromFoldable ["manyBody-spotlight", "collision-spotlight", "center", "links"]) 
      simSt
Ref.write updatedSimState simStateRef
callbacks.onEnableSpotlightMode

-- Exit spotlight mode
simSt <- Ref.read simStateRef
let updatedSimState = actualizeForcesDirect 
      (Set.fromFoldable ["manyBody-compact", "collision-compact", "center", "links"]) 
      simSt
Ref.write updatedSimState simStateRef
```

---

## 5. NODE/LINK FILTERING & RESTORATION (ExpandableBubbles.purs)

### Location
`/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeAtlas/Tabs/ExpandableBubbles.purs` (lines 500-675)

### State Management Pattern
```purescript
-- Store original full datasets for restoration
allNodesRef <- liftEffect $ Ref.new bubbleNodes
allLinksRef <- liftEffect $ Ref.new bubbleLinks

-- Track filtering state
hasFilteredRef <- liftEffect $ Ref.new false
currentSpotlightRef <- liftEffect $ Ref.new (Nothing :: Maybe String)
spotlightSetRef <- liftEffect $ Ref.new (Set.empty :: Set String)
```

### Filtering to Connected Nodes (Spotlight Activation)
```purescript
let spotlightModule moduleId = do
  case Map.lookup moduleId modulesMap of
    Nothing -> Console.log $ "Module not found: " <> moduleId
    Just moduleInfo -> do
      let dependedOnBy = fromMaybe Set.empty $ Map.lookup moduleId dependedOnByMap
          dependencies = sort moduleInfo.depends
          dependedOnByList = sort $ Set.toUnfoldable dependedOnBy :: Array String
      callbacks.onShowModuleDetails moduleInfo.name dependencies dependedOnByList

  -- Filter to connected nodes
  when (not hasFiltered || currentSpotlight /= Just moduleId) do
    let connected = fromMaybe Set.empty $ Map.lookup moduleId adjacencyMap
        connectedIds = Set.toUnfoldable connected :: Array String
        allConnected = Array.cons moduleId connectedIds
    
    -- THIS IS THE KEY FFI CALL: filterToConnectedNodes_
    liftEffect $ pure $ filterToConnectedNodes_ simHandle keyIsID_ allConnected
    Ref.write true hasFilteredRef
    Ref.write (Just moduleId) currentSpotlightRef
    Ref.write (Set.fromFoldable allConnected) spotlightSetRef
    
    -- Switch forces
    simSt <- Ref.read simStateRef
    let updatedSimState = actualizeForcesDirect 
          (Set.fromFoldable ["manyBody-spotlight", "collision-spotlight", "center", "links"]) 
          simSt
    Ref.write updatedSimState simStateRef
    callbacks.onEnableSpotlightMode

  -- Expand the node and its neighbors
  when (not hasFiltered) do
    liftEffect $ expandNodeById_ simHandle nodeRadius declarationsData callsData moduleId true
    traverse_ (\connectedId ->
      liftEffect $ expandNodeById_ simHandle nodeRadius declarationsData callsData connectedId true
    ) connectedIds
```

### Restoring Full Graph (Spotlight Deactivation)
```purescript
let resetSpotlight = do
  hasFiltered <- Ref.read hasFilteredRef
  
  when hasFiltered do
    -- Get original data
    allNodes <- Ref.read allNodesRef
    allLinks <- Ref.read allLinksRef
    
    -- THIS IS THE KEY FFI CALL: restoreAllNodes_
    -- Restores nodes and links to simulation and re-renders DOM
    liftEffect $ restoreAllNodes_ simHandle nodesGroup linksGroup 
      (unsafeCoerce allNodes) (unsafeCoerce allLinks) nodeRadius keyIsID_
    
    -- Switch back to compact forces
    simSt <- Ref.read simStateRef
    let updatedSimState = actualizeForcesDirect 
          (Set.fromFoldable ["manyBody-compact", "collision-compact", "center", "links"]) 
          simSt
    Ref.write updatedSimState simStateRef
    
    -- Hide labels, clear state
    liftEffect $ hideModuleLabels_ nodesGroup
    Ref.write false hasFilteredRef
    Ref.write Nothing currentSpotlightRef
    Ref.write Set.empty spotlightSetRef
    callbacks.onSetCurrentSpotlightModule Nothing
    callbacks.onHideModuleDetails
    callbacks.onDisableSpotlightMode
```

---

## 6. FORCE ACTUALIZATION FUNCTIONS

### Location
`/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Internal/Simulation/Functions.purs` (lines 35-103)

### actualizeForcesDirect (Non-Monadic, for Effect Callbacks)
```purescript
actualizeForcesDirect :: Set Label -> D3SimulationState_ -> D3SimulationState_
actualizeForcesDirect activeForces simState =
  let SimState_ record = simState
      handle = record.handle_
      library = record.forceLibrary
      allLabels = M.keys library
      enableLabels = A.fromFoldable $ Set.intersection activeForces (Set.fromFoldable allLabels)
      disableLabels = A.fromFoldable $ Set.difference (Set.fromFoldable allLabels) activeForces
      -- Update statuses in the force library using the existing helper functions
      updatedLibrary = (enableByLabels handle enableLabels) <$> ((disableByLabels handle disableLabels) <$> library)
  in SimState_ $ record { forceLibrary = updatedLibrary }
```

**Why This Pattern?**
- Takes a `Set Label` of forces to activate
- Computes `enableLabels` = intersection of active set with available forces
- Computes `disableLabels` = all forces NOT in the active set
- Updates force library statuses AND syncs to D3 simulation via FFI calls
- Pure function, works in Effect callbacks where MonadState unavailable

### simulationActualizeForces (Monadic Version)
```purescript
simulationActualizeForces :: forall m row.
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Set Label ->
  m Unit
simulationActualizeForces activeForces = do
  handle <- use _handle
  library <- use _forceLibrary
  let allLabels = M.keys library
      enableLabels = A.fromFoldable $ Set.intersection activeForces (Set.fromFoldable allLabels)
      disableLabels = A.fromFoldable $ Set.difference (Set.fromFoldable allLabels) activeForces
  -- Update status in PureScript state
  simulationEnableForcesByLabel enableLabels
  simulationDisableForcesByLabel disableLabels
  -- Sync to D3 simulation (this is the critical step!)
  updatedLibrary <- use _forceLibrary
  let _ = (updateForceInSimulation handle) <$> updatedLibrary
  pure unit
```

**Differences from actualizeForcesDirect:**
- Monadic version with access to MonadState
- Can be used in Halogen action handlers
- Calls helper functions that modify lens-focused state

---

## 7. THE CRITICAL runSimulation PATTERN (CodeExplorer.purs)

### Location
`/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/CodeExplorer.purs` (lines 289-338)

### Three-Step Pipeline
```purescript
runSimulation :: forall m.
  MonadEffect m =>
  MonadState State m =>
  m Unit
runSimulation = do
  state <- get
  let allModelNodes = getModelNodes state              -- Full dataset
      allModelLinks = getModelLinks state
      nodeFilter = state.scene.chooseNodes             -- Filter predicate
      linkFilter = state.scene.linksShown
      linkForce = state.scene.linksActive
      activeForces = state.scene.activeForces          -- Set of force labels
      nodeInitializers = state.scene.nodeInitializerFunctions

  -- STEP 1: Filter - Apply node filter predicate FIRST
  let filteredNodes = filter nodeFilter allModelNodes
  
  -- STEP 2: Initialize - Run initializers on filtered data
  let initializedNodes = foldl (\nodes fn -> fn nodes) filteredNodes nodeInitializers
  
  -- STEP 3: Simulate - Pass initialized nodes to SimulationM2
  runWithD3_Simulation do
    Graph.updateSimulation
      staging.selections
      { nodes: initializedNodes          -- Already filtered and initialized
      , links: allModelLinks             -- Full link dataset
      , nodeFilter: Nothing              -- Don't filter again!
      , linkFilter: Just linkFilter
      , activeForces: activeForces       -- Which forces to enable
      , linksWithForce: linkForce
      }
      attributesWithCallback
    start
```

### Why This Order Matters (Per Comments)
1. **Tree layouts need homogeneous data**: Only modules in dependency tree, not mixed nodes/packages
2. **Initializers expect pre-filtered data**: Tree layout can't handle 884 mixed nodes, only processes ~90
3. **Don't filter twice**: Pass `nodeFilter: Nothing` to SimulationM2, already done in Step 1

**DEBUGGING NOTE:**
When filtering moved into SimulationM2 (after initializers), tree layout received unfiltered data → only generated coordinates for 1 node → tree rendered only 1 node instead of ~90.

---

## 8. HOW D3 INTERPRETER HANDLES FORCE ACTIVATION

### Location
`/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Interpreter/D3.purs`

### Force Activation Timing
```purescript
-- DURING INITIALIZATION:
-- 2. Set up nodes first (must come before forces)
nodesInSim <- simulationSetNodes config.nodes

-- 3. Activate specified forces (must come BEFORE setting links!)
-- The link force needs to be active in the simulation before links are added
simulationActualizeForces config.activeForces

-- 4. NOW set links (returns simulation-enhanced data)
-- This must come after activating forces because the link force needs to be
-- registered in the D3 simulation before we can add links to it
linksInSim <- simulationSetLinks config.links config.nodes config.keyFn

-- DURING UPDATES:
-- Step 3: Update active forces if provided (must come BEFORE setting links!)
case config.activeForces of
  Nothing -> pure unit
  Just forces -> simulationActualizeForces forces

-- Step 4: Update links if provided (must come AFTER force activation)
linksInSim <- case config.links of
  Just newLinks -> ...
```

**Critical Ordering Rule:** Forces must be activated BEFORE links are set, because D3 needs the link force registered in the simulation to properly initialize link data.

---

## 9. ARCHITECTURE DIFFERENCES: CodeExplorer vs ExpandableBubbles

### CodeExplorer Pattern
- **Simpler**: Uses scenes as complete configurations
- **Force Management**: `toggleForce` adds/removes single force at a time
- **Filtering**: Simple node predicates (all, isPackage, isUsedModule)
- **Spotlight**: Just a filter predicate change + runSimulation
- **State Mutation**: Uses lenses and helper functions (setChooseNodes, setActiveForces)

### ExpandableBubbles Pattern
- **Complex**: Manual state management with multiple Refs
- **Force Management**: Switches entire force sets (compact vs spotlight)
- **Filtering**: FFI-driven filtering with restoration of full graph
- **Spotlight**: Involves node expansion, inter-module links, details panels
- **State Preservation**: Stores original nodes/links for restoration
- **Effect Callbacks**: Uses actualizeForcesDirect for non-monadic context

### Key Architectural Differences

| Aspect | CodeExplorer | ExpandableBubbles |
|--------|--------------|-------------------|
| Force Changes | Toggle individual forces | Switch entire force configurations |
| Node Filtering | Scene predicate in configuration | FFI calls (filterToConnectedNodes_) |
| Restoration | Automatic via scene change | Explicit FFI call (restoreAllNodes_) |
| State Tracking | Scene configuration + simulation state | Additional Refs for filtering state |
| Expansion | Not used | FFI-driven (expandNodeById_) |
| Force Sync | Via simulationActualizeForces | Via actualizeForcesDirect + Refs |
| Scale | 884 nodes, multiple scenes | 100+ modules with detailed declarations |

---

## 10. KEY PATTERNS TO ADOPT

### Pattern 1: Centralized Force Library
Define all forces once in a single Map, keyed by string labels. Use node filters (ForceFilter) to apply forces selectively.

### Pattern 2: Scenes as Configuration
Encapsulate filters, forces, styling, and initializers in SceneConfig objects. Change scenes atomically.

### Pattern 3: Three-Step Simulation Pipeline
1. Filter nodes with predicate
2. Initialize with positioning functions
3. Update simulation with filtered+initialized nodes

### Pattern 4: Force Switching
- **Monadic**: Use `simulationActualizeForces` with Set of labels
- **Non-Monadic**: Use `actualizeForcesDirect` in Effect contexts
- Always pass new active force set, not diffs

### Pattern 5: Spotlight Implementation
- Simple: Change `chooseNodes` predicate + runSimulation
- Advanced: Use FFI filtering + force switching + node expansion

### Pattern 6: State Preservation
Store original data in Refs before filtering, restore explicitly when exiting spotlight.

### Pattern 7: Force Ordering
Always activate forces BEFORE setting links, because D3 needs to register the link force.

---

## 11. FFI FUNCTIONS USED

| Function | Module | Purpose |
|----------|--------|---------|
| `filterToConnectedNodes_` | FFI | Filter simulation to show only specified node IDs |
| `restoreAllNodes_` | FFI | Restore full nodes and links to simulation and re-render |
| `expandNodeById_` | FFI | Expand a node to show its internal declarations |
| `showModuleLabels_` | FFI | Show/unhide module name labels |
| `hideModuleLabels_` | FFI | Hide module name labels |
| `updateNodeExpansion_` | FFI | Update node expansion state |
| `drawInterModuleDeclarationLinks_` | FFI | Draw links between modules' declarations |
| `enableByLabels` | Internal | Enable forces by label |
| `disableByLabels` | Internal | Disable forces by label |
| `updateForceInSimulation` | Internal | Sync force state to D3 |

---

## 12. CONCRETE EXAMPLE: Adding Spotlight to ExpandableBubbles

```purescript
-- In SpotlightModule:
let spotlightModule moduleId = do
  -- 1. Show details and update UI
  case Map.lookup moduleId modulesMap of
    Just moduleInfo -> callbacks.onShowModuleDetails ...
    Nothing -> pure unit

  -- 2. Filter to connected nodes (FFI call)
  when (not hasFiltered || currentSpotlight /= Just moduleId) do
    let connected = fromMaybe Set.empty $ Map.lookup moduleId adjacencyMap
        allConnected = Array.cons moduleId $ Set.toUnfoldable connected
    liftEffect $ filterToConnectedNodes_ simHandle keyIsID_ allConnected
    Ref.write true hasFilteredRef
    Ref.write (Just moduleId) currentSpotlightRef

  -- 3. Switch to spotlight forces
  when (not hasFiltered) do
    simSt <- Ref.read simStateRef
    let updatedSimState = actualizeForcesDirect 
          (Set.fromFoldable ["manyBody-spotlight", "collision-spotlight", "center", "links"]) 
          simSt
    Ref.write updatedSimState simStateRef

  -- 4. Expand nodes
  liftEffect $ expandNodeById_ simHandle nodeRadius declsData callsData moduleId true
  traverse_ (\connectedId -> 
    liftEffect $ expandNodeById_ simHandle nodeRadius declsData callsData connectedId true
  ) (Set.toUnfoldable connected)

-- In ResetSpotlight:
let resetSpotlight = do
  hasFiltered <- Ref.read hasFilteredRef
  when hasFiltered do
    -- Restore original data
    allNodes <- Ref.read allNodesRef
    allLinks <- Ref.read allLinksRef
    liftEffect $ restoreAllNodes_ simHandle nodesGroup linksGroup 
      (unsafeCoerce allNodes) (unsafeCoerce allLinks) nodeRadius keyIsID_
    
    -- Switch back to compact forces
    simSt <- Ref.read simStateRef
    let updatedSimState = actualizeForcesDirect 
          (Set.fromFoldable ["manyBody-compact", "collision-compact", "center", "links"]) 
          simSt
    Ref.write updatedSimState simStateRef
```

---

## Summary

The CodeExplorer and ExpandableBubbles components demonstrate a sophisticated architecture for interactive force-directed visualizations:

1. **Forces** are organized in a centralized library with filtering support
2. **Scenes** encapsulate all configuration needed for visualization
3. **Spotlight** is implemented via node filtering + force switching
4. **Restoration** happens explicitly via FFI or automatically via scene changes
5. **Force Actualization** works both monadically and non-monadically
6. **Pipeline Order** matters: Filter → Initialize → Update Simulation
7. **FFI Integration** enables imperative filtering and restoration operations

The architecture scales from simple spotlight toggles (CodeExplorer) to complex interactive bubbles with detailed expansion (ExpandableBubbles).
