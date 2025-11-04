# CodeExplorer & ExpandableBubbles: Quick Reference Guide

## File Locations

### CodeExplorer Component
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/CodeExplorer.purs` - Main component with action handler
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/CodeExplorerWrapper.purs` - Wrapper component
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/Forces.purs` - Centralized force library (16 forces)
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/State.purs` - State types & helper functions
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/Scenes.purs` - Scene configurations (5 scenes)
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/Actions.purs` - Action types
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeExplorer/Data.purs` - Data loading

### ExpandableBubbles Component
- `/Users/andrew/work/PureScript Tagless D3/src/website/Component/CodeAtlas/Tabs/ExpandableBubbles.purs` - Full implementation

### Core Libraries
- `/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Internal/Simulation/Functions.purs` - Force actualization & simulation functions
- `/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Internal/FFI.purs` - FFI declarations
- `/Users/andrew/work/PureScript Tagless D3/src/lib/PSD3/Interpreter/D3.purs` - D3 interpreter with force handling

---

## Key Concepts at a Glance

### 1. Force Library Setup
**File:** `Forces.purs`

A centralized Map of all forces, keyed by string labels:
- "center", "x", "y" - Positioning forces
- "collide1", "collide2" - Collision avoidance
- "charge1", "charge2", "chargetree" - Repulsion forces
- "clusterx_P", "clustery_P" - Grid forces for packages
- "clusterx_M", "clustery_M" - Grid forces for modules
- "htreeNodesX", "htreeNodesY", "vtreeNodesX", "vtreeNodesY" - Tree layout forces
- "packageOrbit", "moduleOrbit", "unusedOrbit" - Radial forces
- "links" - Link force

Each force can have a node filter (ForceFilter) to apply selectively.

### 2. Scenes as Configurations
**File:** `Scenes.purs`

Five complete scene definitions:
- **packageGridScene** - All nodes in hierarchical grid (cluster view)
- **packageGraphScene** - Packages only, force-directed
- **layerSwarmScene** - Modules with horizontal layering
- **radialTreeScene** - Dependency tree radiating from root
- **horizontalTreeScene** - Dependency tree flowing left-to-right
- **verticalTreeScene** - Dependency tree flowing top-to-bottom

Each scene specifies:
- Node filter predicate
- Link visibility filter
- Which links exert force
- Which forces to activate (Set of labels)
- CSS class
- D3 attributes
- Node initialization functions

### 3. Simple Spotlight Pattern
**File:** `CodeExplorer.purs` lines 126-134

Just change the node filter and call runSimulation:
```purescript
ToggleChildrenOfNode id -> do
  H.modify_ $ setChooseNodes (isPackageOrVisibleModule id)
  runSimulation
```

### 4. Advanced Spotlight Pattern
**File:** `ExpandableBubbles.purs` lines 500-675

More complex, involving:
- FFI filtering: `filterToConnectedNodes_`
- FFI restoration: `restoreAllNodes_`
- Force switching: `actualizeForcesDirect`
- Node expansion: `expandNodeById_`
- State preservation: Store original nodes/links in Refs

### 5. Force Switching
**Options:**
1. **Monadic** (in action handlers): `simulationActualizeForces` from Functions.purs
2. **Non-Monadic** (in Effect callbacks): `actualizeForcesDirect` from Functions.purs

Both take a Set of force labels and activate/deactivate accordingly.

### 6. The runSimulation Pipeline
**File:** `CodeExplorer.purs` lines 289-338

Three-step process (CRITICAL ORDER):
1. Filter nodes with predicate
2. Initialize filtered nodes with positioning functions
3. Update simulation with filtered+initialized nodes (nodeFilter: Nothing!)

---

## Code Examples

### Example 1: Creating a New Force
```purescript
createForce "myForce" 
  (RegularForce ForceManyBody) 
  (Just $ ForceFilter "specific nodes" \d -> datum_.isSpecial d)
  [ F.strength (-50.0), F.theta 0.9 ]
```

### Example 2: Creating a Scene
```purescript
myScene :: SceneConfig
myScene = {
  chooseNodes: isSpecialNode,
  linksShown: isSpecialLink,
  linksActive: const true,
  cssClass: "my-scene",
  attributes: mySceneAttributes,
  activeForces: Set.fromFoldable ["myForce", "collide2", "center", "links"],
  nodeInitializerFunctions: [unpinAllNodes, positionNodesInGrid]
}
```

### Example 3: Switching Scenes
```purescript
Scene PackageGrid -> do
  H.modify_ $ applySceneConfig packageGridScene
  runSimulation
```

### Example 4: Toggling a Single Force
```purescript
ToggleForce label -> do
  H.modify_ $ toggleForce label
  runSimulation
```

### Example 5: Spotlight Mode (Simple)
```purescript
SpotlightNode id -> do
  H.modify_ $ setChooseNodes (connectedTo id)
  runSimulation
```

### Example 6: Spotlight Mode (Advanced)
```purescript
-- Filter to specific nodes
liftEffect $ filterToConnectedNodes_ simHandle keyIsID_ connectedNodeIds

-- Switch forces
simSt <- Ref.read simStateRef
let updatedSim = actualizeForcesDirect (Set.fromFoldable ["spotlight-forces"]) simSt
Ref.write updatedSim simStateRef

-- Restore full graph
liftEffect $ restoreAllNodes_ simHandle nodesGroup linksGroup origNodes origLinks nodeRadius keyIsID_
```

---

## State Type Overview

### CodeExplorer State
```purescript
type State = {
  simulation :: D3SimulationState_,      -- D3 simulation state (nodes, links, forces)
  model :: Maybe SpagoModel,             -- Full dataset
  staging :: Staging D3Selection_ SpagoDataRow,  -- Filtered data ready for D3
  scene :: MiseEnScene,                  -- Current scene configuration
  eventListener :: Maybe (HS.Listener Action),
  tags :: Map.Map NodeID (Set String),   -- Ad-hoc node tagging
  showWelcome :: Boolean
}
```

### MiseEnScene (SceneConfig)
```purescript
type SceneConfig = {
  chooseNodes :: (SpagoSimNode -> Boolean),
  linksShown :: (SpagoGraphLinkID -> Boolean),
  linksActive :: (Datum_ -> Boolean),
  activeForces :: Set Label,
  cssClass :: String,
  attributes :: SpagoSceneAttributes,
  nodeInitializerFunctions :: Array (Array SpagoSimNode -> Array SpagoSimNode)
}
```

---

## Important Insights

### Force Ordering Rule
Forces must be activated BEFORE links are set, because D3 needs the link force registered first.

### Pipeline Order Matters
Filter → Initialize → Update Simulation (do NOT filter again in SimulationM2)

### Two Versions of Force Switching
- **simulationActualizeForces** - Works in MonadState (action handlers)
- **actualizeForcesDirect** - Works in Effect (callbacks, non-monadic)

### Spotlight Can Be Simple or Complex
- **Simple:** Just change node filter + runSimulation
- **Complex:** Requires FFI filtering, restoration, force switching, and state tracking

### Scenes Encapsulate Everything
Change scene = automatically get different filters, forces, styling, and initialization in one atomic operation.

---

## Common Tasks

### Add a New Force to Library
1. Edit `Forces.purs`
2. Add entry to forceLibrary array with unique label
3. Use that label in scene's activeForces Set

### Add a New Scene
1. Create function in `Scenes.purs` returning SceneConfig
2. Add case in `handleAction` for Scene constructor
3. Call `H.modify_ $ applySceneConfig myScene` then `runSimulation`

### Implement Basic Spotlight
1. Define filter predicate
2. Call `H.modify_ $ setChooseNodes filterPredicate`
3. Call `runSimulation`

### Implement Advanced Spotlight
1. Store original nodes/links in Refs
2. Call `filterToConnectedNodes_` to filter
3. Call `actualizeForcesDirect` to switch forces
4. Call `restoreAllNodes_` to restore
5. Update state Refs accordingly

### Toggle Individual Force
1. Call `H.modify_ $ toggleForce "label"`
2. Call `runSimulation`

### Switch Force Sets
1. Create Set of force labels for each mode
2. Call `actualizeForcesDirect` with appropriate Set
3. Store updated state in Ref if non-monadic

---

## Debugging Tips

### Force Not Activating?
- Check force label is exactly right in forceLibrary
- Verify label is in activeForces Set when calling actualizeForcesDirect
- Check node filter allows nodes that the force applies to

### Spotlight Showing Wrong Nodes?
- Verify filterToConnectedNodes_ is called with correct node IDs
- Check adjacency map is built correctly
- Confirm keyIsID_ function works with your data

### Tree Layout Showing Only 1 Node?
- Likely filtering happening AFTER initializers in SimulationM2
- Move filter to Step 1 in runSimulation pipeline
- Pass nodeFilter: Nothing to SimulationM2 to prevent double-filtering

### Layout Not Changing When Scene Switches?
- Verify activeForces Set is different between scenes
- Check nodeInitializerFunctions array is being applied
- Confirm scene's chooseNodes filter is different

---

## References
- Full analysis: `CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md`
- CodeExplorer: 884 nodes, 5 scenes, 16 forces
- ExpandableBubbles: 100+ modules, dual force configurations, FFI-driven filtering
