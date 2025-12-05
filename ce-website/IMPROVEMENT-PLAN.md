# CE-Website Improvement Plan

This document outlines a comprehensive plan to improve the ce-website demo to be exemplary
in terms of idiomatic PureScript, proper PSD3 library usage, and clean Halogen integration.

## Overview

The plan is structured in two phases:
1. **Library Enhancements** - Improve PSD3 libraries to properly support the demo's needs
2. **Demo Refactoring** - Refactor ce-website to use the improved library APIs idiomatically

We start with library enhancements because several ce-website anti-patterns exist
specifically because the library doesn't provide proper alternatives.

---

## Phase 1: Library Enhancements (psd3-simulation) ✅ COMPLETE

All Phase 1 enhancements have been implemented in `psd3-simulation/src/PSD3/ForceEngine/`:
- `Events.purs` - Simulation event types and callbacks
- `Halogen.purs` - Halogen subscription integration
- `Registry.purs` - Named simulation registry
- `Simulation.purs` - First-class drag behavior (`attachDrag`, `attachGroupDrag`)

### 1.1 Simulation Event System ✅

**Problem:** ce-website uses global `Ref` state and polling because there's no way to
get events from the simulation back to Halogen.

**Current API:**
```purescript
onTick :: Effect Unit -> Simulation row linkRow -> Effect Unit
```

**Enhancement:** Add richer event callbacks that can integrate with Halogen's message system.

```purescript
-- New module: PSD3.ForceEngine.Events
module PSD3.ForceEngine.Events
  ( SimulationEvent(..)
  , SimulationCallbacks
  , defaultCallbacks
  , onSimulationEvent
  , onNodeClick
  , onNodeDragStart
  , onNodeDragEnd
  , onSimulationStart
  , onSimulationStop
  , onAlphaChange
  ) where

-- | Events that can occur during simulation lifecycle
data SimulationEvent row
  = Tick (Array (SimulationNode row))  -- Each tick with current nodes
  | AlphaChanged Number                 -- Alpha threshold crossed (e.g., 0.1, 0.01)
  | SimulationStarted
  | SimulationStopped
  | NodeClicked (SimulationNode row)
  | NodeDragStarted (SimulationNode row)
  | NodeDragEnded (SimulationNode row)
  | NodeHovered (SimulationNode row)
  | NodeUnhovered (SimulationNode row)

-- | Callbacks record (all optional via Maybe)
type SimulationCallbacks row =
  { onTick :: Maybe (Array (SimulationNode row) -> Effect Unit)
  , onAlphaChange :: Maybe (Number -> Effect Unit)
  , onStart :: Maybe (Effect Unit)
  , onStop :: Maybe (Effect Unit)
  , onNodeClick :: Maybe (SimulationNode row -> Effect Unit)
  , onNodeDragStart :: Maybe (SimulationNode row -> Effect Unit)
  , onNodeDragEnd :: Maybe (SimulationNode row -> Effect Unit)
  , onNodeHover :: Maybe (SimulationNode row -> Effect Unit)
  , onNodeUnhover :: Maybe (SimulationNode row -> Effect Unit)
  }

defaultCallbacks :: forall row. SimulationCallbacks row
defaultCallbacks = { onTick: Nothing, onAlphaChange: Nothing, ... }

-- | Set callbacks for a simulation
setCallbacks :: forall row linkRow.
  SimulationCallbacks row -> Simulation row linkRow -> Effect Unit
```

**Files to modify:**
- `psd3-simulation/src/PSD3/ForceEngine/Simulation.purs` - Add callbacks field to Simulation type
- `psd3-simulation/src/PSD3/ForceEngine/Events.purs` - New module for event types
- `psd3-simulation/src/PSD3/ForceEngine/Core.js` - Hook into tick loop for callbacks

**Estimated effort:** Small (add callbacks plumbing, most logic exists)

**Status:** ✅ Implemented in `Events.purs`

---

### 1.2 First-Class Drag Behavior Integration ✅

**Problem:** `SimulationDrag` in `Behavior.Types` stores only a `String` ID, but the
`on` function in `Operations.purs` has no way to access actual simulation handles:

```purescript
-- Current (broken):
applyBehavior (Drag (SimulationDrag _)) _ =
  -- Simulation drag requires simulation handle, which is only available in D3v2SimM
  -- This case should not be reached when calling from D3v2M
  pure unit
```

ce-website works around this by calling `Core.attachGroupDragWithReheat` directly.

**Enhancement:** Change `SimulationDrag` to carry the actual simulation handle.

```purescript
-- In PSD3v2.Behavior.Types:
data DragConfig row linkRow
  = SimpleDrag
  | SimulationDrag (Simulation row linkRow)  -- Carry the simulation handle
  | CustomDrag
      { onStart :: Effect Unit
      , onDrag :: { dx :: Number, dy :: Number } -> Effect Unit
      , onEnd :: Effect Unit
      }
```

Then in `Operations.purs`:
```purescript
applyBehavior (Drag (SimulationDrag sim)) element = do
  -- Actually attach simulation-aware drag
  attachSimulationDrag_ element sim.nodes (reheat sim)
```

**Alternative approach:** Keep DragConfig simple but add a dedicated function:

```purescript
-- In PSD3.ForceEngine.Simulation:
attachDragToSelection :: forall row linkRow datum.
  Simulation row linkRow
  -> Selection state elem datum
  -> Effect Unit
```

This is cleaner because it keeps simulation concerns in psd3-simulation rather than
leaking into psd3-selection.

**Files to modify:**
- `psd3-simulation/src/PSD3/ForceEngine/Simulation.purs` - Add `attachDrag` function
- `psd3-simulation/src/PSD3/ForceEngine/Core.js` - Already has `attachGroupDragWithReheat_`
- Remove broken `SimulationDrag` case from `psd3-selection/src/PSD3v2/Selection/Operations.purs`

**Estimated effort:** Small

**Status:** ✅ Implemented in `Simulation.purs` (`attachDrag`, `attachGroupDrag`)

---

### 1.3 Simulation Subscriptions for Halogen ✅

**Problem:** Halogen components need to receive simulation events as `Action`s, but
the current callback-based API requires effectful setup.

**Enhancement:** Provide a subscription-based API that works with Halogen's `Subscribe`.

```purescript
-- New module: PSD3.ForceEngine.Halogen
module PSD3.ForceEngine.Halogen
  ( subscribeToSimulation
  , SimulationEmitter
  ) where

import Halogen.Subscription as HS

-- | Create a Halogen subscription emitter for simulation events
subscribeToSimulation :: forall row linkRow.
  Simulation row linkRow
  -> Effect (HS.Emitter (SimulationEvent row))
```

**Usage in Halogen:**
```purescript
-- In component Initialize:
handleAction Initialize = do
  sim <- liftEffect $ Sim.create defaultConfig
  emitter <- liftEffect $ subscribeToSimulation sim
  void $ H.subscribe $ emitter <#> \event -> case event of
    Sim.Tick nodes -> UpdateNodePositions nodes
    Sim.NodeClicked node -> SelectNode node.id
    Sim.AlphaChanged alpha -> UpdateAlpha alpha
    _ -> NoOp
```

**Files to modify:**
- `psd3-simulation/src/PSD3/ForceEngine/Halogen.purs` - New module
- `psd3-simulation/spago.yaml` - Add halogen-subscriptions dependency

**Estimated effort:** Medium (new integration layer)

**Status:** ✅ Implemented in `Halogen.purs` (`subscribeToSimulation`)

---

### 1.4 Multiple Named Simulations ✅

**Problem:** ce-website creates separate simulations for main view and popup call graph,
but there's no registry or naming convention.

**Current state:** Multiple simulations work (each is independent `Ref`-based state),
but coordination is ad-hoc.

**Enhancement:** Optional simulation registry for debugging and coordination.

```purescript
-- In PSD3.ForceEngine.Simulation:

-- | Create a named simulation (registered globally for debugging)
createNamed :: forall row linkRow.
  String -> SimConfig -> Effect (Simulation row linkRow)

-- | Get all running simulations (for debugging)
getAllSimulations :: Effect (Array { name :: String, alpha :: Number, running :: Boolean })

-- | Stop all simulations (useful for cleanup)
stopAll :: Effect Unit
```

**Estimated effort:** Small (convenience API, existing functionality)

**Status:** ✅ Implemented in `Registry.purs` (`register`, `lookup`, `stopAll`, `debugRegistry`)

---

## Phase 2: Demo Refactoring (ce-website)

### 2.1 Remove Global State Refs 🚧 IN PROGRESS

**Problem:** Nine `globalXxxRef` values created via `unsafePerformEffect`:
1. `globalStateRef` - Scene engine state
2. `globalLinksRef` - Links data
3. `globalDeclarationsRef` - Module declarations
4. `globalFunctionCallsRef` - Function calls
5. `globalModelInfoRef` - Project metadata
6. `globalViewStateRef` - Current view state
7. `globalNavigationStackRef` - Navigation history
8. `globalFocusRef` - Neighborhood focus state
9. `globalCallbacksRef` - Halogen callbacks

**Solution:** Move all state into Halogen component state.

```purescript
-- New state structure in SpagoGridApp.purs:
type State =
  { -- Model data
    model :: Maybe LoadedModel
  , declarations :: DeclarationsMap
  , functionCalls :: FunctionCallsMap

  -- View state
  , viewState :: ViewState
  , navigationStack :: Array ViewState

  -- Simulation (Maybe because created async)
  , simulation :: Maybe (Sim.Simulation SimNodeRow SimLinkRow)

  -- UI state
  , packagePalette :: Array ColorEntry
  , projects :: Array Project
  , selectedProjectId :: Int
  , errorMessage :: Maybe String
  }
```

**Migration steps:**
1. Create new State type with all data
2. Pass state down to Explorer functions as arguments
3. Use Halogen subscriptions for simulation events
4. Remove `unsafePerformEffect` refs one by one

**Progress (Dec 2024):**
- ✅ Modified `renderSVG` to take `ViewState` as parameter (removed global ref read)
- ✅ Modified `renderNodesOnly` to take `ViewState` as parameter (removed global ref read)
- ✅ Updated 4 call sites to pass ViewState explicitly
- 🚧 Remaining: 3 reads of `globalViewStateRef` in state mutation code
- 🚧 Remaining: 8 other global refs still in use

**Files to modify:**
- `ce-website/src/Component/SpagoGridApp.purs` - State restructure
- `ce-website/src/Engine/Explorer.purs` - Remove refs, take state as arguments
- `ce-website/src/Engine/Scene.purs` - Pass state explicitly

**Estimated effort:** Large (structural change)

---

### 2.2 Remove Polling Loop

**Problem:** `pollLoop` checks global refs every 100ms.

**Solution:** Use simulation subscriptions from Phase 1.3.

```purescript
-- Before:
pollLoop = do
  viewState <- liftEffect $ Ref.read Explorer.globalViewStateRef
  H.liftAff $ delay (Milliseconds 100.0)
  pollLoop

-- After:
handleAction Initialize = do
  -- ... create simulation ...
  emitter <- liftEffect $ subscribeToSimulation sim
  void $ H.subscribe $ emitter <#> SimulationEvent

handleAction (SimulationEvent event) = case event of
  Sim.NodeClicked node -> do
    -- Handle node click, update state directly
    H.modify_ _ { selectedModule = Just node.name }
  Sim.Tick nodes -> do
    -- Update DOM positions via minimal effect
    liftEffect $ updateGroupPositions nodes
```

**Files to modify:**
- `ce-website/src/Component/SpagoGridApp.purs` - Replace polling with subscriptions

**Estimated effort:** Medium

---

### 2.3 Move API Communication from FFI to PureScript ✅

**Problem:** `CallGraphPopup.js` makes HTTP requests and handles JSON.

**Solution:** Move all API calls to `Data/Loader.purs`, have JS only handle DOM.

**Status:** ✅ CallGraphPopup is now a pure Halogen component (`Component.CallGraphPopup.purs`)
that uses `Data.Loader` for API calls. The old JS FFI files were deleted.

```purescript
-- In Data/Loader.purs (already has similar functions):
fetchFunctionCallDetails :: String -> String -> Aff (Either String FunctionCallDetails)
fetchFunctionCallDetails moduleName declarationName = do
  callsResult <- fetchModuleFunctionCalls moduleName
  declResult <- fetchModuleDeclarations moduleName
  metricsResult <- fetchModuleMetrics moduleName
  -- Combine and return
```

```javascript
// In CallGraphPopup.js - only DOM operations:
export const showCallGraphPopup_ = (data) => () => {
  // data is already fetched and passed from PureScript
  const popup = document.getElementById("call-graph-popup");
  // ... DOM manipulation only ...
};
```

**Files to modify:**
- `ce-website/src/Data/Loader.purs` - Add combined fetch functions
- `ce-website/src/Engine/CallGraphPopup.purs` - Orchestrate fetch, call FFI with data
- `ce-website/src/Engine/CallGraphPopup.js` - Remove fetch logic, only render

**Estimated effort:** Medium

---

### 2.4 Replace AtomicView.js Force Simulation ✅

**Problem:** `AtomicView.js` creates a second D3 force simulation entirely in JavaScript.

**Solution:** Use PSD3-simulation for the popup's force layout.

**Status:** ✅ AtomicView.js was identified as dead code and deleted. The call graph
functionality is now provided by the pure Halogen `CallGraphPopup` component.

```purescript
-- New: ce-website/src/Engine/AtomicView.purs
module Engine.AtomicView
  ( showAtomicView
  , hideAtomicView
  ) where

import PSD3.ForceEngine.Simulation as Sim

-- | Show the atomic (function call graph) view
showAtomicView :: FunctionCallDetails -> Effect (Maybe (Sim.Simulation CallNodeRow ()))
showAtomicView details = do
  -- Create popup DOM structure (minimal FFI)
  createPopupDOM details.moduleName details.declarationName

  -- Build nodes and links from PureScript data
  let { nodes, links } = buildCallGraph details

  -- Create simulation
  sim <- Sim.create smallGraphConfig
  Sim.setNodes nodes sim
  Sim.setLinks links sim

  -- Add forces (using PSD3)
  Sim.addForce (ForceLink "links" linkConfig) sim
  Sim.addForce (ManyBody "charge" chargeConfig) sim
  Sim.addForce (PositionX "x" xConfig) sim
  Sim.addForce (PositionY "y" yConfig) sim

  -- Set tick callback to update DOM
  Sim.onTick (updatePopupPositions) sim

  -- Start simulation
  Sim.start sim
  pure (Just sim)
```

**Files to modify:**
- `ce-website/src/Engine/AtomicView.purs` - New PureScript implementation
- `ce-website/src/Engine/AtomicView.js` - Reduce to DOM-only operations

**Estimated effort:** Medium

---

### 2.5 Consolidate Color Functions ✅

**Problem:** Color logic duplicated across Explorer, ColorPalette, NarrativePanel.

**Solution:** Single source of truth in `Data/ColorPalette.purs`.

**Status:** ✅ Complete. ColorPalette.purs provides `getNodeStroke`, `getNodeFill`,
`getClusterColor`, and `getPalette` for declaration types. NarrativePanel now uses
`LegendItem` directly from ColorPalette instead of a deprecated local type.

**Files modified:**
- `ce-website/src/Data/ColorPalette.purs` - Already well-organized
- `ce-website/src/Component/NarrativePanel.purs` - Removed deprecated `DeclarationKind` type,
  now uses `LegendItem` directly from ColorPalette

**Estimated effort:** Small

---

### 2.6 Remove Deprecated Code ✅

**Problem:** `PackageGrid`, `ModuleOrbit`, `DependencyTree` ViewStates marked deprecated.

**Solution:** Remove them entirely.

**Status:** ✅ Already complete. ViewState.purs only has: `Treemap`, `TreeLayout`,
`ForceLayout`, `Neighborhood`, `FunctionCalls`. The deprecated constructors were
removed in a previous cleanup.

**Estimated effort:** Small (already done)

---

### 2.7 Type-Safe Scene Selection ✅

**Problem:** Scene selection uses strings instead of types.

**Solution:** Use the existing `SceneConfig` type directly.

**Status:** ✅ Already implemented. `Explorer.purs` has `data SceneId = GridForm | GridRun | OrbitForm | ...`
and `goToScene :: SceneId -> Ref Scene.SceneState -> Effect Unit`.

```purescript
-- Before:
goToScene :: String -> Ref Scene.SceneState -> Effect Unit
goToScene sceneName stateRef = case sceneName of
  "GridForm" -> ...

-- After:
data SceneId = GridForm | GridRun | OrbitForm | OrbitRun | TreeForm | TreeRun

goToScene :: SceneId -> Ref Scene.SceneState -> Effect Unit
goToScene sceneId stateRef = do
  let scene = sceneConfigFor sceneId
  ...

sceneConfigFor :: SceneId -> SceneConfig SimNode SimLink
sceneConfigFor = case _ of
  GridForm -> Scenes.gridFormScene
  GridRun -> Scenes.gridRunScene
  ...
```

**Files to modify:**
- `ce-website/src/Engine/Explorer.purs` - Add SceneId type, refactor goToScene
- `ce-website/src/Component/SpagoGridApp.purs` - Use SceneId instead of strings

**Estimated effort:** Small

---

## Execution Order

### ✅ Completed Tasks (as of Dec 2024)

**Phase 1 - All Complete:**
- ✅ 1.1 Simulation Event System (`Events.purs`)
- ✅ 1.2 First-Class Drag Integration (`Simulation.purs`)
- ✅ 1.3 Simulation Subscriptions for Halogen (`Halogen.purs`)
- ✅ 1.4 Multiple Named Simulations (`Registry.purs`)

**Phase 2 - Partially Complete:**
- ✅ 2.3 Move API Communication from FFI (CallGraphPopup is pure Halogen)
- ✅ 2.4 Replace AtomicView.js (deleted as dead code)
- ✅ 2.5 Consolidate Color Functions (NarrativePanel uses LegendItem from ColorPalette)
- ✅ 2.6 Remove Deprecated Code (deprecated ViewState constructors already removed)
- ✅ 2.7 Type-Safe Scene Selection (SceneId type exists)

### Remaining Tasks (Priority Order)

1. **2.1** Remove Global State Refs (major refactor, in progress)
2. **2.2** Remove Polling Loop (depends on 2.1)

---

## Success Criteria

After completing this plan:

1. **No `unsafePerformEffect`** in ce-website
2. **No polling loops** - all updates are event-driven
3. **FFI is DOM-only** - no business logic, API calls, or simulation in JS
4. **Single source of truth** for colors, view state, scene selection
5. **Type-safe APIs** throughout
6. **Halogen owns state** - D3 is a renderer, not a state manager
7. **PSD3 libraries demonstrate** their intended usage patterns

---

## Open Questions

1. **Ephemeral highlighting exception**: The review suggested highlighting could be
   exempt from Halogen state. Should we:
   - a) Keep highlight state in DOM (current approach)
   - b) Track highlighted node in Halogen state
   - c) Use CSS-only hover effects where possible

2. **Multiple simulations coordination**: When popup opens, should main simulation:
   - a) Pause (freeze positions)
   - b) Continue running
   - c) Slow down (reduce alpha target)

3. **Scene system complexity**: Is the `initRules`/`layout`/`finalRules` pattern
   worth keeping, or should we simplify to just position interpolation?
