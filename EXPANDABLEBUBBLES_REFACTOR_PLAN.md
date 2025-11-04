# ExpandableBubbles Refactoring Plan

## Current Problem

When clicking "Unspotlight", nodes are added back to the DOM but NOT to the D3 simulation. This is because:

1. **`filterToConnectedNodes_` (FFI)** - Removes nodes from simulation by calling `simulation.nodes(filteredNodes)`
2. **`restoreAllNodes_` (FFI)** - Re-renders DOM elements but only calls `simulation.nodes(allNodes)` without proper D3 simulation update
3. **Root cause**: The simulation has internal state (force calculations, velocities, alpha) that needs proper updating through the D3 API, not just setting `.nodes()`

## CodeExplorer's Solution

CodeExplorer doesn't use FFI filtering at all. Instead:

### 1. Uses `SimulationM2.update` function
```purescript
-- From CodeExplorer/CodeExplorer.purs:270-290
update
  { nodes: Just filteredNodes        -- Pre-filtered nodes
  , links: Just filteredLinks        -- Pre-filtered links
  , nodeFilter: Nothing               -- Already filtered, don't filter again
  , linkFilter: Nothing
  , activeForces: Just scene.forces  -- Switch force set atomically
  , config: Nothing
  , keyFn: keyIsID_
  }
```

**Key insight**: The `update` function properly:
- Updates simulation's internal node array
- Re-swizzles links (converts IDs to object references)
- Updates force configurations
- Restarts simulation with proper alpha
- Handles all D3 internals correctly

### 2. Stores visible node set in State, not Refs
```purescript
-- From CodeExplorer/State.purs
type State =
  { ...
  , visibleNodeIds :: Set String  -- Which nodes should be shown
  , currentScene :: SceneId
  }
```

### 3. Monadic scene switching, not Effect callbacks
```purescript
-- Scene switching happens in handleAction (monadic context)
handleAction (SwitchScene newScene) = do
  let filteredNodes = filter (sceneNodePredicate newScene) allNodes
      filteredLinks = filterLinks filteredNodes allLinks

  -- This is in the monad, so we can call update
  update
    { nodes: Just filteredNodes
    , links: Just filteredLinks
    , activeForces: Just (scene.forces newScene)
    , ...
    }
```

## Recommended Refactoring Approach

### Phase 1: Create Force Library (DONE ✓)
Created `ExpandableBubblesForces.purs` with:
- Compact forces (overview mode)
- Spotlight forces (detailed mode)
- Centralized force configuration

### Phase 2: Add Spotlight State to Monadic Context
Instead of:
```purescript
-- Current: Effect callbacks with Refs
let resetSpotlight = do  -- Effect
      allNodes <- Ref.read allNodesRef
      liftEffect $ restoreAllNodes_ simHandle ...
```

Change to:
```purescript
-- Proposed: Store in component state, handle in action
data Action
  = ...
  | SpotlightModule String
  | ResetSpotlight

handleAction (SpotlightModule moduleId) = do
  state <- H.get
  let spotlightSet = getConnectedModules moduleId state.moduleGraphData
      filteredNodes = filter (\n -> Set.member n.id spotlightSet) state.allNodes
      filteredLinks = filterLinks filteredNodes state.allLinks

  -- Use the monadic update function
  void $ liftEffect $ runWithD3_Simulation state.simulation $ update
    { nodes: Just filteredNodes
    , links: Just filteredLinks
    , nodeFilter: Nothing  -- Already filtered
    , activeForces: Just spotlightForces
    , keyFn: keyIsID_
    }

  H.modify_ _ { spotlightModeActive = true, currentSpotlightModule = Just moduleId }

handleAction ResetSpotlight = do
  state <- H.get

  -- Reset to all nodes
  void $ liftEffect $ runWithD3_Simulation state.simulation $ update
    { nodes: Just state.allNodes
    , links: Just state.allLinks
    , nodeFilter: Nothing
    , activeForces: Just compactForces
    , keyFn: keyIsID_
    }

  H.modify_ _ { spotlightModeActive = false, currentSpotlightModule = Nothing }
```

### Phase 3: Integrate with Existing Click Handlers

The click handlers need to dispatch actions instead of calling Effect callbacks:

```purescript
let onClick event datum _ = do
      let clickedId = datum_.id datum
      -- Dispatch to Halogen action system
      callbacks.onShowContextMenu clickedId x y
```

In the Halogen component:
```purescript
handleAction (ShowContextMenu info) = do
  -- Context menu shows, user clicks "Spotlight" or "Unspotlight"
  H.modify_ _ { contextMenu = Just info }

handleAction (SpotlightModuleFromMenu moduleName) = do
  H.modify_ _ { contextMenu = Nothing }
  handleAction (SpotlightModule moduleName)  -- Reuse monadic handler
```

### Phase 4: Remove FFI Filtering Functions

Delete or deprecate:
- `filterToConnectedNodes_` - Replaced by pre-filtering + update
- `restoreAllNodes_` - Replaced by update with all nodes
- `resetNodeFilter_` - Replaced by update
- `switchToSpotlightForces_` / `switchToCompactForces_` - Replaced by update with activeForces

Keep:
- `expandNodeById_` - Still needed for visual expansion
- `showModuleLabels_` / `hideModuleLabels_` - Visual-only effects
- `drawInterModuleDeclarationLinks_` - Declaration rendering

## Key Architecture Changes

### Before (Current):
```
Click → Effect callback → FFI manipulation → Manual DOM update → State sync issues
```

### After (Proposed):
```
Click → Dispatch Action → Halogen handleAction → update (monadic) → D3 handles everything
```

## Benefits

1. **Correct simulation state**: D3 properly manages all nodes with positions, velocities
2. **Simpler code**: No manual DOM manipulation, D3 data join handles it
3. **Consistent with CodeExplorer**: Same patterns, easier to maintain
4. **Better force management**: Proper force library with centralized config
5. **State in one place**: Halogen state, not scattered Refs

## Migration Path

### Minimal change (quickest fix):
1. Keep current callback structure
2. Change `resetSpotlight` to call `update` via `runWithD3_Simulation`
3. Store simulation state in a way accessible from Effect

### Proper refactor (recommended):
1. Move spotlight logic into Halogen actions
2. Use `update` for all filtering/restoration
3. Remove FFI filtering functions
4. Clean up Ref usage

### Example: Minimal Fix for resetSpotlight

```purescript
let resetSpotlight = do
      Console.log "Resetting spotlight to overview"
      hasFiltered <- Ref.read hasFilteredRef

      when hasFiltered do
        -- Get original data
        allNodes <- Ref.read allNodesRef
        allLinks <- Ref.read allLinksRef
        simState <- Ref.read simStateRef

        -- Use proper simulation update (monadic)
        void $ runWithD3_Simulation simState $ update
          { nodes: Just allNodes
          , links: Just allLinks
          , nodeFilter: Nothing
          , linkFilter: Nothing
          , activeForces: Just compactForces
          , config: Nothing
          , keyFn: keyIsID_
          }

        -- Update visual state
        liftEffect $ hideModuleLabels_ nodesGroup

        -- Clear refs
        Ref.write false hasFilteredRef
        Ref.write Nothing currentSpotlightRef
        callbacks.onDisableSpotlightMode
```

## Next Steps

1. **Try minimal fix first**: Update `resetSpotlight` to use `update` function
2. **Test**: Verify nodes are in simulation (check console: `simulation.nodes().length`)
3. **If working**: Consider full refactor for cleaner architecture
4. **If not**: May need to look at how `runWithD3_Simulation` works from Effect context

## Questions to Investigate

1. Can we call `runWithD3_Simulation` from within an Effect context?
2. How does CodeExplorer structure its State to make simulation accessible?
3. Should we move all spotlight logic into Halogen actions for cleaner separation?
