# State Module Refactoring Plan

## Date: 2025-11-06

## Goal

Extract generic scene/state infrastructure from `PSD3.CodeExplorer.State` to make it reusable for new visualizations like BubblePack.

**Scope**: `PSD3.Component.SimulationState` is specifically for **complex, SimulationM2-type visualizations** that combine:
- Force-directed layouts (force simulation)
- Explicit layouts (hierarchies, trees, pack, treemap)
- Scene-based transitions between different views
- Multiple interactive modes

**Not for**: Simple, single-mode visualizations that don't need scene management or simulation state.

## Problem

CodeExplorer's State module mixes:
- Generic simulation component infrastructure (transitions, tags, scene management)
- Spago-specific types (SpagoSimNode, SpagoSceneAttributes, SpagoModel)

This makes it unclear what new visualizations should copy vs adapt.

## Analysis of Current State Module

### ✅ Generic (Extract to Library)

**Core Types** (lines 31-68):
- `TransitionMatrix` - maps scene transitions to animation specs
- `State` record structure - generic fields:
  - `simulation :: D3SimulationState_`
  - `staging :: Staging D3Selection_ d`  (parameterized)
  - `scene :: SceneConfig d attrs`  (parameterized)
  - `currentScene :: scene`  (parameterized)
  - `transitionMatrix :: TransitionMatrix scene`  (parameterized)
  - `eventListener :: Maybe (HS.Listener action)`  (parameterized)
  - `transitionListener :: Maybe (HS.Listener action)`  (parameterized)
  - `tags :: M.Map NodeID (Set String)`
  - `showWelcome :: Boolean`

**Generic Lenses** (lines 89-114):
- `_model`, `_staging`, `_scene`
- `_nodes`, `_links`, `_forces`, `_linksWithForce`
- `_rawdata`, `_enterselections`

**Scene Management** (lines 177-277):
- `updateScene` - update scene configuration
- `applySceneConfig` - apply complete scene
- `applySceneWithTransition` - scene with transition matrix lookup
- Individual setters: `setChooseNodes`, `setLinksShown`, etc.

**Tag System** (lines 280-367):
- Completely generic, parameterized by node type
- `tagNodes`, `untagNodes`, `clearAllTags`
- `getNodeTags`, `nodeHasTag`

### ⚠️ Visualization-Specific (Keep in CodeExplorer)

**Type Specializations**:
- Line 72: `type SceneConfig = Scene.SceneConfig SpagoDataRow SpagoSceneAttributes`
- Line 75: `type MiseEnScene = SceneConfig`
- Lines 117-136: Lenses with Spago-specific types in signatures
- Lines 184-242: Helpers with SpagoSimNode, SpagoGraphLinkID in signatures

**Visualization-Specific Data**:
- Line 46: `model :: Maybe SpagoModel`
- Initial scene construction using SpagoModel data

## Proposed Solution

### 1. Create Generic Library Module: `PSD3.Component.SimulationState`

```purescript
module PSD3.Component.SimulationState where

-- Generic state for visualization components using scene-based simulation
type SimulationComponentState scene action dataRow attrs model =
  { simulation :: D3SimulationState_
  , model :: Maybe model
  , staging :: Staging D3Selection_ dataRow
  , scene :: Scene.SceneConfig dataRow attrs
  , currentScene :: scene
  , transitionMatrix :: TransitionMatrix scene
  , eventListener :: Maybe (HS.Listener action)
  , transitionListener :: Maybe (HS.Listener action)
  , tags :: M.Map NodeID (Set String)
  , showWelcome :: Boolean
  }

type TransitionMatrix scene = M.Map (Tuple scene scene) Scene.TransitionSpec

-- Generic functions (all currently in CodeExplorer.State lines 177-367)
updateScene :: ...
applySceneConfig :: ...
applySceneWithTransition :: ...
tagNodes :: ...
-- etc
```

### 2. Simplify CodeExplorer.State

```purescript
module PSD3.CodeExplorer.State where

import PSD3.Component.SimulationState as SimState

-- Specialize the generic state
type State = SimState.SimulationComponentState Scene Action SpagoDataRow SpagoSceneAttributes SpagoModel

-- Type aliases for convenience
type SceneConfig = Scene.SceneConfig SpagoDataRow SpagoSceneAttributes
type TransitionMatrix = SimState.TransitionMatrix Scene

-- Visualization-specific initialization
initialScene :: M.Map Label Force -> SceneConfig
initialScene forceLibrary = ...

-- Spago-specific lenses and helpers
_sceneAttributes :: Lens' State SpagoSceneAttributes
_sceneAttributes = SimState._scene <<< prop (Proxy :: Proxy "attributes")

-- etc
```

### 3. Benefits

For **new visualizations**:
1. Import `PSD3.Component.SimulationState`
2. Specialize with your types: `type State = SimState.SimulationComponentState MyScene MyAction MyDataRow MyAttributes MyModel`
3. Write only visualization-specific code

For **documentation**:
- Clear template showing required vs optional types
- Generic functions work out of the box
- Only need to document visualization-specific parts

## Implementation Steps

1. ✅ Create this analysis document
2. Create `src/lib/PSD3/Component/SimulationState.purs` with generic types and functions
3. Update `PSD3.CodeExplorer.State` to use the generic module
4. Verify CodeExplorer still builds and works
5. Update VISUALIZATION_GUIDE.md with new State setup section
6. Test by building BubblePack using the new generic infrastructure

## Files to Create/Modify

**New**:
- `src/lib/PSD3/Component/SimulationState.purs`

**Modified**:
- `src/website/Component/CodeExplorer/State.purs` - use generic state
- `docs/guides/VISUALIZATION_GUIDE.md` - add State setup section
- `notes/BUBBLEPACK_LESSONS_LEARNED.md` - mark this issue as resolved

**Future** (when building BubblePack v2):
- `src/website/Component/BubblePack/State.purs` - demonstrate using generic state

## Follow-up: Data Loading Architecture

**TODO**: Investigate and document how data loading is structured in CodeExplorer:
- `PSD3.Data.Tree` module - what's generic vs Spago-specific?
- Data loading pattern in CodeExplorer - how is it interwoven with component?
- Can we extract generic data loading patterns?
- How should new visualizations structure their data loading?

This is separate from State refactoring but equally important for making the architecture clear to new visualizations.
