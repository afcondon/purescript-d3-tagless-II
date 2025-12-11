# Scene System Status (December 2024)

## UPDATE: Refactoring Complete!

The scene system has been consolidated. The library now provides:

- **`PSD3.Scene.Types`** - Core types (SceneConfig, TransitionState, EngineMode, NodeRule, PositionMap)
- **`PSD3.Scene.Engine`** - Generic tick-based engine with adapter pattern
- **`PSD3.Scene.Rules`** - Pure rule helpers
- **`PSD3.Simulation.Scene`** - Legacy module with FFI (applyRulesInPlace_)

ce-website still uses its own engine implementation in `CodeExplorer.Scene` but imports
types from the library via `PSD3.Simulation.Scene`. The types are now unified.

### What Was Deleted
- `PSD3.Scene.purs` - unused umbrella module
- `PSD3.Scene.Transition.purs` - unused time-based transition helpers
- Old `PSD3.Scene.Engine.purs` - unused time-based adapter engine

### New Library Engine
The new `PSD3.Scene.Engine` is tick-based (like ce-website) with an adapter pattern:
- `EngineAdapter node` record for simulation interaction
- `createEngine`, `transitionTo`, `tick` functions
- Supports the three-phase lifecycle (init rules → interpolate → final rules)

---

## Previous Architecture (Historical Reference)

There were **two separate Scene implementations** that needed consolidation:

### 1. Library: `PSD3.Scene.*` (UNUSED)
Located in `psd3-simulation/src/PSD3/Scene/`:
- `Engine.purs` - Abstract adapter-based engine with **time-based** progress
- `Types.purs` - `SceneConfig` with `transition :: TransitionConfig` field
- `Transition.purs` - Time-based transition helpers
- `Rules.purs` - Pure rule helpers (could be useful)

**Status**: Written but NEVER integrated into any app. Dead code.

### 2. Library: `PSD3.Simulation.Scene` (Legacy, USED)
Located in `psd3-simulation/src/PSD3/Simulation/Scene.purs`:
- `SceneConfig` WITHOUT `transition` field
- `TransitionState` without `elapsed` field
- `applyRulesInPlace_` FFI (unique, needed)
- Re-exports `EngineMode` from new module

**Status**: Used by ce-website. Contains the only FFI.

### 3. App: `ce-website/src/CodeExplorer/Scene.purs` (Battle-tested)
- Custom **tick-based** engine (not time-based)
- Hardcoded to `CESimulation` type
- Includes DOM update logic (`updateCirclePositions`, etc.)
- Has `ViewTransition` integration
- Uses `applyRulesInPlace_` from legacy module

**Status**: Production code. The only working Scene system.

## Key Differences

| Aspect | Library Engine | ce-website Engine |
|--------|---------------|-------------------|
| Progress | Time-based (ms) | Tick-based (frames) |
| Adapter | Generic via record | Hardcoded to CESimulation |
| DOM Updates | Caller responsibility | Built-in |
| FFI | Expects adapter | Uses directly |
| SceneConfig | Requires `transition` | No `transition` field |

## What's Actually Used

From the library, ce-website only uses:
- `NodeRule` type
- `SceneConfig` type (legacy version without `transition`)
- `TransitionState` type (legacy version without `elapsed`)
- `EngineMode` enum
- `PositionMap` type
- `applyRulesInPlace_` FFI

## Recommended Cleanup

### Option A: Delete Unused Library Code
1. Delete `PSD3.Scene.Engine` - never used
2. Delete `PSD3.Scene.Transition` - never used
3. Simplify `PSD3.Scene.Types` to just `EngineMode`
4. Keep legacy module for ce-website compatibility
5. Keep `PSD3.Scene.Rules` if useful helpers

### Option B: Promote ce-website Engine
1. Extract ce-website's engine to library
2. Make it generic (not hardcoded to CESimulation)
3. Replace unused library engine
4. Delete legacy module

### Option C: Leave As-Is
1. Keep everything for potential future use
2. Document the situation
3. Clean up when ce-website migrates

## Files to Delete (if choosing Option A)

```
psd3-simulation/src/PSD3/Scene/Engine.purs      # DEAD - unused
psd3-simulation/src/PSD3/Scene/Transition.purs  # DEAD - unused
psd3-simulation/src/PSD3/Scene.purs             # DEAD - umbrella for unused modules
```

## Files to Keep

```
psd3-simulation/src/PSD3/Scene/Types.purs       # KEEP - EngineMode used
psd3-simulation/src/PSD3/Scene/Rules.purs       # KEEP - useful helpers
psd3-simulation/src/PSD3/Simulation/Scene.purs  # KEEP - ce-website uses this
psd3-simulation/src/PSD3/Simulation/Scene.js    # KEEP - FFI for applyRulesInPlace_
```

## Recent Cleanup (This Session)

Removed dead code from legacy module:
- `CSSConfig` - never used (CSS transitions abandoned for interpolation engine)
- `ForceConfig` - placeholder, real one is in `PSD3.Config.Force`
