# Scene System Extraction Plan

Extract the proven scene orchestration pattern from ce-website into psd3-simulation library.

## Current State

### Already in Library (`psd3-simulation`)

**`PSD3.Simulation.Scene`**
- `NodeRule` - selector + transform pattern
- `SceneConfig` - three-phase lifecycle (init → layout → final)
- `TransitionState` - progress tracking
- `EngineMode` - Static | Physics
- `applyRules` / `applyRulesInPlace_` - first-match-wins rule application

**`PSD3.Transition.Tick`**
- `Progress` type (0.0 to 1.0)
- `TickDelta` for frame-based progress
- Easing functions (`easeInOutCubic`, etc.)
- `ticksForDuration` helper

### In ce-website (to extract)

**`Engine.Scene`** - The orchestrator
- `SceneState` - runtime state container
- `transitionTo` - start a scene transition
- `onTick` - main tick handler routing to engines
- DumbEngine - interpolates positions toward targets
- PhysicsEngine routing - delegates to D3 simulation
- `completeTransition` - finalize and enter stable mode
- Force re-initialization after transition

**`Engine.Scenes`** - Declarative scene definitions
- Common predicates (`isPackage`, `isModule`, etc.)
- Common transforms (`pinAtCurrent`, `unpinNode`)
- Scene configs for each view (TreemapForm, TreeForm, etc.)

**`Engine.ViewTransition`** - GUP-style visibility
- Enter/exit animations for nodes becoming visible/hidden
- Separate from position transitions

---

## Proposed Library Structure

```
psd3-simulation/src/PSD3/
├── Simulation/
│   ├── Scene.purs          # (existing) Types + rule application
│   └── Scene.js            # (existing) FFI for applyRulesInPlace_
├── Scene/
│   ├── Engine.purs         # NEW: Tick orchestration
│   ├── Engine.js           # NEW: FFI if needed
│   ├── Transition.purs     # NEW: Position interpolation helpers
│   └── Rules.purs          # NEW: Common rule predicates/transforms
└── Transition/
    └── Tick.purs           # (existing) Progress, easing, timing
```

---

## Module Designs

### 1. `PSD3.Scene.Engine` (NEW)

The core orchestrator, generic over node type.

```purescript
module PSD3.Scene.Engine
  ( -- Types
    SceneEngine
  , EngineConfig
  , EngineState
    -- Creation
  , createEngine
    -- Operations
  , transitionTo
  , onTick
  , getCurrentScene
  , isTransitioning
  ) where

-- | Engine configuration (app provides these)
type EngineConfig node simulation =
  { simulation :: simulation
  , getNodes :: simulation -> Effect (Array node)
  , interpolatePositions :: PositionMap -> PositionMap -> Number -> simulation -> Effect Unit
  , updatePositions :: PositionMap -> simulation -> Effect Unit
  , reinitializeForces :: simulation -> Effect Unit
  , reheat :: simulation -> Effect Unit
  }

-- | Runtime engine state
type EngineState node =
  { currentScene :: Maybe (SceneConfig node)
  , transition :: Maybe (TransitionState node)
  }

-- | Create a scene engine
createEngine
  :: forall node simulation
   . EngineConfig node simulation
  -> Effect (SceneEngine node simulation)

-- | Start transition to a new scene
transitionTo
  :: forall node simulation
   . SceneConfig node
  -> SceneEngine node simulation
  -> Effect Unit

-- | Main tick handler - call this from simulation's onTick
-- | Returns true if still transitioning
onTick
  :: forall node simulation
   . SceneEngine node simulation
  -> Effect Boolean
```

**Key insight**: The engine is parameterized by both node type AND simulation type. The app provides adapter functions (`getNodes`, `interpolatePositions`, etc.) so the engine doesn't need to know about `Sim.Simulation` internals.

### 2. `PSD3.Scene.Transition` (NEW)

Position interpolation utilities.

```purescript
module PSD3.Scene.Transition
  ( -- Types
    TransitionConfig
  , defaultTransition
    -- Position capture
  , capturePositions
    -- Interpolation
  , interpolateProgress
  ) where

-- | Transition timing configuration
type TransitionConfig =
  { duration :: Number        -- milliseconds
  , easing :: Number -> Number  -- easing function
  }

defaultTransition :: TransitionConfig
defaultTransition =
  { duration: 2000.0
  , easing: Tick.easeInOutCubic
  }

-- | Capture current positions from nodes
-- | Generic: just needs nodes with id, x, y
capturePositions
  :: forall node
   . (node -> Int)    -- getId
  -> (node -> Number) -- getX
  -> (node -> Number) -- getY
  -> Array node
  -> PositionMap

-- | Calculate interpolated progress with easing
interpolateProgress
  :: TransitionConfig
  -> Number  -- elapsed ms
  -> Number  -- 0.0 to 1.0, eased
```

### 3. `PSD3.Scene.Rules` (NEW)

Common rule building blocks.

```purescript
module PSD3.Scene.Rules
  ( -- Rule builders
    rule
  , ruleWhen
    -- Common transforms
  , pinAtCurrent
  , pinAt
  , unpin
  , setGridXY
    -- Combinators
  , composeRules
  , firstMatch
  ) where

-- | Build a rule with name, selector, transform
rule
  :: forall node
   . String
  -> (node -> Boolean)
  -> (node -> node)
  -> NodeRule node

-- | Build a rule that only applies when predicate is true
ruleWhen
  :: forall node
   . String
  -> (node -> Boolean)
  -> (node -> node)
  -> NodeRule node
ruleWhen = rule  -- same thing, better name for readability

-- | Pin node at its current x/y position
-- | Requires node to have fx, fy, x, y fields
pinAtCurrent
  :: forall r
   . { x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r }
  -> { x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r }

-- | Pin node at specific position
pinAt
  :: forall r
   . Number -> Number
  -> { fx :: Nullable Number, fy :: Nullable Number | r }
  -> { fx :: Nullable Number, fy :: Nullable Number | r }

-- | Unpin node (clear fx/fy)
unpin
  :: forall r
   . { fx :: Nullable Number, fy :: Nullable Number | r }
  -> { fx :: Nullable Number, fy :: Nullable Number | r }

-- | Set gridX/gridY from a position function
setGridXY
  :: forall r
   . (forall s. { | s } -> { x :: Number, y :: Number })
  -> { gridX :: Number, gridY :: Number | r }
  -> { gridX :: Number, gridY :: Number | r }
```

---

## Migration Path

### Phase 1: Extract without breaking ce-website
1. Create new modules in library
2. ce-website continues using its own `Engine.Scene`
3. Verify library modules compile

### Phase 2: Migrate ce-website to library
1. Update ce-website imports to use library modules
2. App-specific code stays in ce-website (node types, scene configs)
3. Generic orchestration comes from library

### Phase 3: Documentation
1. Update Understanding pages with formalized concepts
2. Add examples to demo-website
3. Create Force Playground using the library Scene system

---

## Design Decisions

### 1. Generic over simulation type

The engine doesn't import `Sim.Simulation` directly. Instead, app provides adapter functions:

```purescript
-- App provides this:
ceEngineConfig :: EngineConfig SimNode CESimulation
ceEngineConfig =
  { simulation: mySim
  , getNodes: Sim.getNodes
  , interpolatePositions: Sim.interpolatePositionsInPlace
  , updatePositions: Sim.updatePositionsInPlace
  , reinitializeForces: myReinitForces
  , reheat: Sim.reheat
  }
```

**Why**: Keeps library decoupled. Could theoretically use with non-D3 simulations.

### 2. Three-phase lifecycle is correct

The `initRules → layout → finalRules` pattern from ce-website works well:
- **initRules**: Set up starting positions (e.g., "grow from root")
- **layout**: Where nodes should end up
- **finalRules**: What happens after (pin/unpin, set gridXY for forces)

### 3. Engine naming

**Decision**: Use `InterpolationEngine` throughout code and docs. "DumbEngine" was a placeholder - retire it completely.

### 4. View transitions are separate

`Engine.ViewTransition` (enter/exit opacity animations) is orthogonal to position transitions. Keep it separate for now - apps that don't need GUP-style visibility changes don't pay for it.

---

## Resolved Questions

1. **Should SceneConfig live in Scene.Engine or stay in Simulation.Scene?**
   - **Decision**: Move to `Scene.Types` for cleaner organization

2. **How to handle CSS transitions?**
   - **Decision**: Include CSS transition support in library (no FFI requirement for users)
   - Follow-on work after initial extraction

3. **Should the engine own the Ref, or take it as parameter?**
   - **Decision**: Engine owns state internally, no Ref exposed to users
   - Consider: Can we use State monad instead of Ref? (Performance implications for tick-rate updates need investigation)

4. **Tick delta: fixed or configurable?**
   - **Decision**: Configurable via TransitionConfig with sensible defaults

## Future Work

- Review all configs across codebase and give them applicative forms for customization
- Implement CSS transitions properly in library

---

## Files to Create

1. `psd3-simulation/src/PSD3/Scene/Engine.purs` - Core orchestrator
2. `psd3-simulation/src/PSD3/Scene/Transition.purs` - Position interpolation
3. `psd3-simulation/src/PSD3/Scene/Rules.purs` - Common rule helpers
4. `psd3-simulation/src/PSD3/Scene/Types.purs` - Shared types (maybe)

## Files to Modify

1. `psd3-simulation/src/PSD3/Simulation/Scene.purs` - Re-export from Scene/*
2. `ce-website/src/Engine/Scene.purs` - Migrate to use library
3. `ce-website/src/Engine/Scenes.purs` - Use library rule helpers

---

## Success Criteria

- [ ] Library compiles with new Scene modules
- [ ] ce-website works unchanged during migration
- [ ] ce-website migrated to use library Scene system
- [ ] At least 50% reduction in ce-website Engine.Scene code
- [ ] New Understanding page can document the formalized system
- [ ] Force Playground can use Scene system for its state management
