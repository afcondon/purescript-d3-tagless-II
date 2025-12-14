# psd3-simulation: Key Insights for Complex Interactive Apps

## The Core Philosophy

The library **rejects D3's default simulation management** in favor of full control. Instead of wrapping `d3.forceSimulation` (which hides state), it implements the simulation loop in PureScript while calling D3's force functions directly. This gives you:
- Type-safe, debuggable state
- Tick-driven execution (you decide when)
- No hidden callbacks or timers

## Critical Gotcha: Force Target Caching

This is the biggest trap. D3's `forceX`/`forceY` forces **cache their target values at initialization time**:

```purescript
-- WRONG - nodes won't move to new positions:
updateGridXWithFn newXFn sim
reheat sim

-- CORRECT - re-initializes forces after update:
updateGridXYAndReinit (Just newXFn) Nothing forceXHandle Nothing sim
```

## State Architecture

The `Simulation` type uses mutable `Ref`s (pragmatic choice for 60fps loops):
- `nodes`, `links`, `forces`, `alpha`, `running`
- Callbacks stored in Refs to avoid per-tick allocations

## Scene System for Multi-View Apps

The innovation here is a **3-phase scene transition engine**:

1. **initRules** - Pin nodes, prepare for transition
2. **layout** - Compute target positions
3. **finalRules** - Unpin, let forces take over

```purescript
treeScene =
  { name: "TreeForm"
  , initRules: [pinAllRule]
  , layout: computeTreePositions
  , finalRules: \_ -> [unpinAllRule]
  , stableMode: Physics  -- or Static
  }

transitionTo treeScene engineRef
```

## Performance Patterns

| Do | Don't |
|----|-------|
| Use FFI `updateCirclePositions` in tick | Use data joins in tick callback |
| Cache node ref, read once | Call `getNodes` repeatedly |
| Store callbacks in `Ref`s | Create closures per-tick |
| Use `filterLinksToSubset` with `IntSet` | Filter with `Array.filter` for large graphs |

## Multi-View Coordination

- **Registry** tracks multiple simulations for debugging/coordination
- **Group-based DOM updates** - different `GroupId`s for different node types
- **Force reconfiguration** - `clearForces` + `addForce` to switch layouts
- **Selective rendering** - filter links to visible nodes only

## Essential Functions to Know

```purescript
-- Lifecycle
create, setNodes, setLinks, addForce, start, stop, tick, reheat

-- Position transitions
pinNodesInPlace, unpinNodesInPlace, interpolatePositionsInPlace

-- The safe way to update grid positions
updateGridXYAndReinit

-- Callbacks (multi-callback API)
createWithCallbacks, defaultCallbacks
```

## Real-World Pattern

```purescript
-- 1. Create simulation
sim <- Sim.create config
Sim.setNodes nodes sim

-- 2. Create scene engine
let adapter = mkAdapter sim
engineRef <- Scene.Engine.createEngine adapter

-- 3. Wire tick callback
Sim.onTick (Scene.Engine.tick engineRef >>= \_ -> updateDOM) sim

-- 4. Transition on user action
Scene.Engine.transitionTo gridScene engineRef
```

The library handles the complexity of coordinating D3 forces, animations, and state so you can focus on your visualization logic rather than fighting D3's hidden state machine.
