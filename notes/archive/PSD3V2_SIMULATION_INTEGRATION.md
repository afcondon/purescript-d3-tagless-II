# PSD3v2 Simulation Integration Design

**Status:** Design Phase
**Date:** 2025-11-14
**Goal:** Integrate SimulationM2 capabilities into PSD3v2 architecture

## Executive Summary

SimulationM2 is already well-designed and uses d3-force appropriately. We should **not** rewrite the physics engine in pure PureScript - that's not the goal. Instead, we extend PSD3v2's tagless final architecture to include simulation capabilities, allowing interpreters to choose whether to support force simulations.

## Why Keep D3-Force?

1. **It's a physics engine** - d3-force implements complex numerical algorithms (velocity Verlet integration, Barnes-Hut approximation for n-body forces, quadtree collision detection)
2. **Months of work to recreate** - Rewriting in PureScript would take significant time and testing
3. **Not the goal** - PSD3v2 is about type-safe DOM manipulation and data joins, not physics simulation
4. **Clean FFI boundary** - Current design isolates d3-force interaction cleanly

## Architecture

### Current State

```purescript
-- PSD3v1 (existing)
class SelectionM selection m where
  appendTo :: ...
  simpleJoin :: ...
  updateJoin :: ...

class SimulationM selection m where
  setNodes :: ...
  setLinks :: ...
  start :: ...

class SimulationM2 selection m where
  init :: SimulationInit -> m { nodes, links }
  update :: SimulationUpdate -> m { nodes, links }
  addTickFunction :: Label -> Step -> m Unit
```

### Proposed PSD3v2 Extension

```purescript
-- PSD3v2 core (exists)
module PSD3v2.Capabilities.Selection where
  class SelectionM sel m where
    select :: ...
    joinData :: ...
    append :: ...
    withTransition :: ...

-- PSD3v2 simulation extension (NEW)
module PSD3v2.Capabilities.Simulation where
  import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, SimVariable, Step)
  import PSD3.Data.Node (SimulationNode, D3Link_Unswizzled, D3Link_Swizzled)

  -- Simulation capability (optional - not all interpreters support it)
  class (SelectionM sel m) <= SimulationM sel m where
    -- Declarative initialization
    initSimulation :: forall a key.
      { nodes :: Array (SimulationNode a)
      , links :: Array D3Link_Unswizzled
      , forces :: Array (Force a)
      , activeForces :: Set Label
      , config :: SimulationVariables
      , keyFn :: SimulationNode a -> key
      } -> m { nodes :: Array (SimulationNode a), links :: Array D3Link_Swizzled }

    -- Declarative update
    updateSimulation :: forall a key.
      { nodes :: Maybe (Array (SimulationNode a))
      , links :: Maybe (Array D3Link_Unswizzled)
      , activeForces :: Maybe (Set Label)
      , config :: Maybe SimulationVariables
      , keyFn :: SimulationNode a -> key
      } -> m { nodes :: Array (SimulationNode a), links :: Array D3Link_Swizzled }

    -- Animation callbacks
    addTickFunction :: forall d. Label -> Step sel d -> m Unit
    removeTickFunction :: Label -> m Unit

    -- Simulation control
    startSimulation :: m Unit
    stopSimulation :: m Unit
    reheatSimulation :: Number -> m Unit

-- D3v2 interpreter (implements simulation)
module PSD3v2.Interpreter.D3v2 where
  instance SimulationM D3v2Selection_ (D3v2M state) where
    initSimulation config = liftEffect $ PSD3.Simulation.Functions.init config
    updateSimulation config = liftEffect $ PSD3.Simulation.Functions.update config
    addTickFunction label step = liftEffect $ PSD3.Simulation.Functions.addTickFunction label step
    startSimulation = liftEffect $ PSD3.Simulation.Functions.start
    -- etc.

-- String interpreter (does NOT implement simulation)
-- No instance SimulationM StringSelection_ StringM
-- This is fine! Not all interpreters need physics.
```

## Integration Strategy

### Phase 1: Add Simulation Capability ✓ (Design)

1. Create `PSD3v2.Capabilities.Simulation`
2. Define `SimulationM` type class
3. Re-export types from `PSD3.Internal.Simulation.Types`
4. Keep FFI in `PSD3.Internal.Simulation.Functions` unchanged

### Phase 2: Implement D3v2 Simulation Support

1. Add `SimulationM D3v2Selection_` instance
2. Delegate to existing `PSD3.Simulation.Functions`
3. Test with simple force graph

### Phase 3: Port LesMis to PSD3v2

1. Use `SelectionM` for DOM (circles, lines)
2. Use `SimulationM` for physics (forces, tick callbacks)
3. Compare with existing LesMis implementation

### Phase 4: Port LesMisGUP

1. Combine `joinData` (from SelectionM) with `updateSimulation` (from SimulationM)
2. Update pattern for force graphs
3. Dynamic data with preserved positions

## Key Design Decisions

### 1. Keep Existing FFI

**Decision:** Do NOT create new PSD3v2-specific FFI for d3-force.

**Rationale:**
- Existing FFI in `PSD3.Internal.Simulation.Functions` works well
- D3v2 interpreter can delegate to it
- Avoids duplication
- If we later want a pure PureScript physics engine, we can add a new interpreter

### 2. Simulation State Management

**Current:** SimulationM requires `MonadState { simulation :: D3SimulationState_ | r }`

**PSD3v2 approach:** Keep this requirement. The interpreter (D3v2M) can manage state.

```purescript
-- D3v2 interpreter monad
newtype D3v2M state a = D3v2M (StateT state Effect a)

-- User code constraint
myForceGraph :: forall m sel.
  SelectionM sel m =>
  SimulationM sel m =>
  MonadState { simulation :: D3SimulationState_ NodeType | r } m =>
  m Unit
```

This is fine! Simulation is inherently stateful. We're not trying to hide that.

### 3. Tick Functions and DOM Updates

**Challenge:** Tick functions need to update DOM 60 times/second during animation.

**Solution:** `Step` type already handles this:

```purescript
data Step selection datum =
  Step selection (Array (Attribute datum))

-- On each tick:
addTickFunction "nodes" $ Step nodeSelection
  [ cx (\d -> d.x)
  , cy (\d -> d.y)
  ]
```

The D3v2 interpreter's `addTickFunction` will use PSD3v2's `setAttrs` internally.

### 4. Force Library Polymorphism

Forces are created with type `Force a` where `a` is the node data type:

```purescript
-- Force configuration (pure)
centerForce :: forall a. Number -> Number -> Force a
chargeForce :: forall a. (SimulationNode a -> Number) -> Force a

-- Used in initialization
initSimulation
  { forces: [ centerForce w h, chargeForce (const (-100.0)) ]
  , ...
  }
```

PSD3v2 just re-exports these from `PSD3.Internal.Simulation.Forces`.

## What PSD3v2 Does NOT Do

1. **Pure PureScript physics** - We use d3-force via FFI
2. **Alternative simulation engines** - Could add later with new interpreter
3. **Hide statefulness** - Simulation is stateful, we embrace it
4. **Replace existing SimulationM2** - We integrate with it

## Benefits of This Approach

1. **Reuse working code** - SimulationM2 is already excellent
2. **Tagless final flexibility** - Other interpreters can skip simulation
3. **Type safety** - Phantom types prevent attribute/data mismatches
4. **Composability** - SelectionM + SimulationM together
5. **No wasted effort** - Don't rewrite d3-force internals

## Example: LesMis with PSD3v2

```purescript
drawLesMis :: forall m sel.
  SelectionM sel m =>
  SimulationM sel m =>
  TransitionM sel m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ LesMisNode | r } m =>
  LesMisData -> m Unit
drawLesMis data = do
  -- Use SelectionM for initial DOM setup
  container <- select "#chart"
  svg <- appendChild SVG [width 800.0, height 600.0] container
  linksGroup <- appendChild Group [class_ "links"] svg
  nodesGroup <- appendChild Group [class_ "nodes"] svg

  -- Use SimulationM to initialize physics
  { nodes: nodesInSim, links: linksInSim } <- initSimulation
    { nodes: data.nodes
    , links: data.links
    , forces: [centerForce 400.0 300.0, chargeForce (const (-100.0))]
    , activeForces: Set.fromFoldable ["center", "charge", "link"]
    , config: defaultSimConfig
    , keyFn: _.id
    }

  -- Use SelectionM for data join
  JoinResult { enter: nodeEnter } <- joinData nodesInSim "circle" nodesGroup
  nodeCircles <- append Circle
    [ cx (_.x)
    , cy (_.y)
    , radius 5.0
    , fill "#4CAF50"
    ] nodeEnter

  JoinResult { enter: linkEnter } <- joinData linksInSim "line" linksGroup
  linkLines <- append Line
    [ x1 (\l -> l.source.x)
    , y1 (\l -> l.source.y)
    , x2 (\l -> l.target.x)
    , y2 (\l -> l.target.y)
    , stroke "#999"
    ] linkEnter

  -- Use SimulationM for animation
  addTickFunction "nodes" $ Step nodeCircles
    [ cx (_.x)
    , cy (_.y)
    ]

  addTickFunction "links" $ Step linkLines
    [ x1 (\l -> l.source.x)
    , y1 (\l -> l.source.y)
    , x2 (\l -> l.target.x)
    , y2 (\l -> l.target.y)
    ]

  startSimulation
```

## Open Questions

1. **Drag behavior** - How does this fit into PSD3v2?
   - Keep as separate capability? `DragM`?
   - Or fold into `SimulationM`?

2. **Zoom behavior** - Same question
   - SelectionM with zoom events?
   - Separate `ZoomM` capability?

3. **State initialization** - How does user initialize simulation state?
   - Provide helper: `initialSimState :: D3SimulationState_ a`?
   - Document the requirement clearly?

## Next Steps

1. Create `PSD3v2.Capabilities.Simulation` module
2. Define `SimulationM` type class
3. Implement `instance SimulationM D3v2Selection_ (D3v2M state)`
4. Port LesMis as proof of concept
5. Iterate based on experience

## References

- Current SimulationM2: `src/lib/PSD3/Capabilities/Simulation.purs`
- Simulation types: `src/lib/PSD3/Internal/Simulation/Types.purs`
- FFI functions: `src/lib/PSD3/Internal/Simulation/Functions.purs`
- LesMis example: `src/website/Viz/LesMis/LesMiserables.purs`
