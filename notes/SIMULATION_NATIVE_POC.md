# Simulation + Native Operations - Proof of Concept

**Goal:** Demonstrate that PSD3v2's native operations can render a force-directed graph using the existing D3 simulation engine, with NO modifications to either system.

## The Test

Create a simple force graph with 3 nodes using:
- **Existing SimulationM2** for physics (forces, tick callbacks)
- **PSD3v2 SelectionM** for rendering (circles, lines)
- **D3v2 interpreter** that supports both capabilities

## Minimal Example

```purescript
module D3.Viz.ThreeNodesForce where

import Prelude
import Control.Monad.State (class MonadState)
import Data.Set as Set
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.Capabilities.Simulation (class SimulationM2, init, addTickFunction, start)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, Step(..))
import PSD3.Internal.Simulation.Forces (centerForce, chargeForce, linkForce)
import PSD3.Data.Node (SimulationNode, D3Link_Unswizzled)
import PSD3v2.Capabilities.Selection (class SelectionM, select, appendChild, joinData, append)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..))
import PSD3v2.Attribute.Types (cx, cy, radius, fill, stroke, strokeWidth, x1, y1, x2, y2, width, height, viewBox, id_, class_)

-- Simple node type
type Node = { id :: String, x :: Number, y :: Number, group :: Int }

-- Sample data
sampleNodes :: Array (SimulationNode Node)
sampleNodes =
  [ { id: "a", x: 0.0, y: 0.0, group: 1, vx: 0.0, vy: 0.0 }
  , { id: "b", x: 0.0, y: 0.0, group: 2, vx: 0.0, vy: 0.0 }
  , { id: "c", x: 0.0, y: 0.0, group: 3, vx: 0.0, vy: 0.0 }
  ]

sampleLinks :: Array D3Link_Unswizzled
sampleLinks =
  [ { source: "a", target: "b", value: 1.0 }
  , { source: "b", target: "c", value: 1.0 }
  ]

-- Forces (using existing library)
forces :: Array (Force Node)
forces =
  [ centerForce 400.0 300.0
  , chargeForce (const (-100.0))
  , linkForce
  ]

-- Draw function that uses BOTH systems
drawThreeNodesForce :: forall m sel.
  SelectionM sel m =>           -- PSD3v2 capability
  SimulationM2 sel m =>         -- Existing simulation capability
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ Node | r } m =>
  String -> m Unit
drawThreeNodesForce containerSelector = do
  -- PART 1: Use PSD3v2 to create DOM structure
  container <- select containerSelector

  svg <- appendChild SVG
    [ width 800.0
    , height 600.0
    , viewBox "0 0 800 600"
    , id_ "three-nodes-force-svg"
    ]
    container

  linksGroup <- appendChild Group
    [ id_ "links"
    , class_ "links"
    ]
    svg

  nodesGroup <- appendChild Group
    [ id_ "nodes"
    , class_ "nodes"
    ]
    svg

  -- PART 2: Use existing SimulationM2 to initialize physics
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: sampleNodes
    , links: sampleLinks
    , forces: forces
    , activeForces: Set.fromFoldable ["center", "charge", "link"]
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: _.id
    , ticks: Map.empty
    }

  -- PART 3: Use PSD3v2 to join data and create elements
  JoinResult { enter: linkEnter } <- joinData linksInSim "line" linksGroup
  linkLines <- append Line
    [ x1 (\l -> l.source.x)
    , y1 (\l -> l.source.y)
    , x2 (\l -> l.target.x)
    , y2 (\l -> l.target.y)
    , stroke "#999"
    , strokeWidth 2.0
    ]
    linkEnter

  JoinResult { enter: nodeEnter } <- joinData nodesInSim "circle" nodesGroup
  nodeCircles <- append Circle
    [ cx (_.x)
    , cy (_.y)
    , radius 10.0
    , fill "#4CAF50"
    , stroke "#fff"
    , strokeWidth 2.0
    ]
    nodeEnter

  -- PART 4: Use existing SimulationM2 for animation
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

  start
  pure unit
```

## What This Proves

1. **PSD3v2 SelectionM** works for initial DOM setup (svg, groups)
2. **PSD3v2 joinData** works with simulation-enhanced data
3. **PSD3v2 append** creates elements with correct attributes
4. **Existing SimulationM2** works unchanged (init, addTickFunction, start)
5. **Step** type works with PSD3v2 selections for tick updates

## Compatibility Requirements

The only requirement is that `Step sel d` must work with PSD3v2 selections:

```purescript
-- Existing Step type (no change)
data Step selection datum = Step selection (Array (Attribute datum))

-- Must work when selection is D3v2Selection_
-- D3v2 interpreter needs to handle Step in addTickFunction
```

## Implementation Notes

### What D3v2 Interpreter Needs

The D3v2 interpreter already has:
- `instance SelectionM D3v2Selection_ (D3v2M state)` ✅
- Access to existing simulation FFI ✅

It needs to support tick functions that use PSD3v2 attributes:

```purescript
-- In D3v2 interpreter
instance SimulationM2 D3v2Selection_ (D3v2M state) where
  addTickFunction label (Step selection attrs) = do
    -- On each tick, use PSD3v2's setAttrs to update positions
    -- This already works because setAttrs is part of SelectionM!
    ...
```

### Potential Issue: Step Type

The existing `Step` type is defined as:

```purescript
data Step selection datum = Step selection (Array (Attribute datum))
```

But PSD3v2 has its own `Attribute` type. We need to check if they're compatible or if we need an adapter.

## Success Criteria

- [ ] Code compiles with no changes to SimulationM2
- [ ] Code compiles with no changes to PSD3v2 core
- [ ] Three nodes appear and animate under force simulation
- [ ] Links connect nodes and follow them during animation
- [ ] D3v2 interpreter handles both SelectionM and SimulationM2

## Next Steps

1. Check attribute type compatibility
2. Ensure D3v2 interpreter supports both capabilities
3. Create ThreeNodesForce.purs example
4. Test and iterate
