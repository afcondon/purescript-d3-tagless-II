# psd3-simulation Test Plan

## Overview

The psd3-simulation package is heavily tied to browser APIs (d3-force, d3-drag, DOM). However, there are pure computational parts that can be tested from the command line, and we can explore Node.js-based testing for the simulation engine itself.

**Key insight**: While the full simulation requires browser APIs, the easing functions, interpolation utilities, and rules system are pure computations that can be tested without a browser.

## Test Categories

### 1. Pure Computation Tests (No Browser Required)

#### Transition/Easing Functions (`Test.Transition.Tick`)

Test all interpolation and easing functions:

```purescript
-- Linear interpolation
lerp 0.0 100.0 0.5 == 50.0
lerp 0.0 100.0 0.0 == 0.0
lerp 0.0 100.0 1.0 == 100.0

-- Clamped interpolation
lerpClamped 0.0 100.0 1.5 == 100.0  -- clamped to max
lerpClamped 0.0 100.0 (-0.5) == 0.0 -- clamped to min

-- Integer interpolation
lerpInt 0 100 0.5 == 50
```

**Easing functions** (all take progress 0-1, return eased value 0-1):

```purescript
-- Linear (identity)
linear 0.5 == 0.5

-- Quadratic
easeInQuad 0.5 == 0.25      -- t²
easeOutQuad 0.5 == 0.75     -- 1-(1-t)²
easeInOutQuad 0.5 == 0.5    -- smooth in and out

-- Cubic
easeInCubic 0.5 == 0.125    -- t³
easeOutCubic 0.5 == 0.875   -- 1-(1-t)³
easeInOutCubic 0.5 == 0.5

-- Generic ease in/out
easeIn 0.5 == 0.25
easeOut 0.5 == 0.75
easeInOut 0.5 == 0.5
```

**Duration calculations**:

```purescript
ticksForDuration 1000 60 == 60  -- 1 second at 60fps
ticksForDuration 500 60 == 30   -- 0.5 seconds at 60fps
```

#### Rules System (`Test.Scene.Rules`)

Test pure rule matching and application:

```purescript
-- Rule building
let rules = [
  rule (\n -> n.id == "fixed") (pinAt 100.0 200.0),
  rule (\n -> n.group == "A") (setPosition 50.0 50.0),
  ruleAll unpin
]

-- Rule application (first match wins)
applyRules rules { id: "fixed", group: "B" } -- applies pinAt 100.0 200.0
applyRules rules { id: "other", group: "A" } -- applies setPosition 50.0 50.0
applyRules rules { id: "other", group: "B" } -- applies unpin

-- applyFirstMatch returns the matched rule
applyFirstMatch rules node -- returns Just transform or Nothing
```

**Transform functions**:

```purescript
-- Pin functions (set fx, fy to fix position)
pinAt 100.0 200.0 node == node { fx = Just 100.0, fy = Just 200.0 }
pinAtCurrent node == node { fx = Just node.x, fy = Just node.y }
unpin node == node { fx = Nothing, fy = Nothing }

-- Position setting
setPosition 100.0 200.0 node == node { x = 100.0, y = 200.0 }
```

#### Configuration Defaults (`Test.Config.Force`)

Test that default configurations have expected values:

```purescript
-- ManyBody force defaults
defaultManyBody.strength == -30.0
defaultManyBody.theta == 0.9
defaultManyBody.distanceMin == 1.0
defaultManyBody.distanceMax == Infinity

-- Collide force defaults
defaultCollide.radius == 1.0
defaultCollide.strength == 1.0
defaultCollide.iterations == 1

-- Link force defaults
defaultLink.distance == 30.0
defaultLink.strength == Nothing  -- auto-calculated
defaultLink.iterations == 1

-- Center force defaults
defaultCenter.x == 0.0
defaultCenter.y == 0.0
defaultCenter.strength == 1.0
```

### 2. Node.js Integration Tests (Optional, Requires Setup)

d3-force works in Node.js without DOM for force calculations. If we set up a Node.js test harness, we can test:

#### Option A: Use jsdom

```javascript
// test-harness.js
const { JSDOM } = require('jsdom');
const dom = new JSDOM('<!DOCTYPE html><svg></svg>');
global.document = dom.window.document;
global.window = dom.window;
```

#### Option B: Direct d3-force Testing

Since d3-force's core algorithms don't require DOM:

```javascript
// node-force-test.js
const d3 = require('d3-force');

const nodes = [
  { id: 'a', x: 0, y: 0 },
  { id: 'b', x: 100, y: 0 },
  { id: 'c', x: 50, y: 50 }
];

const simulation = d3.forceSimulation(nodes)
  .force('charge', d3.forceManyBody())
  .force('center', d3.forceCenter(200, 200))
  .stop();

// Run fixed number of ticks
for (let i = 0; i < 100; i++) simulation.tick();

// Compare final positions to golden file
console.log(JSON.stringify(nodes, null, 2));
```

#### Golden Test Approach

```purescript
-- Given: 5 nodes with initial positions
-- Run: 100 ticks of forceSimulation with manyBody + center
-- Assert: Final positions match golden file (within tolerance)

simulationGoldenTest :: Effect Unit
simulationGoldenTest = do
  let nodes = initialNodes
  finalNodes <- runSimulation 100 nodes [manyBody, center 200.0 200.0]
  assertGoldenWithTolerance "simulation-100ticks.golden.json" 0.001 finalNodes
```

### 3. Browser-Based Integration Tests (Existing)

Keep the existing test harness files for comprehensive browser testing:

- `test-harness/d3-simulation-test.html` - 11 test suites for D3 force simulation
- `test-harness/d3-gup-forces-test.html` - 5 scenarios for GUP + Forces
- `test-harness/force-engine-test.html` - Force engine specific tests
- `test-harness/d3-critical-tests.html` - Critical D3 behavior tests

**Browser tests cover**:
- Force handle mutability
- Node object reference preservation
- General Update Pattern (enter/exit) behavior
- Alpha management and decay
- Force initialization timing
- Drag behavior
- DOM rendering updates
- Visual verification

## Recommended Implementation Strategy

### Phase 1: Pure Tests (Minimum Viable)

Create tests for all pure computations:

```
psd3-simulation/
  test/
    Test/
      Main.purs
      Transition/
        TickSpec.purs      -- Easing and interpolation
      Scene/
        RulesSpec.purs     -- Rule matching and transforms
      Config/
        ForceSpec.purs     -- Default configurations
```

### Phase 2: Node.js Integration (Optional)

If Node.js testing proves feasible:

```
psd3-simulation/
  test/
    node/
      runner.js            -- Node.js test harness
      force-test.js        -- d3-force algorithm tests
    golden/
      simulation-100ticks.golden.json
```

### Phase 3: Browser Tests (Existing)

Keep existing browser-based tests in `test-harness/` for:
- Full integration testing
- Visual verification
- Drag/interaction testing

## Files to Create

```
psd3-simulation/
  test/
    Test/
      Main.purs                    -- Test runner
      Transition/
        TickSpec.purs              -- Easing/interpolation tests
      Scene/
        RulesSpec.purs             -- Rules system tests
      Config/
        ForceSpec.purs             -- Configuration tests
```

## Running Tests

```bash
cd psd3-simulation
spago test
```

## Success Criteria

### Minimum (Pure Tests Only)
- All easing functions tested with known values
- All interpolation functions tested
- Rule application logic verified
- Configuration defaults documented and tested
- `spago test` passes

### Extended (With Node.js)
- d3-force algorithm golden tests pass
- Position calculations reproducible
- Alpha decay behavior verified

### Full (With Browser)
- Existing HTML test harnesses continue to work
- Visual tests in demo-website verify rendering
