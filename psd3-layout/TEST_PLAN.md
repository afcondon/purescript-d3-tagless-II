# psd3-layout Test Plan

## Overview

The psd3-layout package is **ideal for golden testing**. All layout algorithms are pure computations with no browser dependencies. Input hierarchical/graph data produces deterministic coordinate outputs.

**Key insight**: Every layout function is pure—same input always produces same output. This makes golden testing straightforward and reliable.

## Test Categories

### 1. Hierarchy Layout Golden Tests

#### Test Data

Use `flare-2.json` (47KB hierarchical dataset from `docs/data/`) as the primary test input. This dataset has:
- Nested structure (analytics/cluster/graph/animate/etc.)
- Leaf nodes with names and values
- Sufficient complexity to exercise all hierarchy algorithms

#### Tree Layout (`Test.Layout.Tree`)

Test the Reingold-Tilford algorithm:

```purescript
treeGoldenTest :: Effect Unit
treeGoldenTest = do
  input <- loadFlare2
  let hier = hierarchy input getChildren # sum getValue
  let result = tree defaultTreeConfig hier
  -- Each node has: x, y, depth, height, value, data
  assertGolden "tree-flare2.golden.json" (toJSON result)
```

**Test variations**:
- Default configuration
- Custom separation function
- Horizontal vs vertical orientation
- Different layer scaling

#### Cluster Layout (`Test.Layout.Cluster`)

Test dendrogram layout where all leaves align:

```purescript
clusterGoldenTest :: Effect Unit
clusterGoldenTest = do
  input <- loadFlare2
  let hier = hierarchy input getChildren # sum getValue
  let result = cluster defaultClusterConfig hier
  assertGolden "cluster-flare2.golden.json" (toJSON result)

  -- Property test: all leaves have same y-coordinate
  let leaves = getLeaves result
  assert "all leaves aligned" (allEqual (map _.y leaves))
```

#### Pack Layout (`Test.Layout.Pack`)

Test circle packing algorithm:

```purescript
packGoldenTest :: Effect Unit
packGoldenTest = do
  input <- loadFlare2
  let hier = hierarchy input getChildren # sum getValue
  let result = pack { width: 800.0, height: 800.0, padding: 3.0 } hier
  -- Each node has: x, y, r (radius)
  assertGolden "pack-flare2.golden.json" (toJSON result)
```

**Property tests**:
- No circles overlap
- Parent circles contain all children
- Radius proportional to value

#### Treemap Layout (`Test.Layout.Treemap`)

Test rectangular partitioning with multiple tiling strategies:

```purescript
treemapGoldenTest :: Effect Unit
treemapGoldenTest = do
  input <- loadFlare2
  let hier = hierarchy input getChildren # sum getValue

  -- Test each tiling strategy
  for_ [squarify, binary, dice, slice, sliceDice] \tile -> do
    let config = { width: 800.0, height: 600.0, tile, padding: 1.0 }
    let result = treemap config hier
    -- Each node has: x0, y0, x1, y1
    assertGolden ("treemap-" <> tileName tile <> "-flare2.golden.json") (toJSON result)
```

**Property tests**:
- No rectangles overlap
- Parent rectangles contain children
- Total area proportional to values
- `x0 < x1` and `y0 < y1` for all nodes

#### Partition Layout (`Test.Layout.Partition`)

Test for icicle and sunburst charts:

```purescript
partitionGoldenTest :: Effect Unit
partitionGoldenTest = do
  input <- loadFlare2
  let hier = hierarchy input getChildren # sum getValue
  let result = partition { width: 800.0, height: 600.0 } hier
  -- Each node has: x0, y0, x1, y1
  assertGolden "partition-flare2.golden.json" (toJSON result)
```

### 2. Sankey Layout Golden Tests

#### Test Data

Use CSV files for Sankey testing:
- `energy.csv` - Classic energy flow dataset
- `d3-library-deps.csv` - D3 library dependencies

#### Sankey Algorithm (`Test.Layout.Sankey`)

```purescript
sankeyGoldenTest :: Effect Unit
sankeyGoldenTest = do
  csv <- loadCSV "energy.csv"
  let result = computeLayout csv 800.0 600.0
  -- Result contains:
  -- - nodes: Array { x0, x1, y0, y1, depth, height, value, name }
  -- - links: Array { source, target, y0, y1, width, value }
  assertGolden "sankey-energy.golden.json" (toJSON result)
```

**Test with different configurations**:

```purescript
sankeyConfigTest :: Effect Unit
sankeyConfigTest = do
  csv <- loadCSV "energy.csv"

  -- Test different node widths
  let config1 = defaultSankeyConfig { nodeWidth = 20.0 }
  let result1 = computeLayoutWithConfig csv config1
  assertGolden "sankey-wide-nodes.golden.json" (toJSON result1)

  -- Test different padding
  let config2 = defaultSankeyConfig { nodePadding = 20.0 }
  let result2 = computeLayoutWithConfig csv config2
  assertGolden "sankey-padded.golden.json" (toJSON result2)
```

#### Intermediate Steps (`Test.Layout.SankeySteps`)

Using `ComputeWithSteps` for debugging:

```purescript
sankeyStepsTest :: Effect Unit
sankeyStepsTest = do
  csv <- loadCSV "energy.csv"
  let steps = computeLayoutWithSteps csv 800.0 600.0

  -- Test each intermediate step
  assertGolden "sankey-step-1-nodes.golden.json" steps.afterNodeCreation
  assertGolden "sankey-step-2-depth.golden.json" steps.afterDepthCalculation
  assertGolden "sankey-step-3-height.golden.json" steps.afterHeightCalculation
  assertGolden "sankey-step-4-relaxation.golden.json" steps.afterRelaxation
```

### 3. State Machine Layout Tests

```purescript
stateMachineTest :: Effect Unit
stateMachineTest = do
  let states = ["Idle", "Running", "Paused", "Stopped"]
  let transitions =
    [ { from: "Idle", to: "Running", label: "start" }
    , { from: "Running", to: "Paused", label: "pause" }
    , { from: "Paused", to: "Running", label: "resume" }
    , { from: "Running", to: "Stopped", label: "stop" }
    , { from: "Paused", to: "Stopped", label: "stop" }
    ]

  -- Test circular layout
  let circular = layoutStateMachine circularConfig states transitions
  assertGolden "statemachine-circular.golden.json" (toJSON circular)

  -- Test grid layout
  let grid = layoutStateMachine gridConfig states transitions
  assertGolden "statemachine-grid.golden.json" (toJSON grid)
```

### 4. Adjacency Matrix Layout Tests

```purescript
adjacencyTest :: Effect Unit
adjacencyTest = do
  let nodes = ["A", "B", "C", "D"]
  let edges = [("A", "B"), ("B", "C"), ("C", "D"), ("A", "D")]
  let result = adjacencyLayout { cellSize: 20.0, padding: 2.0 } nodes edges
  assertGolden "adjacency-4nodes.golden.json" (toJSON result)
```

### 5. Edge Bundling Tests

```purescript
edgeBundleTest :: Effect Unit
edgeBundleTest = do
  input <- loadFlare2
  let hier = hierarchy input getChildren
  let imports = loadImports "flare-imports.json"
  let result = edgeBundle { beta: 0.85 } hier imports
  assertGolden "edgebundle-flare.golden.json" (toJSON result)
```

## Golden Test Infrastructure

### Directory Structure

```
psd3-layout/
  test/
    Test/
      Main.purs                         -- Test runner
      Layout/
        TreeSpec.purs
        ClusterSpec.purs
        PackSpec.purs
        TreemapSpec.purs
        PartitionSpec.purs
        SankeySpec.purs
        StateMachineSpec.purs
        AdjacencySpec.purs
        EdgeBundleSpec.purs
      Golden/
        Util.purs                       -- Golden file utilities
    golden/
      tree-flare2.golden.json
      cluster-flare2.golden.json
      pack-flare2.golden.json
      treemap-squarify-flare2.golden.json
      treemap-binary-flare2.golden.json
      treemap-dice-flare2.golden.json
      treemap-slice-flare2.golden.json
      treemap-sliceDice-flare2.golden.json
      partition-flare2.golden.json
      sankey-energy.golden.json
      statemachine-circular.golden.json
      statemachine-grid.golden.json
      adjacency-4nodes.golden.json
      edgebundle-flare.golden.json
    data/
      flare-2.json                      -- Copy from docs/data/
      flare-imports.json
      energy.csv
      d3-library-deps.csv
```

### Golden Test Utility Module

```purescript
module Test.Golden.Util where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile, writeTextFile, exists)
import Node.Encoding (Encoding(..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Test.Assert (assert)

-- | Compare result to golden file
-- | If UPDATE_GOLDEN env var is set, update the golden file instead
assertGolden :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a =>
  String -> a -> Effect Unit
assertGolden filename actual = do
  let path = "test/golden/" <> filename
  let actualJson = encodeJson actual

  fileExists <- exists path
  if not fileExists
    then do
      log $ "Creating golden file: " <> path
      writeTextFile UTF8 path (stringify actualJson)
    else do
      expectedText <- readTextFile UTF8 path
      case decodeJson expectedText of
        Left err -> do
          log $ "Failed to parse golden file: " <> printJsonDecodeError err
          assert false
        Right expected ->
          if actual == expected
            then log $ "✓ " <> filename
            else do
              log $ "✗ " <> filename <> " - output differs from golden file"
              log $ "Expected: " <> show expected
              log $ "Actual: " <> show actual
              assert false

-- | Compare with floating point tolerance
assertGoldenWithTolerance :: forall a.
  String -> Number -> a -> Effect Unit
assertGoldenWithTolerance filename tolerance actual = do
  -- Implementation for floating point comparison
  -- Uses tolerance for x, y, width, height, etc.
  pure unit
```

### Updating Golden Files

When algorithms intentionally change, regenerate golden files:

```bash
# Set environment variable to update mode
UPDATE_GOLDEN=true spago test

# Or run specific update script
node scripts/update-golden.js
```

## Property-Based Tests (Enhancement)

In addition to golden tests, add property tests for invariants:

### Treemap Properties

```purescript
treemapProperties :: Effect Unit
treemapProperties = do
  input <- loadFlare2
  let result = treemap defaultConfig (hierarchy input getChildren # sum getValue)

  -- Property: no rectangles overlap
  assert "no overlap" (noOverlap (getLeaves result))

  -- Property: children fit within parent
  assert "containment" (allChildrenContained result)

  -- Property: x0 < x1 and y0 < y1
  assert "valid bounds" (allValidBounds result)
```

### Pack Properties

```purescript
packProperties :: Effect Unit
packProperties = do
  input <- loadFlare2
  let result = pack defaultConfig (hierarchy input getChildren # sum getValue)

  -- Property: no circles overlap (with tolerance for floating point)
  assert "no overlap" (noCircleOverlap 0.01 (getLeaves result))

  -- Property: children within parent
  assert "containment" (allCirclesContained result)
```

### Tree Properties

```purescript
treeProperties :: Effect Unit
treeProperties = do
  input <- loadFlare2
  let result = tree defaultConfig (hierarchy input getChildren)

  -- Property: no sibling overlap at same depth
  assert "no sibling overlap" (noSiblingOverlap result)

  -- Property: parent above children (for top-down tree)
  assert "parent above children" (parentAboveChildren result)
```

## Running Tests

```bash
cd psd3-layout
spago test
```

All tests run from command line, no browser required.

## Success Criteria

1. **Golden tests pass**: All layout algorithms produce expected output for known inputs
2. **Property tests pass**: Layout invariants hold (no overlap, containment, valid bounds)
3. **Reproducible**: Same input always produces same output
4. **Fast**: Tests complete in under 10 seconds
5. **Independent**: Tests don't depend on browser or external services
6. **Maintainable**: Golden files can be updated when algorithms intentionally change

## Test Data Sources

| File | Source | Purpose |
|------|--------|---------|
| flare-2.json | docs/data/ | Hierarchy layouts |
| flare-imports.json | docs/data/ | Edge bundling |
| energy.csv | Classic D3 example | Sankey layout |
| d3-library-deps.csv | Generated | Sankey layout |
