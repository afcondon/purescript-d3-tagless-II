-- | Test.Layout.AdjacencySpec
-- |
-- | Golden tests for the Adjacency Matrix layout algorithm.
-- | Tests layout of square grid visualizations.
module Test.Layout.AdjacencySpec
  ( runAdjacencyTests
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, stringify)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import DataViz.Layout.Adjacency (layout, layoutWithConfig, defaultConfig)
import DataViz.Layout.Adjacency.Types (AdjacencyMatrix, MatrixLayout, MatrixCell, MatrixLabel)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Simple 3x3 matrix
simpleMatrix :: AdjacencyMatrix
simpleMatrix =
  { matrix:
      [ [ 0.0, 1.0, 0.0 ]
      , [ 1.0, 0.0, 1.0 ]
      , [ 0.0, 1.0, 0.0 ]
      ]
  , names: [ "A", "B", "C" ]
  }

-- | 4x4 directed graph matrix (asymmetric)
directedMatrix :: AdjacencyMatrix
directedMatrix =
  { matrix:
      [ [ 0.0, 1.0, 0.0, 1.0 ]  -- A -> B, A -> D
      , [ 0.0, 0.0, 1.0, 0.0 ]  -- B -> C
      , [ 1.0, 0.0, 0.0, 1.0 ]  -- C -> A, C -> D
      , [ 0.0, 0.0, 0.0, 0.0 ]  -- D -> (none)
      ]
  , names: [ "A", "B", "C", "D" ]
  }

-- | Weighted matrix (edges have different weights)
weightedMatrix :: AdjacencyMatrix
weightedMatrix =
  { matrix:
      [ [ 0.0, 5.0, 2.0 ]
      , [ 5.0, 0.0, 8.0 ]
      , [ 2.0, 8.0, 0.0 ]
      ]
  , names: [ "Node1", "Node2", "Node3" ]
  }

-- | Single node matrix (edge case)
singleNode :: AdjacencyMatrix
singleNode =
  { matrix: [ [ 0.0 ] ]
  , names: [ "Single" ]
  }

-- | Convert cell to JSON
cellToJson :: MatrixCell -> Json
cellToJson cell = fromObject $ FO.fromFoldable
  [ "row" /\ fromNumber (Int.toNumber cell.row)
  , "col" /\ fromNumber (Int.toNumber cell.col)
  , "value" /\ fromNumber (roundTo2 cell.value)
  , "rowName" /\ fromString cell.rowName
  , "colName" /\ fromString cell.colName
  , "x" /\ fromNumber (roundTo2 cell.position.x)
  , "y" /\ fromNumber (roundTo2 cell.position.y)
  , "width" /\ fromNumber (roundTo2 cell.position.width)
  , "height" /\ fromNumber (roundTo2 cell.position.height)
  ]

-- | Convert label to JSON
labelToJson :: MatrixLabel -> Json
labelToJson label = fromObject $ FO.fromFoldable
  [ "index" /\ fromNumber (Int.toNumber label.index)
  , "name" /\ fromString label.name
  , "displayName" /\ fromString label.displayName
  , "x" /\ fromNumber (roundTo2 label.position.x)
  , "y" /\ fromNumber (roundTo2 label.position.y)
  ]

-- | Convert full layout to JSON
layoutToJson :: MatrixLayout -> Json
layoutToJson result = fromObject $ FO.fromFoldable
  [ "cells" /\ fromArray (map cellToJson sortedCells)
  , "rowLabels" /\ fromArray (map labelToJson result.rowLabels)
  , "colLabels" /\ fromArray (map labelToJson result.colLabels)
  , "gridWidth" /\ fromNumber (roundTo2 result.gridWidth)
  , "gridHeight" /\ fromNumber (roundTo2 result.gridHeight)
  , "totalWidth" /\ fromNumber (roundTo2 result.totalWidth)
  , "totalHeight" /\ fromNumber (roundTo2 result.totalHeight)
  ]
  where
  -- Sort cells by row then column for deterministic output
  sortedCells = Array.sortBy (\a b -> compare a.row b.row <> compare a.col b.col) result.cells

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Run all Adjacency Matrix layout tests
runAdjacencyTests :: Effect Int
runAdjacencyTests = do
  log "\n=== Adjacency Matrix Layout Tests ==="

  -- Test 1: Simple 3x3 matrix
  log "\nTest 1: Simple 3x3 matrix"
  let result1 = layout simpleMatrix
  log $ "  Cells: " <> show (Array.length result1.cells)
  log $ "  Row labels: " <> show (Array.length result1.rowLabels)
  log $ "  Col labels: " <> show (Array.length result1.colLabels)
  r1 <- assertGolden "adjacency-simple.golden.json" (stringify $ layoutToJson result1)
  logResult "Simple 3x3" r1

  -- Test 2: Directed graph 4x4 matrix
  log "\nTest 2: Directed graph 4x4 matrix"
  let result2 = layout directedMatrix
  log $ "  Cells: " <> show (Array.length result2.cells)
  r2 <- assertGolden "adjacency-directed.golden.json" (stringify $ layoutToJson result2)
  logResult "Directed 4x4" r2

  -- Test 3: Weighted matrix
  log "\nTest 3: Weighted matrix"
  let result3 = layout weightedMatrix
  log $ "  Cells: " <> show (Array.length result3.cells)
  r3 <- assertGolden "adjacency-weighted.golden.json" (stringify $ layoutToJson result3)
  logResult "Weighted" r3

  -- Test 4: Single node matrix (edge case)
  log "\nTest 4: Single node matrix"
  let result4 = layout singleNode
  log $ "  Cells: " <> show (Array.length result4.cells)
  r4 <- assertGolden "adjacency-single.golden.json" (stringify $ layoutToJson result4)
  logResult "Single node" r4

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nAdjacency Matrix tests: " <> show (4 - failures) <> "/4 passed"
  pure failures

logResult :: String -> GoldenResult -> Effect Unit
logResult name GoldenMatch = log $ "  PASS: " <> name
logResult name GoldenCreated = log $ "  CREATED: " <> name <> " (golden file created)"
logResult name (GoldenMismatch _ _) = log $ "  FAIL: " <> name <> " (output differs)"

countFailures :: Array GoldenResult -> Int
countFailures results = go 0 results
  where
  go n arr = case Array.uncons arr of
    Nothing -> n
    Just { head: GoldenMismatch _ _, tail } -> go (n + 1) tail
    Just { head: _, tail } -> go n tail

-- FFI import for parseFloat
foreign import parseFloat :: String -> Number
