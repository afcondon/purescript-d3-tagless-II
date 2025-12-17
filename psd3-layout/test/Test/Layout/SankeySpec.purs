-- | Test.Layout.SankeySpec
-- |
-- | Golden tests for the Sankey layout algorithm.
-- | Tests that the layout produces consistent, deterministic output.
module Test.Layout.SankeySpec
  ( runSankeyTests
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
import DataViz.Layout.Sankey.Compute (computeLayout)
import DataViz.Layout.Sankey.Types (LinkCSVRow, SankeyLayoutResult, SankeyNode, SankeyLink, NodeID(..))
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Simple test data: a small flow network
simpleFlowData :: Array LinkCSVRow
simpleFlowData =
  [ { s: "A", t: "X", v: 10.0 }
  , { s: "A", t: "Y", v: 20.0 }
  , { s: "B", t: "X", v: 15.0 }
  , { s: "B", t: "Y", v: 5.0 }
  , { s: "X", t: "Z", v: 25.0 }
  , { s: "Y", t: "Z", v: 25.0 }
  ]

-- | Linear chain for testing depth calculation
linearChainData :: Array LinkCSVRow
linearChainData =
  [ { s: "Start", t: "Middle", v: 100.0 }
  , { s: "Middle", t: "End", v: 100.0 }
  ]

-- | Fan-out pattern (one source, multiple targets)
fanOutData :: Array LinkCSVRow
fanOutData =
  [ { s: "Source", t: "Target1", v: 30.0 }
  , { s: "Source", t: "Target2", v: 40.0 }
  , { s: "Source", t: "Target3", v: 30.0 }
  ]

-- | Fan-in pattern (multiple sources, one target)
fanInData :: Array LinkCSVRow
fanInData =
  [ { s: "Source1", t: "Sink", v: 30.0 }
  , { s: "Source2", t: "Sink", v: 40.0 }
  , { s: "Source3", t: "Sink", v: 30.0 }
  ]

-- | Convert SankeyNode to JSON for golden comparison
-- | Round numbers to 2 decimal places for stable comparisons
nodeToJson :: SankeyNode -> Json
nodeToJson node = fromObject $ FO.fromFoldable
  [ "name" /\ fromString node.name
  , "x0" /\ fromNumber (roundTo2 node.x0)
  , "y0" /\ fromNumber (roundTo2 node.y0)
  , "x1" /\ fromNumber (roundTo2 node.x1)
  , "y1" /\ fromNumber (roundTo2 node.y1)
  , "value" /\ fromNumber (roundTo2 node.value)
  , "depth" /\ fromNumber (Int.toNumber node.depth)
  , "layer" /\ fromNumber (Int.toNumber node.layer)
  ]

-- | Convert SankeyLink to JSON for golden comparison
linkToJson :: SankeyLink -> Json
linkToJson link = fromObject $ FO.fromFoldable
  [ "sourceIndex" /\ nodeIdToJson link.sourceIndex
  , "targetIndex" /\ nodeIdToJson link.targetIndex
  , "value" /\ fromNumber (roundTo2 link.value)
  , "width" /\ fromNumber (roundTo2 link.width)
  , "y0" /\ fromNumber (roundTo2 link.y0)
  , "y1" /\ fromNumber (roundTo2 link.y1)
  ]

nodeIdToJson :: NodeID -> Json
nodeIdToJson (NodeID i) = fromNumber (Int.toNumber i)

-- | Convert full layout result to JSON
resultToJson :: SankeyLayoutResult -> Json
resultToJson result = fromObject $ FO.fromFoldable
  [ "nodes" /\ fromArray (map nodeToJson sortedNodes)
  , "links" /\ fromArray (map linkToJson sortedLinks)
  ]
  where
  -- Sort nodes by name for consistent output
  sortedNodes = Array.sortBy (\a b -> compare a.name b.name) result.nodes
  -- Sort links by source then target for consistent output
  sortedLinks = Array.sortBy compareLinkIndices result.links
  compareLinkIndices a b =
    compare a.sourceIndex b.sourceIndex <> compare a.targetIndex b.targetIndex

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Run all Sankey layout tests
runSankeyTests :: Effect Int
runSankeyTests = do
  log "\n=== Sankey Layout Tests ==="

  -- Test 1: Simple flow network
  log "\nTest 1: Simple flow network"
  let result1 = computeLayout simpleFlowData 800.0 600.0
  log $ "  Nodes: " <> show (Array.length result1.nodes)
  log $ "  Links: " <> show (Array.length result1.links)
  r1 <- assertGolden "sankey-simple-flow.golden.json" (stringify $ resultToJson result1)
  logResult "Simple flow" r1

  -- Test 2: Linear chain (tests depth calculation)
  log "\nTest 2: Linear chain"
  let result2 = computeLayout linearChainData 800.0 600.0
  log $ "  Nodes: " <> show (Array.length result2.nodes)
  log $ "  Links: " <> show (Array.length result2.links)
  r2 <- assertGolden "sankey-linear-chain.golden.json" (stringify $ resultToJson result2)
  logResult "Linear chain" r2

  -- Test 3: Fan-out pattern
  log "\nTest 3: Fan-out pattern"
  let result3 = computeLayout fanOutData 800.0 600.0
  log $ "  Nodes: " <> show (Array.length result3.nodes)
  log $ "  Links: " <> show (Array.length result3.links)
  r3 <- assertGolden "sankey-fan-out.golden.json" (stringify $ resultToJson result3)
  logResult "Fan-out" r3

  -- Test 4: Fan-in pattern
  log "\nTest 4: Fan-in pattern"
  let result4 = computeLayout fanInData 800.0 600.0
  log $ "  Nodes: " <> show (Array.length result4.nodes)
  log $ "  Links: " <> show (Array.length result4.links)
  r4 <- assertGolden "sankey-fan-in.golden.json" (stringify $ resultToJson result4)
  logResult "Fan-in" r4

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nSankey tests: " <> show (4 - failures) <> "/4 passed"
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
