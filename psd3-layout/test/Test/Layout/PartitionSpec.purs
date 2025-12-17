-- | Test.Layout.PartitionSpec
-- |
-- | Golden tests for the Partition (icicle) layout algorithm.
-- | Tests rectangular partitioning by depth level.
module Test.Layout.PartitionSpec
  ( runPartitionTests
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
import DataViz.Layout.Hierarchy.Partition (PartitionNode(..), HierarchyData(..), hierarchy, partition, defaultPartitionConfig)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Simple hierarchical data
-- |        root
-- |       /    \
-- |      A      B
-- |     / \
-- |    C   D
simpleHierarchy :: HierarchyData String
simpleHierarchy =
  HierarchyData
    { data_: "root"
    , value: Nothing
    , children: Just
        [ HierarchyData
            { data_: "A"
            , value: Nothing
            , children: Just
                [ HierarchyData { data_: "C", value: Just 30.0, children: Nothing }
                , HierarchyData { data_: "D", value: Just 30.0, children: Nothing }
                ]
            }
        , HierarchyData
            { data_: "B"
            , value: Just 40.0
            , children: Nothing
            }
        ]
    }

-- | Deep hierarchy (4 levels)
deepHierarchy :: HierarchyData String
deepHierarchy =
  HierarchyData
    { data_: "root"
    , value: Nothing
    , children: Just
        [ HierarchyData
            { data_: "A"
            , value: Nothing
            , children: Just
                [ HierarchyData
                    { data_: "B"
                    , value: Nothing
                    , children: Just
                        [ HierarchyData { data_: "C", value: Just 50.0, children: Nothing }
                        ]
                    }
                ]
            }
        ]
    }

-- | Wide hierarchy (many siblings)
wideHierarchy :: HierarchyData String
wideHierarchy =
  HierarchyData
    { data_: "root"
    , value: Nothing
    , children: Just
        [ HierarchyData { data_: "A", value: Just 20.0, children: Nothing }
        , HierarchyData { data_: "B", value: Just 30.0, children: Nothing }
        , HierarchyData { data_: "C", value: Just 15.0, children: Nothing }
        , HierarchyData { data_: "D", value: Just 35.0, children: Nothing }
        ]
    }

-- | Single node
singleNode :: HierarchyData String
singleNode =
  HierarchyData
    { data_: "single"
    , value: Just 100.0
    , children: Nothing
    }

-- | Convert PartitionNode to JSON
partitionNodeToJson :: PartitionNode String -> Json
partitionNodeToJson (PartNode node) = fromObject $ FO.fromFoldable
  [ "name" /\ fromString node.data_
  , "depth" /\ fromNumber (Int.toNumber node.depth)
  , "height" /\ fromNumber (Int.toNumber node.height)
  , "value" /\ fromNumber (roundTo2 node.value)
  , "x0" /\ fromNumber (roundTo2 node.x0)
  , "y0" /\ fromNumber (roundTo2 node.y0)
  , "x1" /\ fromNumber (roundTo2 node.x1)
  , "y1" /\ fromNumber (roundTo2 node.y1)
  , "children" /\ fromArray (map partitionNodeToJson node.children)
  ]

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Flatten PartitionNode tree to array
flattenPartitionNodes :: PartitionNode String -> Array (PartitionNode String)
flattenPartitionNodes node@(PartNode n) =
  [ node ] <> Array.concatMap flattenPartitionNodes n.children

-- | Test config with specific size
testConfig :: forall a. { size :: { width :: Number, height :: Number }, padding :: Number, round :: Boolean }
testConfig = { size: { width: 100.0, height: 100.0 }, padding: 0.0, round: false }

-- | Run all Partition layout tests
runPartitionTests :: Effect Int
runPartitionTests = do
  log "\n=== Partition Layout Tests ==="

  -- Test 1: Simple hierarchy
  log "\nTest 1: Simple hierarchy"
  let hier1 = hierarchy simpleHierarchy
  let result1 = partition testConfig hier1
  let nodes1 = flattenPartitionNodes result1
  log $ "  Nodes: " <> show (Array.length nodes1)
  r1 <- assertGolden "partition-simple.golden.json" (stringify $ partitionNodeToJson result1)
  logResult "Simple hierarchy" r1

  -- Test 2: Deep hierarchy
  log "\nTest 2: Deep hierarchy"
  let hier2 = hierarchy deepHierarchy
  let result2 = partition testConfig hier2
  let nodes2 = flattenPartitionNodes result2
  log $ "  Nodes: " <> show (Array.length nodes2)
  r2 <- assertGolden "partition-deep.golden.json" (stringify $ partitionNodeToJson result2)
  logResult "Deep hierarchy" r2

  -- Test 3: Wide hierarchy
  log "\nTest 3: Wide hierarchy"
  let hier3 = hierarchy wideHierarchy
  let result3 = partition testConfig hier3
  let nodes3 = flattenPartitionNodes result3
  log $ "  Nodes: " <> show (Array.length nodes3)
  r3 <- assertGolden "partition-wide.golden.json" (stringify $ partitionNodeToJson result3)
  logResult "Wide hierarchy" r3

  -- Test 4: Single node
  log "\nTest 4: Single node"
  let hier4 = hierarchy singleNode
  let result4 = partition testConfig hier4
  let nodes4 = flattenPartitionNodes result4
  log $ "  Nodes: " <> show (Array.length nodes4)
  r4 <- assertGolden "partition-single.golden.json" (stringify $ partitionNodeToJson result4)
  logResult "Single node" r4

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nPartition tests: " <> show (4 - failures) <> "/4 passed"
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
