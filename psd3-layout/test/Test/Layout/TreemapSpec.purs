-- | Test.Layout.TreemapSpec
-- |
-- | Golden tests for the Treemap layout algorithm.
-- | Tests rectangular partitioning with various tiling strategies.
module Test.Layout.TreemapSpec
  ( runTreemapTests
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
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), TreemapConfig, defaultTreemapConfig, treemap, dice, slice, sliceDice, squarify, phi)
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Simple valued node hierarchy
-- |        root (sum=100)
-- |       /    \
-- |      A(60)  B(40)
-- |     / \
-- |   C(30) D(30)
simpleValuedNode :: ValuedNode String
simpleValuedNode =
  VNode
    { data_: "root"
    , depth: 0
    , height: 2
    , parent: Nothing
    , children:
        [ VNode
            { data_: "A"
            , depth: 1
            , height: 1
            , parent: Nothing
            , children:
                [ VNode { data_: "C", depth: 2, height: 0, parent: Nothing, children: [], value: 30.0 }
                , VNode { data_: "D", depth: 2, height: 0, parent: Nothing, children: [], value: 30.0 }
                ]
            , value: 60.0
            }
        , VNode
            { data_: "B"
            , depth: 1
            , height: 0
            , parent: Nothing
            , children: []
            , value: 40.0
            }
        ]
    , value: 100.0
    }

-- | Wide hierarchy (many children)
wideValuedNode :: ValuedNode String
wideValuedNode =
  VNode
    { data_: "root"
    , depth: 0
    , height: 1
    , parent: Nothing
    , children:
        [ VNode { data_: "A", depth: 1, height: 0, parent: Nothing, children: [], value: 15.0 }
        , VNode { data_: "B", depth: 1, height: 0, parent: Nothing, children: [], value: 25.0 }
        , VNode { data_: "C", depth: 1, height: 0, parent: Nothing, children: [], value: 35.0 }
        , VNode { data_: "D", depth: 1, height: 0, parent: Nothing, children: [], value: 10.0 }
        , VNode { data_: "E", depth: 1, height: 0, parent: Nothing, children: [], value: 15.0 }
        ]
    , value: 100.0
    }

-- | Single node
singleValuedNode :: ValuedNode String
singleValuedNode =
  VNode
    { data_: "single"
    , depth: 0
    , height: 0
    , parent: Nothing
    , children: []
    , value: 100.0
    }

-- | Convert TreemapNode to JSON
treemapNodeToJson :: TreemapNode String -> Json
treemapNodeToJson (TNode node) = fromObject $ FO.fromFoldable
  [ "name" /\ fromString node.data_
  , "depth" /\ fromNumber (Int.toNumber node.depth)
  , "height" /\ fromNumber (Int.toNumber node.height)
  , "value" /\ fromNumber (roundTo2 node.value)
  , "x0" /\ fromNumber (roundTo2 node.x0)
  , "y0" /\ fromNumber (roundTo2 node.y0)
  , "x1" /\ fromNumber (roundTo2 node.x1)
  , "y1" /\ fromNumber (roundTo2 node.y1)
  , "children" /\ fromArray (map treemapNodeToJson node.children)
  ]

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Flatten TreemapNode tree to array
flattenTreemapNodes :: TreemapNode String -> Array (TreemapNode String)
flattenTreemapNodes node@(TNode n) =
  [ node ] <> Array.concatMap flattenTreemapNodes n.children

-- | Config with specific width/height for testing
testConfig :: forall a. TreemapConfig a
testConfig = defaultTreemapConfig { size = { width: 100.0, height: 100.0 } }

-- | Run all Treemap layout tests
runTreemapTests :: Effect Int
runTreemapTests = do
  log "\n=== Treemap Layout Tests ==="

  -- Test 1: Simple hierarchy with squarify (default)
  log "\nTest 1: Simple hierarchy (squarify)"
  let result1 = treemap testConfig simpleValuedNode
  let nodes1 = flattenTreemapNodes result1
  log $ "  Nodes: " <> show (Array.length nodes1)
  r1 <- assertGolden "treemap-simple-squarify.golden.json" (stringify $ treemapNodeToJson result1)
  logResult "Simple squarify" r1

  -- Test 2: Wide hierarchy with squarify
  log "\nTest 2: Wide hierarchy (squarify)"
  let result2 = treemap testConfig wideValuedNode
  let nodes2 = flattenTreemapNodes result2
  log $ "  Nodes: " <> show (Array.length nodes2)
  r2 <- assertGolden "treemap-wide-squarify.golden.json" (stringify $ treemapNodeToJson result2)
  logResult "Wide squarify" r2

  -- Test 3: Simple hierarchy with slice-dice
  log "\nTest 3: Simple hierarchy (slice-dice)"
  let sliceDiceConfig = testConfig { tile = sliceDice }
  let result3 = treemap sliceDiceConfig simpleValuedNode
  let nodes3 = flattenTreemapNodes result3
  log $ "  Nodes: " <> show (Array.length nodes3)
  r3 <- assertGolden "treemap-simple-slicedice.golden.json" (stringify $ treemapNodeToJson result3)
  logResult "Simple slice-dice" r3

  -- Test 4: Single node
  log "\nTest 4: Single node"
  let result4 = treemap testConfig singleValuedNode
  let nodes4 = flattenTreemapNodes result4
  log $ "  Nodes: " <> show (Array.length nodes4)
  r4 <- assertGolden "treemap-single.golden.json" (stringify $ treemapNodeToJson result4)
  logResult "Single node" r4

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nTreemap tests: " <> show (4 - failures) <> "/4 passed"
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
