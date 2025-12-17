-- | Test.Layout.ClusterSpec
-- |
-- | Golden tests for the Cluster (dendrogram) layout algorithm.
-- | Tests that all leaves are aligned at the same y-coordinate.
module Test.Layout.ClusterSpec
  ( runClusterTests
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, stringify)
import Data.Array as Array
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tree (Tree, mkTree)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import DataViz.Layout.Hierarchy.Cluster (cluster, defaultClusterConfig)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Node type for cluster layout tests
type ClusterNode =
  { name :: String
  , x :: Number
  , y :: Number
  , height :: Int
  }

-- | Simple binary tree for basic tests
-- |        root
-- |       /    \
-- |      A      B
-- |     / \    / \
-- |    C   D  E   F
simpleTree :: Tree ClusterNode
simpleTree =
  mkTree (node "root") $ List.fromFoldable
    [ mkTree (node "A") $ List.fromFoldable
        [ mkTree (node "C") List.Nil
        , mkTree (node "D") List.Nil
        ]
    , mkTree (node "B") $ List.fromFoldable
        [ mkTree (node "E") List.Nil
        , mkTree (node "F") List.Nil
        ]
    ]
  where
  node name = { name, x: 0.0, y: 0.0, height: 0 }

-- | Unbalanced tree to test leaf alignment
-- |        root
-- |       /    \
-- |      A      B
-- |     /|\
-- |    C D E
-- |    |
-- |    F
unbalancedTree :: Tree ClusterNode
unbalancedTree =
  mkTree (node "root") $ List.fromFoldable
    [ mkTree (node "A") $ List.fromFoldable
        [ mkTree (node "C") $ List.fromFoldable
            [ mkTree (node "F") List.Nil ]
        , mkTree (node "D") List.Nil
        , mkTree (node "E") List.Nil
        ]
    , mkTree (node "B") List.Nil
    ]
  where
  node name = { name, x: 0.0, y: 0.0, height: 0 }

-- | Linear chain tree
-- |  root -> A -> B -> C
linearChain :: Tree ClusterNode
linearChain =
  mkTree (node "root") $ List.fromFoldable
    [ mkTree (node "A") $ List.fromFoldable
        [ mkTree (node "B") $ List.fromFoldable
            [ mkTree (node "C") List.Nil ]
        ]
    ]
  where
  node name = { name, x: 0.0, y: 0.0, height: 0 }

-- | Wide shallow tree (many siblings)
-- |        root
-- |     /|\ | /|\
-- |    A B C D E F
wideShallow :: Tree ClusterNode
wideShallow =
  mkTree (node "root") $ List.fromFoldable
    [ mkTree (node "A") List.Nil
    , mkTree (node "B") List.Nil
    , mkTree (node "C") List.Nil
    , mkTree (node "D") List.Nil
    , mkTree (node "E") List.Nil
    , mkTree (node "F") List.Nil
    ]
  where
  node name = { name, x: 0.0, y: 0.0, height: 0 }

-- | Convert ClusterNode to JSON for golden comparison
-- | Round numbers to 2 decimal places for stable comparisons
nodeToJson :: ClusterNode -> Json
nodeToJson node = fromObject $ FO.fromFoldable
  [ "name" /\ fromString node.name
  , "x" /\ fromNumber (roundTo2 node.x)
  , "y" /\ fromNumber (roundTo2 node.y)
  , "height" /\ fromNumber (Int.toNumber node.height)
  ]

-- | Convert tree result to JSON array (pre-order traversal)
treeToJson :: Tree ClusterNode -> Json
treeToJson t = fromArray $ map nodeToJson nodes
  where
  -- Collect all nodes via Foldable
  nodes = Array.fromFoldable t

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Run all Cluster layout tests
runClusterTests :: Effect Int
runClusterTests = do
  log "\n=== Cluster Layout Tests ==="

  -- Test 1: Simple binary tree
  log "\nTest 1: Simple binary tree"
  let result1 = cluster defaultClusterConfig simpleTree
  let nodes1 = Array.fromFoldable result1
  log $ "  Nodes: " <> show (Array.length nodes1)
  r1 <- assertGolden "cluster-simple.golden.json" (stringify $ treeToJson result1)
  logResult "Simple binary tree" r1

  -- Verify leaf alignment property
  let leaves1 = Array.filter (\n -> n.height == 0) nodes1
  let leafYs1 = Array.nub $ map (\n -> roundTo2 n.y) leaves1
  log $ "  Leaves: " <> show (Array.length leaves1) <> ", unique y-coords: " <> show (Array.length leafYs1)

  -- Test 2: Unbalanced tree
  log "\nTest 2: Unbalanced tree"
  let result2 = cluster defaultClusterConfig unbalancedTree
  let nodes2 = Array.fromFoldable result2
  log $ "  Nodes: " <> show (Array.length nodes2)
  r2 <- assertGolden "cluster-unbalanced.golden.json" (stringify $ treeToJson result2)
  logResult "Unbalanced tree" r2

  -- Test 3: Linear chain
  log "\nTest 3: Linear chain"
  let result3 = cluster defaultClusterConfig linearChain
  let nodes3 = Array.fromFoldable result3
  log $ "  Nodes: " <> show (Array.length nodes3)
  r3 <- assertGolden "cluster-linear-chain.golden.json" (stringify $ treeToJson result3)
  logResult "Linear chain" r3

  -- Test 4: Wide shallow tree
  log "\nTest 4: Wide shallow tree"
  let result4 = cluster defaultClusterConfig wideShallow
  let nodes4 = Array.fromFoldable result4
  log $ "  Nodes: " <> show (Array.length nodes4)
  r4 <- assertGolden "cluster-wide-shallow.golden.json" (stringify $ treeToJson result4)
  logResult "Wide shallow tree" r4

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nCluster tests: " <> show (4 - failures) <> "/4 passed"
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
