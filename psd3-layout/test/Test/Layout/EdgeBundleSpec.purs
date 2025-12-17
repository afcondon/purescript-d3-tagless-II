-- | Test.Layout.EdgeBundleSpec
-- |
-- | Golden tests for the EdgeBundle (hierarchical edge bundling) layout algorithm.
-- | Tests radial clustering with bundled links.
module Test.Layout.EdgeBundleSpec
  ( runEdgeBundleTests
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, stringify)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.String.CodeUnits as SCU
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import DataViz.Layout.Hierarchy.EdgeBundle (edgeBundle, EdgeBundleConfig, EdgeBundleResult, PositionedNode, BundledLink)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Simple test data type
type TestNode =
  { name :: String
  , size :: Number
  , imports :: Array String
  }

-- | Simple module structure with imports
-- | foo.bar imports foo.baz
-- | foo.baz imports foo.qux
-- | foo.qux imports foo.bar (cycle)
simpleModules :: Array TestNode
simpleModules =
  [ { name: "foo.bar", size: 10.0, imports: [ "foo.baz" ] }
  , { name: "foo.baz", size: 20.0, imports: [ "foo.qux" ] }
  , { name: "foo.qux", size: 15.0, imports: [ "foo.bar" ] }
  ]

-- | Multi-package structure
-- | pkg.a.mod1 imports pkg.b.mod1
-- | pkg.a.mod2 imports pkg.a.mod1
-- | pkg.b.mod1 imports pkg.b.mod2
-- | pkg.b.mod2 imports pkg.a.mod2
multiPackageModules :: Array TestNode
multiPackageModules =
  [ { name: "pkg.a.mod1", size: 10.0, imports: [ "pkg.b.mod1" ] }
  , { name: "pkg.a.mod2", size: 15.0, imports: [ "pkg.a.mod1" ] }
  , { name: "pkg.b.mod1", size: 20.0, imports: [ "pkg.b.mod2" ] }
  , { name: "pkg.b.mod2", size: 12.0, imports: [ "pkg.a.mod2" ] }
  ]

-- | No imports (isolated nodes)
isolatedModules :: Array TestNode
isolatedModules =
  [ { name: "alpha.one", size: 10.0, imports: [] }
  , { name: "alpha.two", size: 20.0, imports: [] }
  , { name: "beta.one", size: 15.0, imports: [] }
  ]

-- | Single module
singleModule :: Array TestNode
singleModule =
  [ { name: "single.module", size: 100.0, imports: [] }
  ]

-- | Config for tests
testConfig :: EdgeBundleConfig TestNode
testConfig =
  { getName: _.name
  , getImports: _.imports
  , beta: 0.85
  , innerRadius: 50.0
  , outerRadius: 100.0
  }

-- | Convert result to JSON
resultToJson :: EdgeBundleResult TestNode -> Json
resultToJson result = fromObject $ FO.fromFoldable
  [ "nodes" /\ fromArray (map nodeToJson sortedNodes)
  , "links" /\ fromArray (map linkToJson sortedLinks)
  ]
  where
  -- Sort for deterministic output
  sortedNodes = Array.sortBy (comparing _.fullName) result.nodes
  sortedLinks = Array.sortBy (\a b -> compare a.source b.source <> compare a.target b.target) result.links

-- | Convert positioned node to JSON
nodeToJson :: PositionedNode TestNode -> Json
nodeToJson node = fromObject $ FO.fromFoldable
  [ "fullName" /\ fromString node.fullName
  , "shortName" /\ fromString node.shortName
  , "x" /\ fromNumber (roundTo4 node.x)  -- radians need more precision
  , "y" /\ fromNumber (roundTo2 node.y)
  , "cartX" /\ fromNumber (roundTo2 node.cartX)
  , "cartY" /\ fromNumber (roundTo2 node.cartY)
  , "isLeaf" /\ fromBoolean node.isLeaf
  , "outgoingCount" /\ fromNumber (Int.toNumber node.outgoingCount)
  , "incomingCount" /\ fromNumber (Int.toNumber node.incomingCount)
  ]

-- | Convert bundled link to JSON
linkToJson :: BundledLink -> Json
linkToJson link = fromObject $ FO.fromFoldable
  [ "source" /\ fromString link.source
  , "target" /\ fromString link.target
  , "pathLength" /\ fromNumber (Int.toNumber $ countPathCommands link.path)
  ]

-- | Count path commands (M, L, C) in an SVG path string
countPathCommands :: String -> Int
countPathCommands s = Array.length $ Array.filter isPathCommand $ SCU.toCharArray s
  where
  isPathCommand c = c == 'L' || c == 'C' || c == 'M'

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Round to 4 decimal places (for radians)
roundTo4 :: Number -> Number
roundTo4 n =
  let str = toStringWith (fixed 4) n
  in parseFloat str

-- | Run all EdgeBundle layout tests
runEdgeBundleTests :: Effect Int
runEdgeBundleTests = do
  log "\n=== EdgeBundle Layout Tests ==="

  -- Test 1: Simple circular dependencies
  log "\nTest 1: Simple modules (circular deps)"
  let result1 = edgeBundle testConfig simpleModules
  log $ "  Nodes: " <> show (Array.length result1.nodes)
  log $ "  Links: " <> show (Array.length result1.links)
  r1 <- assertGolden "edgebundle-simple.golden.json" (stringify $ resultToJson result1)
  logResult "Simple modules" r1

  -- Test 2: Multi-package structure
  log "\nTest 2: Multi-package modules"
  let result2 = edgeBundle testConfig multiPackageModules
  log $ "  Nodes: " <> show (Array.length result2.nodes)
  log $ "  Links: " <> show (Array.length result2.links)
  r2 <- assertGolden "edgebundle-multipackage.golden.json" (stringify $ resultToJson result2)
  logResult "Multi-package" r2

  -- Test 3: Isolated nodes (no imports)
  log "\nTest 3: Isolated modules (no imports)"
  let result3 = edgeBundle testConfig isolatedModules
  log $ "  Nodes: " <> show (Array.length result3.nodes)
  log $ "  Links: " <> show (Array.length result3.links)
  r3 <- assertGolden "edgebundle-isolated.golden.json" (stringify $ resultToJson result3)
  logResult "Isolated modules" r3

  -- Test 4: Single module
  log "\nTest 4: Single module"
  let result4 = edgeBundle testConfig singleModule
  log $ "  Nodes: " <> show (Array.length result4.nodes)
  log $ "  Links: " <> show (Array.length result4.links)
  r4 <- assertGolden "edgebundle-single.golden.json" (stringify $ resultToJson result4)
  logResult "Single module" r4

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nEdgeBundle tests: " <> show (4 - failures) <> "/4 passed"
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
