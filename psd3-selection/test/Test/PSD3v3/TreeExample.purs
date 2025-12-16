-- | PSD3v3 Tree Example
-- |
-- | Demonstrates how the v3 DSL handles tree layout patterns:
-- | - Node positioning from layout coordinates
-- | - Link paths connecting parent to child
-- | - Conditional styling based on depth/leaf status
module Test.PSD3.Expr.TreeExample where

import Prelude hiding (add)

import Data.Array ((..), length)
import Data.Foldable (foldl, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- DSL
import PSD3.Expr.Expr (class NumExpr, class BoolExpr, class CompareExpr, class StringExpr, ifThenElse)
import PSD3.Expr.Expr as E
import PSD3.Expr.Datum (class DatumExpr, field)
import PSD3.Expr.Path (class PathExpr, linkVertical)
import PSD3.Expr.Sugar ((+:), (<.), n, s)

-- Interpreters
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)

-- =============================================================================
-- Tree Data Types (matching typical d3-hierarchy output)
-- =============================================================================

-- | A tree node with layout coordinates (as produced by d3.tree())
type TreeNode =
  { name :: String
  , x :: Number      -- Horizontal position (for horizontal trees, this is "across")
  , y :: Number      -- Vertical position (for horizontal trees, this is "depth")
  , depth :: Int     -- Depth in tree (root = 0)
  , hasChildren :: Boolean
  }

-- | A tree link connecting parent to child
type TreeLink =
  { sourceX :: Number
  , sourceY :: Number
  , targetX :: Number
  , targetY :: Number
  }

-- Row types for DatumExpr constraints
type TreeNodeRow = (name :: String, x :: Number, y :: Number, depth :: Int, hasChildren :: Boolean)
type TreeLinkRow = (sourceX :: Number, sourceY :: Number, targetX :: Number, targetY :: Number)

-- =============================================================================
-- Sample Data (simulating d3.tree() output for a small hierarchy)
-- =============================================================================

-- Tree structure:
--        Root
--       /    \
--      A      B
--     / \      \
--    A1  A2     B1

sampleNodes :: Array TreeNode
sampleNodes =
  [ { name: "Root", x: 150.0, y: 50.0, depth: 0, hasChildren: true }
  , { name: "A", x: 75.0, y: 150.0, depth: 1, hasChildren: true }
  , { name: "B", x: 225.0, y: 150.0, depth: 1, hasChildren: true }
  , { name: "A1", x: 37.5, y: 250.0, depth: 2, hasChildren: false }
  , { name: "A2", x: 112.5, y: 250.0, depth: 2, hasChildren: false }
  , { name: "B1", x: 225.0, y: 250.0, depth: 2, hasChildren: false }
  ]

sampleLinks :: Array TreeLink
sampleLinks =
  [ { sourceX: 150.0, sourceY: 50.0, targetX: 75.0, targetY: 150.0 }   -- Root -> A
  , { sourceX: 150.0, sourceY: 50.0, targetX: 225.0, targetY: 150.0 }  -- Root -> B
  , { sourceX: 75.0, sourceY: 150.0, targetX: 37.5, targetY: 250.0 }   -- A -> A1
  , { sourceX: 75.0, sourceY: 150.0, targetX: 112.5, targetY: 250.0 }  -- A -> A2
  , { sourceX: 225.0, sourceY: 150.0, targetX: 225.0, targetY: 250.0 } -- B -> B1
  ]

-- =============================================================================
-- Field Accessors
-- =============================================================================

-- Node field accessors
nodeName :: forall repr. DatumExpr repr TreeNodeRow => repr String
nodeName = field (Proxy :: Proxy "name")

nodeX :: forall repr. DatumExpr repr TreeNodeRow => repr Number
nodeX = field (Proxy :: Proxy "x")

nodeY :: forall repr. DatumExpr repr TreeNodeRow => repr Number
nodeY = field (Proxy :: Proxy "y")

nodeDepth :: forall repr. DatumExpr repr TreeNodeRow => repr Int
nodeDepth = field (Proxy :: Proxy "depth")

nodeHasChildren :: forall repr. DatumExpr repr TreeNodeRow => repr Boolean
nodeHasChildren = field (Proxy :: Proxy "hasChildren")

-- Link field accessors
linkSourceX :: forall repr. DatumExpr repr TreeLinkRow => repr Number
linkSourceX = field (Proxy :: Proxy "sourceX")

linkSourceY :: forall repr. DatumExpr repr TreeLinkRow => repr Number
linkSourceY = field (Proxy :: Proxy "sourceY")

linkTargetX :: forall repr. DatumExpr repr TreeLinkRow => repr Number
linkTargetX = field (Proxy :: Proxy "targetX")

linkTargetY :: forall repr. DatumExpr repr TreeLinkRow => repr Number
linkTargetY = field (Proxy :: Proxy "targetY")

-- =============================================================================
-- Attribute Expressions (polymorphic!)
-- =============================================================================

-- | Node circle radius - larger for nodes with children
nodeRadius :: forall repr. NumExpr repr => BoolExpr repr => DatumExpr repr TreeNodeRow => repr Number
nodeRadius = ifThenElse nodeHasChildren (n 8.0) (n 5.0)

-- | Node fill color - different for root, branches, leaves
nodeFill :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => StringExpr repr => DatumExpr repr TreeNodeRow => repr String
nodeFill = ifThenElse
  (nodeDepthNum <. n 1.0)  -- Root
  (s "#e74c3c")            -- Red
  (ifThenElse
    nodeHasChildren        -- Branch vs Leaf
    (s "#3498db")          -- Blue for branches
    (s "#2ecc71"))         -- Green for leaves
  where
    -- Convert Int depth to Number for comparison
    nodeDepthNum :: forall r. NumExpr r => DatumExpr r TreeNodeRow => r Number
    nodeDepthNum = E.mul (n 1.0) (unsafeCoerceField nodeDepth)

-- | Label offset - position below node
labelY :: forall repr. NumExpr repr => DatumExpr repr TreeNodeRow => repr Number
labelY = nodeY +: 20.0

-- | Link path using vertical bezier (top-down tree)
treeLinkPath :: forall repr. NumExpr repr => PathExpr repr => DatumExpr repr TreeLinkRow => repr String
treeLinkPath = linkVertical linkSourceX linkSourceY linkTargetX linkTargetY

-- =============================================================================
-- Demo
-- =============================================================================

runExample :: Effect Unit
runExample = do
  log "\n=== PSD3v3 Tree Example ==="

  log "\n--- Node Attribute Expressions ---"
  log "nodeRadius = if hasChildren then 8 else 5"
  log $ "  CodeGen: " <> runCodeGen (nodeRadius :: CodeGen Number)

  log "\nnodeFill = if depth < 1 then red else (if hasChildren then blue else green)"
  log $ "  CodeGen: " <> runCodeGen (nodeFill :: CodeGen String)

  log "\n--- Link Path Expression ---"
  log "treeLinkPath = linkVertical sourceX sourceY targetX targetY"
  log $ "  CodeGen: " <> runCodeGen (treeLinkPath :: CodeGen String)

  log "\n--- Evaluated for Sample Nodes ---"
  log "Node        x       y   radius  fill"
  log "────────────────────────────────────────"
  for_ (indexedArray sampleNodes) \(Tuple i node) -> do
    let x = runEvalD (nodeX :: EvalD TreeNode Number) node i
    let y = runEvalD (nodeY :: EvalD TreeNode Number) node i
    let r = runEvalD (nodeRadius :: EvalD TreeNode Number) node i
    let f = runEvalD (nodeFill :: EvalD TreeNode String) node i
    log $ padRight 12 node.name <>
          padLeft 6 (show x) <> " " <>
          padLeft 6 (show y) <> " " <>
          padLeft 6 (show r) <> "  " <>
          f

  log "\n--- Evaluated Link Paths ---"
  for_ (indexedArray sampleLinks) \(Tuple i link) -> do
    let pathStr = runEvalD (treeLinkPath :: EvalD TreeLink String) link i
    log $ "Link " <> show i <> ": " <> pathStr

  log "\n--- Generated SVG ---"
  log $ generateTreeSVG sampleNodes sampleLinks

  log "\n--- Key Insights ---"
  log "1. Nested conditionals work: if depth<1 then ... else (if hasChildren then ...)"
  log "2. linkVertical produces smooth bezier curves for top-down trees"
  log "3. Same expressions for Eval (runtime) and CodeGen (source code)"

-- =============================================================================
-- SVG Generation (using the v3 DSL!)
-- =============================================================================

generateTreeSVG :: Array TreeNode -> Array TreeLink -> String
generateTreeSVG nodes links =
  """<svg width="300" height="320" xmlns="http://www.w3.org/2000/svg">
  <style>
    .node text { font: 10px sans-serif; text-anchor: middle; }
    .link { fill: none; stroke: #999; stroke-width: 1.5; }
  </style>
  <g class="links">""" <>
  foldl (\acc (Tuple i link) ->
    let path = runEvalD (treeLinkPath :: EvalD TreeLink String) link i
    in acc <> "\n    <path class=\"link\" d=\"" <> path <> "\" />"
  ) "" (indexedArray links) <>
  """
  </g>
  <g class="nodes">""" <>
  foldl (\acc (Tuple i node) ->
    let x = runEvalD (nodeX :: EvalD TreeNode Number) node i
        y = runEvalD (nodeY :: EvalD TreeNode Number) node i
        r = runEvalD (nodeRadius :: EvalD TreeNode Number) node i
        fill = runEvalD (nodeFill :: EvalD TreeNode String) node i
        ly = runEvalD (labelY :: EvalD TreeNode Number) node i
    in acc <> "\n    <g class=\"node\" transform=\"translate(" <> show x <> "," <> show y <> ")\">" <>
              "\n      <circle r=\"" <> show r <> "\" fill=\"" <> fill <> "\" stroke=\"white\" stroke-width=\"2\" />" <>
              "\n      <text y=\"" <> show (ly - y) <> "\">" <> node.name <> "</text>" <>
              "\n    </g>"
  ) "" (indexedArray nodes) <>
  """
  </g>
</svg>"""

-- =============================================================================
-- Helpers
-- =============================================================================

indexedArray :: forall a. Array a -> Array (Tuple Int a)
indexedArray arr = foldl (\acc (Tuple i a) -> acc <> [Tuple i a]) [] (zipWithIndex arr)
  where
    zipWithIndex :: Array a -> Array (Tuple Int a)
    zipWithIndex xs = foldl (\acc x -> acc <> [Tuple (length acc) x]) [] xs

padRight :: Int -> String -> String
padRight n str =
  let len = stringLength str
  in str <> replicate (max 0 (n - len)) " "

padLeft :: Int -> String -> String
padLeft n str =
  let len = stringLength str
  in replicate (max 0 (n - len)) " " <> str

replicate :: Int -> String -> String
replicate n str = foldl (\acc _ -> acc <> str) "" (1..n)

stringLength :: String -> Int
stringLength = go 0
  where
    go acc "" = acc
    go acc s = go (acc + 1) (dropFirst s)

foreign import dropFirst :: String -> String

-- Unsafe coercion for Int field to Number (needed for comparison)
foreign import unsafeCoerceField :: forall repr a b. repr a -> repr b
