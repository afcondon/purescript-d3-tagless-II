-- | PSD3v3 Sankey Example
-- |
-- | Demonstrates how the v3 DSL handles real Sankey diagram patterns:
-- | - Static attributes
-- | - Data-driven attributes with field access
-- | - Computed attributes (path generation)
-- | - Conditional attributes (text anchor based on position)
module Test.PSD3v3.SankeyExample where

import Prelude hiding (add)

import Data.Array ((..), mapWithIndex)
import Data.Foldable (foldl, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- DSL
import PSD3v3.Expr (class NumExpr, class BoolExpr, class CompareExpr, class StringExpr, add, ifThenElse)
import PSD3v3.Expr as E
import PSD3v3.Datum (class DatumExpr, field)
import PSD3v3.Path (class PathExpr, sankeyLink)
import PSD3v3.Sugar ((+:), (-:), (/:), (<.), n, s)

-- Interpreters
import PSD3v3.Interpreter.Eval (EvalD, runEvalD)
import PSD3v3.Interpreter.CodeGen (CodeGen, runCodeGen)

-- =============================================================================
-- Sankey Data Types (simplified)
-- =============================================================================

-- | A Sankey node with layout coordinates
type SankeyNode =
  { name :: String
  , x0 :: Number  -- Left edge
  , x1 :: Number  -- Right edge
  , y0 :: Number  -- Top edge
  , y1 :: Number  -- Bottom edge
  , color :: String
  }

-- | A Sankey link between nodes
type SankeyLink =
  { sourceX :: Number   -- Source node's x1 (right edge)
  , sourceY0 :: Number  -- Top of link at source
  , sourceY1 :: Number  -- Bottom of link at source
  , targetX :: Number   -- Target node's x0 (left edge)
  , targetY0 :: Number  -- Top of link at target
  , targetY1 :: Number  -- Bottom of link at target
  , color :: String
  }

-- Row types for DatumExpr constraints
type SankeyNodeRow = (name :: String, x0 :: Number, x1 :: Number, y0 :: Number, y1 :: Number, color :: String)
type SankeyLinkRow = (sourceX :: Number, sourceY0 :: Number, sourceY1 :: Number, targetX :: Number, targetY0 :: Number, targetY1 :: Number, color :: String)

-- =============================================================================
-- Sample Data
-- =============================================================================

sampleNodes :: Array SankeyNode
sampleNodes =
  [ { name: "Coal", x0: 0.0, x1: 20.0, y0: 0.0, y1: 150.0, color: "#555" }
  , { name: "Gas", x0: 0.0, x1: 20.0, y0: 170.0, y1: 280.0, color: "#777" }
  , { name: "Power", x0: 200.0, x1: 220.0, y0: 50.0, y1: 250.0, color: "#c66" }
  , { name: "Industry", x0: 400.0, x1: 420.0, y0: 0.0, y1: 120.0, color: "#6c6" }
  , { name: "Residential", x0: 400.0, x1: 420.0, y0: 140.0, y1: 220.0, color: "#66c" }
  ]

sampleLinks :: Array SankeyLink
sampleLinks =
  [ { sourceX: 20.0, sourceY0: 0.0, sourceY1: 100.0, targetX: 200.0, targetY0: 50.0, targetY1: 150.0, color: "#a55" }
  , { sourceX: 20.0, sourceY0: 100.0, sourceY1: 150.0, targetX: 200.0, targetY0: 150.0, targetY1: 200.0, color: "#a77" }
  , { sourceX: 20.0, sourceY0: 170.0, sourceY1: 280.0, targetX: 200.0, targetY0: 200.0, targetY1: 250.0, color: "#a99" }
  ]

-- Width for conditional positioning
chartWidth :: Number
chartWidth = 450.0

-- =============================================================================
-- Field Accessors (the "boilerplate" for each datum type)
-- =============================================================================

-- Node field accessors
nodeName :: forall repr. DatumExpr repr SankeyNodeRow => repr String
nodeName = field (Proxy :: Proxy "name")

nodeX0 :: forall repr. DatumExpr repr SankeyNodeRow => repr Number
nodeX0 = field (Proxy :: Proxy "x0")

nodeX1 :: forall repr. DatumExpr repr SankeyNodeRow => repr Number
nodeX1 = field (Proxy :: Proxy "x1")

nodeY0 :: forall repr. DatumExpr repr SankeyNodeRow => repr Number
nodeY0 = field (Proxy :: Proxy "y0")

nodeY1 :: forall repr. DatumExpr repr SankeyNodeRow => repr Number
nodeY1 = field (Proxy :: Proxy "y1")

nodeColor :: forall repr. DatumExpr repr SankeyNodeRow => repr String
nodeColor = field (Proxy :: Proxy "color")

-- Link field accessors
linkSourceX :: forall repr. DatumExpr repr SankeyLinkRow => repr Number
linkSourceX = field (Proxy :: Proxy "sourceX")

linkSourceY0 :: forall repr. DatumExpr repr SankeyLinkRow => repr Number
linkSourceY0 = field (Proxy :: Proxy "sourceY0")

linkSourceY1 :: forall repr. DatumExpr repr SankeyLinkRow => repr Number
linkSourceY1 = field (Proxy :: Proxy "sourceY1")

linkTargetX :: forall repr. DatumExpr repr SankeyLinkRow => repr Number
linkTargetX = field (Proxy :: Proxy "targetX")

linkTargetY0 :: forall repr. DatumExpr repr SankeyLinkRow => repr Number
linkTargetY0 = field (Proxy :: Proxy "targetY0")

linkTargetY1 :: forall repr. DatumExpr repr SankeyLinkRow => repr Number
linkTargetY1 = field (Proxy :: Proxy "targetY1")

linkColor :: forall repr. DatumExpr repr SankeyLinkRow => repr String
linkColor = field (Proxy :: Proxy "color")

-- =============================================================================
-- Attribute Expressions (polymorphic!)
-- =============================================================================

-- | Node rectangle X position
nodeRectX :: forall repr. NumExpr repr => DatumExpr repr SankeyNodeRow => repr Number
nodeRectX = nodeX0

-- | Node rectangle Y position
nodeRectY :: forall repr. NumExpr repr => DatumExpr repr SankeyNodeRow => repr Number
nodeRectY = nodeY0

-- | Node rectangle width (x1 - x0)
nodeRectWidth :: forall repr. NumExpr repr => DatumExpr repr SankeyNodeRow => repr Number
nodeRectWidth = E.sub nodeX1 nodeX0

-- | Node rectangle height (y1 - y0)
nodeRectHeight :: forall repr. NumExpr repr => DatumExpr repr SankeyNodeRow => repr Number
nodeRectHeight = E.sub nodeY1 nodeY0

-- | Label X position (offset from node edge based on position)
-- | If node is on left half, label goes right of node; otherwise left
labelX :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => DatumExpr repr SankeyNodeRow => repr Number
labelX = ifThenElse
  (nodeX0 <. n (chartWidth / 2.0))  -- If on left half
  (nodeX1 +: 6.0)                    -- Position right of node
  (nodeX0 -: 6.0)                    -- Position left of node

-- | Label Y position (vertically centered on node)
labelY :: forall repr. NumExpr repr => DatumExpr repr SankeyNodeRow => repr Number
labelY = add nodeY0 nodeY1 /: 2.0

-- | Text anchor (start or end based on node position)
labelAnchor :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => StringExpr repr => DatumExpr repr SankeyNodeRow => repr String
labelAnchor = ifThenElse
  (nodeX0 <. n (chartWidth / 2.0))
  (s "start")
  (s "end")

-- | Link path using the PathExpr DSL!
linkPath :: forall repr. NumExpr repr => PathExpr repr => DatumExpr repr SankeyLinkRow => repr String
linkPath = sankeyLink linkSourceX linkSourceY0 linkSourceY1 linkTargetX linkTargetY0 linkTargetY1

-- =============================================================================
-- Demo
-- =============================================================================

runExample :: Effect Unit
runExample = do
  log "\n=== PSD3v3 Sankey Example ==="

  log "\n--- Node Attribute Expressions ---"
  log "nodeRectWidth = nodeX1 - nodeX0"
  log $ "  CodeGen: " <> runCodeGen (nodeRectWidth :: CodeGen Number)

  log "\nlabelX = if x0 < chartWidth/2 then x1 + 6 else x0 - 6"
  log $ "  CodeGen: " <> runCodeGen (labelX :: CodeGen Number)

  log "\nlabelAnchor = if x0 < chartWidth/2 then \"start\" else \"end\""
  log $ "  CodeGen: " <> runCodeGen (labelAnchor :: CodeGen String)

  log "\n--- Link Path Expression ---"
  log "linkPath = sankeyLink sourceX sourceY0 sourceY1 targetX targetY0 targetY1"
  log $ "  CodeGen: " <> runCodeGen (linkPath :: CodeGen String)

  log "\n--- Evaluated for Sample Nodes ---"
  log "Node            x     y   width height labelX labelAnchor"
  log "──────────────────────────────────────────────────────────"
  for_ (mapWithIndex Tuple sampleNodes) \(Tuple i node) -> do
    let rx = runEvalD (nodeRectX :: EvalD SankeyNode Number) node i
    let ry = runEvalD (nodeRectY :: EvalD SankeyNode Number) node i
    let rw = runEvalD (nodeRectWidth :: EvalD SankeyNode Number) node i
    let rh = runEvalD (nodeRectHeight :: EvalD SankeyNode Number) node i
    let lx = runEvalD (labelX :: EvalD SankeyNode Number) node i
    let la = runEvalD (labelAnchor :: EvalD SankeyNode String) node i
    log $ padRight 15 node.name <> " " <>
          padLeft 5 (show rx) <> " " <>
          padLeft 5 (show ry) <> " " <>
          padLeft 5 (show rw) <> " " <>
          padLeft 5 (show rh) <> " " <>
          padLeft 6 (show lx) <> " " <>
          la

  log "\n--- Evaluated Link Paths ---"
  for_ (mapWithIndex Tuple sampleLinks) \(Tuple i link) -> do
    let pathStr = runEvalD (linkPath :: EvalD SankeyLink String) link i
    log $ "Link " <> show i <> ": " <> pathStr

  log "\n--- Key Insights ---"
  log "1. Conditional expressions work cleanly: ifThenElse (nodeX0 <. n w/2) ..."
  log "2. Link paths use PathExpr: sankeyLink sourceX sourceY0 sourceY1 ..."
  log "3. CodeGen shows semantic intent, not computed values"
  log "4. Same expressions work for Eval, CodeGen, and SVG rendering"

-- Helpers for formatting
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
    go acc s = go (acc + 1) (drop 1 s)

drop :: Int -> String -> String
drop _ "" = ""
drop 0 s = s
drop n s = drop (n - 1) (dropFirst s)

foreign import dropFirst :: String -> String
