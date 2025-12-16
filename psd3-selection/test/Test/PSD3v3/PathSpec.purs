-- | Tests for PSD3v3 Path DSL
-- |
-- | Demonstrates path generation for trees, Sankey, and chord diagrams
module Test.PSD3.Expr.PathSpec where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Number (pi)

-- DSL
import PSD3.Expr.Expr (class NumExpr, lit)
import PSD3.Expr.Path (class PathExpr, linePath, linkHorizontal, linkVertical, sankeyLink, arc)

-- Interpreters
import PSD3.Expr.Interpreter.Eval (Eval, runEval)
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)

-- =============================================================================
-- Path Examples (polymorphic!)
-- =============================================================================

-- | Simple line from (10,20) to (100,80)
simpleLine :: forall repr. NumExpr repr => PathExpr repr => repr String
simpleLine = linePath (lit 10.0) (lit 20.0) (lit 100.0) (lit 80.0)

-- | Horizontal tree link (typical for left-to-right trees)
treeLink :: forall repr. NumExpr repr => PathExpr repr => repr String
treeLink = linkHorizontal (lit 50.0) (lit 100.0) (lit 200.0) (lit 150.0)

-- | Vertical tree link (typical for top-down trees)
verticalTreeLink :: forall repr. NumExpr repr => PathExpr repr => repr String
verticalTreeLink = linkVertical (lit 100.0) (lit 50.0) (lit 150.0) (lit 200.0)

-- | Sankey flow link
sankeyFlow :: forall repr. NumExpr repr => PathExpr repr => repr String
sankeyFlow = sankeyLink
  (lit 50.0)   -- source x
  (lit 100.0)  -- source y0 (top)
  (lit 140.0)  -- source y1 (bottom)
  (lit 250.0)  -- target x
  (lit 80.0)   -- target y0 (top)
  (lit 120.0)  -- target y1 (bottom)

-- | Pie slice (arc with inner radius 0)
pieSlice :: forall repr. NumExpr repr => PathExpr repr => repr String
pieSlice = arc (lit 0.0) (lit (pi / 2.0)) (lit 0.0) (lit 100.0)

-- | Donut slice (arc with inner radius > 0)
donutSlice :: forall repr. NumExpr repr => PathExpr repr => repr String
donutSlice = arc (lit 0.0) (lit (pi / 2.0)) (lit 50.0) (lit 100.0)

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Simple Line ---"
  log $ "  Eval:    " <> runEval (simpleLine :: Eval String)
  log $ "  CodeGen: " <> runCodeGen (simpleLine :: CodeGen String)

  log "\n--- Horizontal Tree Link ---"
  log $ "  Eval:    " <> runEval (treeLink :: Eval String)
  log $ "  CodeGen: " <> runCodeGen (treeLink :: CodeGen String)

  log "\n--- Vertical Tree Link ---"
  log $ "  Eval:    " <> runEval (verticalTreeLink :: Eval String)
  log $ "  CodeGen: " <> runCodeGen (verticalTreeLink :: CodeGen String)

  log "\n--- Sankey Flow Link ---"
  log $ "  Eval:    " <> runEval (sankeyFlow :: Eval String)
  log $ "  CodeGen: " <> runCodeGen (sankeyFlow :: CodeGen String)

  log "\n--- Pie Slice (0 to π/2) ---"
  log $ "  Eval:    " <> runEval (pieSlice :: Eval String)
  log $ "  CodeGen: " <> runCodeGen (pieSlice :: CodeGen String)

  log "\n--- Donut Slice (inner=50, outer=100) ---"
  log $ "  Eval:    " <> runEval (donutSlice :: Eval String)
  log $ "  CodeGen: " <> runCodeGen (donutSlice :: CodeGen String)

  log "\n--- Key Insight ---"
  log "  Same path expression, multiple interpretations!"
  log "  - Eval: produces SVG path string for rendering"
  log "  - CodeGen: produces source code like (linkHorizontal 50 100 200 150)"
  log ""
  log "  Covers: Trees, Sankey, Chord diagrams, Pie/Donut charts"
  log "  No arbitrary path string building needed!"
