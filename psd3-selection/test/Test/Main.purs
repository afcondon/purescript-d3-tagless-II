module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.SemiQuine.TreeToCodeSpec as TreeToCodeSpec
import Test.PSD3.Expr.ExprSpec as ExprSpec
import Test.PSD3.Expr.PathSpec as PathSpec
import Test.PSD3.Expr.ParabolaExample as ParabolaExample
import Test.PSD3.Expr.SankeyExample as SankeyExample
import Test.PSD3.Expr.TreeExample as TreeExample
import Test.PSD3.Expr.UpdatePatternExample as UpdatePatternExample

main :: Effect Unit
main = do
  log "=== SemiQuine TreeToCode Tests ==="
  TreeToCodeSpec.runTests

  log "\n=== PSD3v3 Expression DSL Tests ==="
  ExprSpec.runTests

  log "\n=== PSD3v3 Path DSL Tests ==="
  PathSpec.runTests

  ParabolaExample.runExample

  SankeyExample.runExample

  TreeExample.runExample

  UpdatePatternExample.runExample

  log "\n=== All tests passed! ==="
