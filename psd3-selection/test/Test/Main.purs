module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.SemiQuine.TreeToCodeSpec as TreeToCodeSpec
import Test.Expr.ExprSpec as ExprSpec
import Test.Expr.PathSpec as PathSpec
import Test.Expr.PolymorphismSpec as PolymorphismSpec
import Test.Expr.UnitsSpec as UnitsSpec
import Test.Expr.ParabolaExample as ParabolaExample
import Test.Expr.SankeyExample as SankeyExample
import Test.Expr.TreeExample as TreeExample
import Test.Expr.UpdatePatternExample as UpdatePatternExample
import Test.Scale.ScaleSpec as ScaleSpec

main :: Effect Unit
main = do
  log "=== SemiQuine TreeToCode Tests ==="
  TreeToCodeSpec.runTests

  log "\n=== Expression DSL Tests ==="
  ExprSpec.runTests

  log "\n=== Path DSL Tests ==="
  PathSpec.runTests

  log "\n=== Interpreter Polymorphism Tests ==="
  PolymorphismSpec.runTests

  log "\n=== Unit Arithmetic Tests ==="
  UnitsSpec.runTests

  log "\n=== Scale Tests ==="
  ScaleSpec.runTests

  ParabolaExample.runExample

  SankeyExample.runExample

  TreeExample.runExample

  UpdatePatternExample.runExample

  log "\n=== All tests passed! ==="
