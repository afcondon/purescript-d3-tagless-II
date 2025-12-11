module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.SemiQuine.TreeToCodeSpec as TreeToCodeSpec
import Test.PSD3v3.ExprSpec as ExprSpec
import Test.PSD3v3.PathSpec as PathSpec
import Test.PSD3v3.ParabolaExample as ParabolaExample

main :: Effect Unit
main = do
  log "=== SemiQuine TreeToCode Tests ==="
  TreeToCodeSpec.runTests

  log "\n=== PSD3v3 Expression DSL Tests ==="
  ExprSpec.runTests

  log "\n=== PSD3v3 Path DSL Tests ==="
  PathSpec.runTests

  ParabolaExample.runExample

  log "\n=== All tests passed! ==="
