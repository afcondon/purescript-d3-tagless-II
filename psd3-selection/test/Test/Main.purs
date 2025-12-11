module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.SemiQuine.TreeToCodeSpec as TreeToCodeSpec
import Test.PSD3v3.ExprSpec as ExprSpec

main :: Effect Unit
main = do
  log "=== SemiQuine TreeToCode Tests ==="
  TreeToCodeSpec.runTests

  log "\n=== PSD3v3 Expression DSL Tests ==="
  ExprSpec.runTests

  log "\n=== All tests passed! ==="
