module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Transition.TickSpec as TickSpec
import Test.Scene.RulesSpec as RulesSpec
import Test.Config.ForceSpec as ForceSpec

main :: Effect Unit
main = do
  log "=== psd3-simulation Pure Computation Tests ==="
  log ""
  log "These tests cover the pure computational aspects of the simulation package"
  log "that can be verified without browser APIs."

  log "\n=== Transition/Tick Tests ==="
  TickSpec.runTests

  log "\n=== Scene/Rules Tests ==="
  RulesSpec.runTests

  log "\n=== Config/Force Tests ==="
  ForceSpec.runTests

  log "\n=== All tests passed! ==="
