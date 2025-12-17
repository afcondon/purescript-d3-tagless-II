-- | Test.Main
-- |
-- | Main test runner for psd3-layout golden tests.
-- | Runs all layout algorithm tests and reports results.
module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Layout.SankeySpec (runSankeyTests)

main :: Effect Unit
main = do
  log "================================"
  log "psd3-layout Golden Tests"
  log "================================"

  -- Run all test suites
  sankeyFailures <- runSankeyTests

  -- Summary
  let totalFailures = sankeyFailures
  log "\n================================"
  log "Summary"
  log "================================"

  if totalFailures == 0 then do
    log "All tests passed!"
  else do
    log $ "FAILURES: " <> show totalFailures <> " test(s) failed"

  log ""
