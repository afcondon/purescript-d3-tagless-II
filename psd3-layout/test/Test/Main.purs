-- | Test.Main
-- |
-- | Main test runner for psd3-layout golden tests.
-- | Runs all layout algorithm tests and reports results.
module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Layout.SankeySpec (runSankeyTests)
import Test.Layout.TreeSpec (runTreeTests)
import Test.Layout.ClusterSpec (runClusterTests)
import Test.Layout.StateMachineSpec (runStateMachineTests)
import Test.Layout.AdjacencySpec (runAdjacencyTests)
import Test.Layout.PackSpec (runPackTests)
import Test.Layout.TreemapSpec (runTreemapTests)
import Test.Layout.PartitionSpec (runPartitionTests)
import Test.Layout.EdgeBundleSpec (runEdgeBundleTests)

main :: Effect Unit
main = do
  log "================================"
  log "psd3-layout Golden Tests"
  log "================================"

  -- Run all test suites
  sankeyFailures <- runSankeyTests
  treeFailures <- runTreeTests
  clusterFailures <- runClusterTests
  stateMachineFailures <- runStateMachineTests
  adjacencyFailures <- runAdjacencyTests
  packFailures <- runPackTests
  treemapFailures <- runTreemapTests
  partitionFailures <- runPartitionTests
  edgebundleFailures <- runEdgeBundleTests

  -- Summary
  let totalFailures = sankeyFailures + treeFailures + clusterFailures + stateMachineFailures + adjacencyFailures + packFailures + treemapFailures + partitionFailures + edgebundleFailures
  log "\n================================"
  log "Summary"
  log "================================"
  log $ "Sankey: " <> show (4 - sankeyFailures) <> "/4 passed"
  log $ "Tree: " <> show (4 - treeFailures) <> "/4 passed"
  log $ "Cluster: " <> show (4 - clusterFailures) <> "/4 passed"
  log $ "StateMachine: " <> show (4 - stateMachineFailures) <> "/4 passed"
  log $ "Adjacency: " <> show (4 - adjacencyFailures) <> "/4 passed"
  log $ "Pack: " <> show (4 - packFailures) <> "/4 passed"
  log $ "Treemap: " <> show (4 - treemapFailures) <> "/4 passed"
  log $ "Partition: " <> show (4 - partitionFailures) <> "/4 passed"
  log $ "EdgeBundle: " <> show (4 - edgebundleFailures) <> "/4 passed"

  if totalFailures == 0 then do
    log "\nAll tests passed!"
  else do
    log $ "\nFAILURES: " <> show totalFailures <> " test(s) failed"

  log ""
