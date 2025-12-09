-- | DataViz.Layout.Sankey.CompareOrderings
-- |
-- | Test module to compare the existing D3-style heuristic algorithm
-- | with the new Markov Chain + Partition Refinement method.
module DataViz.Layout.Sankey.CompareOrderings
  ( compareOrderings
  , ComparisonResult
  ) where

import Prelude

import Data.Array (filter, length)
import Data.Array as Array
import DataViz.Layout.Sankey.CSV (parseSankeyCSV)
import DataViz.Layout.Sankey.Compute (computeLayout)
import DataViz.Layout.Sankey.MarkovChain (LayerOrdering, calculateWeightedCrossing, defaultMarkovChainConfig)
import DataViz.Layout.Sankey.MarkovChainIntegration (computeMarkovChainOrdering, sankeyToLayerData)
import DataViz.Layout.Sankey.PartitionRefinement (defaultPartitionRefinementConfig, refineOrdering)
import DataViz.Layout.Sankey.Types (SankeyNode)

-- | Result of comparing the two ordering algorithms
type ComparisonResult =
  { heuristicCrossing :: Number      -- ^ Weighted crossing from D3-style heuristic
  , markovChainCrossing :: Number    -- ^ Weighted crossing from Stage 1 (Markov Chain)
  , refinedCrossing :: Number        -- ^ Weighted crossing after Stage 2 (Partition Refinement)
  , bestRefinedCrossing :: Number    -- ^ Best crossing seen during refinement iterations
  , numNodes :: Int                  -- ^ Number of nodes in the graph
  , numLinks :: Int                  -- ^ Number of links in the graph
  , numLayers :: Int                 -- ^ Number of layers
  , refinementIterations :: Int      -- ^ Number of refinement iterations performed
  }

-- | Compare the two ordering algorithms on the same input data
-- | Returns metrics showing how each algorithm performed
compareOrderings :: String -> Number -> Number -> ComparisonResult
compareOrderings csvData width height =
  let
    -- Parse CSV data
    linkInputs = parseSankeyCSV csvData

    -- ===== Run D3-style heuristic =====
    heuristicResult = computeLayout linkInputs width height

    -- Convert heuristic result to layer format for crossing calculation
    { edgesByLayer: heuristicEdges, layers: heuristicLayers } =
      sankeyToLayerData heuristicResult.nodes heuristicResult.links

    -- Get ordering from heuristic (nodes are already ordered by y position within each layer)
    heuristicOrderings = extractOrderingsFromNodes heuristicResult.nodes (length heuristicLayers)

    -- Calculate weighted crossing for heuristic
    heuristicCrossing = calculateWeightedCrossing heuristicOrderings heuristicEdges

    -- ===== Run Markov Chain method =====
    markovResult = computeMarkovChainOrdering defaultMarkovChainConfig heuristicResult.nodes heuristicResult.links

    -- Calculate weighted crossing for Markov Chain (Stage 1)
    markovChainCrossing = calculateWeightedCrossing markovResult.orderings heuristicEdges

    -- ===== Run Partition Refinement (Stage 2) =====
    refinedResult = refineOrdering defaultPartitionRefinementConfig markovResult.orderings heuristicEdges

  in
    { heuristicCrossing
    , markovChainCrossing
    , refinedCrossing: refinedResult.weightedCrossing
    , bestRefinedCrossing: refinedResult.bestWeightedCrossing
    , numNodes: length heuristicResult.nodes
    , numLinks: length heuristicResult.links
    , numLayers: length heuristicLayers
    , refinementIterations: refinedResult.iterations
    }

-- | Extract orderings from positioned nodes (sorted by y position within each layer)
extractOrderingsFromNodes :: Array SankeyNode -> Int -> Array LayerOrdering
extractOrderingsFromNodes nodes numLayers =
  map (\layerIdx ->
    let
      layerNodes = filter (\n -> n.layer == layerIdx) nodes
      -- Sort by y0 position (top to bottom)
      sorted = Array.sortBy (\a b -> compare a.y0 b.y0) layerNodes
      -- Extract local indices (we need to build a map from NodeID to local index)
      localIndices = Array.mapWithIndex (\i _ -> i) sorted
    in
      localIndices
  ) (Array.range 0 (numLayers - 1))
