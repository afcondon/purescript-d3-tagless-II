-- | DataViz.Layout.Sankey.MarkovChainWithSteps
-- |
-- | Version of Markov Chain layout that captures intermediate states.
-- | Since Markov Chain doesn't have iterations like D3's relaxation,
-- | we capture steps at key phases:
-- | - Step 0: Initial layout (from D3-style computation)
-- | - Step 1: After Markov Chain ordering
-- | - Step 2: After Partition Refinement
-- | - Steps 3-N: Additional states (padding to match D3 step count)
module DataViz.Layout.Sankey.MarkovChainWithSteps
  ( computeMarkovLayoutWithSteps
  ) where

import Prelude

import Data.Array (filter, find, foldl, sortBy, (..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import DataViz.Layout.Sankey.Compute (computeLayoutWithConfig)
import DataViz.Layout.Sankey.MarkovChain (defaultMarkovChainConfig)
import DataViz.Layout.Sankey.MarkovChainIntegration (computeMarkovChainOrdering, applyOrderingToNodes, sankeyToLayerData)
import DataViz.Layout.Sankey.PartitionRefinement (defaultPartitionRefinementConfig, refineOrdering)
import DataViz.Layout.Sankey.Types (LinkCSVRow, SankeyLink, SankeyNode, SankeyStep, defaultSankeyConfig)

-- | Compute Markov Chain layout with intermediate steps captured
computeMarkovLayoutWithSteps
  :: Array LinkCSVRow
  -> Number  -- width
  -> Number  -- height
  -> Int     -- maxIterations (used to match D3's step count)
  -> Array SankeyStep
computeMarkovLayoutWithSteps linkInputs width height maxIterations =
  let
    config = defaultSankeyConfig width height
    extent = config.extent

    -- Step 0: Initial D3-style layout
    initialResult = computeLayoutWithConfig linkInputs config
    step0 =
      { iteration: 0
      , label: "Initial (D3-style layout)"
      , nodes: initialResult.nodes
      , links: initialResult.links
      }

    -- Compute the Markov Chain ordering
    markovResult = computeMarkovChainOrdering defaultMarkovChainConfig
      initialResult.nodes initialResult.links

    -- Apply the Markov ordering to nodes
    markovNodes = applyOrderingToNodes
      markovResult
      initialResult.nodes
      extent.y0
      extent.y1
      config.nodePadding

    markovLinks = calculateLinkPositions markovNodes initialResult.links

    step1 =
      { iteration: 1
      , label: "After Markov Chain ordering"
      , nodes: markovNodes
      , links: markovLinks
      }

    -- Apply Partition Refinement (Stage 2)
    { edgesByLayer } = sankeyToLayerData initialResult.nodes initialResult.links
    refinedResult = refineOrdering defaultPartitionRefinementConfig
      markovResult.orderings edgesByLayer

    -- Apply the refined ordering
    refinedNodes = applyOrderingToNodes
      (markovResult { orderings = refinedResult.bestOrderings })
      initialResult.nodes
      extent.y0
      extent.y1
      config.nodePadding

    refinedLinks = calculateLinkPositions refinedNodes initialResult.links

    step2 =
      { iteration: 2
      , label: "After Partition Refinement"
      , nodes: refinedNodes
      , links: refinedLinks
      }

    -- Create the base steps
    baseSteps = [step0, step1, step2]

    -- Pad to match maxIterations + 1 steps (0 to maxIterations)
    -- For steps beyond step2, just repeat the final state
    paddedSteps =
      if maxIterations + 1 > 3 then
        baseSteps <> map (\i ->
          { iteration: i
          , label: "Final state (step " <> show i <> ")"
          , nodes: refinedNodes
          , links: refinedLinks
          }
        ) (3 .. maxIterations)
      else
        Array.take (maxIterations + 1) baseSteps

  in paddedSteps

-- | Calculate link y0/y1 positions based on node positions
calculateLinkPositions :: Array SankeyNode -> Array SankeyLink -> Array SankeyLink
calculateLinkPositions nodes links =
  let
    -- Set y0 for each link (at source node)
    linksWithY0 = foldl
      (\currentLinks node ->
        let
          outgoingLinks = filter (\l -> l.sourceIndex == node.index) currentLinks
          sorted = sortBy
            (\a b ->
              case find (\n -> n.index == a.targetIndex) nodes,
                   find (\n -> n.index == b.targetIndex) nodes of
                Just na, Just nb -> compare na.y0 nb.y0
                _, _ -> EQ
            )
            outgoingLinks
          withY0 = foldl
            (\acc link ->
              let
                y = acc.y + link.width / 2.0
                updated = link { y0 = y }
              in
                { y: acc.y + link.width
                , links: Array.snoc acc.links updated
                }
            )
            { y: node.y0, links: [] }
            sorted
        in
          map
            (\link ->
              case find (\l -> l.index == link.index) withY0.links of
                Just updatedLink -> updatedLink
                Nothing -> link
            )
            currentLinks
      )
      links
      nodes

    -- Set y1 for each link (at target node)
    linksWithY1 = foldl
      (\currentLinks node ->
        let
          incomingLinks = filter (\l -> l.targetIndex == node.index) currentLinks
          sorted = sortBy
            (\a b ->
              case find (\n -> n.index == a.sourceIndex) nodes,
                   find (\n -> n.index == b.sourceIndex) nodes of
                Just na, Just nb -> compare na.y0 nb.y0
                _, _ -> EQ
            )
            incomingLinks
          withY1 = foldl
            (\acc link ->
              let
                y = acc.y + link.width / 2.0
                updated = link { y1 = y }
              in
                { y: acc.y + link.width
                , links: Array.snoc acc.links updated
                }
            )
            { y: node.y0, links: [] }
            sorted
        in
          map
            (\link ->
              case find (\l -> l.index == link.index) withY1.links of
                Just updatedLink -> updatedLink
                Nothing -> link
            )
            currentLinks
      )
      linksWithY0
      nodes
  in
    linksWithY1
