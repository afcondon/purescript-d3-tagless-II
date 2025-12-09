-- | DataViz.Layout.Sankey.ComputeWithSteps
-- |
-- | Version of Sankey layout that captures intermediate states at each iteration.
-- | Uses the same algorithm as Compute.purs but captures states after each
-- | relaxation iteration for debugging and visual comparison.
module DataViz.Layout.Sankey.ComputeWithSteps
  ( computeLayoutWithSteps
  ) where

import Prelude

import Data.Array (filter, foldl, length, mapWithIndex, (!!), (..))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pow)
import DataViz.Layout.Sankey.Compute (computeLayoutWithConfig)
import DataViz.Layout.Sankey.Types (LinkCSVRow, SankeyConfig, SankeyLink, SankeyNode, SankeyStep, defaultSankeyConfig)

-- Constants
epsilon :: Number
epsilon = 1.0e-6

-- | Compute Sankey layout with intermediate steps captured
-- | Runs the full D3-style algorithm but captures state after each relaxation iteration
computeLayoutWithSteps
  :: Array LinkCSVRow
  -> Number  -- width
  -> Number  -- height
  -> Int     -- maxIterations
  -> Array SankeyStep
computeLayoutWithSteps linkInputs width height maxIterations =
  let
    -- Get configurations for each iteration count
    -- We run the full layout with iterations=0, 1, 2, ... to get each step
    steps = map (\iter ->
      let
        config = (defaultSankeyConfig width height) { iterations = iter }
        result = computeLayoutWithConfig linkInputs config
        label = if iter == 0
                then "Initial (no relaxation)"
                else "After " <> show iter <> " relaxation iteration" <> (if iter > 1 then "s" else "")
      in
        { iteration: iter
        , label: label
        , nodes: result.nodes
        , links: result.links
        }
    ) (0 .. maxIterations)
  in steps
