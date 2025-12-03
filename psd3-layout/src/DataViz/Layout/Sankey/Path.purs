-- | DataViz.Layout.Sankey.Path
-- |
-- | SVG path generation for Sankey link curves.
-- | Generates the characteristic curved ribbons for flow visualization.
module DataViz.Layout.Sankey.Path
  ( findNode
  , generateLinkPath
  , generateStraightPath
  ) where

import Prelude
import Data.Number.Format (toString)
import DataViz.Layout.Sankey.Types (SankeyLink, SankeyNode, NodeID)
import Data.Array (find)
import Data.Maybe (Maybe(..))

-- | Helper to find node by ID
findNode :: Array SankeyNode -> NodeID -> Maybe SankeyNode
findNode nodes nodeId = find (\n -> n.index == nodeId) nodes

-- | Generate SVG path data for a Sankey link
-- | Creates a cubic bezier curve ribbon from source to target
-- | This draws a filled area (not just a stroke) by creating a closed path
generateLinkPath :: Array SankeyNode -> SankeyLink -> String
generateLinkPath nodes link =
  let
    sourceNode = findNode nodes link.sourceIndex
    targetNode = findNode nodes link.targetIndex
  in
    case sourceNode, targetNode of
      Just source, Just target ->
        let
          -- Source node right edge
          x0 = source.x1
          -- Target node left edge
          x1 = target.x0

          -- Link connects at these y-coordinates
          -- These are the CENTER points of the link
          y0 = link.y0
          y1 = link.y1

          -- Link width (visual thickness)
          width = link.width

          -- Top and bottom edges of the ribbon at source
          y0Top = y0 - width / 2.0
          y0Bottom = y0 + width / 2.0

          -- Top and bottom edges of the ribbon at target
          y1Top = y1 - width / 2.0
          y1Bottom = y1 + width / 2.0

          -- Control point x (halfway between source and target for s-curve)
          xi = (x0 + x1) / 2.0

        -- Draw ribbon as a closed path:
        -- 1. Start at top of source
        -- 2. Curve to top of target
        -- 3. Line down to bottom of target
        -- 4. Curve back to bottom of source
        -- 5. Line up to close
        in
          "M" <> toString x0 <> "," <> toString y0Top
            <> " C"
            <> toString xi
            <> ","
            <> toString y0Top
            <> " "
            <> toString xi
            <> ","
            <> toString y1Top
            <> " "
            <> toString x1
            <> ","
            <> toString y1Top
            <> " L"
            <> toString x1
            <> ","
            <> toString y1Bottom
            <> " C"
            <> toString xi
            <> ","
            <> toString y1Bottom
            <> " "
            <> toString xi
            <> ","
            <> toString y0Bottom
            <> " "
            <> toString x0
            <> ","
            <> toString y0Bottom
            <>
              " Z"

      _, _ -> "" -- Invalid link, return empty path

-- | Alternative: Generate horizontal link path (straight lines)
-- | Useful for debugging or different visual styles
generateStraightPath :: Array SankeyNode -> SankeyLink -> String
generateStraightPath nodes link =
  let
    sourceNode = findNode nodes link.sourceIndex
    targetNode = findNode nodes link.targetIndex
  in
    case sourceNode, targetNode of
      Just source, Just target ->
        let
          x0 = source.x1
          y0 = link.y0
          x1 = target.x0
          y1 = link.y1
        in
          "M" <> toString x0 <> "," <> toString y0
            <> " L"
            <> toString x1
            <> ","
            <> toString y1

      _, _ -> ""
