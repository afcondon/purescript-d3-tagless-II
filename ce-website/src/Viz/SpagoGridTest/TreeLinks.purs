-- | Tree Link Rendering
-- |
-- | Pure PureScript radial link path generation for tree visualization.
module Viz.SpagoGridTest.TreeLinks
  ( radialLinkPath
  , makeTreeLinkPathFromNodes
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (atan2, cos, sin, sqrt)
import Types (SimNode, SimLink)

-- | Radial link path generator
-- | Creates cubic Bezier curves that follow the radial structure.
-- | Takes Cartesian coordinates and converts back to polar for proper radial control points.
-- |
-- | TODO: Promote to D3.Layout.Hierarchy.Link as `linkBezierRadialCartesian`
-- | (complements existing `linkBezierRadial` which takes angle/radius coords)
radialLinkPath :: Number -> Number -> Number -> Number -> String
radialLinkPath x1 y1 x2 y2 =
  let
    -- Convert cartesian back to polar to calculate proper radial control points
    angle1 = atan2 y1 x1
    radius1 = sqrt (x1 * x1 + y1 * y1)
    angle2 = atan2 y2 x2
    radius2 = sqrt (x2 * x2 + y2 * y2)

    -- Control points in polar coordinates:
    -- CP1: parent's angle, halfway to child's radius
    -- CP2: child's angle, halfway to child's radius
    midRadius = (radius1 + radius2) / 2.0

    -- Convert control points back to cartesian
    cp1x = midRadius * cos angle1
    cp1y = midRadius * sin angle1
    cp2x = midRadius * cos angle2
    cp2y = midRadius * sin angle2
  in
    "M" <> show x1 <> "," <> show y1 <>
    "C" <> show cp1x <> "," <> show cp1y <>
    " " <> show cp2x <> "," <> show cp2y <>
    " " <> show x2 <> "," <> show y2

-- | Generate a radial link path for a link using node map for position lookup
-- | Uses treeX/treeY from nodes (Cartesian coordinates)
makeTreeLinkPathFromNodes :: Map Int SimNode -> SimLink -> String
makeTreeLinkPathFromNodes nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      radialLinkPath sourceNode.treeX sourceNode.treeY targetNode.treeX targetNode.treeY
    _, _ -> ""
