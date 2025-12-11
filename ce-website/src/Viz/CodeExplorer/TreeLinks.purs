-- | Tree Link Rendering
-- |
-- | Pure PureScript link path generation for tree visualization.
-- | Supports both radial and vertical link layouts.
module Viz.CodeExplorer.TreeLinks
  ( radialLinkPath
  , verticalLinkPath
  , makeTreeLinkPathFromNodes
  , makeVerticalTreeLinkPathFromNodes
  , makeVerticalLinkPathFromCurrentPositions
  ) where

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import DataViz.Layout.Hierarchy.Link (linkBezierRadialCartesian, linkBezierVertical)
import Types (SimNode, SimLink)

-- | Radial link path generator (re-export from library)
radialLinkPath :: Number -> Number -> Number -> Number -> String
radialLinkPath = linkBezierRadialCartesian

-- | Vertical link path generator (re-export from library)
verticalLinkPath :: Number -> Number -> Number -> Number -> String
verticalLinkPath = linkBezierVertical

-- | Generate a radial link path for a link using node map for position lookup
-- | Uses treeX/treeY from nodes (Cartesian coordinates)
makeTreeLinkPathFromNodes :: Map Int SimNode -> SimLink -> String
makeTreeLinkPathFromNodes nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      radialLinkPath sourceNode.treeX sourceNode.treeY targetNode.treeX targetNode.treeY
    _, _ -> ""

-- | Generate a vertical link path for a link using node map for position lookup
-- | Uses treeX/treeY from nodes (Cartesian coordinates)
makeVerticalTreeLinkPathFromNodes :: Map Int SimNode -> SimLink -> String
makeVerticalTreeLinkPathFromNodes nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      verticalLinkPath sourceNode.treeX sourceNode.treeY targetNode.treeX targetNode.treeY
    _, _ -> ""

-- | Generate a vertical link path using current animated positions (x, y)
-- | Used for tick-based animation where nodes are interpolating toward tree positions
makeVerticalLinkPathFromCurrentPositions :: Map Int SimNode -> SimLink -> String
makeVerticalLinkPathFromCurrentPositions nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      verticalLinkPath sourceNode.x sourceNode.y targetNode.x targetNode.y
    _, _ -> ""
