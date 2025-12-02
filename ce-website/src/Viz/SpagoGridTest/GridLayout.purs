-- | Grid Layout for Code Explorer
-- |
-- | Calculates grid positions for packages and their modules.
-- | Packages are arranged in a grid, modules cluster around their package.
module Viz.SpagoGridTest.GridLayout
  ( recalculateGridPositions
  , calculateGridPositions
  , viewBoxWidth
  , viewBoxHeight
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber, ceil)
import Data.Number (sqrt) as Num
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Types (SimNode, NodeType(..))

-- =============================================================================
-- Constants
-- =============================================================================

viewBoxWidth :: Number
viewBoxWidth = 4000.0

viewBoxHeight :: Number
viewBoxHeight = 2500.0

-- =============================================================================
-- Grid Layout
-- =============================================================================

-- | Calculate grid positions for all nodes based on package count
-- | Packages get grid positions, modules inherit their package's position
recalculateGridPositions :: Array SimNode -> Int -> Array SimNode
recalculateGridPositions nodes packageCount =
  let
    aspect = viewBoxWidth / viewBoxHeight
    gridCols = ceil (Num.sqrt (toNumber packageCount * aspect))
    gridRows = ceil (toNumber packageCount / toNumber gridCols)

    margin = 0.1
    usableWidth = viewBoxWidth * (1.0 - 2.0 * margin)
    usableHeight = viewBoxHeight * (1.0 - 2.0 * margin)

    spacingX = usableWidth / toNumber gridCols
    spacingY = usableHeight / toNumber gridRows

    gridColsN = toNumber gridCols
    gridRowsN = toNumber gridRows

    updateNode node = case node.nodeType of
      PackageNode ->
        let
          idx = node.id
          row = idx / gridCols
          col = idx `mod` gridCols
          gx = (toNumber col - gridColsN / 2.0 + 0.5) * spacingX
          gy = (toNumber row - gridRowsN / 2.0 + 0.5) * spacingY
        in node { gridX = gx, gridY = gy, x = gx, y = gy }

      ModuleNode ->
        let
          pkgIdx = node.cluster
          pkgRow = pkgIdx / gridCols
          pkgCol = pkgIdx `mod` gridCols
          pkgX = (toNumber pkgCol - gridColsN / 2.0 + 0.5) * spacingX
          pkgY = (toNumber pkgRow - gridRowsN / 2.0 + 0.5) * spacingY
        in node { gridX = pkgX, gridY = pkgY, x = pkgX, y = pkgY }
  in
    map updateNode nodes

-- | Calculate grid positions for scene transitions
-- | Recalculates positions based on node IDs (not stored gridX/gridY which may be stale)
calculateGridPositions :: Array SimNode -> Object { x :: Number, y :: Number }
calculateGridPositions nodes =
  let
    -- Count packages to determine grid layout
    packageCount = Array.length $ Array.filter (\n -> n.nodeType == PackageNode) nodes

    aspect = viewBoxWidth / viewBoxHeight
    gridCols = ceil (Num.sqrt (toNumber packageCount * aspect))
    gridRows = ceil (toNumber packageCount / toNumber gridCols)

    spacingX = viewBoxWidth * 0.8 / toNumber gridCols
    spacingY = viewBoxHeight * 0.8 / toNumber gridRows

    gridColsN = toNumber gridCols
    gridRowsN = toNumber gridRows

    getGridPos node = case node.nodeType of
      PackageNode ->
        let
          idx = node.id
          row = idx / gridCols
          col = idx `mod` gridCols
          gx = (toNumber col - gridColsN / 2.0 + 0.5) * spacingX
          gy = (toNumber row - gridRowsN / 2.0 + 0.5) * spacingY
        in Tuple (show node.id) { x: gx, y: gy }

      ModuleNode ->
        let
          pkgIdx = node.cluster
          pkgRow = pkgIdx / gridCols
          pkgCol = pkgIdx `mod` gridCols
          pkgX = (toNumber pkgCol - gridColsN / 2.0 + 0.5) * spacingX
          pkgY = (toNumber pkgRow - gridRowsN / 2.0 + 0.5) * spacingY
        in Tuple (show node.id) { x: pkgX, y: pkgY }
  in
    Object.fromFoldable $ map getGridPos nodes
