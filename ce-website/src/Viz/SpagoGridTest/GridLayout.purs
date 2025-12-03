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
-- Grid Configuration
-- =============================================================================

-- | Grid layout configuration computed from package count
type GridConfig =
  { gridCols :: Int
  , gridColsN :: Number
  , gridRowsN :: Number
  , spacingX :: Number
  , spacingY :: Number
  }

-- | Create grid config from package count
mkGridConfig :: Int -> GridConfig
mkGridConfig packageCount =
  let
    aspect = viewBoxWidth / viewBoxHeight
    gridCols = ceil (Num.sqrt (toNumber packageCount * aspect))
    gridRows = ceil (toNumber packageCount / toNumber gridCols)
    spacingX = viewBoxWidth * 0.8 / toNumber gridCols
    spacingY = viewBoxHeight * 0.8 / toNumber gridRows
  in
    { gridCols
    , gridColsN: toNumber gridCols
    , gridRowsN: toNumber gridRows
    , spacingX
    , spacingY
    }

-- | Get grid position for a package index
gridPosForIndex :: GridConfig -> Int -> { x :: Number, y :: Number }
gridPosForIndex cfg idx =
  let
    row = idx / cfg.gridCols
    col = idx `mod` cfg.gridCols
    x = (toNumber col - cfg.gridColsN / 2.0 + 0.5) * cfg.spacingX
    y = (toNumber row - cfg.gridRowsN / 2.0 + 0.5) * cfg.spacingY
  in
    { x, y }

-- | Get grid index for a node (packages use their id, modules use their cluster)
nodeGridIndex :: SimNode -> Int
nodeGridIndex node = case node.nodeType of
  PackageNode -> node.id
  ModuleNode -> node.cluster

-- =============================================================================
-- Public API
-- =============================================================================

-- | Calculate grid positions for all nodes based on package count
-- | Packages get grid positions, modules inherit their package's position
-- | Also sets x/y to the grid position (for initialization)
recalculateGridPositions :: Array SimNode -> Int -> Array SimNode
recalculateGridPositions nodes packageCount =
  let
    cfg = mkGridConfig packageCount
    updateNode node =
      let { x: gx, y: gy } = gridPosForIndex cfg (nodeGridIndex node)
      in node { gridX = gx, gridY = gy, x = gx, y = gy }
  in
    map updateNode nodes

-- | Calculate grid positions for scene transitions
-- | Returns PositionMap for use with DumbEngine interpolation
calculateGridPositions :: Array SimNode -> Object { x :: Number, y :: Number }
calculateGridPositions nodes =
  let
    packageCount = Array.length $ Array.filter (\n -> n.nodeType == PackageNode) nodes
    cfg = mkGridConfig packageCount
  in
    Object.fromFoldable $ map (\n -> Tuple (show n.id) (gridPosForIndex cfg (nodeGridIndex n))) nodes
