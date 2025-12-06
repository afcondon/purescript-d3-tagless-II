-- | DataViz.Layout.Adjacency.Layout
-- |
-- | Pure layout algorithms for adjacency matrix visualization.
-- | Computes cell positions and label placements from matrix data.
module DataViz.Layout.Adjacency.Layout
  ( layout
  , layoutWithConfig
  , LayoutConfig
  , defaultConfig
  , shortenName
  ) where

import Prelude

import Data.Array (length, mapWithIndex, (!!))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import DataViz.Layout.Adjacency.Types (AdjacencyMatrix, MatrixCell, MatrixLabel, MatrixLayout, CellPosition, LabelPosition)

-- | Configuration for matrix layout
type LayoutConfig =
  { cellSize :: Number       -- Size of each cell (square)
  , labelMargin :: Number    -- Space between labels and grid
  , labelWidth :: Number     -- Reserved width for row labels
  , labelHeight :: Number    -- Reserved height for column labels
  }

-- | Default layout configuration
defaultConfig :: LayoutConfig
defaultConfig =
  { cellSize: 20.0
  , labelMargin: 8.0
  , labelWidth: 100.0
  , labelHeight: 80.0
  }

-- | Layout a matrix with default configuration
layout :: AdjacencyMatrix -> MatrixLayout
layout = layoutWithConfig defaultConfig

-- | Layout a matrix with custom configuration
layoutWithConfig :: LayoutConfig -> AdjacencyMatrix -> MatrixLayout
layoutWithConfig config { matrix, names } =
  let
    n = length names

    -- Compute grid dimensions
    gridWidth = config.cellSize * (toNumber n)
    gridHeight = config.cellSize * (toNumber n)

    -- Total dimensions including labels
    totalWidth = config.labelWidth + config.labelMargin + gridWidth
    totalHeight = config.labelHeight + config.labelMargin + gridHeight

    -- Offset for the grid (after labels)
    gridOffsetX = config.labelWidth + config.labelMargin
    gridOffsetY = config.labelHeight + config.labelMargin

    -- Build cells
    cells = buildCells config gridOffsetX gridOffsetY matrix names

    -- Build labels
    rowLabels = buildRowLabels config gridOffsetY names
    colLabels = buildColLabels config gridOffsetX names
  in
    { cells
    , rowLabels
    , colLabels
    , gridWidth
    , gridHeight
    , totalWidth
    , totalHeight
    }

-- | Build all cells with positions
buildCells :: LayoutConfig -> Number -> Number -> Array (Array Number) -> Array String -> Array MatrixCell
buildCells config offsetX offsetY matrix names =
  Array.concat $ mapWithIndex buildRow matrix
  where
  buildRow :: Int -> Array Number -> Array MatrixCell
  buildRow rowIdx row = mapWithIndex (buildCell rowIdx) row

  buildCell :: Int -> Int -> Number -> MatrixCell
  buildCell rowIdx colIdx value =
    let
      position = cellPosition config offsetX offsetY rowIdx colIdx
      rowName = fromMaybe "" $ names !! rowIdx
      colName = fromMaybe "" $ names !! colIdx
    in
      { row: rowIdx
      , col: colIdx
      , value
      , rowName
      , colName
      , position
      }

-- | Compute position for a single cell
cellPosition :: LayoutConfig -> Number -> Number -> Int -> Int -> CellPosition
cellPosition config offsetX offsetY rowIdx colIdx =
  { x: offsetX + (toNumber colIdx) * config.cellSize
  , y: offsetY + (toNumber rowIdx) * config.cellSize
  , width: config.cellSize
  , height: config.cellSize
  }

-- | Build row labels (on the left side)
buildRowLabels :: LayoutConfig -> Number -> Array String -> Array MatrixLabel
buildRowLabels config gridOffsetY names = mapWithIndex buildLabel names
  where
  buildLabel :: Int -> String -> MatrixLabel
  buildLabel idx name =
    let
      -- Position label at the center of its row, right-aligned
      y = gridOffsetY + (toNumber idx + 0.5) * config.cellSize
      x = config.labelWidth  -- Right edge of label area
      position :: LabelPosition
      position = { x, y, anchor: "end", rotation: 0.0 }
    in
      { index: idx
      , name
      , displayName: shortenName name
      , isRow: true
      , position
      }

-- | Build column labels (on the top)
buildColLabels :: LayoutConfig -> Number -> Array String -> Array MatrixLabel
buildColLabels config gridOffsetX names = mapWithIndex buildLabel names
  where
  buildLabel :: Int -> String -> MatrixLabel
  buildLabel idx name =
    let
      -- Position label at the center of its column, rotated
      x = gridOffsetX + (toNumber idx + 0.5) * config.cellSize
      y = config.labelHeight  -- Bottom of label area
      -- Rotate -45 degrees for readability
      position :: LabelPosition
      position = { x, y, anchor: "start", rotation: -45.0 }
    in
      { index: idx
      , name
      , displayName: shortenName name
      , isRow: false
      , position
      }

-- | Shorten a module name for display (last component only)
shortenName :: String -> String
shortenName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just lastPart -> lastPart
    Nothing -> name
