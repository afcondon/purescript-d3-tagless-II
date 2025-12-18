-- | VizMatrix Data
-- |
-- | Sample datasets for the VizMatrix demo.
module VizMatrix.Data
  ( getDataset
  , chessBoard
  , goBoard
  , sudokuBoard
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import VizMatrix.Types (BoardSquare, DatasetId(..))

-- =============================================================================
-- Dataset Lookup
-- =============================================================================

-- | Get the dataset for a given identifier
getDataset :: DatasetId -> Array BoardSquare
getDataset = case _ of
  DataChess -> chessBoard
  DataGo -> goBoard
  DataSudoku -> sudokuBoard

-- =============================================================================
-- Chess Board
-- =============================================================================

-- | Starting position chess board (8×8)
chessBoard :: Array BoardSquare
chessBoard = do
  row <- Array.range 0 7
  col <- Array.range 0 7
  pure
    { row
    , col
    , value: chessPiece row col
    , color: Just $ if (row + col) `mod` 2 == 0 then "#F0D9B5" else "#B58863"
    }

chessPiece :: Int -> Int -> String
chessPiece row col = case row of
  0 -> case col of
    0 -> "♜"
    1 -> "♞"
    2 -> "♝"
    3 -> "♛"
    4 -> "♚"
    5 -> "♝"
    6 -> "♞"
    7 -> "♜"
    _ -> ""
  1 -> "♟"
  6 -> "♙"
  7 -> case col of
    0 -> "♖"
    1 -> "♘"
    2 -> "♗"
    3 -> "♕"
    4 -> "♔"
    5 -> "♗"
    6 -> "♘"
    7 -> "♖"
    _ -> ""
  _ -> ""

-- =============================================================================
-- Go Board
-- =============================================================================

-- | Go board with some stones (9×9 for simplicity)
goBoard :: Array BoardSquare
goBoard = do
  row <- Array.range 0 8
  col <- Array.range 0 8
  pure
    { row
    , col
    , value: goStone row col
    , color: Just "#DCB35C"  -- Wood color
    }

-- | A simple go position (joseki corner pattern)
goStone :: Int -> Int -> String
goStone row col = case row, col of
  -- Top-right corner joseki
  2, 6 -> "⚫"
  2, 7 -> "⚪"
  3, 6 -> "⚪"
  3, 7 -> "⚫"
  4, 6 -> "⚫"
  -- Center star point with stone
  4, 4 -> "⚫"
  -- Bottom-left corner
  6, 2 -> "⚪"
  7, 2 -> "⚫"
  7, 3 -> "⚪"
  -- Star points (just dots for empty intersections at star points)
  2, 2 -> "·"
  2, 4 -> "·"
  4, 2 -> "·"
  6, 4 -> "·"
  6, 6 -> "·"
  _, _ -> ""

-- =============================================================================
-- Sudoku Board
-- =============================================================================

-- | A sudoku puzzle (9×9)
sudokuBoard :: Array BoardSquare
sudokuBoard = do
  row <- Array.range 0 8
  col <- Array.range 0 8
  pure
    { row
    , col
    , value: sudokuCell row col
    , color: Just $ sudokuColor row col
    }

-- | Sudoku cell value (a classic puzzle with some givens)
sudokuCell :: Int -> Int -> String
sudokuCell row col = case row, col of
  -- Row 0
  0, 0 -> "5"
  0, 1 -> "3"
  0, 4 -> "7"
  -- Row 1
  1, 0 -> "6"
  1, 3 -> "1"
  1, 4 -> "9"
  1, 5 -> "5"
  -- Row 2
  2, 1 -> "9"
  2, 2 -> "8"
  2, 7 -> "6"
  -- Row 3
  3, 0 -> "8"
  3, 4 -> "6"
  3, 8 -> "3"
  -- Row 4
  4, 0 -> "4"
  4, 3 -> "8"
  4, 5 -> "3"
  4, 8 -> "1"
  -- Row 5
  5, 0 -> "7"
  5, 4 -> "2"
  5, 8 -> "6"
  -- Row 6
  6, 1 -> "6"
  6, 6 -> "2"
  6, 7 -> "8"
  -- Row 7
  7, 3 -> "4"
  7, 4 -> "1"
  7, 5 -> "9"
  7, 8 -> "5"
  -- Row 8
  8, 4 -> "8"
  8, 7 -> "7"
  8, 8 -> "9"
  _, _ -> ""

-- | Sudoku background color (alternating 3×3 boxes)
sudokuColor :: Int -> Int -> String
sudokuColor row col =
  let
    boxRow = row / 3
    boxCol = col / 3
    isAlternate = (boxRow + boxCol) `mod` 2 == 0
  in
    if isAlternate then "#E8E8E8" else "#FFFFFF"
