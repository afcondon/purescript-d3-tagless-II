-- | DataViz.Layout.Adjacency.Types
-- |
-- | Types for adjacency matrix visualization.
-- | An adjacency matrix shows relationships between nodes as a grid
-- | where each cell represents a connection between row and column nodes.
module DataViz.Layout.Adjacency.Types
  ( AdjacencyMatrix(..)
  , MatrixCell(..)
  , MatrixLabel(..)
  , MatrixLayout(..)
  , CellPosition
  , LabelPosition
  ) where

import Prelude

-- | The adjacency matrix data structure
-- | Contains raw matrix values and associated node names
type AdjacencyMatrix =
  { matrix :: Array (Array Number)  -- NxN matrix of connection weights
  , names :: Array String           -- Node names for rows/columns
  }

-- | Position of a cell in the grid
type CellPosition =
  { x :: Number      -- Left edge x coordinate
  , y :: Number      -- Top edge y coordinate
  , width :: Number  -- Cell width
  , height :: Number -- Cell height
  }

-- | Position of a label
type LabelPosition =
  { x :: Number
  , y :: Number
  , anchor :: String     -- "start", "middle", or "end"
  , rotation :: Number   -- Rotation in degrees
  }

-- | A cell in the laid out matrix
-- | Contains both the data and computed position
type MatrixCell =
  { row :: Int           -- Row index
  , col :: Int           -- Column index
  , value :: Number      -- Connection weight (0 = no connection)
  , rowName :: String    -- Name of the row node
  , colName :: String    -- Name of the column node
  , position :: CellPosition
  }

-- | A label for row or column
type MatrixLabel =
  { index :: Int         -- Index in the names array
  , name :: String       -- Full node name
  , displayName :: String -- Shortened name for display
  , isRow :: Boolean     -- True for row labels, false for column labels
  , position :: LabelPosition
  }

-- | Complete layout output ready for rendering
type MatrixLayout =
  { cells :: Array MatrixCell      -- All cells with positions
  , rowLabels :: Array MatrixLabel -- Labels on the left
  , colLabels :: Array MatrixLabel -- Labels on the top
  , gridWidth :: Number            -- Total width of the cell grid
  , gridHeight :: Number           -- Total height of the cell grid
  , totalWidth :: Number           -- Including labels
  , totalHeight :: Number          -- Including labels
  }
