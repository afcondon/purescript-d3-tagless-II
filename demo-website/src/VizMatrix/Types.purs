-- | VizMatrix Types
-- |
-- | Defines the types, AST identifiers, and dataset identifiers for the
-- | VizMatrix demo - a showcase of Type × AST × Data combinations.
module VizMatrix.Types
  ( -- * Data Types (LHS cards)
    DataTypeId(..)
  , dataTypeLabel
  , dataTypeDescription
  , allDataTypes
    -- * AST Types (TOP cards)
  , AstId(..)
  , astLabel
  , astDescription
  , allAsts
    -- * Dataset Identifiers (RHS cards)
  , DatasetId(..)
  , datasetLabel
  , datasetDescription
  , allDatasets
    -- * Compatibility
  , isCompatible
  , compatibleAsts
  , compatibleDatasets
    -- * Board Square (for Board Games family)
  , BoardSquare
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

-- =============================================================================
-- Data Types (LHS cards)
-- =============================================================================

-- | Identifiers for data types
data DataTypeId
  = TypeBoardSquare  -- { row, col, value }
  -- Future: TypeGapminderRow, TypeHierNode, TypeLetter

derive instance eqDataTypeId :: Eq DataTypeId
derive instance ordDataTypeId :: Ord DataTypeId

dataTypeLabel :: DataTypeId -> String
dataTypeLabel = case _ of
  TypeBoardSquare -> "BoardSquare"

dataTypeDescription :: DataTypeId -> String
dataTypeDescription = case _ of
  TypeBoardSquare -> "{ row, col, value }"

allDataTypes :: Array DataTypeId
allDataTypes = [ TypeBoardSquare ]

-- =============================================================================
-- AST Types (TOP cards)
-- =============================================================================

-- | Identifiers for AST structures
data AstId
  = AstGrid  -- Grid layout for board games
  -- Future: AstScatter, AstBubble, AstTree, AstPack, AstTreemap, AstUpdateJoin

derive instance eqAstId :: Eq AstId
derive instance ordAstId :: Ord AstId

astLabel :: AstId -> String
astLabel = case _ of
  AstGrid -> "Grid"

astDescription :: AstId -> String
astDescription = case _ of
  AstGrid -> "Row × Column layout"

allAsts :: Array AstId
allAsts = [ AstGrid ]

-- =============================================================================
-- Dataset Identifiers (RHS cards)
-- =============================================================================

-- | Identifiers for datasets
data DatasetId
  = DataChess      -- 8×8 chess board
  | DataGo         -- 9×9 Go board (simplified)
  | DataSudoku     -- 9×9 Sudoku puzzle
  -- Future: DataGapminder2007, DataFlareTree, DataFlarePack, etc.

derive instance eqDatasetId :: Eq DatasetId
derive instance ordDatasetId :: Ord DatasetId

datasetLabel :: DatasetId -> String
datasetLabel = case _ of
  DataChess -> "Chess"
  DataGo -> "Go"
  DataSudoku -> "Sudoku"

datasetDescription :: DatasetId -> String
datasetDescription = case _ of
  DataChess -> "8×8 board"
  DataGo -> "9×9 board"
  DataSudoku -> "9×9 puzzle"

allDatasets :: Array DatasetId
allDatasets = [ DataChess, DataGo, DataSudoku ]

-- =============================================================================
-- Compatibility Matrix
-- =============================================================================

-- | Check if a type, AST, and dataset combination is compatible
isCompatible :: DataTypeId -> AstId -> DatasetId -> Boolean
isCompatible typeId astId datasetId =
  Array.elem astId (compatibleAsts typeId) &&
  Array.elem datasetId (compatibleDatasets typeId)

-- | Get ASTs compatible with a data type
compatibleAsts :: DataTypeId -> Array AstId
compatibleAsts = case _ of
  TypeBoardSquare -> [ AstGrid ]

-- | Get datasets compatible with a data type
compatibleDatasets :: DataTypeId -> Array DatasetId
compatibleDatasets = case _ of
  TypeBoardSquare -> [ DataChess, DataGo, DataSudoku ]

-- =============================================================================
-- Concrete Data Types
-- =============================================================================

-- | A square on a game board
type BoardSquare =
  { row :: Int
  , col :: Int
  , value :: String  -- The content (piece, number, stone)
  , color :: Maybe String  -- Optional background color
  }
