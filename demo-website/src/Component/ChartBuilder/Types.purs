module Component.ChartBuilder.Types where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import EmmetParser.Types (ParseError, ValidationError)
import TreeBuilder3.Types (TreeNode)
import Data.Tree (Tree)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Eq.Generic (genericEq)

-- =============================================================================
-- Chart Types
-- =============================================================================

data ChartType
  = BarChart
  | LineChart
  | ScatterChart

derive instance genericChartType :: Generic ChartType _
instance showChartType :: Show ChartType where
  show = genericShow

instance eqChartType :: Eq ChartType where
  eq = genericEq

chartTypeLabel :: ChartType -> String
chartTypeLabel = case _ of
  BarChart -> "Bar Chart"
  LineChart -> "Line Chart"
  ScatterChart -> "Scatter Chart"

-- =============================================================================
-- Dataset Types
-- =============================================================================

data DatasetId
  = FruitSales
  | MonthlyTemp
  | TestScores
  | ProgLangs

derive instance genericDatasetId :: Generic DatasetId _
instance showDatasetId :: Show DatasetId where
  show = genericShow

instance eqDatasetId :: Eq DatasetId where
  eq = genericEq

datasetIdLabel :: DatasetId -> String
datasetIdLabel = case _ of
  FruitSales -> "Fruit Sales"
  MonthlyTemp -> "Monthly Temperature"
  TestScores -> "Test Scores"
  ProgLangs -> "Programming Languages"

-- | Parse dataset ID from string (for select dropdown)
parseDatasetId :: String -> DatasetId
parseDatasetId = case _ of
  "FruitSales" -> FruitSales
  "MonthlyTemp" -> MonthlyTemp
  "TestScores" -> TestScores
  "ProgLangs" -> ProgLangs
  _ -> FruitSales  -- Default fallback

-- | Data point with category and value
type DataPoint =
  { category :: String
  , value :: Number
  }

-- | Dataset with metadata and data
type Dataset =
  { id :: DatasetId
  , name :: String
  , description :: String
  , data :: Array DataPoint
  , color :: String  -- Hex color for visual distinction
  }

-- =============================================================================
-- Component State
-- =============================================================================

type State =
  { emmetInput :: String
  , selectedChartType :: ChartType
  , selectedDataset :: DatasetId
  , parseResult :: Maybe ParseResult
  , showRecipe :: Boolean
  }

type ParseResult = Either ParseError (Either ValidationError (Tree TreeNode))

-- =============================================================================
-- Component Actions
-- =============================================================================

data Action
  = Initialize
  | UpdateEmmet String
  | SelectChartType ChartType
  | SelectDataset DatasetId
  | CopyRecipe
  | LoadRecipe ChartType
  | ToggleRecipe
  | ResetToRecipe
