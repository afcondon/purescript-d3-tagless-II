module Component.ChartBuilder.Data where

import Prelude

import Component.ChartBuilder.Types (Dataset, DatasetId(..), DataPoint)
import Data.Array as Array
import Data.Maybe (Maybe)

-- =============================================================================
-- All Datasets
-- =============================================================================

allDatasets :: Array Dataset
allDatasets =
  [ fruitSalesDataset
  , monthlyTempDataset
  , testScoresDataset
  , progLangsDataset
  ]

-- | Get dataset by ID
getDataset :: DatasetId -> Maybe Dataset
getDataset id = Array.find (\d -> d.id == id) allDatasets

-- =============================================================================
-- Dataset Definitions
-- =============================================================================

fruitSalesDataset :: Dataset
fruitSalesDataset =
  { id: FruitSales
  , name: "Fruit Sales"
  , description: "Weekly fruit sales in dozens"
  , color: "#22c55e"
  , data:
      [ { category: "Apples", value: 42.0 }
      , { category: "Oranges", value: 67.0 }
      , { category: "Bananas", value: 89.0 }
      , { category: "Grapes", value: 34.0 }
      , { category: "Berries", value: 56.0 }
      ]
  }

monthlyTempDataset :: Dataset
monthlyTempDataset =
  { id: MonthlyTemp
  , name: "Monthly Temperature"
  , description: "Average temperature (°F) by month"
  , color: "#ef4444"
  , data:
      [ { category: "Jan", value: 32.0 }
      , { category: "Feb", value: 35.0 }
      , { category: "Mar", value: 45.0 }
      , { category: "Apr", value: 58.0 }
      , { category: "May", value: 68.0 }
      , { category: "Jun", value: 77.0 }
      , { category: "Jul", value: 82.0 }
      , { category: "Aug", value: 80.0 }
      , { category: "Sep", value: 72.0 }
      , { category: "Oct", value: 60.0 }
      , { category: "Nov", value: 47.0 }
      , { category: "Dec", value: 35.0 }
      ]
  }

testScoresDataset :: Dataset
testScoresDataset =
  { id: TestScores
  , name: "Test Scores"
  , description: "Student test scores (percentage)"
  , color: "#3b82f6"
  , data:
      [ { category: "Alice", value: 85.0 }
      , { category: "Bob", value: 92.0 }
      , { category: "Carol", value: 78.0 }
      , { category: "David", value: 88.0 }
      , { category: "Eve", value: 95.0 }
      ]
  }

progLangsDataset :: Dataset
progLangsDataset =
  { id: ProgLangs
  , name: "Programming Languages"
  , description: "Subjective awesomeness ratings"
  , color: "#8b5cf6"
  , data:
      [ { category: "PureScript", value: 99.0 }
      , { category: "Haskell", value: 95.0 }
      , { category: "Elm", value: 87.0 }
      , { category: "TypeScript", value: 72.0 }
      , { category: "JavaScript", value: 42.0 }
      ]
  }
