module PSD3.Wizard.Datasets where

import Prelude

import Data.Array (filter)

-- | Field definition for dataset type inference
type FieldDef =
  { name :: String
  , fieldType :: String
  }

-- | Dataset metadata
type Dataset =
  { id :: String
  , name :: String
  , description :: String
  , difficulty :: Difficulty
  , fields :: Array FieldDef
  , dataPreview :: Array String  -- First few data points as strings for display
  , suggestedViz :: String
  , educationalNote :: String
  }

data Difficulty
  = Beginner
  | Intermediate
  | Advanced

derive instance eqDifficulty :: Eq Difficulty

instance showDifficulty :: Show Difficulty where
  show Beginner = "Beginner"
  show Intermediate = "Intermediate"
  show Advanced = "Advanced"

-- | Registry of all available example datasets
allDatasets :: Array Dataset
allDatasets =
  [ scatterDataset
  , sineWaveDataset
  , barChartDataset
  , anscombeDataset
  , groupedBarDataset
  , multiLineDataset
  ]

-- | Simple 2D scatter plot data
scatterDataset :: Dataset
scatterDataset =
  { id: "scatter"
  , name: "Scatter Plot"
  , description: "Random points in 2D space - perfect for your first visualization"
  , difficulty: Beginner
  , fields:
      [ { name: "x", fieldType: "Number" }
      , { name: "y", fieldType: "Number" }
      ]
  , dataPreview:
      [ "{ x: 10.0, y: 20.0 }"
      , "{ x: 25.0, y: 45.0 }"
      , "{ x: 40.0, y: 30.0 }"
      , "..."
      ]
  , suggestedViz: "Scatter plot with circles"
  , educationalNote: "Start here! Simple 2D data is the easiest way to learn PSD3's data binding patterns."
  }

-- | Sine wave data for line charts
sineWaveDataset :: Dataset
sineWaveDataset =
  { id: "sinewave"
  , name: "Sine Wave"
  , description: "Smooth wave pattern - great for line charts and transitions"
  , difficulty: Beginner
  , fields:
      [ { name: "x", fieldType: "Number" }
      , { name: "y", fieldType: "Number" }
      ]
  , dataPreview:
      [ "{ x: 0.0, y: 100.0 }"
      , "{ x: 5.0, y: 125.0 }"
      , "{ x: 10.0, y: 145.0 }"
      , "..."
      ]
  , suggestedViz: "Line chart or area chart"
  , educationalNote: "Demonstrates smooth continuous data - perfect for learning line generators and paths."
  }

-- | Monthly sales data for bar charts
barChartDataset :: Dataset
barChartDataset =
  { id: "barchart"
  , name: "Monthly Sales"
  , description: "12 months of sales data - classic bar chart material"
  , difficulty: Beginner
  , fields:
      [ { name: "x", fieldType: "Number" }
      , { name: "y", fieldType: "Number" }
      ]
  , dataPreview:
      [ "{ x: 0.0, y: 30.0 }"
      , "{ x: 1.0, y: 45.0 }"
      , "{ x: 2.0, y: 60.0 }"
      , "..."
      ]
  , suggestedViz: "Bar chart or column chart"
  , educationalNote: "Perfect for learning data joins - each bar represents one data point."
  }

-- | Anscombe's Quartet - famous statistical dataset
anscombeDataset :: Dataset
anscombeDataset =
  { id: "anscombe"
  , name: "Anscombe's Quartet"
  , description: "Four datasets with identical statistics but very different patterns"
  , difficulty: Beginner
  , fields:
      [ { name: "x", fieldType: "Number" }
      , { name: "y", fieldType: "Number" }
      ]
  , dataPreview:
      [ "-- Dataset 1:"
      , "{ x: 10.0, y: 8.04 }"
      , "{ x: 8.0, y: 6.95 }"
      , "..."
      ]
  , suggestedViz: "Four small multiples scatter plots"
  , educationalNote: "Famous dataset showing why visualization matters - all four have identical mean, variance, and correlation, but look completely different when plotted!"
  }

-- | Grouped bar chart data (state demographics)
groupedBarDataset :: Dataset
groupedBarDataset =
  { id: "groupedbar"
  , name: "State Demographics"
  , description: "Population by state and age group - for grouped/stacked visualizations"
  , difficulty: Intermediate
  , fields:
      [ { name: "state", fieldType: "String" }
      , { name: "age", fieldType: "String" }
      , { name: "population", fieldType: "Number" }
      ]
  , dataPreview:
      [ "{ state: \"CA\", age: \"<10\", population: 5038433.0 }"
      , "{ state: \"CA\", age: \"10-19\", population: 5055706.0 }"
      , "{ state: \"TX\", age: \"<10\", population: 4131018.0 }"
      , "..."
      ]
  , suggestedViz: "Grouped bar chart or stacked bar chart"
  , educationalNote: "Multi-dimensional data with categorical and numerical fields - great for learning data nesting and grouping patterns."
  }

-- | Multi-line chart data (time series)
multiLineDataset :: Dataset
multiLineDataset =
  { id: "multiline"
  , name: "Unemployment Rates"
  , description: "Unemployment rates for 4 cities over time - multi-series data"
  , difficulty: Intermediate
  , fields:
      [ { name: "series", fieldType: "String" }
      , { name: "date", fieldType: "String" }
      , { name: "value", fieldType: "Number" }
      ]
  , dataPreview:
      [ "{ series: \"San Francisco\", date: \"2000-01\", value: 3.5 }"
      , "{ series: \"San Francisco\", date: \"2002-01\", value: 5.2 }"
      , "{ series: \"New York\", date: \"2000-01\", value: 5.3 }"
      , "..."
      ]
  , suggestedViz: "Multi-line chart with legend"
  , educationalNote: "Time-series data with multiple series - demonstrates data grouping by series and line generators."
  }

-- | Find dataset by ID
findDataset :: String -> Array Dataset -> Array Dataset
findDataset id = filter (\d -> d.id == id)

-- | Get datasets by difficulty
getByDifficulty :: Difficulty -> Array Dataset -> Array Dataset
getByDifficulty difficulty = filter (\d -> d.difficulty == difficulty)
