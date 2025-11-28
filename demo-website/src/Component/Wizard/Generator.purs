module PSD3.Wizard.Generator where

import Prelude

import Data.Array (intercalate)
import Data.String (Pattern(..), replaceAll)
import PSD3.Wizard.Datasets (Dataset, FieldDef)
import PSD3.Wizard.Templates as Templates

-- | Generated file with name and content
type GeneratedFile =
  { filename :: String
  , content :: String
  }

-- | Configuration for code generation
type GeneratorConfig =
  { vizName :: String       -- e.g., "MyViz"
  , dataset :: Dataset      -- Selected dataset with fields
  , includeMain :: Boolean  -- Whether to generate Main.purs
  }

-- | Generate all files for a visualization project
generateProject :: GeneratorConfig -> Array GeneratedFile
generateProject config =
  let
    dataTypeName = config.vizName <> "Data"
    coerceFnName = "coerceTo" <> dataTypeName
    exampleData = generateExampleData config.dataset

    baseFiles =
      [ { filename: config.vizName <> "/Unsafe.purs"
        , content: Templates.generateUnsafe config.vizName dataTypeName config.dataset.fields
        }
      , { filename: config.vizName <> "/Model.purs"
        , content: Templates.generateModel config.vizName dataTypeName config.dataset.fields exampleData
        }
      , { filename: config.vizName <> "/Draw.purs"
        , content: Templates.generateDraw config.vizName dataTypeName config.dataset.fields coerceFnName
        }
      , { filename: config.vizName <> "/index.html"
        , content: Templates.generateHTML config.vizName
        }
      , { filename: config.vizName <> "/README.md"
        , content: Templates.generateREADME config.vizName dataTypeName config.dataset.fields
        }
      ]

    mainFile =
      if config.includeMain
      then [ { filename: "Main.purs"
             , content: Templates.generateMain config.vizName dataTypeName
             }
           ]
      else []
  in
    baseFiles <> mainFile

-- | Generate example data array as PureScript code string
-- | This creates realistic data based on the dataset's preview
generateExampleData :: Dataset -> String
generateExampleData dataset = case dataset.id of
  "scatter" -> scatterData
  "sinewave" -> sineWaveData
  "barchart" -> barChartData
  "anscombe" -> anscombeData
  "groupedbar" -> groupedBarData
  "multiline" -> multiLineData
  _ -> fallbackData dataset.fields

-- | Scatter plot data (random points)
scatterData :: String
scatterData = """[ { x: 10.0, y: 20.0 }
  , { x: 25.0, y: 45.0 }
  , { x: 40.0, y: 30.0 }
  , { x: 55.0, y: 60.0 }
  , { x: 70.0, y: 40.0 }
  , { x: 85.0, y: 70.0 }
  , { x: 100.0, y: 55.0 }
  , { x: 115.0, y: 80.0 }
  , { x: 130.0, y: 65.0 }
  , { x: 145.0, y: 90.0 }
  ]"""

-- | Sine wave data (smooth wave pattern)
sineWaveData :: String
sineWaveData = """[ { x: 0.0, y: 100.0 }
  , { x: 5.0, y: 125.0 }
  , { x: 10.0, y: 145.0 }
  , { x: 15.0, y: 159.0 }
  , { x: 20.0, y: 165.0 }
  , { x: 25.0, y: 159.0 }
  , { x: 30.0, y: 145.0 }
  , { x: 35.0, y: 125.0 }
  , { x: 40.0, y: 100.0 }
  , { x: 45.0, y: 75.0 }
  , { x: 50.0, y: 55.0 }
  , { x: 55.0, y: 41.0 }
  , { x: 60.0, y: 35.0 }
  , { x: 65.0, y: 41.0 }
  , { x: 70.0, y: 55.0 }
  , { x: 75.0, y: 75.0 }
  , { x: 80.0, y: 100.0 }
  ]"""

-- | Bar chart data (monthly values)
barChartData :: String
barChartData = """[ { x: 0.0, y: 30.0 }
  , { x: 1.0, y: 45.0 }
  , { x: 2.0, y: 60.0 }
  , { x: 3.0, y: 55.0 }
  , { x: 4.0, y: 70.0 }
  , { x: 5.0, y: 65.0 }
  , { x: 6.0, y: 80.0 }
  , { x: 7.0, y: 75.0 }
  , { x: 8.0, y: 90.0 }
  , { x: 9.0, y: 85.0 }
  , { x: 10.0, y: 95.0 }
  , { x: 11.0, y: 100.0 }
  ]"""

-- | Anscombe's Quartet (first dataset)
anscombeData :: String
anscombeData = """[ { x: 10.0, y: 8.04 }
  , { x: 8.0, y: 6.95 }
  , { x: 13.0, y: 7.58 }
  , { x: 9.0, y: 8.81 }
  , { x: 11.0, y: 8.33 }
  , { x: 14.0, y: 9.96 }
  , { x: 6.0, y: 7.24 }
  , { x: 4.0, y: 4.26 }
  , { x: 12.0, y: 10.84 }
  , { x: 7.0, y: 4.82 }
  , { x: 5.0, y: 5.68 }
  ]"""

-- | Grouped bar chart data (state demographics)
groupedBarData :: String
groupedBarData = """[ { state: "CA", age: "<10", population: 5038433.0 }
  , { state: "CA", age: "10-19", population: 5055706.0 }
  , { state: "CA", age: "20-29", population: 5310481.0 }
  , { state: "CA", age: "30-39", population: 5354112.0 }
  , { state: "TX", age: "<10", population: 4131018.0 }
  , { state: "TX", age: "10-19", population: 3770007.0 }
  , { state: "TX", age: "20-29", population: 3863019.0 }
  , { state: "TX", age: "30-39", population: 3696647.0 }
  , { state: "NY", age: "<10", population: 2275293.0 }
  , { state: "NY", age: "10-19", population: 2363801.0 }
  , { state: "NY", age: "20-29", population: 2885845.0 }
  , { state: "NY", age: "30-39", population: 2754048.0 }
  ]"""

-- | Multi-line chart data (unemployment rates)
multiLineData :: String
multiLineData = """[ { series: "San Francisco", date: "2000-01", value: 3.5 }
  , { series: "San Francisco", date: "2002-01", value: 5.2 }
  , { series: "San Francisco", date: "2004-01", value: 4.8 }
  , { series: "San Francisco", date: "2006-01", value: 3.9 }
  , { series: "San Francisco", date: "2008-01", value: 4.2 }
  , { series: "San Francisco", date: "2010-01", value: 8.4 }
  , { series: "San Francisco", date: "2012-01", value: 6.8 }
  , { series: "New York", date: "2000-01", value: 5.3 }
  , { series: "New York", date: "2002-01", value: 7.2 }
  , { series: "New York", date: "2004-01", value: 6.1 }
  , { series: "New York", date: "2006-01", value: 4.9 }
  , { series: "New York", date: "2008-01", value: 5.3 }
  , { series: "New York", date: "2010-01", value: 9.2 }
  , { series: "New York", date: "2012-01", value: 7.8 }
  , { series: "Austin", date: "2000-01", value: 2.9 }
  , { series: "Austin", date: "2002-01", value: 4.8 }
  , { series: "Austin", date: "2004-01", value: 4.2 }
  , { series: "Austin", date: "2006-01", value: 3.5 }
  , { series: "Austin", date: "2008-01", value: 3.8 }
  , { series: "Austin", date: "2010-01", value: 6.5 }
  , { series: "Austin", date: "2012-01", value: 5.1 }
  , { series: "Seattle", date: "2000-01", value: 4.1 }
  , { series: "Seattle", date: "2002-01", value: 6.3 }
  , { series: "Seattle", date: "2004-01", value: 5.5 }
  , { series: "Seattle", date: "2006-01", value: 4.2 }
  , { series: "Seattle", date: "2008-01", value: 4.5 }
  , { series: "Seattle", date: "2010-01", value: 7.8 }
  , { series: "Seattle", date: "2012-01", value: 6.2 }
  ]"""

-- | Generic fallback data generator based on field definitions
fallbackData :: Array FieldDef -> String
fallbackData fields =
  let
    sampleRow = "{ " <> intercalate ", " (map generateFieldValue fields) <> " }"
    rows = intercalate "\n  , " [sampleRow, sampleRow, sampleRow]
  in
    "[ " <> rows <> "\n  ]"
  where
    generateFieldValue :: FieldDef -> String
    generateFieldValue field = case field.fieldType of
      "Number" -> field.name <> ": 0.0"
      "String" -> field.name <> ": \"" <> field.name <> "\""
      "Int" -> field.name <> ": 0"
      _ -> field.name <> ": 0.0"
