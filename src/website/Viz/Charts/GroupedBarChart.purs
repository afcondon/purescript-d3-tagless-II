module D3.Viz.GroupedBarChart where

-- | Grouped Bar Chart
-- | Based on https://observablehq.com/@d3/grouped-bar-chart/2
-- | Copyright 2018–2020 Observable, Inc.
-- | ISC License

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, height, strokeColor, text, textAnchor, transform, viewBox, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (GroupedBarData)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import Data.Array (catMaybes, concat, filter, groupBy, nub, length, findIndex, index, take)
import Data.Array as Data.Array
import Data.Foldable (traverse_, foldr, maximum)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Effect.Class (class MonadEffect)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Foreign (Foreign, readString, readNumber)
import Foreign.Index (readProp)

-- Group data by state
groupByState :: Array GroupedBarData -> Array (Array GroupedBarData)
groupByState data' =
  let states = nub $ map _.state data'
      getStateData state' = filter (\d -> d.state == state') data'
  in map getStateData states

-- Get all unique states
getStates :: Array GroupedBarData -> Array String
getStates = nub <<< map _.state

-- Get all unique age groups
getAges :: Array GroupedBarData -> Array String
getAges = nub <<< map _.age

-- Parse CSV row from Foreign (wide format to long format)
-- Expected columns: State, Under 5 Years, 5 to 13 Years, 14 to 17 Years, 18 to 24 Years, 25 to 44 Years, 45 to 64 Years, 65 Years and Over
parseCSVRow :: Foreign -> Maybe (Array GroupedBarData)
parseCSVRow row = do
  stateF <- hush $ runExcept $ readProp "State" row
  state <- hush $ runExcept $ readString stateF

  -- Parse each age column
  let ageColumns =
        [ {name: "Under 5 Years", label: "Under 5"}
        , {name: "5 to 13 Years", label: "5 to 13"}
        , {name: "14 to 17 Years", label: "14 to 17"}
        , {name: "18 to 24 Years", label: "18 to 24"}
        , {name: "25 to 44 Years", label: "25 to 44"}
        , {name: "45 to 64 Years", label: "45 to 64"}
        , {name: "65 Years and Over", label: "65+"}
        ]

  let parseAge col = do
        valF <- hush $ runExcept $ readProp col.name row
        -- Try reading as number first, then as string
        population <- case hush $ runExcept $ readNumber valF of
          Just n -> Just n
          Nothing -> do
            str <- hush $ runExcept $ readString valF
            fromString str
        pure { state, age: col.label, population }

  pure $ catMaybes $ map parseAge ageColumns

-- Parse entire CSV and flatten to single array
parsePopulationCSV :: Array Foreign -> Array GroupedBarData
parsePopulationCSV rows = concat $ catMaybes $ map parseCSVRow rows

-- Draw grouped bar chart
-- Matches Observable implementation with 928x600 dimensions
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array GroupedBarData -> Selector D3Selection_ -> m Unit
draw data' selector = do
  let dims = { width: 928.0, height: 600.0, marginTop: 10.0, marginRight: 10.0, marginBottom: 20.0, marginLeft: 40.0 }
  let chartWidth = dims.width - dims.marginLeft - dims.marginRight
  let chartHeight = dims.height - dims.marginTop - dims.marginBottom

  -- Get dimensions for scales
  let states = getStates data'
  let ages = getAges data'
  let maxPop = fromMaybe 0.0 $ maximum $ map _.population data'

  -- Calculate scales with Observable's padding values
  -- fx scale: 0.1 padding between state groups
  -- x scale: 0.05 internal padding between age bars
  let stateCount = toNumber $ length states
  let ageCount = toNumber $ length ages
  let fxPadding = 0.1  -- Band scale padding between state groups
  let xPadding = 0.05  -- Band scale padding within age groups
  let stateWidth = chartWidth / (stateCount + fxPadding * (stateCount - 1.0))
  let barWidth = (stateWidth * (1.0 - fxPadding)) / (ageCount + xPadding * (ageCount - 1.0))
  let stateSpacing = stateWidth * (1.0 + fxPadding)

  -- Helper to get state x position (with band scale padding)
  let getStateX state' =
        fromMaybe 0.0 $ map (\i -> toNumber i * stateSpacing) $ findIndex (\s -> s == state') states

  -- Helper to get age x offset within state (with internal padding)
  let getAgeOffset age' =
        let barSpacing = barWidth * (1.0 + xPadding)
        in fromMaybe 0.0 $ map (\i -> toNumber i * barSpacing) $ findIndex (\a -> a == age') ages

  -- Color scale using d3.schemeSpectral (Observable's choice)
  -- These are the Spectral colors from ColorBrewer
  let ageColors =
        [ "#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b"
        , "#e6f598", "#abdda4", "#66c2a5", "#3288bd"
        ]
  let getAgeColor age' =
        fromMaybe "#999999" $ do
          i <- findIndex (\a -> a == age') ages
          index ageColors i

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      classed "grouped-bar-chart"
    , width dims.width
    , height dims.height
    , viewBox 0.0 0.0 dims.width dims.height
    ]

  chartGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show dims.marginLeft <> "," <> show dims.marginTop <> ")" ]
    , classed "chart"
    ]

  -- Draw bars
  let drawBar :: GroupedBarData -> m Unit
      drawBar d = do
        let barHeight = (d.population / maxPop) * chartHeight
        let barX = getStateX d.state + getAgeOffset d.age
        let barY = chartHeight - barHeight
        _ <- appendTo chartGroup Rect [
            x barX
          , y barY
          , width barWidth
          , height barHeight
          , fill (getAgeColor d.age)
          , strokeColor "white"
          , classed "bar"
          ]
        pure unit

  _ <- traverse_ drawBar data'

  -- Draw x-axis (state labels)
  xAxis <- appendTo chartGroup Group [
      transform [ \_ -> "translate(0," <> show chartHeight <> ")" ]
    , classed "x-axis"
    ]

  let drawStateLabel :: String -> m Unit
      drawStateLabel state' = do
        let labelX = getStateX state' + stateWidth / 2.0
        _ <- appendTo xAxis Text [
            x labelX
          , y 20.0
          , text state'
          , textAnchor "middle"
          , classed "axis-label"
          ]
        pure unit

  _ <- traverse_ drawStateLabel states

  -- Draw y-axis
  yAxis <- appendTo chartGroup Group [
      classed "y-axis"
    ]

  -- Add a few y-axis ticks
  let yTicks = [0.0, maxPop / 4.0, maxPop / 2.0, (3.0 * maxPop) / 4.0, maxPop]
  let drawYTick :: Number -> m Unit
      drawYTick value = do
        let tickY = chartHeight - (value / maxPop) * chartHeight
        _ <- appendTo yAxis Text [
            x (-10.0)
          , y (tickY + 4.0)
          , text (formatSI value)
          , textAnchor "end"
          , classed "axis-label"
          ]
        pure unit

  _ <- traverse_ drawYTick yTicks

  pure unit

-- Format large numbers with SI prefix (millions)
formatSI :: Number -> String
formatSI n =
  if n >= 1000000.0
    then toString (n / 1000000.0) <> "M"
    else toString n
