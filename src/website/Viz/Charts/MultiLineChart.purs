module D3.Viz.MultiLineChart where

-- | Multi-Line Chart
-- | Based on https://observablehq.com/@d3/multi-line-chart/2
-- | Copyright 2018–2023 Observable, Inc.
-- | ISC License

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, d, fill, height, strokeColor, strokeWidth, text, textAnchor, transform, viewBox, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (MultiLineData)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import Data.Array (filter, findIndex, head, length, mapMaybe, nub, sortBy)
import Data.Array as Data.Array
import Data.Foldable (maximum, minimum, traverse_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String.CodeUnits as Data.String.CodeUnits
import Effect.Class (class MonadEffect)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Foreign (Foreign, readString)
import Foreign.Index (readProp)

-- Group data by series
groupBySeries :: Array MultiLineData -> Array (Array MultiLineData)
groupBySeries data' =
  let series = nub $ map _.series data'
      getSeriesData s = filter (\d -> d.series == s) data'
  in map getSeriesData series

-- Get all unique series names
getSeries :: Array MultiLineData -> Array String
getSeries = nub <<< map _.series

-- Get all unique dates
getDates :: Array MultiLineData -> Array String
getDates = nub <<< map _.date

-- Extract year from date string (YYYY-MM-DD format)
getYear :: String -> Maybe String
getYear dateStr =
  case Data.String.CodeUnits.take 4 dateStr of
    year | Data.String.CodeUnits.length year == 4 -> Just year
    _ -> Nothing

-- | Parse CSV row from Foreign
-- | Expected format: { division: String, date: String, unemployment: String }
parseCSVRow :: Foreign -> Maybe MultiLineData
parseCSVRow row = do
  divisionF <- hush $ runExcept $ readProp "division" row
  division <- hush $ runExcept $ readString divisionF
  dateF <- hush $ runExcept $ readProp "date" row
  date <- hush $ runExcept $ readString dateF
  unemploymentF <- hush $ runExcept $ readProp "unemployment" row
  unemploymentStr <- hush $ runExcept $ readString unemploymentF
  unemployment <- fromString unemploymentStr
  pure { series: division, date, value: unemployment }

-- | Draw multi-line chart from CSV data
drawFromCSV :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array Foreign -> Selector D3Selection_ -> m Unit
drawFromCSV csvData selector =
  draw (mapMaybe parseCSVRow csvData) selector

-- Create SVG path data for a line
linePath :: Number -> Number -> Number -> Number -> Array MultiLineData -> String
linePath chartWidth chartHeight maxValue minValue points =
  let dateCount = toNumber $ length points
      xScale i = (toNumber i) * (chartWidth / (dateCount - 1.0))
      yScale v = chartHeight - ((v - minValue) / (maxValue - minValue)) * chartHeight

      pointToString i pt =
        let xPos = xScale i
            yPos = yScale pt.value
        in if i == 0
           then "M " <> toString xPos <> " " <> toString yPos
           else " L " <> toString xPos <> " " <> toString yPos

  in case points of
       [] -> ""
       arr -> foldlWithIndex (\i acc pt -> acc <> pointToString i pt) "" arr


-- Draw multi-line chart
-- Matches Observable implementation with 928x600 dimensions
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array MultiLineData -> Selector D3Selection_ -> m Unit
draw data' selector = do
  let dims = { width: 928.0, height: 600.0, marginTop: 20.0, marginRight: 20.0, marginBottom: 30.0, marginLeft: 30.0 }
  let chartWidth = dims.width - dims.marginLeft - dims.marginRight
  let chartHeight = dims.height - dims.marginTop - dims.marginBottom

  -- Get data ranges
  let allValues = map _.value data'
  let maxValue = fromMaybe 0.0 $ maximum allValues
  let minValue = fromMaybe 0.0 $ minimum allValues

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      classed "multi-line-chart"
    , width dims.width
    , height dims.height
    , viewBox 0.0 0.0 dims.width dims.height
    ]

  chartGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show dims.marginLeft <> "," <> show dims.marginTop <> ")" ]
    , classed "chart"
    ]

  -- Draw lines for each series
  -- Observable style: all lines steelblue with mix-blend-mode: multiply
  let drawLine :: Array MultiLineData -> m Unit
      drawLine points = case head points of
        Nothing -> pure unit
        Just firstPoint -> do
          let pathData = linePath chartWidth chartHeight maxValue minValue points
          let seriesName = firstPoint.series
          _ <- appendTo chartGroup Path [
              d pathData
            , strokeColor "#4682b4"  -- steelblue for all lines
            , strokeWidth 1.5
            , fill "none"
            , classed ("line series-" <> seriesName)  -- Add series class for hover
            ]
          pure unit

  let groupedData = groupBySeries data'
  _ <- traverse_ drawLine groupedData

  -- Draw y-axis
  yAxis <- appendTo chartGroup Group [
      classed "y-axis"
    ]

  let yTicks = [0.0, 5.0, 10.0, 15.0]
  let yScale v = chartHeight - ((v - minValue) / (maxValue - minValue)) * chartHeight
  let drawYTick :: Number -> m Unit
      drawYTick value = do
        let tickY = yScale value
        _ <- appendTo yAxis Text [
            x (-5.0)  -- Moved closer to avoid truncation
          , y (tickY + 4.0)
          , text (toString value <> "%")
          , textAnchor "end"
          , classed "axis-label"
          ]
        pure unit

  _ <- traverse_ drawYTick yTicks

  -- Draw x-axis (years)
  xAxis <- appendTo chartGroup Group [
      transform [ \_ -> "translate(0," <> show chartHeight <> ")" ]
    , classed "x-axis"
    ]

  -- Get unique years from data for x-axis labels
  let allDates = sortBy compare $ getDates data'
  let dateCount = toNumber $ length allDates
  let xScale i = (toNumber i) * (chartWidth / (dateCount - 1.0))

  -- Sample years to avoid overcrowding - show every 2nd year
  let yearsToShow = ["2000", "2002", "2004", "2006", "2008", "2010", "2012"]
  let drawXTick :: String -> m Unit
      drawXTick year = do
        -- Find the first date index that matches this year
        case filter (\d -> case getYear d of
                            Just y -> y == year
                            Nothing -> false) allDates of
          [] -> pure unit
          dates -> case head dates of
            Nothing -> pure unit
            Just firstDate ->
              case Data.Array.findIndex (\d -> d == firstDate) allDates of
                Nothing -> pure unit
                Just idx -> do
                  let tickX = xScale idx
                  _ <- appendTo xAxis Text [
                      x tickX
                    , y 20.0
                    , text year
                    , textAnchor "middle"
                    , classed "axis-label"
                    ]
                  pure unit

  _ <- traverse_ drawXTick yearsToShow

  -- Note: Observable version uses hover interaction to show series labels
  -- rather than a static legend. The interactivity is handled via CSS/JS
  -- with mix-blend-mode and opacity changes on hover.

  pure unit
