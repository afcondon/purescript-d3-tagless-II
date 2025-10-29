module D3.Viz.MultiLineChart where

-- | Multi-Line Chart
-- | Based on https://observablehq.com/@d3/multi-line-chart/2
-- | Copyright 2018–2023 Observable, Inc.
-- | ISC License

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, d, fill, height, strokeColor, strokeWidth, text, textAnchor, transform, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (MultiLineData)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import Data.Array (filter, head, length, nub)
import Data.Foldable (maximum, minimum, traverse_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toString)
import Effect.Class (class MonadEffect)

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
            x (-10.0)
          , y (tickY + 4.0)
          , text (toString value <> "%")
          , textAnchor "end"
          , classed "axis-label"
          ]
        pure unit

  _ <- traverse_ drawYTick yTicks

  -- Note: Observable version uses hover interaction to show series labels
  -- rather than a static legend. The interactivity is handled via CSS/JS
  -- with mix-blend-mode and opacity changes on hover.

  pure unit
