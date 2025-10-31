module D3.Viz.BarChart where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, height, strokeColor, strokeWidth, transform, viewBox, width, x, y)
import PSD3.Internal.Axes (axisBottom_, axisLeft_, callAxis_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (DataPoint)
import PSD3.Internal.Scales.Linear (applyScale_, createLinearScale_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Shared.ChartDimensions (defaultDimensions, innerWidth, innerHeight)
import Data.Array (length)
import Data.Foldable (maximum, minimum, traverse_)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)

-- Snippet_Start
-- Name: BarChartDraw
-- Main drawing function for bar chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array DataPoint -> Selector D3Selection_ -> m Unit
draw dataPoints selector = do
  let dims = defaultDimensions
  let iWidth = innerWidth dims
  let iHeight = innerHeight dims

  -- Calculate data extents
  let xValues = map _.x dataPoints
  let yValues = map _.y dataPoints
  let minX = fromMaybe 0.0 $ minimum xValues
  let maxX = fromMaybe 100.0 $ maximum xValues
  let minY = 0.0  -- Start bars from zero
  let maxY = fromMaybe 100.0 $ maximum yValues

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed "bar-chart"
    , width dims.width
    , height dims.height
    ]

  -- Calculate bar width (with padding)
  let numBars = length dataPoints
  let barWidth = if numBars > 0 then (iWidth / (Int.toNumber numBars)) * 0.8 else 0.0

  -- Create a group for the chart content (offset by margins)
  chartGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show dims.margin.left <> "," <> show dims.margin.top <> ")" ]
    ]

  -- Create scales
  xScale <- liftEffect $ createLinearScale_ { domain: [minX, maxX], range: [0.0, iWidth] }
  yScale <- liftEffect $ createLinearScale_ { domain: [minY, maxY], range: [iHeight, 0.0] }

  -- Add <g> elements to hold each of the axes
  xAxisGroup <- appendTo chartGroup Group [
      classed "x-axis"
    , transform [ \_ -> "translate(0," <> show iHeight <> ")" ]
    ]
  yAxisGroup <- appendTo chartGroup Group [ 
      classed "y-axis"
    , transform [ \_ -> "translate(" <> show ((barWidth / 2.0 * -1.0) - 5.0) <> ",0)" ]
  ]
  -- Draw the axes into the SVG, inside their respective <g> group elements
  _ <- liftEffect $ callAxis_ xAxisGroup (axisBottom_ xScale)
  _ <- liftEffect $ callAxis_ yAxisGroup (axisLeft_ yScale)

  -- Add bars
  let addBar :: DataPoint -> m Unit
      addBar point = do
        let xPos = applyScale_ xScale point.x - (barWidth / 2.0)
        let yPos = applyScale_ yScale point.y
        let barHeight = iHeight - yPos
        _ <- appendTo chartGroup Rect [
            x xPos
          , y yPos
          , width barWidth
          , height barHeight
          , fill "#4a90e2"
          , strokeColor "#357abd"
          , strokeWidth 1.0
          , classed "bar"
          ]
        pure unit

  _ <- traverse_ addBar dataPoints


  pure unit
-- Snippet_End
