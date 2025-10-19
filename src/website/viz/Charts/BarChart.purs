module D3.Examples.BarChart where

import Prelude

import D3.Attributes.Sugar (classed, fill, height, strokeColor, strokeWidth, transform, viewBox, width, x, y)
import D3.Axes (axisBottom, axisLeft, callAxis)
import D3.Data.Types (D3Selection_, Element(..), Selector)
import D3.Examples.Charts.Model (DataPoint, monthlySales)
import D3.Scales.Linear (applyScale, createLinearScale)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach)
import Data.Array (length, mapWithIndex)
import Data.Foldable (maximum, minimum, traverse_)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)

-- Chart dimensions and margins
type ChartDimensions = {
    width :: Number
  , height :: Number
  , margin :: { top :: Number, right :: Number, bottom :: Number, left :: Number }
}

defaultDimensions :: ChartDimensions
defaultDimensions = {
    width: 800.0
  , height: 400.0
  , margin: { top: 20.0, right: 30.0, bottom: 30.0, left: 40.0 }
}

-- Calculate inner dimensions (accounting for margins)
innerWidth :: ChartDimensions -> Number
innerWidth dims = dims.width - dims.margin.left - dims.margin.right

innerHeight :: ChartDimensions -> Number
innerHeight dims = dims.height - dims.margin.top - dims.margin.bottom

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

  -- Create a group for the chart content (offset by margins)
  chartGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show dims.margin.left <> "," <> show dims.margin.top <> ")" ]
    ]

  -- Create scales
  xScale <- liftEffect $ createLinearScale { domain: [minX, maxX], range: [0.0, iWidth] }
  yScale <- liftEffect $ createLinearScale { domain: [minY, maxY], range: [iHeight, 0.0] }

  -- Add axes
  xAxisGroup <- appendTo chartGroup Group [
      classed "x-axis"
    , transform [ \_ -> "translate(0," <> show iHeight <> ")" ]
    ]
  yAxisGroup <- appendTo chartGroup Group [ classed "y-axis" ]

  _ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
  _ <- liftEffect $ callAxis yAxisGroup (axisLeft yScale)

  -- Calculate bar width (with padding)
  let numBars = length dataPoints
  let barWidth = if numBars > 0 then (iWidth / (Int.toNumber numBars)) * 0.8 else 0.0

  -- Add bars
  let addBar :: DataPoint -> m Unit
      addBar point = do
        let xPos = applyScale xScale point.x - (barWidth / 2.0)
        let yPos = applyScale yScale point.y
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
