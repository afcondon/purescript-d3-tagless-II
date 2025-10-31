module D3.Viz.LineChart where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, d, fill, strokeColor, strokeWidth, transform, viewBox, width, height)
import PSD3.Internal.Axes (axisBottom_, axisLeft_, callAxis_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (DataPoint)
import PSD3.Internal.Generators.Line (createLineGenerator_, generateLinePath_)
import PSD3.Internal.Scales.Linear (createLinearScale_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Shared.ChartDimensions (defaultDimensions, innerWidth, innerHeight)
import Data.Foldable (maximum, minimum)
import Data.Maybe (fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)

-- Snippet_Start
-- Name: LineChartDraw
-- Main drawing function for line chart
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
  let minY = fromMaybe 0.0 $ minimum yValues
  let maxY = fromMaybe 100.0 $ maximum yValues

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed "line-chart"
    , width dims.width
    , height dims.height
    ]

  -- Create a group for the chart content (offset by margins)
  chartGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show dims.margin.left <> "," <> show dims.margin.top <> ")" ]
    ]

  -- Create scales
  xScale <- liftEffect $ createLinearScale_ { domain: [minX, maxX], range: [0.0, iWidth] }
  yScale <- liftEffect $ createLinearScale_ { domain: [minY, maxY], range: [iHeight, 0.0] }

  -- Add axes
  xAxisGroup <- appendTo chartGroup Group [
      classed "x-axis"
    , transform [ \_ -> "translate(0," <> show iHeight <> ")" ]
    ]
  yAxisGroup <- appendTo chartGroup Group [ classed "y-axis" ]

  _ <- liftEffect $ callAxis_ xAxisGroup (axisBottom_ xScale)
  _ <- liftEffect $ callAxis_ yAxisGroup (axisLeft_ yScale)

  -- Create line generator and add the line path
  lineGen <- liftEffect $ createLineGenerator_ { xScale, yScale }
  let pathData = generateLinePath_ lineGen dataPoints

  _ <- appendTo chartGroup Path [
      d pathData
    , fill "none"
    , strokeColor "#4a90e2"
    , strokeWidth 2.0
    , classed "line"
    ]

  pure unit
-- Snippet_End
