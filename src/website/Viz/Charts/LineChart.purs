module D3.Viz.LineChart where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, d, fill, radius, strokeColor, strokeWidth, transform, viewBox, width, height)
import PSD3.Internal.Axes (Axis_, axisBottom_, axisLeft_, callAxis_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (DataPoint, sineWaveData)
import PSD3.Internal.Generators.Line (createLineGenerator_, generateLinePath_)
import PSD3.Internal.Scales.Linear (LinearScale_, createLinearScale_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes)
import Data.Array (length)
import Data.Foldable (maximum, minimum)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Utility (getWindowWidthHeight)

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
