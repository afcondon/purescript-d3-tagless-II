module D3.Examples.LineChart where

import Prelude

import D3.Attributes.Sugar (classed, d, fill, radius, strokeColor, strokeWidth, viewBox, width, height)
import D3.Data.Types (D3Selection_, Element(..), Selector)
import D3.Examples.Charts.Model (DataPoint, sineWaveData)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach, setAttributes)
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

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed "line-chart"
    , width dims.width
    , height dims.height
    ]

  -- Create a group for the chart content (offset by margins)
  chartGroup <- appendTo svg Group [
      -- TODO: need transform attribute
      -- transform $ "translate(" <> show dims.margin.left <> "," <> show dims.margin.top <> ")"
    ]

  -- TODO: Create scales
  -- xScale <- liftEffect $ createLinearScale { domain: [minX, maxX], range: [0.0, iWidth] }
  -- yScale <- liftEffect $ createLinearScale { domain: [minY, maxY], range: [iHeight, 0.0] }

  -- TODO: Add axes
  -- xAxis <- appendTo chartGroup Group [ classed "x-axis" ]
  -- yAxis <- appendTo chartGroup Group [ classed "y-axis" ]
  -- _ <- liftEffect $ callAxis xAxis (axisBottom xScale)
  -- _ <- liftEffect $ callAxis yAxis (axisLeft yScale)

  -- TODO: Add the line path
  -- linePath <- appendTo chartGroup Path [
  --     d $ lineGenerator xScale yScale dataPoints
  --   , fill "none"
  --   , strokeColor "#4a90e2"
  --   , strokeWidth 2.0
  --   ]

  -- For now, just create a placeholder circle to test
  _ <- appendTo chartGroup Circle [
      radius 5.0
    , fill "#4a90e2"
    ]

  pure unit
