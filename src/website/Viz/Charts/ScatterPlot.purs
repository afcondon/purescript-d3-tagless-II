module D3.Viz.ScatterPlot where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fontSize, height, radius, strokeColor, strokeWidth, text, textAnchor, transform, viewBox, width, x, y)
import PSD3.Internal.Axes (axisBottom_, axisLeft_, callAxis_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (DataPoint, QuartetData)
import PSD3.Internal.Scales.Linear (applyScale_, createLinearScale_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Shared.ChartDimensions (defaultDimensions, innerWidth, innerHeight)
import Data.Foldable (maximum, minimum, traverse_)
import Data.Maybe (fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)

-- Snippet_Start
-- Name: ScatterPlotDraw
-- Main drawing function for scatter plot
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
    , classed "scatter-plot"
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

  -- Add data points as circles
  let addPoint :: DataPoint -> m Unit
      addPoint point = do
        let xPos = applyScale_ xScale point.x
        let yPos = applyScale_ yScale point.y
        _ <- appendTo chartGroup Circle [
            cx xPos
          , cy yPos
          , radius 5.0
          , fill "#4a90e2"
          , strokeColor "#357abd"
          , strokeWidth 1.0
          , classed "scatter-point"
          ]
        pure unit

  _ <- traverse_ addPoint dataPoints

  pure unit
-- Snippet_End

-- Snippet_Start
-- Name: ScatterPlotQuartet
-- Draw Anscombe's Quartet as small multiples (2x2 grid)
drawQuartet :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  QuartetData -> Selector D3Selection_ -> m Unit
drawQuartet quartet selector = do
  -- Overall dimensions for the quartet display
  let totalWidth = 900.0
  let totalHeight = 700.0
  let padding = 60.0

  -- Calculate dimensions for each subplot
  let plotWidth = (totalWidth - padding * 3.0) / 2.0
  let plotHeight = (totalHeight - padding * 3.0) / 2.0
  let margin = { top: 30.0, right: 20.0, bottom: 40.0, left: 50.0 }
  let iWidth = plotWidth - margin.left - margin.right
  let iHeight = plotHeight - margin.top - margin.bottom

  -- Use fixed scale domains for all four plots (for valid comparison)
  let xDomain = [0.0, 20.0]
  let yDomain = [0.0, 14.0]

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 totalWidth totalHeight
    , classed "scatter-quartet"
    , width totalWidth
    , height totalHeight
    ]

  -- Helper function to draw a single subplot
  let drawSubplot :: String -> Array DataPoint -> Number -> Number -> m Unit
      drawSubplot title dataPoints xOffset yOffset = do
        -- Create group for this subplot
        subplotGroup <- appendTo svg Group [
            classed "subplot"
          , transform [ \_ -> "translate(" <> show (xOffset + margin.left) <> "," <> show (yOffset + margin.top) <> ")" ]
          ]

        -- Add title
        _ <- appendTo svg Text [
            x (xOffset + plotWidth / 2.0)
          , y (yOffset + 15.0)
          , text title
          , textAnchor "middle"
          , fontSize 16.0
          , classed "subplot-title"
          ]

        -- Create scales
        xScale <- liftEffect $ createLinearScale_ { domain: xDomain, range: [0.0, iWidth] }
        yScale <- liftEffect $ createLinearScale_ { domain: yDomain, range: [iHeight, 0.0] }

        -- Add axes
        xAxisGroup <- appendTo subplotGroup Group [
            classed "x-axis"
          , transform [ \_ -> "translate(0," <> show iHeight <> ")" ]
          ]
        yAxisGroup <- appendTo subplotGroup Group [ classed "y-axis" ]

        _ <- liftEffect $ callAxis_ xAxisGroup (axisBottom_ xScale)
        _ <- liftEffect $ callAxis_ yAxisGroup (axisLeft_ yScale)

        -- Add data points
        let addPoint :: DataPoint -> m Unit
            addPoint point = do
              let xPos = applyScale_ xScale point.x
              let yPos = applyScale_ yScale point.y
              _ <- appendTo subplotGroup Circle [
                  cx xPos
                , cy yPos
                , radius 4.0
                , fill "#e74c3c"
                , strokeColor "#c0392b"
                , strokeWidth 1.5
                , classed "scatter-point"
                ]
              pure unit

        _ <- traverse_ addPoint dataPoints
        pure unit

  -- Draw all four subplots in a 2x2 grid
  _ <- drawSubplot "Dataset I" quartet.dataset1 padding (padding)
  _ <- drawSubplot "Dataset II" quartet.dataset2 (padding + plotWidth + padding) (padding)
  _ <- drawSubplot "Dataset III" quartet.dataset3 padding (padding + plotHeight + padding)
  _ <- drawSubplot "Dataset IV" quartet.dataset4 (padding + plotWidth + padding) (padding + plotHeight + padding)

  pure unit
-- Snippet_End
