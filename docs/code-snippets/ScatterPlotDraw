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

  -- Add data points as circles
  let addPoint :: DataPoint -> m Unit
      addPoint point = do
        let xPos = applyScale xScale point.x
        let yPos = applyScale yScale point.y
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
