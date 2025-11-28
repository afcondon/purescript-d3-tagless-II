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
        xScale <- liftEffect $ createLinearScale { domain: xDomain, range: [0.0, iWidth] }
        yScale <- liftEffect $ createLinearScale { domain: yDomain, range: [iHeight, 0.0] }

        -- Add axes
        xAxisGroup <- appendTo subplotGroup Group [
            classed "x-axis"
          , transform [ \_ -> "translate(0," <> show iHeight <> ")" ]
          ]
        yAxisGroup <- appendTo subplotGroup Group [ classed "y-axis" ]

        _ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
        _ <- liftEffect $ callAxis yAxisGroup (axisLeft yScale)

        -- Add data points
        let addPoint :: DataPoint -> m Unit
            addPoint point = do
              let xPos = applyScale xScale point.x
              let yPos = applyScale yScale point.y
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
