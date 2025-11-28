-- Main drawing function for bubble chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Json -> Selector D3Selection_ -> m Unit
draw jsonData selector = do
  let dims = { width: 900.0, height: 900.0 }

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed "bubble-chart"
    , width dims.width
    , height dims.height
    ]

  -- Create main group
  chartGroup <- appendTo svg Group [
      classed "bubbles"
    ]

  -- Create hierarchy from parsed JSON
  let hierarchy = hierarchyFromJSON_ (unsafeCoerce jsonData)
  let sortedHierarchy = treeSortForCirclePack_ hierarchy

  -- Create and configure pack layout
  let packLayout0 = packLayout_ unit
  let packLayout1 = packSetSize_ packLayout0 dims.width dims.height
  let packLayout = packSetPadding_ packLayout1 2.0

  -- Apply pack layout to hierarchy
  let packedRoot = runPackLayout_ packLayout sortedHierarchy

  -- Get all nodes (descendants)
  let nodes = descendants_ packedRoot

  -- Draw each bubble
  let drawBubble :: forall r. D3_TreeNode r -> m Unit
      drawBubble node = do
        let xPos = hNodeX_ node
        let yPos = hNodeY_ node
        let r = hNodeR_ node
        let depth = hNodeDepth_ node
        let name = getName node

        -- Only draw bubbles with radius > 0
        when (r > 0.0) do
          -- Draw circle
          _ <- appendTo chartGroup Circle [
              cx xPos
            , cy yPos
            , radius r
            , fill (depthColor $ round depth)
            , fillOpacity 0.7
            , strokeColor "#ffffff"
            , strokeWidth 1.0
            , classed "bubble"
            ]

          -- Add label for larger bubbles
          when (r > 20.0) do
            _ <- appendTo chartGroup Text [
                x xPos
              , y yPos
              , text name
              , textAnchor "middle"
              , fontSize (min 12.0 (r / 3.0))
              , fill "#ffffff"
              , classed "bubble-label"
              ]
            pure unit

          pure unit

  _ <- traverse_ drawBubble nodes

  pure unit
