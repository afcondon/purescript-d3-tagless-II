drawTreeVertical :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector D3Selection_ -> m D3Selection_
drawTreeVertical json selector = do
  (Tuple width height) <- liftEffect getWindowWidthHeight
  let root = hierarchyFromJSON_ json
      numberOfLevels = (hNodeHeight_ root) + 1.0
      spacing = { interChild: 10.0, interLevel: height / numberOfLevels }
      layout = (getLayout TidyTree) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
      laidOutRoot = layout `runLayoutFn_` root
      { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot
      xExtent = abs $ xMax - xMin
      yExtent = abs $ yMax - yMin
      pad n = n * 1.2
      vtreeYOffset = (abs (height - yExtent)) / 2.0
      vtreeXOffset = xMin
      config = {
          layout: Vertical
        , selector
        , linkPath: verticalLink
        , spacing
        , viewbox: [ viewBox vtreeXOffset (-vtreeYOffset) (pad xExtent) (pad yExtent)
                   , preserveAspectRatio $ AspectRatio XMid YMid Meet ]
        , nodeTransform: [ transform [ positionXY ] ]
        , color: d3SchemeCategory10N_ 5.0
        , svg: { width, height }
      }
  Tree.draw config laidOutRoot
