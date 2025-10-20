drawTreeHorizontal :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector D3Selection_ -> m D3Selection_
drawTreeHorizontal json selector = do
  (Tuple width height) <- liftEffect getWindowWidthHeight
  let root = hierarchyFromJSON_ json
      numberOfLevels = (hNodeHeight_ root) + 1.0
      spacing = { interChild: 10.0, interLevel: width / numberOfLevels }
      layout = (getLayout TidyTree) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
      laidOutRoot = layout `runLayoutFn_` root
      { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot
      xExtent = abs $ xMax - xMin
      pad n = n * 1.2
      htreeYOffset = xMin
      config = {
          layout: Horizontal
        , selector
        , linkPath: horizontalLink
        , spacing
        , viewbox: [ viewBox (-xExtent * 0.1) (pad htreeYOffset) (pad yExtent) (pad xExtent)
                   , preserveAspectRatio $ AspectRatio XMin YMid Meet ]
        , nodeTransform: [ transform [ positionXYreflected ] ]
        , color: d3SchemeCategory10N_ 4.0
        , svg: { width, height }
      }
      yExtent = abs $ yMax - yMin
  Tree.draw config laidOutRoot
