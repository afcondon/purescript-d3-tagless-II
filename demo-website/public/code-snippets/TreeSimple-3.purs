drawTreeRadial :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector D3Selection_ -> m D3Selection_
drawTreeRadial json selector = do
  (Tuple width height) <- liftEffect getWindowWidthHeight
  let root = hierarchyFromJSON_ json
      layout = ((getLayout TidyTree) `treeSetSize_` [ 2.0 * pi, (width / 2.0) - 100.0 ])
                                     `treeSetSeparation_` radialSeparation
      laidOutRoot = layout `runLayoutFn_` root
      { yMax } = treeMinMax_ laidOutRoot
      radialRadius = yMax
      radialExtent = 2.0 * radialRadius
      config = {
          layout: Radial
        , selector
        , linkPath: radialLink treeDatum_.x treeDatum_.y
        , spacing: { interChild: 0.0, interLevel: 0.0 }
        , viewbox: [ viewBox (-radialRadius * 1.2) (-radialRadius * 1.2) (radialExtent * 1.2) (radialExtent * 1.2)
                   , preserveAspectRatio $ AspectRatio XMin YMin Meet ]
        , nodeTransform: [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]
        , color: d3SchemeCategory10N_ 6.0
        , svg: { width, height }
      }
  Tree.draw config laidOutRoot
