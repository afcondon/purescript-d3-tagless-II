-- | Pretty much the most basic example imaginable, three ints represented by three circles
drawThreeCircles :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m D3Selection_
drawThreeCircles selector = do
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) 20.0 120.0 60.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []
  circles     <- simpleJoin circleGroup Circle [32, 57, 293] keyIsID_ 
  setAttributes circles [ fill "green"
                        , cx xFromIndex
                        , cy 50.0
                        , radius 10.0 ]

  pure circles
