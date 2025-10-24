drawWithData :: forall m. SelectionM D3Selection_ m => Model -> Selector D3Selection_-> m D3Selection_
drawWithData circleData selector = do
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) (-100.0) 320.0 160.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []

  circles     <- simpleJoin circleGroup Circle circleData keyIsID_ 
  setAttributes circles [ strokeColor datum_.color
                        , strokeWidth 3.0
                        , fill "none"
                        , cx datum_.x
                        , cy datum_.y
                        , radius 10.0 ]
  pure circles
