draw :: forall m selection. Bind m => SelectionM selection m => 
  ScriptConfig -> FlareTreeNode ->  m selection
draw config tree = do
  root       <- attach config.selector  
  svg        <- appendTo root Svg (config.viewbox <> 
                                    [ classed "tree", width config.svg.width, height config.svg.height ]) 
  container  <- appendTo svg  Group [ fontFamily      "sans-serif", fontSize 10.0 ]
  links      <- appendTo container Group [ classed "links"]
  nodes      <- appendTo container Group [ classed "nodes"]

  theLinks_  <- simpleJoin links Path (links_ tree) keyIsID_
  setAttributes theLinks_ [ strokeWidth 1.5, strokeColor config.color, strokeOpacity 0.4, fill "none", config.linkPath ]

  -- we make a group to hold the node circle and the label text
  nodeJoin_  <- simpleJoin nodes Group (descendants_ tree) keyIsID_ 
  setAttributes nodeJoin_ config.nodeTransform

  theNodes <- appendTo nodeJoin_ Circle
                [ fill         (\(d :: Datum_) -> if treeDatum_.hasChildren d then "#999" else "#555")
                , radius       2.5
                , strokeColor "white"
                ]

  theLabels <- appendTo nodeJoin_ Text
                [ dy         0.31
                , x          (treeDatum_.textX config.layout)
                , textAnchor (treeDatum_.textAnchor config.layout)
                , text       treeDatum_.name
                , fill       config.color
                ]               
  pure svg
