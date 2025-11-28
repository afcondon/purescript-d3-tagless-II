-- | recipe for this force layout graph
draw :: forall row m. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m => 
  SimulationM D3Selection_ m =>
  LesMisRawModel -> Selector D3Selection_ -> m Unit
draw model selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  (root :: D3Selection_) <- attach selector
  svg        <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "lesmis" ]
  linksGroup <- appendTo svg Group  [ classed "link", strokeColor "#999", strokeOpacity 0.6 ]
  nodesGroup <- appendTo svg Group  [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ]
  
  -- in contrast to a simple SelectionM function, we have additional typeclass capabilities for simulation
  -- which we use here to introduce the nodes and links to the simulation
  nodesInSim <- setNodes model.nodes -- no staging here, we just load the nodes straight into Sim
  linksInSim <- setLinks model.links model.nodes keyIsID_

  -- joining the data from the model after it has been put into the simulation
  nodesSelection <- simpleJoin nodesGroup Circle nodesInSim keyIsID_ 
  setAttributes nodesSelection [ radius 5.0, fill datum_.colorByGroup ] 
  linksSelection <- simpleJoin linksGroup Line   linksInSim keyIsID_ 
  setAttributes linksSelection [ strokeWidth (sqrt <<< link_.value), strokeColor link_.color ]

  -- both links and nodes are updated on each step of the simulation, 
  -- in this case it's a simple translation of underlying (x,y) data for the circle centers
  -- tick functions have names, in this case "nodes" and "links"
  addTickFunction "nodes" $ Step nodesSelection [ cx datum_.x, cy datum_.y  ]
  addTickFunction "links" $ Step linksSelection [ x1 (_.x <<< link_.source)
                                                , y1 (_.y <<< link_.source)
                                                , x2 (_.x <<< link_.target)
                                                , y2 (_.y <<< link_.target)
                                                ]
  -- use default drag function (simply drags the element that's clicked on)                                              
  _ <- nodesSelection `on` Drag (CustomDrag "lesmis" simdrag)
  -- TODO create inner <g> and apply the zoom functionality to it
  _ <- svg `on`  Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                      , scale  : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                      , name   : "LesMis"
                      , target : svg
                      }
  setConfigVariable $ Alpha 1.0
  pure unit
