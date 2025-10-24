-- Main drawing function for Sankey diagram
draw :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { sankeyLayout :: SankeyLayoutState_ | row } m =>
  SankeyM D3Selection_ m =>
  SankeyData -> Selector D3Selection_ -> m Unit
draw sankeyData selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight

  (root :: D3Selection_) <- attach selector
  svg  <- appendTo root Svg [ viewBox 0.0 0.0 w h, classed "sankey" ]

  -- Create groups for links and nodes
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]
  labelsGroup <- appendTo svg Group [ classed "labels" ]

  -- Pass data through Sankey layout generator
  -- This computes positions and dimensions for nodes and links
  layoutResult <- setSankeyData sankeyData w h

  -- Join and render links (as paths with custom link path generator)
  linksSelection <- simpleJoin linksGroup Path layoutResult.links keyForLink
  setAttributes linksSelection
    [ classed "sankey-link"
    , fill "none"
    , strokeWidth link_.width
    , strokeOpacity 0.5
    , d sankeyLinkPath_
    , strokeColor link_.color
    ]

  -- Join and render nodes (as rectangles)
  nodesSelection <- simpleJoin nodesGroup Rect layoutResult.nodes keyForNode
  setAttributes nodesSelection
    [ classed "sankey-node"
    , x node_.x0
    , y node_.y0
    , width (\n -> node_.x1 n - node_.x0 n)
    , height (\n -> node_.y1 n - node_.y0 n)
    , fill node_.color
    , fillOpacity 0.8
    ]

  -- Add labels for nodes
  labelsSelection <- simpleJoin labelsGroup Text layoutResult.nodes keyForNode
  setAttributes labelsSelection
    [ classed "sankey-label"
    , x (\n -> if node_.x0 n < w / 2.0 then node_.x1 n + 6.0 else node_.x0 n - 6.0)
    , y (\n -> (node_.y0 n + node_.y1 n) / 2.0)
    , dy 4.0  -- TODO: should be "0.35em" but dy is not polymorphic in our library
    , textAnchor (\n -> if node_.x0 n < w / 2.0 then "start" else "end")
    , text node_.name
    ]

  pure unit
