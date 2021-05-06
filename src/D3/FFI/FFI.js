const debug = false;
// *****************************************************************************************************************
// ************************** functions from d3js zoom module         *****************************************
// *****************************************************************************************************************

// foreign import attachZoom :: D3Selection_ -> ZoomConfigDefault_ -> D3Selection_
exports.d3AttachZoomDefaultExtent_ = selection => config => { 
  if (debug) { showAttachZoomDefaultExtent_(config.target)(config); }
  function zoomed({transform}) {
    (config.target).attr("transform", transform);
  }
  // "If extent is not specified, returns the current extent accessor, which
  // defaults to [[0, 0], [width, height]] where width is the client width of the
  // element and height is its client height; for SVG elements, the nearest
  // ancestor SVG element’s viewBox, or width and height attributes, are used.""
  return selection.call(d3.zoom() 
                  .scaleExtent(config.scaleExtent)
                  .on(`zoom.${config.qualifier}`, zoomed));
                  // .on("zoom", zoomed));
                }
                
// foreign import attachZoom :: D3Selection_ -> ZoomConfig_ -> D3Selection_
exports.d3AttachZoom_ = selection => config => { 
  if (debug) { showAttachZoom_(config.target)(config) }
  function zoomed({transform}) { // TODO try arrow function below instead
    (config.target).attr("transform", transform);
  }
  return selection.call(d3.zoom()
                          .extent(config.extent)
                          .scaleExtent(config.scaleExtent)
                          .on(`zoom.${config.qualifier}`, zoomed));
                          // .on("zoom", zoomed));
}

exports.showAttachZoomDefaultExtent_ = selection => config => {
  return (`\t${selection}.call(zoom ${config})`)
}
exports.showAttachZoom_ = selection => config => {
  return (`\t${selection}.call(zoom ${config})`)
}
// *****************************************************************************************************************
// ************************** functions from d3js Selection & Transition         ***********************************
// *****************************************************************************************************************
exports.emptyD3Data_ = null

// d3SelectAll_ :: Selector -> D3Selection
exports.d3SelectAllInDOM_ = selector => { // NB not USING selection but want it cause called from inside State Monad
  if (debug) { showSelectAllInDOM_(selector); }
  return d3.selectAll(selector);
} 
// d3SelectAll_ :: Selector -> D3Selection
exports.d3SelectionSelectAll_ = selector => selection => {
  if (debug) { showSelectionSelectAll_(selector)(selection); }
  return selection.selectAll(selector);
} 
// d3Enter_ :: D3Selection -> D3Selection
exports.d3EnterAndAppend_ = element => selection => {
  if (debug) { showEnterAndAppend_(element)(selection); }
  return selection.enter().append(element);
}
// d3Exit_ :: D3Selection -> D3Selection
exports.d3Exit_ = selection => {
  if (debug) { showExit_(selection); }
  return selection.exit();
}
// d3AddTransition :: D3Selection -> D3Selection
exports.d3AddTransition_ = selection => transition => {
  var handle; 
  if (debug) { showAddTransition_(selection)(transition); }
  if (transition.name == "") {
    handle = selection.transition();
    // if transition is unnamed we configure it...
    if (transition.duration != 0) {
      handle.duration(transition.duration);
    }
    if (transition.delay != 0) {
      handle.delay(transition.delay);
    }
  } else {
    handle = selection.transition(transition.name);
  }
  return handle; 
}

// d3RemoveSelection_ :: D3Selection -> D3Selection
exports.d3RemoveSelection_ = selection => {
  if (debug) { showRemoveSelection_(selection); }
  return selection.remove();
}
// d3Append_ :: String -> D3Selection -> D3Selection
exports.d3Append_ = element => selection => {
  if (debug) { showAppend_(element)(selection); }
  return selection.append(element);
}
// d3Data_ :: D3Data -> D3Selection -> D3Selection
exports.d3Data_ = data => selection => {
  if (debug) { showData_(data)(selection); }
  return selection.data(data, d => d );
}
// d3Data_ :: D3Data -> KeyFunction -> D3Selection -> D3Selection
exports.d3KeyFunction_ = data => keyFunction => selection => {
  if (debug) { showKeyFunction_(data)(keyFunction)(selection); }
  return selection.data(data, keyFunction);
}
// d3SetAttr_      :: String -> D3Attr -> D3Selection -> Unit 
exports.d3SetAttr_ = name => value => selection => {
  if (debug) { showSetAttr_(name)(value)(selection); }
  return selection.attr(name, value);
}
// d3SetAttr_      :: String -> D3Attr -> D3Selection -> Unit 
exports.d3SetText_ = value => selection => {
  if (debug) { showSetText_(value, selection); }
  return selection.text(value);
}

exports.showSelectAllInDOM_ = selector => {
  return (`\td3SelectAllInDOM: ${selector}`)
}
exports.showSelectAll_ = selector => selection => {
  return (`\td3SelectionSelectAll: ${selection}.selectAll(${selector})`)
}
exports.showEnterAndAppend_ = element => selection => {
  return (`\td3EnterAndAppend: ${selection}.enter().append(${element})`)
}
exports.showExit_ = selection => {
  return (`\td3Exit: ${selection}.exit()`)
}
exports.showAddTransition_ = selection => transition => {
  if (transition.name == "") {
    const statement1 = `\td3addTransition: ${selection}.transition(${transition})`;
    var statement2 = "";
    var statement3 = "";
    if (transition.duration != 0) {
      statement2 = `transition.duration(${transition.duration})`;
    }
    if (transition.delay != 0) {
      statement3 = `\t\ttransition.delay(${transition.delay})`;
    }
    return (statement1 + statement2 + statement3);
  } else {
    return(`\td3addNamedTransition: ${selection}.transition(${transition})`);
  }
}
exports.showRemoveSelection_ = selection => {
  return (`\td3Remove: ${selection}.remove()`)
}
exports.showAppend_ = element => selection => {
  return (`\td3Append: ${selection}.append(${element})`)
}
exports.showKeyFunction_ = data => keyFunction => selection => {
  return (`\td3Data: ${selection}.data(${data}, ${keyFunction})`)
}
exports.showData_ = data => selection => {
  return (`\td3Data: ${selection}.data(${data})`)
}
exports.showSetAttr_ = name => value => selection => {
  return (`\t${selection}.attr(${name}, ${value})`);
}
exports.showSetText_ = value => selection => {
  return (`\t${selection}.text(${value})`)
}
exports.defaultDrag_ = selection => { 
  var drag = function() {
    function dragstarted(event, d) {
      d.fx = d.x;
      d.fy = d.y;
    }
    
    function dragged(event,d) {
      d.fx = event.x;
      d.fy = event.y;
    }
    
    function dragended(event,d) {
      d.fx = null;
      d.fy = null;
    }
    
    return d3.drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended);  
      }

    selection.call(drag())
}

exports.defaultDrag_ = selection => { 
  var drag = function() {
    function dragstarted(event, d) {
      d.fx = d.x;
      d.fy = d.y;
    }
    
    function dragged(event,d) {
      d.fx = event.x;
      d.fy = event.y;
    }
    
    function dragended(event,d) {
      d.fx = null;
      d.fy = null;
    }
    
    return d3.drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended);  
      }

    selection.call(drag())
}

exports.disableDrag_ = selection => {
  return selection.on(".drag", null)
}

exports.selectionOn_ = selection => event => callback => {
  // selection.on("mouseenter", e => { console.log(`mouseenter!!! ${e}`);} )
  selection.on(event, callback);
  return selection; // seems that D3's selection.on doesn't return the selection, oddly
}
// *****************************************************************************************************************
// ************************** functions from d3js Simulation module         *****************************************
// *****************************************************************************************************************

//            SIMULATION functions
exports.initSimulation_ = nodes => config => {
  const simulation = 
  d3.forceSimulation(nodes)
  .alpha(config.alpha) // default is 1
  .alphaTarget(config.alphaTarget) // default is 0
  .alphaMin(config.alphaMin) // default is 0.0001
  .alphaDecay(config.alphaDecay) // default is 0.0228
  .velocityDecay(config.velocityDecay) // default is 0.4
  if(debug){ console.log(`initSimulation${simulation}`)}
  return simulation;
}
//  :: Simulation -> Array NativeNode -> Array NativeNode
exports.setNodes_ = simulation => nodes => { 
  if(debug){ console.log(`${simulation}.nodes(${nodes})`)}
  simulation.nodes(nodes)
  return simulation;
}
//  :: Simulation -> Array NativeLink -> Array NativeLink
exports.setLinks_ = simulation => links => { 
  simulation.force("links", d3.forceLink(links).id(d => d.id))
  return simulation;
}
// :: NativeSelection -> Number -> Unit
exports.setAlphaTarget_ = simulation => target => simulation.alphaTarget(target)
//  :: NativeSelection -> Unit
exports.startSimulation_ = simulation => simulation.restart()
//  :: NativeSelection -> Unit
exports.stopSimulation_ = simulation => simulation.stop()

// simulation.on("tick", () => {
exports.onTick_ = simulation => name => tickFn => {
  // if(debug){ console.log(`${simulation}.onTick(${tickFn})`)}
  return simulation.on("tick."+name, () => tickFn());
}

// default drag function for simulations, restarts simulation while dragging
exports.defaultSimulationDrag_ = selection => simulation => {
  
  var drag = function(simulation) {
    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }
    
    function dragged(event,d) {
      d.fx = event.x;
      d.fy = event.y;
    }
    
    function dragended(event,d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }
    
    return d3.drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended);  
  }

  selection.call(drag(simulation))
}

//            FORCE functions 
// :: Simulation -> Unit
exports.forceMany_ = simulation => label => simulation.force(label, d3.forceManyBody().strength(-30))
// :: Simulation -> Number -> Number -> Unit
exports.forceCenter_ = simulation => label => cx => cy => simulation.force(label, d3.forceCenter(cx,cy))
// :: Simulation -> Unit
// exports.forceLinks = simulation => label => simulation.x()
// :: Simulation -> Number -> Unit
exports.forceCollideFixed_ = simulation => label => radius => simulation.force(label, d3.forceCollide(radius))
// :: Simulation -> Number -> Unit
exports.forceCollideFn_ = simulation => label => radiusFn => simulation.force(label, d3.forceCollide(radiusFn))
// :: Simulation -> Number -> Unit
exports.forceX_ = simulation => label => cx => simulation.force(label, d3.forceX(cx))
// :: Simulation -> Number -> Unit
exports.forceY_ = simulation => label => cy => simulation.force(label, d3.forceY(cy))
// :: Simulation -> Number -> Number -> Unit
exports.forceRadial_ = simulation => label => cx => cy => simulation.force(label, d3.forceRadial(cx, cy))


// *****************************************************************************************************************
// ************************** functions from d3js Hierarchy module         *****************************************
// *****************************************************************************************************************
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
exports.hierarchyFromJSON_ = json => d3.hierarchy(json).sort((a, b) => d3.ascending(a.data.name, b.data.name))

// foreign import hasChildren              :: Datum_ -> Boolean
exports.hasChildren_ = d => !d.children

// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.links_ = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.descendants_ = tree => tree.descendants()

// foreign import find_        :: D3HierarchicalNode_ -> (Datum_ -> Boolean) -> Nullable D3HierarchicalNode_
exports.find_ = tree => filter => tree.find(filter)

// foreign import initTree_ :: Unit -> D3TreeLike_
exports.initTree_ = () => d3.tree()
// foreign import initRadial :: Unit -> D3TreeLike_
exports.initRadial_ = () => d3.tree()
// foreign import initTree_ :: Unit -> D3TreeLike_
exports.initCluster_ = () => d3.cluster()
// foreign import treeSetRoot_ :: D3TreeLike_ -> D3HierarchicalNode_ -> D3HierarchicalNode_
exports.treeSetRoot_ = tree => root => tree(root)
// foreign import treeSetNodeSize_ :: D3TreeLike_ -> Array Number -> D3TreeLike_
exports.treeSetNodeSize_ = tree => widthHeight => tree.nodeSize(widthHeight) 
// foreign import treeSetSize_     :: D3TreeLike_ -> Array Number -> D3TreeLike_
exports.treeSetSize_ = tree => widthHeight => tree.size(widthHeight)

exports.treeMinMax_ = root => {
  let x0 = Infinity;
  let x1 = -x0;
  let y0 = Infinity;
  let y1 = -y0;
  root.each(d => {
    if (d.x > x1) x1 = d.x;
    if (d.x < x0) x0 = d.x;
    if (d.y > y1) y1 = d.y; // don't know if we will ever need the u versions but we'll calc anyway
    if (d.y < y0) y0 = d.y;
  });
  return { xMin: x0, xMax: x1, yMin: y0, yMax: y1 };
}

exports.linkHorizontal_ = d3.linkHorizontal().x(d => d.y).y(d => d.x)
exports.linkVertical_   = d3.linkVertical().x(d => d.x).y(d => d.y)

exports.linkClusterHorizontal_ = levelSpacing => d => 
  `M${d.target.y}, ${d.target.x}
   C${d.source.y + levelSpacing / 2},${d.target.x}
   ${d.source.y + levelSpacing / 2},${d.source.x}
   ${d.source.y},${d.source.x}`

exports.linkClusterVertical_ = levelSpacing => d => 
  `M${d.target.x}, ${d.target.y}
   C${d.target.x}, ${d.source.y + levelSpacing / 2}
   ${d.source.x},${d.source.y + levelSpacing / 2}
   ${d.source.x},${d.source.y}`
  

// foreign import d3LinkRadial_            :: (Datum_ -> Number) -> (Datum_ -> Number) -> (Datum_ -> String)
exports.linkRadial_ = angleFn => radiusFn => d3.linkRadial().angle(angleFn).radius(radiusFn);

// treeSetSeparation_ :: D3TreeLike_ -> (D3HierarchicalNode_ -> D3HierarchicalNode_ -> Number) -> D3TreeLike_
exports.treeSetSeparation_ = tree => separationFn => tree.separation(separationFn); 

// foreign import shareParent :: D3HierarchicalNode_ -> D3HierarchicalNode_ -> Boolean
exports.sharesParent_ = a => b => (a.parent == b.parent)

// foreign import autoBox_ :: Datum_ -> Array Number
exports.autoBox_ = () => {
  document.body.appendChild(this);
  const {x, y, width, height} = this.getBBox();
  document.body.removeChild(this);
  return [x, y, width, height];
}

// hNodeDepth_  :: D3HierarchicalNode_ -> Int
exports.hNodeDepth_ = node => node.depth;

// hNodeHeight_ :: D3HierarchicalNode_ -> Int
exports.hNodeHeight_ = node => node.height;

// hNodeX_      :: D3HierarchicalNode_ -> Number
exports.hNodeX_ = node => node.x;

// hNodeY_      :: D3HierarchicalNode_ -> Number
exports.hNodeY_ = node => node.y;
