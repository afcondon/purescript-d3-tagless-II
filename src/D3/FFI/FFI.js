const debug = false
// *****************************************************************************************************************
// ************************** functions from d3js zoom module         *****************************************
// *****************************************************************************************************************

// foreign import attachZoom :: D3Selection_ -> ZoomConfigDefault_ -> D3Selection_
exports.d3AttachZoomDefaultExtent_ = selection => config => {
  if (debug) {
    showAttachZoomDefaultExtent_(config.target)(config)
  }
  function zoomed ({ transform }) {
    config.target.attr('transform', transform)
  }
  // "If extent is not specified, returns the current extent accessor, which
  // defaults to [[0, 0], [width, height]] where width is the client width of the
  // element and height is its client height; for SVG elements, the nearest
  // ancestor SVG element’s viewBox, or width and height attributes, are used.""
  return selection.call(
    d3
      .zoom()
      .scaleExtent(config.scaleExtent)
      .on(`zoom.${config.name}`, zoomed)
  )
  // .on("zoom", zoomed));
}

// foreign import attachZoom :: D3Selection_ -> ZoomConfig_ -> D3Selection_
exports.d3AttachZoom_ = selection => config => {
  if (debug) {
    showAttachZoom_(config.target)(config)
  }
  function zoomed ({ transform }) {
    // TODO try arrow function below instead
    config.target.attr('transform', transform)
  }
  return selection.call(
    d3
      .zoom()
      .extent(config.extent)
      .scaleExtent(config.scaleExtent)
      .on(`zoom.${config.name}`, zoomed)
  )
  // .on("zoom", zoomed));
}

exports.showAttachZoomDefaultExtent_ = selection => config => {
  return `\t${selection}.call(zoom ${config})`
}
exports.showAttachZoom_ = selection => config => {
  return `\t${selection}.call(zoom ${config})`
}
// *****************************************************************************************************************
// ************************** functions from d3js Selection & Transition         ***********************************
// *****************************************************************************************************************
exports.emptyD3Data_ = null

// d3SelectAll_ :: Selector -> D3Selection_
exports.d3SelectAllInDOM_ = selector => {
  if (debug) {
    showSelectAllInDOM_(selector)
  }
  return d3.selectAll(selector)
}
// d3SelectFirstInDOM_ :: Selector -> D3Selection_
exports.d3SelectFirstInDOM_ = selector => {
  if (debug) {
    showSelectAllInDOM_(selector)
  }
  return d3.select(selector)
}
// d3SelectAll_ :: Selector -> D3Selection_
exports.d3SelectionSelectAll_ = selector => selection => {
  if (debug) {
    showSelectionSelectAll_(selector)(selection)
  }
  return selection.selectAll(selector)
}
// d3SelectionIsEmpty_   :: D3Selection_ -> Boolean
exports.d3SelectionIsEmpty_ = selection => selection.empty()

// d3Select_ :: Selector -> D3Selection_
exports.d3SelectionSelect_ = selector => selection => {
  if (debug) {
    showSelectionSelect_(selector)(selection)
  }
  return selection.select(selector)
}
// d3Enter_ :: D3Selection_ -> D3Selection_
exports.d3EnterAndAppend_ = element => selection => {
  if (debug) {
    showEnterAndAppend_(element)(selection)
  }
  return selection.enter().append(element)
}
// d3Exit_ :: D3Selection_ -> D3Selection_
exports.d3Exit_ = selection => {
  if (debug) {
    showExit_(selection)
  }
  return selection.exit()
}
// d3AddTransition :: D3Selection_ -> D3Selection_
exports.d3AddTransition_ = selection => transition => {
  var handle
  if (debug) {
    showAddTransition_(selection)(transition)
  }
  if (transition.name == '') {
    handle = selection.transition()
    // if transition is unnamed we configure it...
    if (transition.duration != 0) {
      handle.duration(transition.duration)
    }
    if (transition.delay != 0) {
      handle.delay(transition.delay)
    }
  } else {
    handle = selection.transition(transition.name)
  }
  return handle
}

// d3RemoveSelection_ :: D3Selection_ -> D3Selection_
exports.d3RemoveSelection_ = selection => {
  if (debug) {
    showRemoveSelection_(selection)
  }
  return selection.remove()
}
// d3FilterSelection_    :: D3Selection_ -> Selector   -> D3Selection_
exports.d3FilterSelection_ = selection => selector => selection.filter(selector)

// d3OrderSelection_     :: D3Selection_ -> D3Selection_
exports.d3OrderSelection_ = selection => selection.order()
// d3RaiseSelection_     :: D3Selection_ -> D3Selection_
exports.d3RaiseSelection_ = selection => selection.raise()
// d3LowerSelection_     :: D3Selection_ -> D3Selection_
exports.d3LowerSelection_ = selection => selection.lower()
// d3SortSelection_      :: forall d. D3Selection_ -> (d -> d -> Int) -> D3Selection_
exports.d3SortSelection_ = selection => compare => selection.sort(compare)


// d3Append_ :: String -> D3Selection_ -> D3Selection_
exports.d3Append_ = element => selection => {
  if (debug) {
    showAppend_(element)(selection)
  }
  return selection.append(element)
}
// d3Data_ :: D3Data -> D3Selection_ -> D3Selection_
exports.d3Data_ = data => selection => {
  if (debug) {
    showData_(data)(selection)
  }
  return selection.data(data, d => d)
}
// d3Data_ :: D3Data -> KeyFunction -> D3Selection_ -> D3Selection_
exports.d3KeyFunction_ = data => keyFunction => selection => {
  if (debug) {
    showKeyFunction_(data)(keyFunction)(selection)
  }
  return selection.data(data, keyFunction)
}
// d3SetAttr_      :: String -> D3Attr -> D3Selection -> Unit
exports.d3SetAttr_ = name => value => selection => {
  if (debug) {
    showSetAttr_(name)(value)(selection)
  }
  return selection.attr(name, value)
}
// d3SetAttr_      :: String -> D3Attr -> D3Selection -> Unit
exports.d3SetText_ = value => selection => {
  if (debug) {
    showSetText_(value, selection)
  }
  return selection.text(value)
}
// d3SetProperty_   :: D3Attr -> D3Selection_ -> D3Selection_
exports.d3SetProperty_ = value => selection => {
  if (debug) {
    showSetProperty_(value, selection)
  }
  return selection.property(value)
}
// d3SetHTML_       :: D3Attr -> D3Selection_ -> D3Selection_
exports.d3SetHTML_ = value => selection => {
  if (debug) {
    showSetHTML_(value, selection)
  }
  return selection.html(value)
}

// TODO all these show statements are getting pruned out in DCE and causing crash at runtime if you have debug enabled
// so either have to backtrack on the extraction to functions or find a way to avoid them being pruned
exports.showSelectAllInDOM_ = selector => {
  return `\td3SelectAllInDOM: ${selector}`
}
exports.showSelectAll_ = selector => selection => {
  return `\td3SelectionSelectAll: ${selection}.selectAll(${selector})`
}
exports.showEnterAndAppend_ = element => selection => {
  return `\td3EnterAndAppend: ${selection}.enter().append(${element})`
}
exports.showExit_ = selection => {
  return `\td3Exit: ${selection}.exit()`
}
exports.showAddTransition_ = selection => transition => {
  if (transition.name == '') {
    const statement1 = `\td3addTransition: ${selection}.transition(${transition})`
    var statement2 = ''
    var statement3 = ''
    if (transition.duration != 0) {
      statement2 = `transition.duration(${transition.duration})`
    }
    if (transition.delay != 0) {
      statement3 = `\t\ttransition.delay(${transition.delay})`
    }
    return statement1 + statement2 + statement3
  } else {
    return `\td3addNamedTransition: ${selection}.transition(${transition})`
  }
}
exports.showRemoveSelection_ = selection => {
  return `\td3Remove: ${selection}.remove()`
}
exports.showAppend_ = element => selection => {
  return `\td3Append: ${selection}.append(${element})`
}
exports.showKeyFunction_ = data => keyFunction => selection => {
  return `\td3Data: ${selection}.data(${data}, ${keyFunction})`
}
exports.showData_ = data => selection => {
  return `\td3Data: ${selection}.data(${data})`
}
exports.showSetAttr_ = name => value => selection => {
  return `\t${selection}.attr(${name}, ${value})`
}
exports.showSetText_ = value => selection => {
  return `\t${selection}.text(${value})`
}
exports.showSetHTML_ = value => selection => {
  return `\t${selection}.html(${value})`
}
exports.showSetProperty_ = value => selection => {
  return `\t${selection}.property(${value})`
}
exports.showSetOrdering_ = ordering => selection => {
  return `\t${selection}.${ordering}()`
}
exports.defaultSimulationDrag_ = selection => simulation => selection.call(simdrag(simulation))

const simdrag = simulation => {
  
  function dragstarted(event) {
    if (!event.active) simulation.alphaTarget(0.3).restart();
    event.subject.fx = event.subject.x;
    event.subject.fy = event.subject.y;
  }
  
  function dragged(event) {
    event.subject.fx = event.x;
    event.subject.fy = event.y;
  }
  
  function dragended(event) {
    if (!event.active) simulation.alphaTarget(0);
    event.subject.fx = null;
    event.subject.fy = null;
  }
  
  return d3.drag()
      .on("start", dragstarted)
      .on("drag", dragged)
      .on("end", dragended);
}

// const drag = function () {

//   function dragstarted() {
//     d3.select(this).attr("stroke", "black");
//   }

//   function dragged(event, d) { // TODO this would only work for circles anyway, gotta be improved
//     d3.select(this).raise().attr("cx", d.x = event.x).attr("cy", d.y = event.y);
//   }

//   function dragended() {
//     d3.select(this).attr("stroke", null);
//   }

//   return d3.drag()
//       .on("start", dragstarted)
//       .on("drag", dragged)
//       .on("end", dragended);
// }

exports.disableDrag_ = selection => {
  return selection.on('.drag', null)
}

exports.selectionOn_ = selection => event => callback => {
  // selection.on("mouseenter", e => { console.log(`mouseenter!!! ${e}`);} )
  selection.on(event, callback)
  return selection // seems that D3's selection.on doesn't return the selection, oddly
}
// *****************************************************************************************************************
// ************************** functions from d3js Simulation module         *****************************************
// *****************************************************************************************************************

//            SIMULATION functions
exports.initSimulation_ = config => { // TODO bug not passed
  const simulation = d3
    .forceSimulation()
    .alpha(config.alpha) // default is 1
    .alphaTarget(config.alphaTarget) // default is 0
    .alphaMin(config.alphaMin) // default is 0.0001
    .alphaDecay(config.alphaDecay) // default is 0.0228
    .velocityDecay(config.velocityDecay) // default is 0.4
  if (true) {
    console.log(`initSimulation${simulation}`)
  }
  return simulation
}
exports.configSimulation_ = simulation => config => {
  simulation
    .alpha(config.alpha) // default is 1
    .alphaTarget(config.alphaTarget) // default is 0
    .alphaMin(config.alphaMin) // default is 0.0001
    .alphaDecay(config.alphaDecay) // default is 0.0228
    .velocityDecay(config.velocityDecay) // default is 0.4
  if (debug) {
    console.log(`configSimulation${simulation}${config}`)
  }
  return simulation
}
//  :: Simulation -> Array NativeNode -> Array NativeNode
exports.setNodes_ = simulation => nodes => {
  if (debug) {
    console.log(`${simulation}.nodes(${nodes})`)
  }
  console.log('setting nodes in simulation');
  simulation.nodes(nodes)
  return simulation.nodes()
}
exports.setLinks_ = linkForce => links => idFn => {
  console.log('making link force for simulation');
  linkForce.links(links)
  linkForce.id(idFn)
  return links
}
exports.unsetLinks_ = simulation => {
  const linkForce = d3.forceLink([])
  console.log('removing all links from simulation');
  simulation.force('links', linkForce)
  return simulation
}

//  :: Simulation -> Array NativeLink -> ???
// exports.makeLinksForce_ = config => d3.forceLink(config.links).id(d => d.id).strength(config.strength);
// removeForceByName_  :: D3Simulation_ -> String -> D3Simulation_
// exports.removeForceByName_ = simulation => name => simulation.force(name, null)
// setLinks_ :: ForceHandle -> Array (D3_Simulation_Link d r) ->
// links_        :: forall d r. ForceHandle_ -> Array (D3_Simulation_Link d r)
exports.getLinks_ = linkForce => linkForce.links()
// setNodes_        :: forall d.   D3Simulation_ -> Array (D3_Simulation_Node d) -> Array (D3_Simulation_Node d)
exports.getNodes_ = simulation => simulation.nodes()

// :: NativeSelection -> Number -> Unit
exports.setAlphaTarget_ = simulation => target => simulation.alphaTarget(target)
// setAlpha_              :: D3Simulation_ -> Number -> Unit
exports.setAlpha_ = simulation => alpha => simulation.alpha(alpha)
// setAlphaMin_           :: D3Simulation_ -> Number -> Unit
exports.setAlphaMin_ = simulation => alphaMin => simulation.alphaMin(alphaMin)
// setAlphaDecay_         :: D3Simulation_ -> Number -> Unit
exports.setAlphaDecay_ = simulation => alphaDecay => simulation.alphaDecay(alphaDecay)
// setAlphaTarget_        :: D3Simulation_ -> Number -> Unit
exports.setAlphaTarget_ = simulation => alphaTarget => simulation.alphaTarget(alphaTarget)
// setVelocityDecay_      :: D3Simulation_ -> Number -> Unit
exports.setVelocityDecay_ = simulation => velocityDecay => simulation.velocityDecay(velocityDecay)

//  :: NativeSelection -> Unit
exports.startSimulation_ = simulation => simulation.restart()
//  :: NativeSelection -> Unit
exports.stopSimulation_ = simulation => simulation.stop()

// simulation.on("tick", () => {
exports.onTick_ = simulation => name => tickFn => {
  // if(debug){ console.log(`${simulation}.onTick(${tickFn})`)}
  return simulation.on('tick.' + name, () => {
    // console.log(`calling the tick function tick.${name}`);
    tickFn()
  })
}
//  disableTick_ :: D3Simulation_ -> String -> Unit
exports.disableTick_ = simulation => name => {
  return simulation.on('tick.' + name, () => null)
}
// defaultNodeTick_       :: String -> D3Simulation_ -> D3Selection_ -> Unit
exports.defaultNodeTick_ = label => simulation => nodeSelection => {
  simulation.on('tick.' + label, () => {
    nodeSelection.attr('cx', d => d.x)
                 .attr('cy', d => d.y )
  })
}
// defaultLinkTick_       :: String -> D3Simulation_ -> D3Selection_ -> Unit
exports.defaultLinkTick_ = label => simulation => linkSelection => {
  simulation.on('tick.' + label, () => {
    linkSelection.attr("x1", d => d.source.x)
                 .attr("y1", d => d.source.y)
                 .attr("x2", d => d.target.x)
                 .attr("y2", d => d.target.y);
  })
}


// default drag function for simulations, restarts simulation while dragging
exports.defaultSimulationDrag_ = selection => simulation => {
  var drag = function (simulation) {
    function dragstarted (event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart()
      d.fx = d.x
      d.fy = d.y
    }

    function dragged (event, d) {
      d.fx = event.x
      d.fy = event.y
    }

    function dragended (event, d) {
      if (!event.active) simulation.alphaTarget(0)
      d.fx = null
      d.fy = null
    }

    return d3
      .drag()
      .on('start', dragstarted)
      .on('drag', dragged)
      .on('end', dragended)
  }

  selection.call(drag(simulation))
}

//          constructors for FORCE handlers

// forceCenter_       :: ForceCenterConfig_       -> D3ForceHandle_
exports.forceCenter_ = () => d3.forceCenter()
// forceCollideFn_    :: ForceCollideConfig_      -> D3ForceHandle_
exports.forceCollideFn_ = () => d3.forceCollide()
// forceMany_         :: ForceManyConfig_         -> D3ForceHandle_
exports.forceMany_ = () => d3.forceManyBody()
// forceRadial_       :: ForceRadialConfig_       -> D3ForceHandle_
exports.forceRadial_ = () => d3.forceRadial()
// forceX_            :: ForceXConfig_            -> D3ForceHandle_
exports.forceX_ = () => d3.forceX()
// forceY_            :: ForceYConfig_            -> D3ForceHandle_
exports.forceY_ = () => d3.forceY()
// forceLink_         :: ForceLinkConfig_         -> D3ForceHandle_
exports.forceLink_ = () => d3.forceLink()
// forceCustom_       :: CustomForceConfig_       -> D3ForceHandle_
exports.forceCustom_ = forceFn => forceFn()

// setForceRadius_      :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceRadius_ = force => attr => force.radius(attr)
// setForceStrength_    :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceStrength_ = force => attr => force.strength(attr)
// setForceCx_          :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceCx_ = force => attr => force.cx(attr)
// setForceCy_          :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceCy_ = force => attr => force.cy(attr)
// setForceTheta_       :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceTheta_ = force => attr => force.theta(attr)
// setForceDistanceMin_ :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceDistanceMin_ = force => attr => force.distanceMin(attr)
// setForceDistanceMax_ :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceDistanceMax_ = force => attr => force.distanceMax(attr)
// setForceIterations_  :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceIterations_ = force => attr => force.iterations(attr)
// setForceX_           :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceX_ = force => attr => force.x(attr)
// setForceY_           :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceY_ = force => attr => force.y(attr)
// setForceDistance_    :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
exports.setForceDistance_ = force => attr => force.distance(attr)
// dummyForceHandle_  :: D3ForceHandle_ -- used for fixed "forces", is null under the hood
exports.dummyForceHandle_ = null
// setAsNullForceInSimulation_ :: D3Simulation_ -> String -> D3Simulation_
exports.setAsNullForceInSimulation_ = simulation => label => {
  simulation.force(label, null)
}
exports.removeFixForceXY_ = simulation => filterFn => {
  let nodes = simulation.nodes()
  for (let index = 0; index < nodes.length; index++) {
    if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
      nodes[index].fx = null;
      nodes[index].fy = null;
    }
  }
}
exports.removeFixForceX_ = simulation => filterFn => {
  let nodes = simulation.nodes()
  for (let index = 0; index < nodes.length; index++) {
    if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
      nodes[index].fx = null;
    }
  }
}
exports.removeFixForceY_ = simulation => filterFn => {
  let nodes = simulation.nodes()
  for (let index = 0; index < nodes.length; index++) {
    if (filterFn(nodes[index])) { // only unfix nodes that this thing applies to
      nodes[index].fy = null;
    }
  }
}
// applyFixForceInSimulationXY_ :: D3Simulation_ -> String -> (Datum_ -> PointXY) -> D3Simulation_ 
exports.applyFixForceInSimulationXY_ = simulation => label => fn => filterFn => {
  // get nodes from simulation
  // set each node's fx,fy using f function
  let nodes = simulation.nodes()
  let filteredNodes = nodes.filter(filterFn)
  for (let index = 0; index < filteredNodes.length; index++) {
      let gridXY = fn(filteredNodes[index])(index) 
      // console.log(`FixForce applies to ${filteredNodes[index].name} at index: ${index} and put it at (${gridXY.x},${gridXY.y})`);
      filteredNodes[index].fx = gridXY.x
      filteredNodes[index].fy = gridXY.y;
      filteredNodes[index].fixIndex_ = index; // in case _other_ elements need to know the cluster point of this element, because it's index is a filtered index
    }
  // console.log(`fix force ${label} fixing position of nodes, filtered by ${filterFn} using function ${fn}`);
}
// applyFixForceInSimulationX_  :: D3Simulation_ -> String -> (Datum_ -> Number)  -> D3Simulation_ 
exports.applyFixForceInSimulationX_ = simulation => label => fn => filterFn => {
  let nodes = simulation.nodes()
  for (let index = 0; index < nodes.length; index++) {
    if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
      let gridXY = fn(nodes[index]) 
      nodes[index].fx = gridXY.x
    }
  }}
// applyFixForceInSimulationY_  :: D3Simulation_ -> String -> (Datum_ -> Number)  -> D3Simulation_ 
exports.applyFixForceInSimulationY_ = simulation => label => fn => filterFn => {
  let nodes = simulation.nodes()
  for (let index = 0; index < nodes.length; index++) {
    if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
      let gridXY = fn(nodes[index]) 
      nodes[index].fy = gridXY.y;
    }
  }}

// putForceInSimulation_ :: D3Simulation_ -> String -> D3ForceHandle_ -> D3Simulation_
exports.putForceInSimulation_ = simulation => label => force => {
  console.log(`Putting ${label} force in the simulation`);
  simulation.force(label, force)
}
// putForceInSimulationWithFilter_ :: D3Simulation_ -> String -> (Datum_ -> Boolean) -> D3ForceHandle_ -> D3Simulation_
exports.putForceInSimulationWithFilter_ = simulation => label => filterFn => force => {
  console.log(`Putting ${label} force in the simulation`);
  console.log("remember to put in the filter here"); // TODO
  simulation.force(label, force)
}

// pinNode_ :: Number -> Number -> GraphNode_ -> Unit
exports.pinNode_ = fx => fy => node => {
  node.fx = fx
  node.fy = fy
  delete node.vx // which would otherwise result in this being positioned AND fixed
}

// unpinNode_ :: GraphNode_ -> Unit
exports.unpinNode_ = node => {
  delete node.fx
  delete node.fy
}

// setPositionToNaN_ :: forall d. Array (D3_SimulationNode d) -> Unit
exports.setPositionToNaN_ = nodes => {
  for (let index = 0; index < nodes.length; index++) {
    nodes[index].x = NaN;
  }
}


// *****************************************************************************************************************
// ************************** functions from d3js Hierarchy module         *****************************************
// *****************************************************************************************************************
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
exports.hierarchyFromJSON_ = json => d3.hierarchy(json)
//.sort((a, b) => d3.ascending(a.data.name, b.data.name))
exports.treeSortForCirclePack_ = root =>
  root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.value - a.value
    })

exports.treeSortForTreeMap_ = root =>
  root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.height - a.height || b.value - a.value
    })

exports.treeSortForTree_ = root =>
  root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.height - a.height || a.id.localeCompare(b.id)
    })
exports.treeSortForTree_Spago = root =>
  root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      // console.log(`comparing ${a.data.name}, height ${a.height} with ${b.data.name} height ${b.height}`);
      const result =
        b.height - a.height || a.data.name.localeCompare(b.data.name)
      // console.log(`result: ${result}`);
      return result
    })

// foreign import hasChildren              :: Datum_ -> Boolean
exports.hasChildren_ = d => !d.children

// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.links_ = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.descendants_ = tree => tree.descendants()
exports.ancestors_ = tree => tree.ancestors()
exports.leaves_ = tree => tree.leaves()
exports.path_ = from => to => tree.path(from, to)

// foreign import find_        :: D3HierarchicalNode_ -> (Datum_ -> Boolean) -> Nullable D3HierarchicalNode_
exports.find_ = tree => filter => tree.find(filter)

// getTreeLayoutFn_    :: Unit -> TreeLayoutFn
exports.getTreeLayoutFn_ = () => d3.tree()
// getClusterLayoutFn_ :: Unit -> TreeLayoutFn
exports.getClusterLayoutFn_ = () => d3.cluster()

exports.runLayoutFn_ = layout => root => layout(root)
// foreign import initTree_ :: Unit -> D3TreeLike_
// foreign import initTree_ :: Unit -> D3TreeLike_
// foreign import treeSetNodeSize_ :: D3TreeLike_ -> Array Number -> D3TreeLike_
exports.treeSetNodeSize_ = tree => widthHeight => tree.nodeSize(widthHeight)
// foreign import treeSetSize_     :: D3TreeLike_ -> Array Number -> D3TreeLike_
exports.treeSetSize_ = tree => widthHeight => tree.size(widthHeight)

exports.treeMinMax_ = root => {
  let max_x = -(Infinity) // start max with smallest possible number
  let min_x = Infinity    // start min with the largest possible number
  let max_y = -(Infinity)
  let min_y = Infinity
  root.each(d => {
    if (d.x > max_x) max_x = d.x // if we find a value greater than current max, that's our new maximum
    if (d.y > max_y) max_y = d.y 

    if (d.x < min_x) min_x = d.x // if we find a value less than current min, that's our new minimum
    if (d.y < min_y) min_y = d.y 

    console.log(`node ${d} (${min_x}, ${min_y}) (${max_x}, ${max_y})`);
  })      
  return { xMin: min_x, xMax: max_x, yMin: min_y, yMax: max_y }
}

exports.linkHorizontal_ = d3
  .linkHorizontal()
  .x(d => d.y)
  .y(d => d.x)
exports.linkVertical_ = d3
  .linkVertical()
  .x(d => d.x)
  .y(d => d.y)

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
exports.linkRadial_ = angleFn => radiusFn =>
  d3
    .linkRadial()
    .angle(angleFn)
    .radius(radiusFn)

// treeSetSeparation_ :: D3TreeLike_ -> (D3HierarchicalNode_ -> D3HierarchicalNode_ -> Number) -> D3TreeLike_
exports.treeSetSeparation_ = tree => separationFn =>
  tree.separation(separationFn)

// foreign import shareParent :: D3HierarchicalNode_ -> D3HierarchicalNode_ -> Boolean
exports.sharesParent_ = a => b => a.parent == b.parent

// foreign import autoBox_ :: Datum_ -> Array Number
exports.autoBox_ = () => {
  document.body.appendChild(this)
  const { x, y, width, height } = this.getBBox()
  document.body.removeChild(this)
  return [x, y, width, height]
}

// hNodeDepth_  :: D3HierarchicalNode_ -> Int
exports.hNodeDepth_ = node => node.depth

// hNodeHeight_ :: D3HierarchicalNode_ -> Int
exports.hNodeHeight_ = node => node.height

// hNodeX_      :: D3HierarchicalNode_ -> Number
exports.hNodeX_ = node => node.x

// hNodeY_      :: D3HierarchicalNode_ -> Number
exports.hNodeY_ = node => node.y
