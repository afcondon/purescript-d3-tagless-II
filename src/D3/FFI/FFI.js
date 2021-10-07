const debug = false
// *****************************************************************************************************************
// ************************** functions from d3js Selection & Transition         ***********************************
// *****************************************************************************************************************
exports.emptyD3Data_ = null
exports.d3Append_ = element => selection => { return selection.append(element) }
exports.d3Data_ = data => selection => { return selection.data(data) }
exports.d3DataWithKeyFunction_ = data => keyFn => selection => { return selection.data(data, keyFn) }
exports.d3EnterAndAppend_ = element => selection => { return selection.enter().append(element) }
exports.d3GetExitSelection_ = selection => { return selection.exit() }
exports.d3GetEnterSelection_ = selection => { return selection.enter() }
exports.d3GetSelectionData_ = selection => { return selection.data() }
exports.d3FilterSelection_ = selection => selector => selection.filter(selector)
exports.d3LowerSelection_ = selection => selection.lower()
exports.d3MergeSelectionWith_ = enter => update => { return enter.merge(update); }
exports.d3OrderSelection_ = selection => selection.order()
exports.d3RaiseSelection_ = selection => selection.raise()
exports.d3RemoveSelection_ = selection => { return selection.remove() }
exports.d3SelectAllInDOM_ = selector => { return d3.selectAll(selector) }
exports.d3SelectFirstInDOM_ = selector => { return d3.select(selector) }
exports.d3SelectionIsEmpty_ = selection => selection.empty()
exports.d3SelectionSelect_ = selector => selection => { return selection.select(selector) }
exports.d3SelectionSelectAll_ = selector => selection => { return selection.selectAll(selector) }
exports.d3SetAttr_ = name => value => selection => { return selection.attr(name, value) }
exports.d3SetHTML_ = value => selection => { return selection.html(value) }
exports.d3SetProperty_ = value => selection => { return selection.property(value) }
exports.d3SetText_ = value => selection => { return selection.text(value) }
exports.d3SortSelection_ = selection => compare => selection.sort(compare)
exports.simulationDrag_ = label => selection => simulation => dragFn => selection.call(dragFn(label, simulation))
exports.disableDrag_ = selection => { return selection.on('.drag', null) }
exports.getIndexFromDatum_ = datum => { return (typeof datum.index == `undefined`) ? "?" : datum.index }
exports.selectionOn_ = selection => event => callback => { return selection.on(event, callback) }
exports.d3AddTransition_ = selection => transition => {
  var handle
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
// *****************************************************************************************************************
// *****  there will either need to be quite a range of these functions or a way of writing them in Purs     *******
// *****  this is really down in the weeds of D3 without supporting abstractions in the PS library           *******
// *****  CONCRETE EXAMPLE: this defaults to updating fx but in Spago example position is on parent, using   *******
// *****  transforms to move both circle and label together (only way to position a <group> in SVG)
// *****************************************************************************************************************
exports.simdrag = (label,simulation) => {  
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
          .on('start.' + label, dragstarted)
          .on('drag.' + label, dragged)
          .on('end.' + label, dragended);
}
// *****************************************************************************************************************
// ************************** functions from d3js Simulation module         *****************************************
// *****************************************************************************************************************
exports.linksForceName = "links"
exports.dummyForceHandle_ = null
exports.disableTick_ = simulation => name => { return simulation.on('tick.' + name, () => null) }
exports.forceCenter_ = () => d3.forceCenter()
exports.forceCollideFn_ = () => d3.forceCollide()
exports.forceCustom_ = forceFn => forceFn()
exports.forceLink_ = () => d3.forceLink().id(d => d.id)
exports.forceMany_ = () => d3.forceManyBody()
exports.forceRadial_ = () => d3.forceRadial()
exports.forceX_ = () => d3.forceX()
exports.forceY_ = () => d3.forceY()
exports.getLinksFromForce_ = linkForce => linkForce.links()
exports.getNodes_ = simulation => simulation.nodes()
exports.keyIsID_ = d => { 
  // console.log(`FFI: looking up the id of node: ${d.id}`);
  return d.id;
}
exports.keyIsSourceTarget_ = d => { 
  // console.log(`FFI: looking up the id of node: ${[d.source, d.target]}`);
  return [d.source, d.target];
}
exports.setAlpha_ = simulation => alpha => {
  console.log(`FFI: setting simulation.alpha to ${alpha}`);
  simulation.alpha(alpha)
}
exports.setAlphaDecay_ = simulation => alphaDecay => simulation.alphaDecay(alphaDecay)
exports.setAlphaMin_ = simulation => alphaMin => simulation.alphaMin(alphaMin)
exports.setAlphaTarget_ = simulation => alphaTarget => simulation.alphaTarget(alphaTarget)
exports.setAlphaTarget_ = simulation => target => simulation.alphaTarget(target)
exports.setAsNullForceInSimulation_ = simulation => label => simulation.force(label, null)
exports.setForceCx_ = force => attr => force.cx(attr)
exports.setForceCy_ = force => attr => force.cy(attr)
exports.setForceDistance_ = force => attr => force.distance(attr)
exports.setForceDistanceMax_ = force => attr => force.distanceMax(attr)
exports.setForceDistanceMin_ = force => attr => force.distanceMin(attr)
exports.setForceIterations_ = force => attr => force.iterations(attr)
exports.setForceRadius_ = force => attr => force.radius(attr)
exports.setForceStrength_ = force => attr => force.strength(attr)
exports.setForceTheta_ = force => attr => force.theta(attr)
exports.setForceX_ = force => attr => force.x(attr)
exports.setForceY_ = force => attr => force.y(attr)
exports.setLinksKeyFunction_ = force => attr => force.id(attr)
exports.setVelocityDecay_ = simulation => velocityDecay => simulation.velocityDecay(velocityDecay)
exports.startSimulation_ = simulation => {
  console.log(`FFI: restarting the simulation, alpha is: ${simulation.alpha()}`);
  simulation.restart()
}
exports.stopSimulation_ = simulation => simulation.stop()
exports.initSimulation_ = config => keyFn => { 
  const simulation = d3
    .forceSimulation([])
    .force(exports.linksForceName, d3.forceLink([]).id(keyFn))
    .alpha(config.alpha) // default is 1
    .alphaTarget(config.alphaTarget) // default is 0
    .alphaMin(config.alphaMin) // default is 0.0001
    .alphaDecay(config.alphaDecay) // default is 0.0228
    .velocityDecay(config.velocityDecay) // default is 0.4
  if (true) {
    console.log(`FFI: initSimulation${simulation}`)
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
    console.log(`FFI: configSimulation${simulation}${config}`)
  }
  return simulation
}
exports.readSimulationVariables = simulation => {
  return {
    alpha: simulation.alpha(),
    alphaTarget: simulation.alphaTarget(),
    alphaMin: simulation.alphaMin(),
    alphaDecay: simulation.alphaDecay(),
    velocityDecay: simulation.velocityDecay()
  }
}
unpin = d => {
  d.fx = null
  d.fy = null
  return d;
}
// we create an object that contains only those fields that we want to override what was in the existing selection's data
// concretely, if we want update to change fx/fy status then we put that data in here otherwise it will be unchanged
// no matter what the incoming data object has for fx/fy
getBaseForAssign = (newNodeMap, key) => {
  let newnode = newNodeMap.get(key)
  if (newnode) {
    var updatedCount;
    if (typeof newnode.updatedCount === 'undefined') {
      updatedCount = 0;
    } else {
      updatedCount = newnode.updatedCount + 1;
    }
    return { fx: newnode.fx, fy: newnode.fy, updatedCount: updatedCount }
  } else {
    return d
  }
}

exports.d3PreserveSimulationPositions_ = selection => nodedata => keyFn => {
  // create a map from our chosen id to the OLD obj reference, got from the data thats attached to selection
  const oldNodeMap = new Map(selection.data().map(d => [keyFn(d), d])); 
  // create a map from our chosen id to the NEW / incoming obj reference
  const newNodeMap = new Map(nodedata.map(d => [keyFn(d), d])); 
  // we need to copy the fx/fy (at least) from the updating data 
  console.log(`FFI: d3PreserveSimulationPositions_ given ${nodedata.length} nodes, in selection ${selection.data().length}`);
  
  // REVIEW (also what if we wanted r, say, or x, to change???)
  // we need to be able to specify which fields are to change, ideally, and which are not
  let updatedNodeData = nodedata.map(d => {
    let id = keyFn(d)
    let newNode = newNodeMap.get(id)
    let shell = {}
    if (newNode) {
      console.log(`FFI: copying fx/fy from incoming node to old object (if present)`);
      shell = { fx: newNode.fx, fy: newNode.fy, gridXY: newNode.gridXY, updated: true }
    }
    return Object.assign(oldNodeMap.get(id) || d, shell)
  });
  return updatedNodeData
}
exports.d3PreserveLinkReferences_ = link => links => {
  const old = new Map(link.data().map(d => [exports.getLinkID_(d), d])); 
  let updatedLinkData = links.map(d => Object.assign(old.get(exports.getLinkID_(d)) || d, {} ));
  // now, based on link signature, we should really de-swizzle here? and we may HAVE TO do so
  return updatedLinkData
}
exports.getIDsFromNodes_ = nodes => keyFn => {
  const keys = [];
  for (let i = 0; i < nodes.length; i++) {
    keys[i] = keyFn(nodes[i]);
  }
  return keys
}

exports.setNodes_ = simulation => nodes => {
  console.log(`FFI: setting nodes in simulation, there are ${nodes.length} nodes`);
  simulation.nodes(nodes)
  return simulation.nodes()
}
// we're going to always use the same name for the links force denominated by the linksForceName string
exports.setLinks_ = simulation => links => {
  console.log(`FFI: setting links in simulation, there are ${links.length} links`);
  simulation.force(exports.linksForceName).links(links)
}
// returns array of links with ids replaced by object references, invalid links are discarded
exports.swizzleLinks_ = links => simNodes => keyFn => {
  console.log(`FFI: swizzling links in simulation, there are ${links.length} links`);
  const nodeById = new Map(simNodes.map(d => [keyFn(d), d])); // creates a map from our chosen id to the old obj reference
  // we could use the copy approach from d3PreserveSimulationPositions here so that links animate
  const swizzledLinks = links.filter( (link, index, arr ) => {
    // look up both source and target (which could be id or obj reference)
    // if both source and target are found in nodeMap then we can swizzle and return true
    // else we just return false and this node will go in the bit bucket
    if (typeof link.source !== "object") { 
      link.source = nodeById.get(link.source) // try to get object reference if we don't have it
    } else {
      link.source = nodeById.get(keyFn(link.source)) // try to replace object reference with new object reference
    }
    if (typeof link.target !== "object") { 
      link.target = nodeById.get(link.target)
    } else {
      link.target = nodeById.get(keyFn(link.target))
    }
    // now let's see what we got from that and if we have a valid link or not
    if (typeof link.source === 'undefined' || link.target === 'undefined') {
      return false; // filter this node
    } else {
      link.id = keyFn(link.source) + "-" + keyFn(link.target)
      return true // we've updated the 
    }
  })
  return swizzledLinks
}
exports.unsetLinks_ = simulation => {
  const linkForce = d3.forceLink([])
  console.log('FFI: removing all links from simulation');
  simulation.force(exports.linksForceName, linkForce)
  return simulation
}
// this will work on both swizzled and unswizzled links
exports.getLinkID_ = keyFn => link => { // version for generating an ID for the link object
  const sourceID = (typeof link.source == `object`) ? keyFn(link.source) : link.source
  const targetID = (typeof link.target == `object`) ? keyFn(link.target) : link.target
  return sourceID + "-" + targetID 
}
exports.getLinkIDs_ = keyFn => link => { // version for generating the pairs to check against node ids for pruning
  const sourceID = (typeof link.source == `object`) ? keyFn(link.source) : link.source
  const targetID = (typeof link.target == `object`) ? keyFn(link.target) : link.target
  return { sourceID, targetID } 
}
exports.getLinksFromSimulation_ = simulation => {
  linksForce = simulation.force(exports.linksForceName)
  if (typeof linksForce === `undefined`) {
    return [] // either the force wasn't found, or the force wasn't a links force
  }
  const result = linksForce.links()
  if (typeof result === `undefined`) {
    return []
  }
  return result
}
exports.onTick_ = simulation => name => tickFn => {
  var result = simulation.on('tick.' + name, () => {
    tickFn()
  })
  return result;
}
exports.defaultNodeTick_ = label => simulation => nodeSelection => {
  simulation.on('tick.' + label, () => {
    nodeSelection.attr('cx', d => d.x)
                 .attr('cy', d => d.y )
  })
}
exports.defaultLinkTick_ = label => simulation => linksShown => {
  simulation.on('tick.' + label, () => {
    linksShown.attr("x1", d => d.source.x)
                 .attr("y1", d => d.source.y)
                 .attr("x2", d => d.target.x)
                 .attr("y2", d => d.target.y);
  })
}
exports.lookupForceByName_ = simulation => name => {
  let lookup = simulation.force(name)
  if (typeof lookup === `undefined`) {
    return null;
  }
  return lookup;
}
exports.removeFixForceXY_ = simulation => filterFn => {
  let filteredNodes = simulation.nodes().filter(filterFn)
  for (let index = 0; index < filteredNodes.length; index++) {
      // console.log(`removing FixForceXY from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fx = null;
      filteredNodes[index].fy = null;
    }
}
exports.removeFixForceX_ = simulation => filterFn => {
  let filteredNodes = simulation.nodes().filter(filterFn)
  for (let index = 0; index < filteredNodes.length; index++) { // TODO do this with map
    // console.log(`removing FixForceX from node: ${filteredNodes[index].id}`);
    filteredNodes[index].fx = null;
  }
}
exports.removeFixForceY_ = simulation => filterFn => {
  let filteredNodes = simulation.nodes().filter(filterFn)
  for (let index = 0; index < filteredNodes.length; index++) {
      // console.log(`removing FixForceY from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fy = null;
  }
}
exports.applyFixForceInSimulationXY_ = simulation => label => fn => filterFn => {

  let nodes = simulation.nodes()   // get nodes from simulation
  let filteredNodes = nodes.filter(filterFn)
  for (let index = 0; index < filteredNodes.length; index++) {
      let i = index
      let position = fn(filteredNodes[i])(i)   // set each node's fx,fy using fn function
      filteredNodes[i].fx = position.x
      filteredNodes[i].fy = position.y;
      filteredNodes[i].fixIndex_ = i; // in case _other_ elements need to know the cluster point of this element, because it's index is a filtered index
    }
}
exports.applyFixForceInSimulationX_ = simulation => label => fn => filterFn => {
  let nodes = simulation.nodes()
  for (let index = 0; index < nodes.length; index++) {
    if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
      let position = fn(nodes[index]) 
      nodes[index].fx = position.x
    }
  }}
exports.applyFixForceInSimulationY_ = simulation => label => fn => filterFn => {
  let nodes = simulation.nodes()
  for (let index = 0; index < nodes.length; index++) {
    if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
      let position = fn(nodes[index]) 
      nodes[index].fy = position.y;
    }
  }}
exports.putForceInSimulation_ = simulation => label => force => {
  // console.log(`FFI: Putting ${label} force in the simulation`);
  simulation.force(label, force)
}
// exports.restartLinksForceInSimulation_ = simulation => force => links => {
//   console.log(`Re-enabling links force in the simulation`);
//   simulation.force(this.linksForceName, force)
//   simulation.links(links) // NB these links are the SWIZZLED links that are cached in the D3SimulationState_
// }
// exports.putForceInSimulationWithFilter_ = simulation => label => filterFn => force => {
//   console.log(`FFI: Putting ${label} force in the simulation`);
//   console.log("remember to put in the filter here"); // TODO
//   simulation.force(label, force)
// }
// REVIEW a whole group of side effecting function
exports.pinNode_ = fx => fy => node => {
  node.fx = fx
  node.fy = fy
  return node
}
exports.pinNamedNode_ = name => fx => fy => node => {
  if (node.name === name) {
    node.fx = fx
    node.fy = fy
  }
  return node
}
exports.pinTreeNode_ = node => { node.fx = node.treeX; node.fy = node.treeY; return node } // if treeX/Y is null, no harm!
exports.setInSimNodeFlag_ = node => { node.inSim = true; return node } 
exports.unsetInSimNodeFlag_ = node => { node.inSim = false; return node  }
exports.unpinNode_ = node => { node.fx = null; node.fy = null; return node }
// *****************************************************************************************************************
// ************************** functions from d3js Hierarchy module         *****************************************
// *****************************************************************************************************************
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
exports.ancestors_ = tree => tree.ancestors()
exports.descendants_ = tree => tree.descendants()
exports.find_ = tree => filter => tree.find(filter)
exports.getClusterLayoutFn_ = () => d3.cluster()
exports.getTreeLayoutFn_ = () => d3.tree()
exports.hasChildren_ = d => (d.children === 'undefined') ? false : true 
exports.getHierarchyValue_ = d => (d.value === 'undefined') ? null : d.value // returns a Nullable Number 
exports.getHierarchyChildren_ = d => !d.children ? [] : d.children 
exports.getHierarchyParent_ = d => !d.parent ? [] : d.parent // don't think this can ever be null in valid hierarchy node but this gives us confidence that PureScript type is right 
exports.hierarchyFromJSON_ = json => d3.hierarchy(json)
exports.hNodeDepth_ = node => node.depth
exports.hNodeHeight_ = node => node.height
exports.hNodeX_ = node => node.x
exports.hNodeY_ = node => node.y
exports.leaves_ = tree => tree.leaves()
exports.links_ = tree => tree.links()
exports.path_ = from => to => tree.path(from, to)
exports.runLayoutFn_ = layout => root => layout(root)
exports.sharesParent_ = a => b => a.parent == b.parent
exports.treeSetNodeSize_ = tree => widthHeight => tree.nodeSize(widthHeight)
exports.treeSetSeparation_ = tree => separationFn => tree.separation(separationFn)
exports.treeSetSize_ = tree => widthHeight => tree.size(widthHeight)
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
      const result =
        b.height - a.height || a.data.name.localeCompare(b.data.name)
      return result
    })
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
    // console.log(`FFI: node ${d} (${min_x}, ${min_y}) (${max_x}, ${max_y})`);
  })      
  return { xMin: min_x, xMax: max_x, yMin: min_y, yMax: max_y }
}
exports.linkHorizontal_ = d3
  .linkHorizontal()
  .x(d => d.y)
  .y(d => d.x)
exports.linkHorizontal2_ = d3
  .linkHorizontal()
  .x(d => d.x)
  .y(d => d.y)
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
exports.linkRadial_ = angleFn => radiusFn =>
  d3
    .linkRadial()
    .angle(angleFn)
    .radius(radiusFn)
    exports.autoBox_ = () => {
      document.body.appendChild(this)
      const { x, y, width, height } = this.getBBox()
      document.body.removeChild(this)
      return [x, y, width, height]
    }
// *****************************************************************************************************************
// ************************** functions from d3js zoom module         *****************************************
// *****************************************************************************************************************

exports.d3AttachZoomDefaultExtent_ = selection => config => {
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
}
exports.d3AttachZoom_ = selection => config => {
  selection.call(
    d3
      .zoom()
      .extent(config.extent) // extent is [ [], [] ]
      .scaleExtent(config.scaleExtent)
      .on(`zoom.${config.name}`, (event) => { config.target.attr('transform', event.transform) })
  )
  return selection
}
exports.showAttachZoomDefaultExtent_ = selection => config => { return `\t${selection}.call(zoom ${config})` }
exports.showAttachZoom_ = selection => config => {
  return `\t${selection}.call(zoom ${config})`
}
