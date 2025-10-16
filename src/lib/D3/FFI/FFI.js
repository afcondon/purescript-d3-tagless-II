const debug = false
export const emptyD3Data_ = null
export function d3Append_(element) { return selection => { return selection.append(element) } }
export function d3Data_(data) { return selection => { return selection.data(data) } }
export function d3DataWithKeyFunction_(data) { return keyFn => selection => { return selection.data(data, keyFn) } }
export function d3EnterAndAppend_(element) { return selection => { return selection.enter().append(element) } }
export function d3GetExitSelection_(selection) { return selection.exit() }
export function d3GetEnterSelection_(selection) { return selection.enter() }
export function d3GetSelectionData_(selection) { return selection.data() }
export function d3FilterSelection_(selection) { return selector => selection.filter(selector) }
export function d3LowerSelection_(selection) { return selection.lower() }
export function d3MergeSelectionWith_(enter) { return update => { return enter.merge(update); } }
export function d3OrderSelection_(selection) { return selection.order() }
export function d3RaiseSelection_(selection) { return selection.raise() }
export function d3RemoveSelection_(selection) { return selection.remove() }
export function d3SelectAllInDOM_(selector) { return d3.selectAll(selector) }
export function d3SelectFirstInDOM_(selector) { return d3.select(selector) }
export function d3SelectionIsEmpty_(selection) { return selection.empty() }
export function d3SelectionSelect_(selector) { return selection => { return selection.select(selector) } }
export function d3SelectionSelectAll_(selector) { return selection => { return selection.selectAll(selector) } }
export function d3SetAttr_(name) { return value => selection => { return selection.attr(name, value) } }
export function d3SetHTML_(value) { return selection => { return selection.html(value) } }
export function d3SetProperty_(value) { return selection => { return selection.property(value) } }
export function d3SetText_(value) { return selection => { return selection.text(value) } }
export function d3SortSelection_(selection) { return compare => selection.sort(compare) }
export function simulationDrag_(label) { return selection => simulation => dragFn => selection.call(dragFn(label, simulation)) }
export function disableDrag_(selection) { return selection.on('.drag', null) }
export function getIndexFromDatum_(datum) { return (typeof datum.index == `undefined`) ? "?" : datum.index }
export function selectionOn_(selection) { return event => callback => { return selection.on(event, callback) } }
export function d3AddTransition_(selection) {
  return transition => {
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
}
// *****************************************************************************************************************
// *****  there will either need to be quite a range of these functions or a way of writing them in Purs     *******
// *****  this is really down in the weeds of D3 without supporting abstractions in the PS library           *******
// *****  CONCRETE EXAMPLE: this defaults to updating fx but in Spago example position is on parent, using   *******
// *****  transforms to move both circle and label together (only way to position a <group> in SVG)
// *****************************************************************************************************************
export function simdrag(label, simulation) {
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
export const linksForceName = "links"
export const dummyForceHandle_ = null
export function disableTick_(simulation) { return name => { return simulation.on('tick.' + name, () => null) } }
export function forceCenter_() { return d3.forceCenter() }
export function forceCollideFn_() { return d3.forceCollide() }
export function forceCustom_(forceFn) { return forceFn() }
export function forceLink_() { return d3.forceLink().id(d => d.id) }
export function forceMany_() { return d3.forceManyBody() }
export function forceRadial_() { return d3.forceRadial() }
export function forceX_() { return d3.forceX() }
export function forceY_() { return d3.forceY() }
export function getLinksFromForce_(linkForce) { return linkForce.links() }
export function getNodes_(simulation) { return simulation.nodes() }
export function keyIsID_(d) {
  // console.log(`FFI: looking up the id of node: ${d.id}`);
  return d.id;
}
export function keyIsSourceTarget_(d) {
  // console.log(`FFI: looking up the id of node: ${[d.source, d.target]}`);
  return [d.source, d.target];
}
export function setAlpha_(simulation) {
  return alpha => {
    console.log(`FFI: setting simulation.alpha to ${alpha}`);
    simulation.alpha(alpha)
  }
}
export function setAlphaDecay_(simulation) { return alphaDecay => simulation.alphaDecay(alphaDecay) }
export function setAlphaMin_(simulation) { return alphaMin => simulation.alphaMin(alphaMin) }
export function setAlphaTarget_(simulation) { return alphaTarget => simulation.alphaTarget(alphaTarget) }
export function setAsNullForceInSimulation_(simulation) { return label => simulation.force(label, null) }
export function setForceCx_(force) { return attr => force.cx(attr) }
export function setForceCy_(force) { return attr => force.cy(attr) }
export function setForceDistance_(force) { return attr => force.distance(attr) }
export function setForceDistanceMax_(force) { return attr => force.distanceMax(attr) }
export function setForceDistanceMin_(force) { return attr => force.distanceMin(attr) }
export function setForceIterations_(force) { return attr => force.iterations(attr) }
export function setForceRadius_(force) { return attr => force.radius(attr) }
export function setForceStrength_(force) { return attr => force.strength(attr) }
export function setForceTheta_(force) { return attr => force.theta(attr) }
export function setForceX_(force) { return attr => force.x(attr) }
export function setForceY_(force) { return attr => force.y(attr) }
export function setLinksKeyFunction_(force) { return attr => force.id(attr) }
export function setVelocityDecay_(simulation) { return velocityDecay => simulation.velocityDecay(velocityDecay) }
export function startSimulation_(simulation) {
  console.log(`FFI: restarting the simulation, alpha is: ${simulation.alpha()}`);
  simulation.restart()
}
export function stopSimulation_(simulation) { return simulation.stop() }
export function initSimulation_(config) {
  return keyFn => {
    const simulation = d3
      .forceSimulation([])
      .force(linksForceName, d3.forceLink([]).id(keyFn))
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
}
export function configSimulation_(simulation) {
  return config => {
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
}
export function readSimulationVariables(simulation) {
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

export function d3PreserveSimulationPositions_(selection) {
  return nodedata => keyFn => {
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
}
export function d3PreserveLinkReferences_(link) {
  return links => {
    const old = new Map(link.data().map(d => [getLinkID_(d), d]));
    let updatedLinkData = links.map(d => Object.assign(old.get(getLinkID_(d)) || d, {}));
    // now, based on link signature, we should really de-swizzle here? and we may HAVE TO do so
    return updatedLinkData
  }
}
export function getIDsFromNodes_(nodes) {
  return keyFn => {
    const keys = [];
    for (let i = 0; i < nodes.length; i++) {
      keys[i] = keyFn(nodes[i]);
    }
    return keys
  }
}

export function setNodes_(simulation) {
  return nodes => {
    console.log(`FFI: setting nodes in simulation, there are ${nodes.length} nodes`);
    simulation.nodes(nodes)
    return simulation.nodes()
  }
}
// we're going to always use the same name for the links force denominated by the linksForceName string
export function setLinks_(simulation) {
  return links => {
    console.log(`FFI: setting links in simulation, there are ${links.length} links`);
    simulation.force(linksForceName).links(links)
  }
}
// returns array of links with ids replaced by object references, invalid links are discarded
export function swizzleLinks_(links) {
  return simNodes => keyFn => {
    console.log(`FFI: swizzling links in simulation, there are ${links.length} links`);
    const nodeById = new Map(simNodes.map(d => [keyFn(d), d])); // creates a map from our chosen id to the old obj reference
    // we could use the copy approach from d3PreserveSimulationPositions here so that links animate
    const swizzledLinks = links.filter((link, index, arr) => {
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
}
export function unsetLinks_(simulation) {
  const linkForce = d3.forceLink([])
  console.log('FFI: removing all links from simulation');
  simulation.force(linksForceName, linkForce)
  return simulation
}
// this will work on both swizzled and unswizzled links
export function getLinkID_(keyFn) {
  return link => { // version for generating an ID for the link object
    const sourceID = (typeof link.source == `object`) ? keyFn(link.source) : link.source
    const targetID = (typeof link.target == `object`) ? keyFn(link.target) : link.target
    return sourceID + "-" + targetID
  }
}
export function getLinkIDs_(keyFn) {
  return link => { // version for generating the pairs to check against node ids for pruning
    const sourceID = (typeof link.source == `object`) ? keyFn(link.source) : link.source
    const targetID = (typeof link.target == `object`) ? keyFn(link.target) : link.target
    return { sourceID, targetID }
  }
}
export function getLinksFromSimulation_(simulation) {
  linksForce = simulation.force(linksForceName)
  if (typeof linksForce === `undefined`) {
    return [] // either the force wasn't found, or the force wasn't a links force
  }
  const result = linksForce.links()
  if (typeof result === `undefined`) {
    return []
  }
  return result
}
export function onTick_(simulation) {
  return name => tickFn => {
    var result = simulation.on('tick.' + name, () => {
      tickFn()
    })
    return result;
  }
}
export function defaultNodeTick_(label) {
  return simulation => nodeSelection => {
    simulation.on('tick.' + label, () => {
      nodeSelection.attr('cx', d => d.x)
        .attr('cy', d => d.y)
    })
  }
}
export function defaultLinkTick_(label) {
  return simulation => linksShown => {
    simulation.on('tick.' + label, () => {
      linksShown.attr("x1", d => d.source.x)
        .attr("y1", d => d.source.y)
        .attr("x2", d => d.target.x)
        .attr("y2", d => d.target.y);
    })
  }
}
export function lookupForceByName_(simulation) {
  return name => {
    let lookup = simulation.force(name)
    if (typeof lookup === `undefined`) {
      return null;
    }
    return lookup;
  }
}
export function removeFixForceXY_(simulation) {
  return filterFn => {
    let filteredNodes = simulation.nodes().filter(filterFn)
    for (let index = 0; index < filteredNodes.length; index++) {
      // console.log(`removing FixForceXY from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fx = null;
      filteredNodes[index].fy = null;
    }
  }
}
export function removeFixForceX_(simulation) {
  return filterFn => {
    let filteredNodes = simulation.nodes().filter(filterFn)
    for (let index = 0; index < filteredNodes.length; index++) { // TODO do this with map
      // console.log(`removing FixForceX from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fx = null;
    }
  }
}
export function removeFixForceY_(simulation) {
  return filterFn => {
    let filteredNodes = simulation.nodes().filter(filterFn)
    for (let index = 0; index < filteredNodes.length; index++) {
      // console.log(`removing FixForceY from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fy = null;
    }
  }
}
export function applyFixForceInSimulationXY_(simulation) {
  return label => fn => filterFn => {

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
}
export function applyFixForceInSimulationX_(simulation) {
  return label => fn => filterFn => {
    let nodes = simulation.nodes()
    for (let index = 0; index < nodes.length; index++) {
      if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
        let position = fn(nodes[index])
        nodes[index].fx = position.x
      }
    }
  }
}
export function applyFixForceInSimulationY_(simulation) {
  return label => fn => filterFn => {
    let nodes = simulation.nodes()
    for (let index = 0; index < nodes.length; index++) {
      if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
        let position = fn(nodes[index])
        nodes[index].fy = position.y;
      }
    }
  }
}
export function putForceInSimulation_(simulation) {
  return label => force => {
    // console.log(`FFI: Putting ${label} force in the simulation`);
    simulation.force(label, force)
  }
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
export function pinNode_(fx) {
  return fy => node => {
    node.fx = fx
    node.fy = fy
    return node
  }
}
export function pinNamedNode_(name) {
  return fx => fy => node => {
    if (node.name === name) {
      node.fx = fx
      node.fy = fy
    }
    return node
  }
}
export function pinTreeNode_(node) { node.fx = node.treeX; node.fy = node.treeY; return node } // if treeX/Y is null, no harm!
export function setInSimNodeFlag_(node) { node.inSim = true; return node }
export function unsetInSimNodeFlag_(node) { node.inSim = false; return node }
export function unpinNode_(node) { node.fx = null; node.fy = null; return node }
// *****************************************************************************************************************
// ************************** functions from d3js Hierarchy module         *****************************************
// *****************************************************************************************************************
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
export function ancestors_(tree) { return tree.ancestors() }
export function descendants_(tree) { return tree.descendants() }
export function find_(tree) { return filter => tree.find(filter) }
export function getClusterLayoutFn_() { return d3.cluster() }
export function getTreeLayoutFn_() { return d3.tree() }
export function hasChildren_(d) { return (d.children === 'undefined') ? false : true }
export function getHierarchyValue_(d) { return (d.value === 'undefined') ? null : d.value } // returns a Nullable Number 
export function getHierarchyChildren_(d) { return !d.children ? [] : d.children }
export function getHierarchyParent_(d) { return !d.parent ? [] : d.parent } // don't think this can ever be null in valid hierarchy node but this gives us confidence that PureScript type is right 
export function hierarchyFromJSON_(json) { return d3.hierarchy(json) }
export function hNodeDepth_(node) { return node.depth }
export function hNodeHeight_(node) { return node.height }
export function hNodeX_(node) { return node.x }
export function hNodeY_(node) { return node.y }
export function leaves_(tree) { return tree.leaves() }
export function links_(tree) { return tree.links() }
export function path_(from) { return to => tree.path(from, to) }
export function runLayoutFn_(layout) { return root => layout(root) }
export function sharesParent_(a) { return b => a.parent == b.parent }
export function treeSetNodeSize_(tree) { return widthHeight => tree.nodeSize(widthHeight) }
export function treeSetSeparation_(tree) { return separationFn => tree.separation(separationFn) }
export function treeSetSize_(tree) { return widthHeight => tree.size(widthHeight) }
export function treeSortForCirclePack_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.value - a.value
    })
}
export function treeSortForTreeMap_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.height - a.height || b.value - a.value
    })
}
export function treeSortForTree_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.height - a.height || a.id.localeCompare(b.id)
    })
}
export function treeSortForTree_Spago(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      const result =
        b.height - a.height || a.data.name.localeCompare(b.data.name)
      return result
    })
}
export function treeMinMax_(root) {
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
export const linkHorizontal_ = d3
  .linkHorizontal()
  .x(d => d.y)
  .y(d => d.x)
export const linkHorizontal2_ = d3
  .linkHorizontal()
  .x(d => d.x)
  .y(d => d.y)
export const linkVertical_ = d3
  .linkVertical()
  .x(d => d.x)
  .y(d => d.y)
export function linkClusterHorizontal_(levelSpacing) {
  return d =>
    `M${d.target.y}, ${d.target.x}
   C${d.source.y + levelSpacing / 2},${d.target.x}
   ${d.source.y + levelSpacing / 2},${d.source.x}
   ${d.source.y},${d.source.x}`
}
export function linkClusterVertical_(levelSpacing) {
  return d =>
    `M${d.target.x}, ${d.target.y}
   C${d.target.x}, ${d.source.y + levelSpacing / 2}
   ${d.source.x},${d.source.y + levelSpacing / 2}
   ${d.source.x},${d.source.y}`
}
export function linkRadial_(angleFn) {
  return radiusFn =>
    d3
      .linkRadial()
      .angle(angleFn)
      .radius(radiusFn)
}
export function autoBox_() {
  document.body.appendChild(this)
  const { x, y, width, height } = this.getBBox()
  document.body.removeChild(this)
  return [x, y, width, height]
}
// *****************************************************************************************************************
// ************************** functions from d3js Chord module         *****************************************
// *****************************************************************************************************************
export function chordLayout_(matrix) {
  return d3.chord()(matrix);
}
export function chordGroups_(chordLayout) { return chordLayout.groups }
export function chordArray_(chordLayout) {
  return Array.from(chordLayout);
}
export function ribbonGenerator_() { return d3.ribbon() }
export function arcGenerator_() { return d3.arc() }
export function ribbonPath_(generator) { return chord => generator(chord) }
export function arcPath_(generator) { return group => generator(group) }
export function setRibbonRadius_(generator) { return radius => { generator.radius(radius); return generator } }
export function setArcInnerRadius_(generator) { return radius => { generator.innerRadius(radius); return generator } }
export function setArcOuterRadius_(generator) { return radius => { generator.outerRadius(radius); return generator } }
// *****************************************************************************************************************
// ************************** functions from d3js Pack (bubble) module         *****************************************
// *****************************************************************************************************************
export function packLayout_() { return d3.pack() }
export function packSetSize_(layout) { return width => height => { layout.size([width, height]); return layout } }
export function packSetPadding_(layout) { return padding => { layout.padding(padding); return layout } }
export function runPackLayout_(layout) { return root => layout(root) }
export function hNodeR_(node) { return node.r }
// *****************************************************************************************************************
// ************************** functions from d3js zoom module         *****************************************
// *****************************************************************************************************************

export function d3AttachZoomDefaultExtent_(selection) {
  return config => {
    function zoomed({ transform }) {
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
}
export function d3AttachZoom_(selection) {
  return config => {
    selection.call(
      d3
        .zoom()
        .extent(config.extent) // extent is [ [], [] ]
        .scaleExtent(config.scaleExtent)
        .on(`zoom.${config.name}`, (event) => { config.target.attr('transform', event.transform) })
    )
    return selection
  }
}
export function showAttachZoomDefaultExtent_(selection) { return config => { return `\t${selection}.call(zoom ${config})` } }
export function showAttachZoom_(selection) {
  return config => {
    return `\t${selection}.call(zoom ${config})`
  }
}
