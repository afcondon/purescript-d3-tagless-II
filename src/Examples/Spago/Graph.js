"use strict"
var svg;
var spotlitNode;
var sourceNodes;
var targetNodes;
var otherNodes;
var highlightLinks;
var nodeSelection;
var running = false;

exports.toggleSimulation_ = simulation => {
  if (running) {
    simulation.restart()
    running = true;
  } else {
    simulation.stop()
    running = false;
  }
}

exports.unfixElementsNode = element => {
  element.__data__._fx = element.__data__.fx
  element.__data__._fy = element.__data__.fy
  delete element.__data__.fx
  delete element.__data__.fy
}
exports.refixElementsNode = element => {
  element.__data__.fx = element.__data__._fx
  element.__data__.fy = element.__data__._fy
}

//  markAsSpotlit_ :: String -> Unit
exports.markAsSpotlit_ = id => simulation => selection => filterlinks => sources => targets => {
  simulation.stop()

  svg = d3.select("div#spago svg")

  nodeSelection = svg.selectAll("g.nodes g")

  highlightLinks = selection.selectAll("g.links line").filter (d => d.source.id == id || d.target.id == id);
  highlightLinks.classed("highlight",true) // not distinguishing source and target RN

  simulation.force("collide",
    d3
      .forceCollide()
      .radius(d => (sources.includes(d.id) || targets.includes(d.id)) ? (d.radius * 2) : d.radius )
  )
  spotlitNode = nodeSelection.filter((d,i) => d.id == id)
  sourceNodes = nodeSelection.filter((d,i) => targets.includes(d.id))
  targetNodes = nodeSelection.filter((d,i) => sources.includes(d.id))
  otherNodes  = nodeSelection.filter((d,i) => !((d.id == id)||(targets.includes(d.id))||(sources.includes(d.id))))
  
  svg.classed("spotlight", true)
  sourceNodes.classed("source", true).raise()
  targetNodes.classed("target", true).raise()
  spotlitNode.classed("spotlight", true).raise()

  for (const element of otherNodes) {
    if(element.__data__.id != id) {
      exports.unfixElementsNode(element)
    }
  }

  simulation.restart().alpha(0.2);
}

//  markAsSpotlit_ :: String -> Unit
exports.removeSpotlight_ = simulation => selection => links => {
  simulation.stop();
  
  nodeSelection = svg.selectAll("g.nodes g")
  for (const element of nodeSelection) {
    exports.refixElementsNode(element)
  }
  // move the radii back to what they were before
  simulation.force("collide", d3.forceCollide().radius(d => d.radius ) )
  
  svg.classed("spotlight", false)
  spotlitNode.classed("spotlight", false)
  sourceNodes.classed("source", false)
  targetNodes.classed("target", false)
  highlightLinks.classed("highlight",false)

  simulation.restart();
}

