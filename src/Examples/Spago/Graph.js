"use strict"
var svg;
var spotlitNode;
var sourceNodes;
var targetNodes;
var highlightLinks;
var oldLinks;

//  markAsSpotlit_ :: String -> Unit
exports.markAsSpotlit_ = id => simulation => selection => filterlinks => sources => targets => {
  svg = d3.select("div#spago svg")

  const nodeSelection = svg.selectAll("g.nodes g")

  highlightLinks = selection.selectAll("line");

  // Apply the general update pattern to the links.
  oldLinks = selection
  highlightLinks = highlightLinks.data(filterlinks, d => d.index);
  highlightLinks.exit().remove();
  highlightLinks = highlightLinks.enter().append("line").merge(highlightLinks);

  // Update and restart the simulation.
  // simulation.nodes(nodes);
  simulation.stop()
  simulation.force("links").links(filterlinks);
  simulation.alpha(1).restart();

  spotlitNode = nodeSelection.filter((d,i) => d.id == id)
  sourceNodes = nodeSelection.filter((d,i) => targets.includes(d.id))
  targetNodes = nodeSelection.filter((d,i) => sources.includes(d.id))
  
  svg.classed("spotlight", true)
  sourceNodes.classed("node source", true).raise()
  targetNodes.classed("node target", true).raise()
  spotlitNode.classed("node spotlight", true).raise()
  for (const element of sourceNodes) {
    element.__data__._fx = element.__data__.fx
    element.__data__._fy = element.__data__.fy
    delete element.__data__.fx
    delete element.__data__.fy
  }
  for (const element of targetNodes) {
    element.__data__._fx = element.__data__.fx
    element.__data__._fy = element.__data__.fy
    delete element.__data__.fx
    delete element.__data__.fy
  }

  highlightLinks.classed("source",true)
}

//  markAsSpotlit_ :: String -> Unit
exports.removeSpotlight_ = () => {
  for (const element of sourceNodes) {
    element.__data__.fx = element.__data__._fx
    element.__data__.fy = element.__data__._fy
  }
  for (const element of targetNodes) {
    element.__data__.fx = element.__data__._fx
    element.__data__.fy = element.__data__._fy
  }

  svg.classed("spotlight", false)
  spotlitNode.classed("spotlight", false)
  sourceNodes.classed("source", false)
  targetNodes.classed("target", false)
  highlightLinks.classed("source",false)
}

