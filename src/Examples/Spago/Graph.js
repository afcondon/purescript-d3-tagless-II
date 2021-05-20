"use strict"
var svg;
var spotlitNode;
var sourceNodes;
var targetNodes;
var highlightLinks;
var nodeSelection;

//  markAsSpotlit_ :: String -> Unit
exports.markAsSpotlit_ = id => simulation => selection => filterlinks => sources => targets => {
  svg = d3.select("div#spago svg")

  nodeSelection = svg.selectAll("g.nodes g")

  highlightLinks = selection.selectAll("line");

  // Apply the general update pattern to the links.
  highlightLinks = highlightLinks.data(filterlinks, d => d.index);
  highlightLinks.exit().remove();
  highlightLinks = highlightLinks.enter().append("line").merge(highlightLinks);

  // Update and restart the simulation.
  // simulation.nodes(nodes);
  simulation.stop()
  simulation.force("links").links(filterlinks);
  simulation.force("collide",
    d3
      .forceCollide()
      .radius(d => d.id == id ? 100 : 50 )
      .iterations(5)
  )
  spotlitNode = nodeSelection.filter((d,i) => d.id == id)
  sourceNodes = nodeSelection.filter((d,i) => targets.includes(d.id))
  targetNodes = nodeSelection.filter((d,i) => sources.includes(d.id))
  
  svg.classed("spotlight", true)
  sourceNodes.classed("node source", true).raise()
  targetNodes.classed("node target", true).raise()
  for (const element of nodeSelection) {
    if(element.__data__.id != id) {
      element.__data__._fx = element.__data__.fx
      element.__data__._fy = element.__data__.fy
      delete element.__data__.fx
      delete element.__data__.fy
    }
  }
  spotlitNode.classed("node spotlight", true).raise()
  
  highlightLinks.classed("source",true)

  simulation.restart();
}

//  markAsSpotlit_ :: String -> Unit
exports.removeSpotlight_ = selection => links => {
  for (const element of nodeSelection) {
    element.__data__.fx = element.__data__._fx
    element.__data__.fy = element.__data__._fy
  }

  var restorationLinks = selection.selectAll("line");

  // Apply the general update pattern to the links.
  restorationLinks = restorationLinks.data(links, d => d.index);
  restorationLinks.exit().remove(); // links that were only in "pruned links" will go away again
  restorationLinks = restorationLinks.enter().append("line").merge(restorationLinks);
  
  svg.classed("spotlight", false)
  spotlitNode.classed("spotlight", false)
  sourceNodes.classed("source", false)
  targetNodes.classed("target", false)
  highlightLinks.classed("source",false)
}

