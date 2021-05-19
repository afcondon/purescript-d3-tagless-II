"use strict"
var svg;
var spotlitNode;
var sourceNodes;
var targetNodes;


//  markAsSpotlit_ :: String -> Unit
exports.markAsSpotlit_ = id => sources => targets => {
  svg = d3.select("div#spago svg")
  const selection = svg.selectAll("g.nodes g")

  spotlitNode = selection.filter((d,i) => d.index == id)
  sourceNodes = selection.filter((d,i) => targets.includes(d.id))
  targetNodes = selection.filter((d,i) => sources.includes(d.id))

  svg.classed("spotlight", true)
  spotlitNode.classed("node spotlight", true).raise()
  sourceNodes.classed("node source", true).raise()
  targetNodes.classed("node target", true).raise()
}

//  markAsSpotlit_ :: String -> Unit
exports.removeSpotlight_ = () => {
  svg.classed("spotlight", false)
  spotlitNode.classed("spotlight", false)
  sourceNodes.classed("source", false)
  targetNodes.classed("target", false)
}

