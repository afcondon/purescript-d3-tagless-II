"use strict"
var svg;
var spotlitNode;
var sourceNodes;
var targetNodes;
var sourceLinks;
var targetLinks;


//  markAsSpotlit_ :: String -> Unit
exports.markAsSpotlit_ = id => sources => targets => {
  svg = d3.select("div#spago svg")
  const nodeSelection = svg.selectAll("g.nodes g")
  const linkSelection = svg.selectAll("g.links line")

  spotlitNode = nodeSelection.filter((d,i) => d.id == id)
  sourceNodes = nodeSelection.filter((d,i) => targets.includes(d.id))
  targetNodes = nodeSelection.filter((d,i) => sources.includes(d.id))

  sourceLinks = linkSelection.filter((d,i) => d.source.id == id)
  targetLinks = linkSelection.filter((d,i) => d.target.id == id)
  
  svg.classed("spotlight", true)
  spotlitNode.classed("node spotlight", true).raise()
  sourceNodes.classed("node source", true).raise()
  targetNodes.classed("node target", true).raise()
  sourceLinks.classed("source",true)
  targetLinks.classed("target",true)
}

//  markAsSpotlit_ :: String -> Unit
exports.removeSpotlight_ = () => {
  svg.classed("spotlight", false)
  spotlitNode.classed("spotlight", false)
  sourceNodes.classed("source", false)
  targetNodes.classed("target", false)
  sourceLinks.classed("source",false)
  targetLinks.classed("target",false)
}

