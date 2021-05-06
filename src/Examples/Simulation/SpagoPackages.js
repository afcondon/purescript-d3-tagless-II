"use strict"

//  markAsSpotlit_ :: String -> Unit
exports.markAsSpotlit_ = id => sources => targets => {
  const svg = d3.select("div#spago svg")
  const selection = svg.selectAll("g.nodes g")

  const spotlitNode = selection.filter((d,i) => d.id == id)
  const sourceNodes = selection.filter((d,i) => targets.includes(d.id))
  const targetNodes = selection.filter((d,i) => sources.includes(d.id))

  svg.classed("spotlight", true)
  spotlitNode.classed("node spotlight", true)
  sourceNodes.classed("node source", true)
  targetNodes.classed("node target", true)
}

//  markAsSpotlit_ :: String -> Unit
exports.removeSpotlight_ = id => sources => targets => {
  const svg = d3.select("div#spago svg")
  const selection = svg.select("g.nodes g.node")

  const spotlitNode = selection.filter((d,i) => d.id == id)
  const sourceNodes = selection.filter((d,i) => targets.includes(d.id))
  const targetNodes = selection.filter((d,i) => sources.includes(d.id))

  svg.classed("spotlight", false)
  spotlitNode.classed("node", false)
  sourceNodes.classed("node", false)
  targetNodes.classed("node", false)
}

