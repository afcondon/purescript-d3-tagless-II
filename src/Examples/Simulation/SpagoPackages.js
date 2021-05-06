"use strict"

exports.readModelData_ = modulesBody => packagesBody => lsdepsBody => {
  const modules  = decodeModulesFile(modulesBody);
  const packages = decodePackagesFile(packagesBody);
  const lsDeps   = decodeLsDepsFile(lsdepsBody);

  return { modules, packages, lsDeps }
}

// module has key, path & depends
const decodeModulesFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const modules = Object.keys(json).map(key => { return { key: key, depends: json[key].depends, path: json[key].path }; })

  return modules;
}

// package has key and depends
const decodePackagesFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const packages = Object.keys(json).map(key => { return { key: key, depends: json[key].depends }; })

  return packages;
}
// lsdep has key === packageName, version, repo { tag, contents }
const decodeLsDepsFile = function (filecontents) {
  const jsonlines = splitIntoLines(filecontents)
  jsonlines.length = jsonlines.length - 1
  var objectArray = jsonlines.map(d => JSON.parse(d))
  return objectArray;
}

function splitIntoLines (str) {
  // See http://www.unicode.org/reports/tr18/#RL1.6
  return str.split(/\r\n|[\n\v\f\r\u0085\u2028\u2029]/);
}

// these are both essentially just unsafeCoerce
// makeGraphLinks_ :: forall r id. Array { source :: id, target :: id | r } -> Array GraphLink_
exports.makeGraphLinks_ = (links) => links
// makeGraphNodes_ :: forall r id. Array { id :: id | r }                   -> Array GraphNode_
exports.makeGraphNodes_ = (nodes) => nodes


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

// pinNode_ :: Number -> Number -> GraphNode_ -> Unit
exports.pinNode_ = fx => fy => node => {
  node.fx = fx;
  node.fy = fy;
  delete node.vx; // which would otherwise result in this being positioned AND fixed
}

// unpinNode_ :: GraphNode_ -> Unit
exports.unpinNode_ = node => {
  delete node.fx;
  delete node.fy;
}

exports.nanNodes_ = nodes => {
  for (let index = 0; index < nodes.length; index++) {
    nodes[index].vx = NaN;
  }
}
