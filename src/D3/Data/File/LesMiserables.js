"use strict"

exports.readJSONJS = filecontents => decodeFile(filecontents)

const decodeFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const links = json.links.map(d => Object.create(d))
  const nodes = json.nodes.map(d => Object.create({data: d}))
  // return { links: links, nodes: nodes }
  return { links: links, nodes: json.nodes }
}
