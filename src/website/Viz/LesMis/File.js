"use strict"

export function readJSONJS_(filecontents) { return decodeFile(filecontents) }

const decodeFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const links = json.links.map(d => Object.create(d))

  // D3 flattens structures, so create flat nodes with simulation fields initialized
  const nodes = json.nodes.map(d => ({
    id: d.id,
    group: d.group,
    x: 0.0,
    y: 0.0,
    vx: 0.0,
    vy: 0.0,
    fx: null,
    fy: null
  }))

  return { links: links, nodes: nodes }
}
