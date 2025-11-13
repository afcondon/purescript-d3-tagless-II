"use strict"

export function readJSONJS_(filecontents) { return decodeFile(filecontents) }

const decodeFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const links = json.links.map(d => Object.create(d))

  // Create nodes with flat structure - D3 extends objects with simulation fields
  // No nesting needed - SimulationNode is row-polymorphic
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
