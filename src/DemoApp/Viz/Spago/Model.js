// ********************************************************************************
// functionality to "explode" a package by removing it and replacing with it's constituent modules
// ********************************************************************************
export function explodePackages_(event) {
  return simulation => id => nodetype => {
    event.stopPropagation()
    if (nodetype === "package") {
      console.log('clicked on a package');
      return
    }
  };
}
// ********************************************************************************
// functionality to spotlight the immediate graph neighbours of a module 
// (and only modules at present)
// ********************************************************************************
let spotlitSelection
let spotlitNode
let sourcesSelection
let targetSelection
let spotlitID
let spotlit = false;

export function cancelSpotlight_(simulation) {
  console.log("cancelling spotlight");
  if (spotlit) {
    unSpotlightNeighbours_(simulation)
  }
}

export function toggleSpotlight_(event) {
  return simulation => id => nodetype => {
    event.stopPropagation()
    if (nodetype === "package") {
      return
    }
    if ((spotlit && id !== spotlitID)) {
      console.log(`changing spotlight from ${spotlitID} to ${id}`);
      unSpotlightNeighbours_(simulation)
      spotlightNeighbours_(simulation, id, nodetype)
    } else if (spotlit && id === spotlitID) {
      console.log(`cancelling spotlight on ${spotlitID}`);
      unSpotlightNeighbours_(simulation)
    } else {
      console.log(`setting a spotlight on ${id}`);
      spotlightNeighbours_(simulation, id, nodetype)
    }
  };
}

// TODO implement this as purescript-function-called from FFI??
spotlightNeighbours_ = (simulation, id, nodetype) => {
  // else
  spotlit = true;
  spotlitID = id
  simulation.stop()
  svg = d3.select('div.svg-container svg')
  nodeSelection = svg.selectAll('g.nodes g')
  spotlitSelection = nodeSelection.filter((d, i) => d.id == id)

  spotlitNode = spotlitSelection.node()
  // check if fx already set, don't reset if so
  spotlitNode.__data__.fx = spotlitNode.__data__.fx || spotlitNode.__data__.x
  spotlitNode.__data__.fy = spotlitNode.__data__.fy || spotlitNode.__data__.y
  const targets = spotlitNode.__data__.links.targets
  const sources = spotlitNode.__data__.links.sources

  sourcesSelection = nodeSelection.filter((d, i) => sources.includes(d.id))
  targetSelection = nodeSelection.filter((d, i) => targets.includes(d.id))

  svg.classed('spotlight', true)
  sourcesSelection.classed('source', true)
  targetSelection.classed('target', true)
  spotlitSelection.classed('spotlight', true)
  spotlitSelection.classed('source target', false)

  simulation.force(
    'collide',
    d3
      .forceCollide()
      .radius(d =>
        sources.includes(d.id) || targets.includes(d.id) ? d.r * 4 : (d.id === d.containerID) ? 10.0 : d.r
      )
  )
  simulation.alpha(1).restart()
}
unSpotlightNeighbours_ = (simulation) => {
  simulation.stop()
  svg.classed('spotlight', false)
  spotlitNode.__data__.fx = null
  spotlitNode.__data__.fy = null
  spotlitSelection.classed('spotlight', false)
  sourcesSelection.classed('source', false)
  targetSelection.classed('target', false)
  // move the radii back to what they were before
  simulation.force(
    'collide',
    d3.forceCollide().radius(d => (d.id === d.containerID) ? 10.0 : d.r)
  )
  simulation.restart()
  spotlit = false
}