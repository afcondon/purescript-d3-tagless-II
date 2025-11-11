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

// ********************************************************************************
// Gentle transition for pinned layouts (grid, trees)
// ********************************************************************************
// Transition nodes to pinned positions with smooth D3 animation
// Works with Group elements positioned via transform attribute
// Used for scene transitions: PackageGrid, RadialTree, HorizontalTree, VerticalTree
export const transitionNodesToPinnedPositions_ = (svgSelector) => (nodeSelector) => (linkSelector) => (pinnedNodes) => (onComplete) => () => {
  console.log(`Starting pinned layout transition for ${pinnedNodes.length} nodes`);
  console.log(`SVG: ${svgSelector}, Nodes: ${nodeSelector}, Links: ${linkSelector}`);

  // Select the SVG and node/link elements
  const svg = d3.select(svgSelector);
  const nodes = svg.selectAll(nodeSelector);
  const links = svg.selectAll(linkSelector);

  if (nodes.empty()) {
    console.error(`Could not find ${nodeSelector} elements to transition`);
    onComplete();
    return;
  }

  console.log(`Found ${nodes.size()} nodes and ${links.size()} links to transition`);

  // Create transition (1500ms duration)
  const t = d3.transition().duration(1500);

  // Transition nodes (Groups positioned via transform attribute)
  nodes
    .transition(t)
    .attr('transform', (d, i) => {
      const pinnedNode = pinnedNodes[i];
      if (pinnedNode && pinnedNode.fx !== null && pinnedNode.fx !== undefined &&
          pinnedNode.fy !== null && pinnedNode.fy !== undefined) {
        return `translate(${pinnedNode.fx},${pinnedNode.fy})`;
      }
      // Fallback to current position
      return `translate(${d.x},${d.y})`;
    })
    .on('end', function(d, i) {
      // Call completion callback after last node finishes
      if (i === pinnedNodes.length - 1) {
        console.log('Pinned layout transition complete');
        onComplete();
      }
    });

  // Transition links (Lines with x1/y1/x2/y2 attributes)
  links
    .transition(t)
    .attr('x1', function(d) {
      const sourceIndex = pinnedNodes.findIndex(n => n.id === d.source.id);
      if (sourceIndex >= 0 && pinnedNodes[sourceIndex].fx !== null) {
        return pinnedNodes[sourceIndex].fx;
      }
      return d.source.x;
    })
    .attr('y1', function(d) {
      const sourceIndex = pinnedNodes.findIndex(n => n.id === d.source.id);
      if (sourceIndex >= 0 && pinnedNodes[sourceIndex].fy !== null) {
        return pinnedNodes[sourceIndex].fy;
      }
      return d.source.y;
    })
    .attr('x2', function(d) {
      const targetIndex = pinnedNodes.findIndex(n => n.id === d.target.id);
      if (targetIndex >= 0 && pinnedNodes[targetIndex].fx !== null) {
        return pinnedNodes[targetIndex].fx;
      }
      return d.target.x;
    })
    .attr('y2', function(d) {
      const targetIndex = pinnedNodes.findIndex(n => n.id === d.target.id);
      if (targetIndex >= 0 && pinnedNodes[targetIndex].fy !== null) {
        return pinnedNodes[targetIndex].fy;
      }
      return d.target.y;
    });
};