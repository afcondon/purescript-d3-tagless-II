// FFI for ModuleGraph module

// Set fx and fy on a node object to pin it to a position
export const setNodePosition_ = (node) => (fx) => (fy) => {
  node.fx = fx;
  node.fy = fy;
  return node;
};

// Restart the simulation without resetting alpha to 1.0
// (unlike startSimulation_ which always sets alpha to 1.0)
export const restartSimulationWithCurrentAlpha_ = (simulation) => {
  console.log(`Grid transition: restarting simulation, alpha: ${simulation.alpha()}`);
  simulation.restart();
  return simulation;
};

// Unpin all nodes by clearing both fx and fy
export const unpinAllNodesCompletely_ = (simulation) => {
  console.log(`Unpinning all ${simulation.nodes().length} nodes (clearing fx and fy)`);
  simulation.nodes().forEach(node => {
    node.fx = null;
    node.fy = null;
  });
  return simulation;
};

// Transition nodes to grid positions with smooth animation
// Returns Effect Unit (a thunk) as per PureScript FFI conventions
export const transitionNodesToGrid_ = (gridNodes) => (onComplete) => () => {
  console.log(`Starting grid transition for ${gridNodes.length} nodes`);

  // Select the SVG and then the node groups
  const svg = d3.select('.module-graph');
  const nodeGroups = svg.selectAll('.node-group');

  if (nodeGroups.empty()) {
    console.error('Could not find .node-group elements to transition');
    // Still call the completion callback even if selection fails
    onComplete();
    return;
  }

  console.log(`Found ${nodeGroups.size()} node groups to transition`);

  // Also select links to transition them
  const links = svg.selectAll('.link line');
  console.log(`Found ${links.size()} links to transition`);

  // Create a transition that both nodes and links will use
  const t = d3.transition().duration(1500);

  // Transition nodes
  nodeGroups
    .transition(t)
    .attr('transform', (d, i) => {
      // Get grid position for this node from gridNodes array
      const gridNode = gridNodes[i];
      if (gridNode) {
        return `translate(${gridNode.fx},${gridNode.fy})`;
      }
      return `translate(${d.x},${d.y})`; // Fallback to current position
    })
    .on('end', function(d, i) {
      // On transition end for the last node, call completion callback (which is a thunk)
      if (i === gridNodes.length - 1) {
        console.log('Grid transition complete, calling callback');
        onComplete(); // Call the Effect thunk
      }
    });

  // Transition links by updating their endpoints
  links
    .transition(t)
    .attr('x1', function(d) {
      // Find the source node's grid position
      const sourceIndex = gridNodes.findIndex(n => n.id === d.source.id);
      return sourceIndex >= 0 ? gridNodes[sourceIndex].fx : d.source.x;
    })
    .attr('y1', function(d) {
      const sourceIndex = gridNodes.findIndex(n => n.id === d.source.id);
      return sourceIndex >= 0 ? gridNodes[sourceIndex].fy : d.source.y;
    })
    .attr('x2', function(d) {
      // Find the target node's grid position
      const targetIndex = gridNodes.findIndex(n => n.id === d.target.id);
      return targetIndex >= 0 ? gridNodes[targetIndex].fx : d.target.x;
    })
    .attr('y2', function(d) {
      const targetIndex = gridNodes.findIndex(n => n.id === d.target.id);
      return targetIndex >= 0 ? gridNodes[targetIndex].fy : d.target.y;
    });
};
