// FFI for LesMiserables grid transition

// Transition nodes to grid positions with smooth D3 animation
// Generic version that works with any visualization by taking class selectors
export const transitionNodesToGridPositions_ = (svgClass) => (nodeClass) => (linkClass) => (gridNodes) => (onComplete) => () => {
  console.log(`Starting grid transition for ${gridNodes.length} nodes`);
  console.log(`SVG selector: ${svgClass}, Node selector: ${nodeClass}, Link selector: ${linkClass}`);

  // Select the SVG and then the node elements
  const svg = d3.select(svgClass);
  const nodes = svg.selectAll(nodeClass);

  if (nodes.empty()) {
    console.error(`Could not find ${nodeClass} elements to transition`);
    // Still call the completion callback even if selection fails
    onComplete();
    return;
  }

  console.log(`Found ${nodes.size()} nodes to transition`);

  // Also select links to transition them
  const links = svg.selectAll(linkClass);
  console.log(`Found ${links.size()} links to transition`);

  // Create a transition that both nodes and links will use
  const t = d3.transition().duration(1500);

  // Transition nodes (circles positioned via cx/cy attributes)
  nodes
    .transition(t)
    .attr('cx', (d, i) => {
      const gridNode = gridNodes[i];
      if (gridNode && gridNode.fx !== null && gridNode.fx !== undefined) {
        return gridNode.fx;
      }
      return d.x; // Fallback to current position
    })
    .attr('cy', (d, i) => {
      const gridNode = gridNodes[i];
      if (gridNode && gridNode.fy !== null && gridNode.fy !== undefined) {
        return gridNode.fy;
      }
      return d.y; // Fallback to current position
    })
    .on('end', function(d, i) {
      // On transition end for the last node, call completion callback
      if (i === gridNodes.length - 1) {
        console.log('Grid transition complete, calling callback');
        onComplete(); // Call the Effect thunk
      }
    });

  // Transition links (lines with x1/y1/x2/y2 attributes)
  links
    .transition(t)
    .attr('x1', function(d) {
      // Find the source node's grid position
      const sourceIndex = gridNodes.findIndex(n => n.id === d.source.id);
      if (sourceIndex >= 0 && gridNodes[sourceIndex].fx !== null) {
        return gridNodes[sourceIndex].fx;
      }
      return d.source.x;
    })
    .attr('y1', function(d) {
      const sourceIndex = gridNodes.findIndex(n => n.id === d.source.id);
      if (sourceIndex >= 0 && gridNodes[sourceIndex].fy !== null) {
        return gridNodes[sourceIndex].fy;
      }
      return d.source.y;
    })
    .attr('x2', function(d) {
      const targetIndex = gridNodes.findIndex(n => n.id === d.source.id);
      if (targetIndex >= 0 && gridNodes[targetIndex].fx !== null) {
        return gridNodes[targetIndex].fx;
      }
      return d.target.x;
    })
    .attr('y2', function(d) {
      const targetIndex = gridNodes.findIndex(n => n.id === d.target.id);
      if (targetIndex >= 0 && gridNodes[targetIndex].fy !== null) {
        return gridNodes[targetIndex].fy;
      }
      return d.target.y;
    });
};
