// Highlight connected nodes on hover

// Highlight all nodes connected to the given node
export function highlightConnected_(node) {
  return function() {
    if (!node || !node.links) return;

    // Get all connected node IDs (both directions)
    const connectedIds = new Set();

    // Nodes this node depends on (targets)
    if (node.links.targets) {
      node.links.targets.forEach(id => connectedIds.add(id));
    }

    // Nodes that depend on this node (sources)
    if (node.links.sources) {
      node.links.sources.forEach(id => connectedIds.add(id));
    }

    // Also highlight tree children if any
    if (node.links.treeChildren) {
      node.links.treeChildren.forEach(id => connectedIds.add(id));
    }

    // Select all g elements with bound data (node groups)
    // The svg container has class 'overlay', find all g children with data
    const nodeGroups = d3.selectAll('svg.overlay g').filter(function(d) {
      return d && d.id !== undefined;
    });

    // Add highlight class to connected nodes
    nodeGroups.classed('highlighted-connection', function(d) {
      return d && connectedIds.has(d.id);
    });

    // Also highlight the hovered node itself
    nodeGroups.classed('highlighted-source', function(d) {
      return d && d.id === node.id;
    });

    // Show label for hovered node only
    nodeGroups.select('text').classed('show-label', function(d) {
      return d && d.id === node.id;
    });

    // Dim non-connected nodes
    nodeGroups.classed('dimmed', function(d) {
      return d && d.id !== node.id && !connectedIds.has(d.id);
    });
  };
}

// Remove all highlights
export function clearHighlights_() {
  d3.selectAll('.highlighted-connection').classed('highlighted-connection', false);
  d3.selectAll('.highlighted-source').classed('highlighted-source', false);
  d3.selectAll('.dimmed').classed('dimmed', false);
  d3.selectAll('.show-label').classed('show-label', false);
}
