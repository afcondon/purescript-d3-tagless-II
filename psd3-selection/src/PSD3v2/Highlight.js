// Highlight connected nodes on hover
//
// Uses D3 selection to efficiently apply CSS classes to circles
// based on node connectivity (targets/sources arrays).

import { select } from "d3-selection";

// Highlight connected nodes
// containerSelector: CSS selector for the container with circles
// nodeId: ID of the hovered node
// targets: Array of IDs this node depends on
// sources: Array of IDs that depend on this node
export function highlightConnected_(containerSelector) {
  return function(nodeId) {
    return function(targets) {
      return function(sources) {
        return function() {
          // Build sets for O(1) lookup
          const targetSet = new Set(targets);
          const sourceSet = new Set(sources);

          // Select all circles in the container
          const circles = select(containerSelector).selectAll("circle");

          // Apply classes based on connectivity
          circles.each(function(d) {
            if (!d || d.id === undefined) return;

            const el = select(this);
            const id = d.id;

            if (id === nodeId) {
              // The hovered node
              el.classed("highlighted-source", true);
              el.classed("dimmed", false);
            } else if (targetSet.has(id)) {
              // Nodes this node depends on (upstream)
              el.classed("highlighted-upstream", true);
              el.classed("dimmed", false);
            } else if (sourceSet.has(id)) {
              // Nodes that depend on this node (downstream)
              el.classed("highlighted-downstream", true);
              el.classed("dimmed", false);
            } else {
              // Unconnected nodes
              el.classed("dimmed", true);
            }
          });
        };
      };
    };
  };
}

// Clear all highlight classes
export function clearHighlights_(containerSelector) {
  return function() {
    const circles = select(containerSelector).selectAll("circle");

    circles
      .classed("highlighted-source", false)
      .classed("highlighted-upstream", false)
      .classed("highlighted-downstream", false)
      .classed("dimmed", false);
  };
}
