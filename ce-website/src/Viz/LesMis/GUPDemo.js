// FFI for fast tick updates
// Uses the D3 selection directly to update positions without data joins

import { select } from "d3-selection";

// Update node positions - O(n) DOM updates only, no data join
export const updateNodePositions = (_selection) => () => {
  // Use direct selectAll on the known container
  // The selection object from PureScript wraps D3 selection
  select("#lesmis-gup-nodes")
    .selectAll("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y);
};

// Update link positions - O(n) DOM updates only, no data join
export const updateLinkPositions = (_selection) => () => {
  select("#lesmis-gup-links")
    .selectAll("line")
    .attr("x1", d => d.source.x)
    .attr("y1", d => d.source.y)
    .attr("x2", d => d.target.x)
    .attr("y2", d => d.target.y);
};
