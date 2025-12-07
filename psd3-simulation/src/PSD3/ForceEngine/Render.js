// FFI for fast tick updates
// Uses D3's selection.attr() which reads from __data__ on each element

import { select } from "d3-selection";

// Update circle positions (cx, cy) from bound data's x, y fields
// O(n) DOM updates only, no data join
export const updateCirclePositions_ = (selector) => () => {
  select(selector).selectAll("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y);
};

// Update line positions from swizzled link data
// Expects data with source.x, source.y, target.x, target.y
// O(n) DOM updates only, no data join
export const updateLinkPositions_ = (selector) => () => {
  select(selector)
    .selectAll("line")
    .attr("x1", d => d.source.x)
    .attr("y1", d => d.source.y)
    .attr("x2", d => d.target.x)
    .attr("y2", d => d.target.y);
};

// Update group positions (transform) from bound data's x, y fields
// For bubble packs and other grouped visualizations
// O(n) DOM updates only, no data join
export const updateGroupPositions_ = (selector) => () => {
  select(selector).selectAll("g.module-pack")
    .attr("transform", d => `translate(${d.x},${d.y})`);
};

// Update tree link paths using current node positions
// Links have source/target as node IDs; we look up positions from the node circles
// O(n) DOM updates only, no data join
export const updateTreeLinkPaths_ = (selector) => () => {
  // Build a map from node ID to current position from circle elements
  const nodePositions = {};
  select("#explorer-nodes").selectAll("circle").each(function(d) {
    if (d && d.id !== undefined) {
      nodePositions[d.id] = { x: d.x, y: d.y };
    }
  });

  // Update path d attribute based on current node positions
  select(selector).selectAll("path.tree-link").attr("d", d => {
    const source = nodePositions[d.source];
    const target = nodePositions[d.target];
    if (!source || !target) return "";

    // Vertical bezier link path (same logic as PureScript verticalLinkPath)
    const midY = (source.y + target.y) / 2.0;
    return `M${source.x},${source.y}C${source.x},${midY} ${target.x},${midY} ${target.x},${target.y}`;
  });
};

// Generic position update with custom callback
// The callback receives the D3 selection and can set any attributes
export const updatePositions_ = (selector) => (callback) => () => {
  const sel = select(selector).selectAll("*");
  callback(sel)();
};
