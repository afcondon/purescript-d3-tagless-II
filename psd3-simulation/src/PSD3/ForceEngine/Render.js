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

// Generic position update with custom callback
// The callback receives the D3 selection and can set any attributes
export const updatePositions_ = (selector) => (callback) => () => {
  const sel = select(selector).selectAll("*");
  callback(sel)();
};
