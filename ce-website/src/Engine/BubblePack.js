// FFI for BubblePack - properly binds SimNode data to groups
// Drag behavior is attached separately via library (Core.attachDragWithReheat)

import { select } from "d3-selection";

// Render a re-export (umbrella) module - hollow ring to show "pass-through" nature
// isReexport: true for re-export modules, false for empty modules
export const renderReexportModule_ = (containerSelector) => (node) => (isReexport) => () => {
  const container = select(containerSelector);

  // Create group with SimNode bound as __data__
  const group = container.append("g")
    .datum(node)
    .attr("class", `module-pack pack-${node.id} ${isReexport ? 'reexport-module' : 'empty-module'}`)
    .attr("transform", `translate(${node.x},${node.y})`);

  if (isReexport) {
    // Re-export module: hollow ring with dashed stroke
    // Outer ring
    group.append("circle")
      .attr("cx", 0)
      .attr("cy", 0)
      .attr("r", node.r * 1.5)
      .attr("fill", "none")
      .attr("stroke", "#9467bd")  // Purple for re-export
      .attr("stroke-width", 3)
      .attr("stroke-dasharray", "5,3")
      .attr("class", "reexport-ring");

    // Inner dot to mark center
    group.append("circle")
      .attr("cx", 0)
      .attr("cy", 0)
      .attr("r", 4)
      .attr("fill", "#9467bd")
      .attr("class", "reexport-center");
  } else {
    // Empty module: simple small circle
    group.append("circle")
      .attr("cx", 0)
      .attr("cy", 0)
      .attr("r", node.r)
      .attr("fill", "#ccc")
      .attr("stroke", "#999")
      .attr("stroke-width", 1)
      .attr("class", "empty-module-circle");
  }
};

// Render a complete bubble pack with SimNode bound to the group
// Parameters:
// - containerSelector: selector for parent element (e.g., "#explorer-nodes")
// - node: SimNode with x, y, r, id, name fields
// - packCircles: Array of { x, y, r, depth, data_ } objects from pack layout
// - centerOffset: offset to center pack circles
// - categoryColorFn: function to get fill color for a pack circle
export const renderBoundBubblePack_ = (containerSelector) => (node) => (packCircles) => (centerOffset) => (categoryColorFn) => () => {
  const container = select(containerSelector);

  // Create group with SimNode bound as __data__
  // This is critical for updateGroupPositions to work
  const group = container.append("g")
    .datum(node)  // Bind SimNode so tick can read x/y
    .attr("class", `module-pack pack-${node.id}`)
    .attr("transform", `translate(${node.x},${node.y})`);

  // Append circles for pack layout
  group.selectAll("circle")
    .data(packCircles)
    .enter()
    .append("circle")
    .attr("cx", d => d.x - centerOffset)
    .attr("cy", d => d.y - centerOffset)
    .attr("r", d => d.r)
    .attr("fill", d => categoryColorFn(d)())  // Call PureScript function
    .attr("fill-opacity", d => {
      if (d.depth === 0) return 0.3;  // Module outline
      if (d.depth === 1) return 0.6;  // Categories
      return 0.8;  // Declarations
    })
    .attr("stroke", "#fff")
    .attr("stroke-width", 0.5)
    .attr("class", d => {
      if (d.depth === 0) return "pack-module";
      if (d.depth === 1) return "pack-category";
      return "pack-declaration";
    });
};
