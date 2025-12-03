// FFI for BubblePack - properly binds SimNode data to groups
// Drag behavior is attached separately via library (Core.attachDragWithReheat)

import { select } from "d3-selection";

// Category legend data - matches categoryColor/categoryColorIntense in BubblePack.purs
const legendData = [
  { label: "Type Class", color: "#9467bd", intense: "#7b3fa9" },
  { label: "Data Type", color: "#2ca02c", intense: "#1a8a1a" },
  { label: "Type Synonym", color: "#17becf", intense: "#0d9fb0" },
  { label: "Extern Data", color: "#bcbd22", intense: "#9a9b0a" },
  { label: "Alias", color: "#7f7f7f", intense: "#5f5f5f" },
  { label: "Value", color: "#1f77b4", intense: "#0d5a91" },
];

// Render the color legend as a floating HTML element in top-right corner
export const renderColorLegend_ = () => {
  // Remove any existing legend
  const existing = document.getElementById("color-legend");
  if (existing) existing.remove();

  // Append to body for position:fixed to work correctly
  // (transforms on parent elements can break fixed positioning)
  const container = document.body;

  // Create legend div
  const legend = document.createElement("div");
  legend.id = "color-legend";
  legend.style.cssText = `
    position: fixed;
    top: 20px;
    right: 20px;
    background: rgba(0, 0, 0, 0.8);
    border-radius: 8px;
    padding: 12px 16px;
    font-family: system-ui, -apple-system, sans-serif;
    z-index: 1000;
    pointer-events: none;
  `;

  // Title
  const title = document.createElement("div");
  title.textContent = "Declaration Types";
  title.style.cssText = `
    color: #fff;
    font-size: 14px;
    font-weight: bold;
    margin-bottom: 10px;
    text-align: center;
  `;
  legend.appendChild(title);

  // Legend items
  legendData.forEach(item => {
    const row = document.createElement("div");
    row.style.cssText = `
      display: flex;
      align-items: center;
      margin: 6px 0;
    `;

    // Color circles container (SVG for circles)
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", "24");
    svg.setAttribute("height", "24");
    svg.style.marginRight = "8px";

    // Outer circle (category color)
    const outer = document.createElementNS("http://www.w3.org/2000/svg", "circle");
    outer.setAttribute("cx", "12");
    outer.setAttribute("cy", "12");
    outer.setAttribute("r", "10");
    outer.setAttribute("fill", item.color);
    outer.setAttribute("fill-opacity", "0.6");
    svg.appendChild(outer);

    // Inner circle (intense color)
    const inner = document.createElementNS("http://www.w3.org/2000/svg", "circle");
    inner.setAttribute("cx", "12");
    inner.setAttribute("cy", "12");
    inner.setAttribute("r", "6");
    inner.setAttribute("fill", item.intense);
    inner.setAttribute("fill-opacity", "0.9");
    svg.appendChild(inner);

    row.appendChild(svg);

    // Label
    const label = document.createElement("span");
    label.textContent = item.label;
    label.style.cssText = `
      color: #fff;
      font-size: 12px;
    `;
    row.appendChild(label);

    legend.appendChild(row);
  });

  container.appendChild(legend);
};

// Remove the color legend
export const clearColorLegend_ = () => {
  const legend = document.getElementById("color-legend");
  if (legend) legend.remove();
};

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
