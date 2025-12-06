// FFI for AdjacencyMatrix
import { select } from "d3-selection";
import { showTooltip_, hideTooltip_ } from "../../psd3-selection/src/PSD3v2/Tooltip.js";

// Color constants
const noConnectionColor = "#f7fafc";
const connectionColor = "#4a5568";

// Clear any existing matrix SVG
export const clearMatrixSvg_ = () => {
  select("#adjacency-matrix-container").selectAll("*").remove();
};

// Split string on dots
export const splitOnDotImpl = (str) => str.split(".");

// Render the complete adjacency matrix
// layout: { cells, rowLabels, colLabels, gridWidth, gridHeight, totalWidth, totalHeight }
export const renderAdjacencyMatrix_ = (layout) => (centralName) => (importNames) => (dependentNames) => () => {
  // Clear existing
  clearMatrixSvg_();

  // Get or create container
  let container = select("#adjacency-matrix-container");
  if (container.empty()) {
    container = select("#explorer-svg")
      .append("g")
      .attr("id", "adjacency-matrix-container");
  }

  // Center the matrix in the viewport
  // The SVG viewBox is centered at (0,0) with negative offsets
  const offsetX = -layout.totalWidth / 2;
  const offsetY = -layout.totalHeight / 2;

  const svg = container.append("g")
    .attr("class", "adjacency-matrix")
    .attr("transform", `translate(${offsetX},${offsetY})`);

  // Draw cells
  const cellsGroup = svg.append("g")
    .attr("class", "matrix-cells");

  cellsGroup.selectAll("rect.matrix-cell")
    .data(layout.cells)
    .enter()
    .append("rect")
    .attr("class", "matrix-cell")
    .attr("x", d => d.position.x)
    .attr("y", d => d.position.y)
    .attr("width", d => d.position.width)
    .attr("height", d => d.position.height)
    .attr("fill", d => d.value > 0 ? connectionColor : noConnectionColor)
    .attr("stroke", "#e2e8f0")
    .attr("stroke-width", 0.5)
    .style("cursor", d => d.value > 0 ? "pointer" : "default")
    .on("mouseenter", function(event, d) {
      if (d.value > 0) {
        select(this)
          .attr("stroke", "#2d3748")
          .attr("stroke-width", 2);

        // Highlight corresponding row and column labels
        svg.selectAll("text.row-label")
          .attr("font-weight", l => l.index === d.row ? "bold" : "normal")
          .attr("fill", l => l.index === d.row ? "#ffffff" : "#e2e8f0");
        svg.selectAll("text.col-label")
          .attr("font-weight", l => l.index === d.col ? "bold" : "normal")
          .attr("fill", l => l.index === d.col ? "#ffffff" : "#e2e8f0");

        // Show tooltip
        const tooltipContent = `
          <div class="tooltip-header">${d.rowName}</div>
          <div class="tooltip-package">imports ${d.colName}</div>
        `;
        showTooltip_(tooltipContent)(event.pageX)(event.pageY)();
      }
    })
    .on("mouseleave", function(event, d) {
      select(this)
        .attr("stroke", "#e2e8f0")
        .attr("stroke-width", 0.5);

      // Reset label styles
      svg.selectAll("text.row-label")
        .attr("font-weight", "normal")
        .attr("fill", "#e2e8f0");
      svg.selectAll("text.col-label")
        .attr("font-weight", "normal")
        .attr("fill", "#e2e8f0");

      hideTooltip_();
    });

  // Draw row labels (left side)
  const rowLabelsGroup = svg.append("g")
    .attr("class", "matrix-row-labels");

  rowLabelsGroup.selectAll("text.row-label")
    .data(layout.rowLabels)
    .enter()
    .append("text")
    .attr("class", "row-label")
    .attr("x", d => d.position.x - 5)
    .attr("y", d => d.position.y)
    .attr("text-anchor", d => d.position.anchor)
    .attr("dominant-baseline", "middle")
    .attr("font-size", "11px")
    .attr("fill", "#e2e8f0")
    .text(d => d.displayName)
    .on("mouseenter", function(event, d) {
      // Highlight this label
      select(this)
        .attr("font-weight", "bold")
        .attr("fill", "#ffffff");

      // Highlight cells in this row
      cellsGroup.selectAll("rect.matrix-cell")
        .attr("stroke", c => c.row === d.index ? "#2d3748" : "#e2e8f0")
        .attr("stroke-width", c => c.row === d.index ? 1.5 : 0.5);

      // Show tooltip
      const tooltipContent = `<div class="tooltip-header">${d.name}</div>`;
      showTooltip_(tooltipContent)(event.pageX)(event.pageY)();
    })
    .on("mouseleave", function() {
      select(this)
        .attr("font-weight", "normal")
        .attr("fill", "#e2e8f0");

      cellsGroup.selectAll("rect.matrix-cell")
        .attr("stroke", "#e2e8f0")
        .attr("stroke-width", 0.5);

      hideTooltip_();
    });

  // Draw column labels (top, rotated)
  const colLabelsGroup = svg.append("g")
    .attr("class", "matrix-col-labels");

  colLabelsGroup.selectAll("text.col-label")
    .data(layout.colLabels)
    .enter()
    .append("text")
    .attr("class", "col-label")
    .attr("x", d => d.position.x)
    .attr("y", d => d.position.y - 5)
    .attr("text-anchor", d => d.position.anchor)
    .attr("font-size", "11px")
    .attr("fill", "#e2e8f0")
    .attr("transform", d => `rotate(${d.position.rotation},${d.position.x},${d.position.y - 5})`)
    .text(d => d.displayName)
    .on("mouseenter", function(event, d) {
      // Highlight this label
      select(this)
        .attr("font-weight", "bold")
        .attr("fill", "#ffffff");

      // Highlight cells in this column
      cellsGroup.selectAll("rect.matrix-cell")
        .attr("stroke", c => c.col === d.index ? "#2d3748" : "#e2e8f0")
        .attr("stroke-width", c => c.col === d.index ? 1.5 : 0.5);

      // Show tooltip
      const tooltipContent = `<div class="tooltip-header">${d.name}</div>`;
      showTooltip_(tooltipContent)(event.pageX)(event.pageY)();
    })
    .on("mouseleave", function() {
      select(this)
        .attr("font-weight", "normal")
        .attr("fill", "#e2e8f0");

      cellsGroup.selectAll("rect.matrix-cell")
        .attr("stroke", "#e2e8f0")
        .attr("stroke-width", 0.5);

      hideTooltip_();
    });

  // Add diagonal indicator (self-loops - should be empty for our data)
  // Optional: draw a faint diagonal line as reference
  const gridOffsetX = layout.rowLabels[0]?.position.x + 8 || 100;
  const gridOffsetY = layout.colLabels[0]?.position.y + 5 || 100;

  svg.append("line")
    .attr("class", "matrix-diagonal")
    .attr("x1", gridOffsetX)
    .attr("y1", gridOffsetY)
    .attr("x2", gridOffsetX + layout.gridWidth)
    .attr("y2", gridOffsetY + layout.gridHeight)
    .attr("stroke", "#e2e8f0")
    .attr("stroke-width", 1)
    .attr("stroke-dasharray", "4,4")
    .attr("opacity", 0.5);
};
