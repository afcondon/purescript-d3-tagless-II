// FFI for AdjacencyMatrix
import { select } from "d3-selection";
import { scaleLinear } from "d3-scale";
import { interpolateGreens, interpolateOranges } from "d3-scale-chromatic";
import { showTooltip_, hideTooltip_ } from "../../psd3-selection/src/PSD3v2/Tooltip.js";

// Color constants
const noConnectionColor = "#1a202c";  // Dark background for empty cells
const outboundBaseColor = "#48bb78";  // Green for outbound (imports)
const inboundBaseColor = "#ed8936";   // Orange for inbound (imported by)

// Clear any existing matrix SVG
export const clearMatrixSvg_ = () => {
  select("#adjacency-matrix-container").selectAll("*").remove();
};

// Helper to create triangle path for upper-left half of cell
const upperLeftTriangle = (x, y, w, h) =>
  `M${x},${y} L${x + w},${y} L${x},${y + h} Z`;

// Helper to create triangle path for lower-right half of cell
const lowerRightTriangle = (x, y, w, h) =>
  `M${x + w},${y} L${x + w},${y + h} L${x},${y + h} Z`;

// Get color with intensity based on value (0-1 scale)
const getOutboundColor = (intensity) => {
  if (intensity === 0) return noConnectionColor;
  // Use a green scale: darker = more connections
  return interpolateGreens(0.3 + intensity * 0.7);
};

const getInboundColor = (intensity) => {
  if (intensity === 0) return noConnectionColor;
  // Use an orange scale: darker = more connections
  return interpolateOranges(0.3 + intensity * 0.7);
};

// Render the complete adjacency matrix
// layout: { cells, rowLabels, colLabels, gridWidth, gridHeight, totalWidth, totalHeight }
// Now expects cells to have outbound and inbound fields
export const renderAdjacencyMatrix_ = (layout) => (centralName) => (importNames) => (dependentNames) => (maxConnections) => () => {
  renderAdjacencyMatrixWithOffset_(layout)(centralName)(importNames)(dependentNames)(maxConnections)(0)(0)(1.0)();
};

// Render adjacency matrix at a specific offset (for triptych view)
// panelOffsetX, panelOffsetY: position offset from center (before centering the matrix)
// scale: scale factor (e.g., 0.33 for triptych)
export const renderAdjacencyMatrixWithOffset_ = (layout) => (centralName) => (importNames) => (dependentNames) => (maxConnections) => (panelOffsetX) => (panelOffsetY) => (scale) => () => {
  // Clear existing (only if rendering at origin - triptych handles its own clearing)
  if (panelOffsetX === 0 && panelOffsetY === 0) {
    clearMatrixSvg_();
  }

  // Get or create container
  let container = select("#adjacency-matrix-container");
  if (container.empty()) {
    container = select("#explorer-svg")
      .append("g")
      .attr("id", "adjacency-matrix-container");
  }

  // Center the matrix in its panel
  // The SVG viewBox is centered at (0,0) with negative offsets
  const matrixCenterX = -layout.totalWidth / 2;
  const matrixCenterY = -layout.totalHeight / 2;

  const svg = container.append("g")
    .attr("class", "adjacency-matrix")
    .attr("transform", `translate(${panelOffsetX},${panelOffsetY}) scale(${scale}) translate(${matrixCenterX},${matrixCenterY})`);

  // Draw cells as split triangles
  const cellsGroup = svg.append("g")
    .attr("class", "matrix-cells");

  // Normalize values for color intensity (use maxConnections for scaling)
  const normalizeValue = (val) => maxConnections > 0 ? Math.min(val / maxConnections, 1) : 0;

  // For each cell, draw background rect + two triangles
  const cellGroups = cellsGroup.selectAll("g.matrix-cell")
    .data(layout.cells)
    .enter()
    .append("g")
    .attr("class", "matrix-cell")
    .style("cursor", d => (d.outbound > 0 || d.inbound > 0) ? "pointer" : "default");

  // Background rectangle (for empty cells and border)
  cellGroups.append("rect")
    .attr("class", "cell-bg")
    .attr("x", d => d.position.x)
    .attr("y", d => d.position.y)
    .attr("width", d => d.position.width)
    .attr("height", d => d.position.height)
    .attr("fill", noConnectionColor)
    .attr("stroke", "#2d3748")
    .attr("stroke-width", 0.5);

  // Upper-left triangle (outbound: row imports col)
  cellGroups.append("path")
    .attr("class", "cell-outbound")
    .attr("d", d => upperLeftTriangle(d.position.x, d.position.y, d.position.width, d.position.height))
    .attr("fill", d => getOutboundColor(normalizeValue(d.outbound)))
    .attr("stroke", "none");

  // Lower-right triangle (inbound: col imports row)
  cellGroups.append("path")
    .attr("class", "cell-inbound")
    .attr("d", d => lowerRightTriangle(d.position.x, d.position.y, d.position.width, d.position.height))
    .attr("fill", d => getInboundColor(normalizeValue(d.inbound)))
    .attr("stroke", "none");

  // Diagonal line separating triangles (only when there's data)
  cellGroups.filter(d => d.outbound > 0 || d.inbound > 0)
    .append("line")
    .attr("class", "cell-divider")
    .attr("x1", d => d.position.x + d.position.width)
    .attr("y1", d => d.position.y)
    .attr("x2", d => d.position.x)
    .attr("y2", d => d.position.y + d.position.height)
    .attr("stroke", "#4a5568")
    .attr("stroke-width", 0.5);

  // Interaction handlers on the cell group
  cellGroups
    .on("mouseenter", function(event, d) {
      if (d.outbound > 0 || d.inbound > 0) {
        select(this).select(".cell-bg")
          .attr("stroke", "#ffffff")
          .attr("stroke-width", 2);

        // Highlight corresponding row and column labels
        svg.selectAll("text.row-label")
          .attr("font-weight", l => l.index === d.row ? "bold" : "normal")
          .attr("fill", l => l.index === d.row ? "#ffffff" : "#e2e8f0");
        svg.selectAll("text.col-label")
          .attr("font-weight", l => l.index === d.col ? "bold" : "normal")
          .attr("fill", l => l.index === d.col ? "#ffffff" : "#e2e8f0");

        // Build tooltip content
        let tooltipLines = [`<div class="tooltip-header">${d.rowName} ↔ ${d.colName}</div>`];
        if (d.outbound > 0) {
          tooltipLines.push(`<div class="tooltip-package" style="color: ${outboundBaseColor}">→ imports: ${d.outbound}</div>`);
        }
        if (d.inbound > 0) {
          tooltipLines.push(`<div class="tooltip-package" style="color: ${inboundBaseColor}">← imported by: ${d.inbound}</div>`);
        }
        showTooltip_(tooltipLines.join(''))(event.pageX)(event.pageY)();
      }
    })
    .on("mouseleave", function(event, d) {
      select(this).select(".cell-bg")
        .attr("stroke", "#2d3748")
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
