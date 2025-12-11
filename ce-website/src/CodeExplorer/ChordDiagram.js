// FFI for ChordDiagram
import { select } from "d3-selection";
import { showTooltip_, hideTooltip_ } from "../../psd3-selection/src/PSD3v2/Tooltip.js";

// Clear any existing chord diagram SVG
export const clearChordSvg_ = () => {
  select("#chord-diagram-container").selectAll("*").remove();
};

// Render the complete chord diagram
// arcs: Array of { path, color, moduleName, index }
// ribbons: Array of { path, color, sourceModule, targetModule, sourceIndex, targetIndex }
// labels: Array of { text, x, y, anchor, rotation, index }
export const renderChordDiagram_ = (width) => (height) => (arcs) => (ribbons) => (labels) => () => {
  renderChordDiagramWithOffset_(width)(height)(arcs)(ribbons)(labels)(0)(0)(1.0)();
};

// Render chord diagram at a specific offset (for triptych view)
// offsetX, offsetY: position offset from center
// scale: scale factor (e.g., 0.33 for triptych)
export const renderChordDiagramWithOffset_ = (width) => (height) => (arcs) => (ribbons) => (labels) => (offsetX) => (offsetY) => (scale) => () => {
  // Clear existing (only if rendering at origin - triptych handles its own clearing)
  if (offsetX === 0 && offsetY === 0) {
    clearChordSvg_();
  }

  // Get or create container
  let container = select("#chord-diagram-container");
  if (container.empty()) {
    container = select("#explorer-svg")
      .append("g")
      .attr("id", "chord-diagram-container");
  }

  // The SVG viewBox is centered at (0,0) with negative offsets, so (0,0) is the center
  // Create SVG structure with offset and scale for triptych layout
  const svg = container.append("g")
    .attr("class", "chord-diagram")
    .attr("transform", `translate(${offsetX},${offsetY}) scale(${scale})`);

  // Ribbons (rendered first so arcs appear on top)
  const ribbonsGroup = svg.append("g")
    .attr("class", "chord-ribbons");

  ribbonsGroup.selectAll("path.ribbon")
    .data(ribbons)
    .enter()
    .append("path")
    .attr("class", "ribbon")
    .attr("d", d => d.path)
    .attr("fill", d => d.color)
    .attr("fill-opacity", 0.65)
    .attr("stroke", "#000")
    .attr("stroke-width", 0.5)
    .attr("stroke-opacity", 0.2)
    .style("cursor", "pointer")
    .on("mouseenter", function(event, d) {
      // Highlight this ribbon
      select(this)
        .attr("fill-opacity", 0.9)
        .attr("stroke-opacity", 0.8)
        .attr("stroke-width", 1);

      // Show tooltip
      const tooltipContent = `
        <div class="tooltip-header">${d.sourceModule}</div>
        <div class="tooltip-package">→ ${d.targetModule}</div>
      `;
      showTooltip_(tooltipContent)(event.pageX)(event.pageY)();
    })
    .on("mouseleave", function() {
      select(this)
        .attr("fill-opacity", 0.65)
        .attr("stroke-opacity", 0.2)
        .attr("stroke-width", 0.5);
      hideTooltip_();
    });

  // Arcs (outer segments)
  const arcsGroup = svg.append("g")
    .attr("class", "chord-arcs");

  arcsGroup.selectAll("path.arc")
    .data(arcs)
    .enter()
    .append("path")
    .attr("class", "arc")
    .attr("d", d => d.path)
    .attr("fill", d => d.color)
    .attr("stroke", "#fff")
    .attr("stroke-width", 2)
    .style("cursor", "pointer")
    .on("mouseenter", function(event, d) {
      // Highlight this arc
      select(this)
        .attr("stroke", "#000")
        .attr("stroke-width", 3);

      // Highlight connected ribbons
      ribbonsGroup.selectAll("path.ribbon")
        .attr("fill-opacity", r =>
          (r.sourceIndex === d.index || r.targetIndex === d.index) ? 0.9 : 0.2
        );

      // Show tooltip
      const tooltipContent = `
        <div class="tooltip-header">${d.moduleName}</div>
      `;
      showTooltip_(tooltipContent)(event.pageX)(event.pageY)();
    })
    .on("mouseleave", function() {
      select(this)
        .attr("stroke", "#fff")
        .attr("stroke-width", 2);

      // Reset ribbon opacity
      ribbonsGroup.selectAll("path.ribbon")
        .attr("fill-opacity", 0.65);

      hideTooltip_();
    });

  // Labels
  const labelsGroup = svg.append("g")
    .attr("class", "chord-labels");

  labelsGroup.selectAll("text.chord-label")
    .data(labels)
    .enter()
    .append("text")
    .attr("class", "chord-label")
    .attr("x", d => d.x)
    .attr("y", d => d.y)
    .attr("text-anchor", d => d.anchor)
    .attr("dominant-baseline", "middle")
    .attr("font-size", "14px")
    .attr("font-weight", "500")
    .attr("fill", "#e2e8f0")
    .attr("transform", d => {
      // Rotate text to follow the circle
      const angle = d.rotation;
      // Flip text on the left side so it's readable
      if (angle > 90 || angle < -90) {
        return `rotate(${angle + 180},${d.x},${d.y})`;
      }
      return `rotate(${angle},${d.x},${d.y})`;
    })
    .text(d => d.text);
};
