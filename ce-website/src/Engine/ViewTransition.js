// FFI for ViewTransition - tick-driven opacity/radius updates
import { select } from "d3-selection";

// Update circle visual properties (opacity, radius) based on transition state
// Expects each circle's __data__ to have transitionOpacity and transitionRadius fields
// If these fields are missing/undefined, uses defaults (opacity: 1, radius: from r field)
export const updateCircleTransitions_ = (selector) => () => {
  select(selector).selectAll("circle")
    .attr("opacity", d => d.transitionOpacity !== undefined ? d.transitionOpacity : 1.0)
    .attr("r", d => d.transitionRadius !== undefined ? d.transitionRadius : d.r);
};

// Remove circles that have completed their exit animation
// Removes elements where exitComplete is true
export const removeCompletedExits_ = (selector) => () => {
  select(selector).selectAll("circle")
    .filter(d => d.exitComplete === true)
    .remove();
};

// Add entering nodes to the DOM
// Takes the nodes group selector and array of nodes to add
// Returns nothing - nodes should already have proper transitionOpacity/transitionRadius set
export const addEnteringNodes_ = (selector) => (nodes) => () => {
  if (nodes.length === 0) return;

  const container = select(selector);

  nodes.forEach(node => {
    container.append("circle")
      .datum(node)
      .attr("cx", node.x)
      .attr("cy", node.y)
      .attr("r", node.transitionRadius !== undefined ? node.transitionRadius : node.r)
      .attr("opacity", node.transitionOpacity !== undefined ? node.transitionOpacity : 1.0)
      .attr("fill", node.fill || "#666")
      .attr("stroke", node.stroke || "#fff")
      .attr("stroke-width", 1)
      .attr("class", node.class || "node");
  });
};

// Batch update nodes with new transition state
// Updates the __data__ on each matching element with new transition values
// IMPORTANT: Nodes NOT in the transitionMap are not visible in the current view
// and should be removed (exitComplete = true)
export const updateNodeTransitionState_ = (selector) => (transitionMap) => () => {
  select(selector).selectAll("circle")
    .each(function(d) {
      const nodeId = d.id;
      if (transitionMap[nodeId]) {
        const state = transitionMap[nodeId];
        d.transitionOpacity = state.opacity;
        d.transitionRadius = state.radius;
        d.exitComplete = state.exitComplete || false;
      } else {
        // Node is NOT in the current view - mark for removal
        // This handles packages/unused modules when switching to Tree view
        d.transitionOpacity = 0.0;
        d.transitionRadius = 0;
        d.exitComplete = true;
      }
    });
};

// =============================================================================
// Package Labels for TopoGraph View
// =============================================================================

// Render package labels for TopoGraph view
// Takes array of package nodes (with name, topoX, topoY, r)
export const renderPackageLabels_ = (packages) => () => {
  // Clear any existing labels
  clearPackageLabels_()();

  const container = select("#explorer-nodes");
  if (container.empty()) return;

  // Create a group for package labels
  const labelsGroup = container.append("g")
    .attr("class", "package-labels");

  // Add text labels for each package
  labelsGroup.selectAll("text.package-label")
    .data(packages)
    .enter()
    .append("text")
    .attr("class", "package-label")
    .attr("x", d => d.topoX)
    .attr("y", d => d.topoY + d.r + 14)  // Position below the circle
    .attr("text-anchor", "middle")
    .attr("dominant-baseline", "hanging")
    .attr("font-size", "12px")
    .attr("font-weight", "500")
    .attr("fill", "#e2e8f0")
    .attr("pointer-events", "none")
    .text(d => d.name);
};

// Clear package labels
export const clearPackageLabels_ = () => () => {
  select("#explorer-nodes").selectAll("g.package-labels").remove();
};

// Update package label positions (for animation/transition)
export const updatePackageLabelPositions_ = () => () => {
  select("#explorer-nodes").selectAll("text.package-label")
    .each(function(d) {
      // Update position from bound data (which is updated by simulation)
      select(this)
        .attr("x", d.x || d.topoX)
        .attr("y", (d.y || d.topoY) + d.r + 14);
    });
};
