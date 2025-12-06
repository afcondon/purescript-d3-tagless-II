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
