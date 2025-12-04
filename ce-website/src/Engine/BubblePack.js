// FFI for BubblePack - properly binds SimNode data to groups
// Drag behavior is attached separately via library (Core.attachDragWithReheat)

import { select } from "d3-selection";

// Color legend is now handled by Halogen NarrativePanel component (Component.NarrativePanel)

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

// Highlight modules by their relationship to a declaration
// callerModules: Array of module names that call the declaration (orange)
// calleeModules: Array of module names that the declaration calls (green)
// sourceModule: The module containing the hovered declaration (cyan)
export const highlightCallGraph_ = (sourceModule) => (callerModules) => (calleeModules) => () => {
  const callerSet = new Set(callerModules);
  const calleeSet = new Set(calleeModules);

  // Select all module-pack groups and apply highlighting
  select("#explorer-nodes").selectAll("g.module-pack").each(function() {
    const group = select(this);
    const node = group.datum();
    if (!node) return;

    const moduleName = node.name;

    // Remove existing highlight classes
    group.classed("highlight-caller", false)
         .classed("highlight-callee", false)
         .classed("highlight-source", false)
         .classed("highlight-dimmed", false);

    // Apply new classes based on relationship
    if (moduleName === sourceModule) {
      group.classed("highlight-source", true);
    } else if (callerSet.has(moduleName)) {
      group.classed("highlight-caller", true);
    } else if (calleeSet.has(moduleName)) {
      group.classed("highlight-callee", true);
    } else {
      group.classed("highlight-dimmed", true);
    }
  });

  // Also highlight the neighborhood links
  select("#explorer-links").selectAll("line").each(function() {
    const line = select(this);
    // Dim all links during highlight
    line.classed("highlight-dimmed", true);
  });
};

// Clear all call graph highlighting
export const clearCallGraphHighlight_ = () => {
  select("#explorer-nodes").selectAll("g.module-pack")
    .classed("highlight-caller", false)
    .classed("highlight-callee", false)
    .classed("highlight-source", false)
    .classed("highlight-dimmed", false);

  select("#explorer-links").selectAll("line")
    .classed("highlight-dimmed", false);
};

// Render a complete bubble pack with SimNode bound to the group
// Parameters:
// - containerSelector: selector for parent element (e.g., "#explorer-nodes")
// - node: SimNode with x, y, r, id, name fields
// - packCircles: Array of { x, y, r, depth, data_, category } objects from pack layout
// - centerOffset: offset to center pack circles
// - categoryColorFn: function to get fill color for a pack circle
// - onDeclarationClick: callback for declaration click (module, declaration) -> Effect Unit
// - onDeclarationHover: callback for declaration hover (module, declaration, kind) -> Effect Unit
// - onDeclarationLeave: callback for declaration mouse leave -> Effect Unit
export const renderBoundBubblePack_ = (containerSelector) => (node) => (packCircles) => (centerOffset) => (categoryColorFn) => (onDeclarationClick) => (onDeclarationHover) => (onDeclarationLeave) => () => {
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
    })
    .style("cursor", d => d.depth === 2 ? "pointer" : "default")
    .on("click", function(event, d) {
      if (d.depth === 2) {
        // Declaration clicked - call PureScript callback with module, name, and kind
        event.stopPropagation();  // Don't bubble to module group
        onDeclarationClick(node.name)(d.data_)(d.category)();
      }
    })
    .on("mouseenter", function(event, d) {
      if (d.depth === 2) {
        select(this)
          .attr("stroke", "#000")
          .attr("stroke-width", 2);
        // Call hover callback for value declarations
        if (d.category === "value") {
          onDeclarationHover(node.name)(d.data_)(d.category)();
        }
      }
    })
    .on("mouseleave", function(event, d) {
      if (d.depth === 2) {
        select(this)
          .attr("stroke", "#fff")
          .attr("stroke-width", 0.5);
        // Call leave callback
        if (d.category === "value") {
          onDeclarationLeave();
        }
      }
    });
};
