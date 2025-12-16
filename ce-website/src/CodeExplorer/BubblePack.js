// FFI for BubblePack - properly binds SimNode data to groups
// Drag behavior is attached separately via library (Core.attachDragWithReheat)

import { select } from "d3-selection";
import { showTooltip_, hideTooltip_ } from "../../psd3-selection/src/PSD3/Tooltip.js";

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

// Highlight individual functions by their relationship to a hovered declaration
// sourceFunc: "Module.funcName" - the hovered function (cyan glow)
// callerFuncs: Array of "Module.funcName" strings that call the source (orange)
// calleeFuncs: Array of "Module.funcName" strings that the source calls (green)
export const highlightCallGraph_ = (sourceFunc) => (callerFuncs) => (calleeFuncs) => () => {
  // Build sets for O(1) lookup - these are full "Module.func" keys
  const callerSet = new Set(callerFuncs);
  const calleeSet = new Set(calleeFuncs);

  // Parse source function
  const sourceParts = parseQualifiedName(sourceFunc);

  // Select all module-pack groups and process each declaration circle
  select("#explorer-nodes").selectAll("g.module-pack").each(function() {
    const group = select(this);
    const node = group.datum();
    if (!node) return;

    const moduleName = node.name;
    const isSourceModule = moduleName === sourceParts.module;

    // Process each declaration circle (depth 2)
    group.selectAll("circle.pack-declaration").each(function() {
      const circle = select(this);
      const d = circle.datum();
      if (!d || d.depth !== 2) return;

      const funcKey = moduleName + "." + d.data_;

      // Clear existing highlight classes from circle
      circle.classed("fn-highlight-source", false)
            .classed("fn-highlight-caller", false)
            .classed("fn-highlight-callee", false)
            .classed("fn-highlight-dimmed", false);

      // Apply highlight based on relationship
      if (funcKey === sourceFunc) {
        circle.classed("fn-highlight-source", true);
      } else if (callerSet.has(funcKey)) {
        circle.classed("fn-highlight-caller", true);
      } else if (calleeSet.has(funcKey)) {
        circle.classed("fn-highlight-callee", true);
      } else {
        circle.classed("fn-highlight-dimmed", true);
      }
    });

    // Also dim category circles (depth 1) and module outline (depth 0)
    group.selectAll("circle.pack-category, circle.pack-module").each(function() {
      const circle = select(this);
      circle.classed("fn-highlight-dimmed", true);
    });
  });

  // Dim the neighborhood links during function highlight
  select("#explorer-links").selectAll("line").each(function() {
    select(this).classed("fn-highlight-dimmed", true);
  });
};

// Draw temporary edges from source function to its callers and callees
// sourceFunc: "Module.funcName"
// callerFuncs: Array of caller "Module.funcName"
// calleeFuncs: Array of callee "Module.funcName"
export const drawFunctionEdges_ = (sourceFunc) => (callerFuncs) => (calleeFuncs) => () => {
  // Remove any existing function edges
  select("#explorer-nodes").selectAll(".fn-edge").remove();

  // Find source circle position
  const sourcePos = findFunctionCirclePosition(sourceFunc);
  if (!sourcePos) return;

  // Create edges group if not exists
  let edgesGroup = select("#explorer-nodes").select(".fn-edges-group");
  if (edgesGroup.empty()) {
    edgesGroup = select("#explorer-nodes").insert("g", ":first-child")
      .attr("class", "fn-edges-group");
  }

  // Draw edges to callers (orange, incoming)
  callerFuncs.forEach(callerFunc => {
    const callerPos = findFunctionCirclePosition(callerFunc);
    if (callerPos) {
      edgesGroup.append("line")
        .attr("class", "fn-edge fn-edge-caller")
        .attr("x1", callerPos.x)
        .attr("y1", callerPos.y)
        .attr("x2", sourcePos.x)
        .attr("y2", sourcePos.y)
        .attr("stroke", "#ff7f0e")  // Orange for callers
        .attr("stroke-width", 2)
        .attr("stroke-opacity", 0.7)
        .attr("marker-end", "url(#arrow-caller)");
    }
  });

  // Draw edges to callees (green, outgoing)
  calleeFuncs.forEach(calleeFunc => {
    const calleePos = findFunctionCirclePosition(calleeFunc);
    if (calleePos) {
      edgesGroup.append("line")
        .attr("class", "fn-edge fn-edge-callee")
        .attr("x1", sourcePos.x)
        .attr("y1", sourcePos.y)
        .attr("x2", calleePos.x)
        .attr("y2", calleePos.y)
        .attr("stroke", "#2ca02c")  // Green for callees
        .attr("stroke-width", 2)
        .attr("stroke-opacity", 0.7)
        .attr("marker-end", "url(#arrow-callee)");
    }
  });
};

// Clear function edges
export const clearFunctionEdges_ = () => {
  select("#explorer-nodes").selectAll(".fn-edge").remove();
};

// Draw all function edges for a module at once
// edges: Array of { source: "Module.func", target: "Module.func", isOutgoing: boolean }
// isOutgoing: true = this module calls target, false = target calls this module
export const drawModuleEdges_ = (edges) => () => {
  // Remove any existing function edges
  select("#explorer-nodes").selectAll(".fn-edge").remove();

  // Create edges group if not exists
  let edgesGroup = select("#explorer-nodes").select(".fn-edges-group");
  if (edgesGroup.empty()) {
    edgesGroup = select("#explorer-nodes").insert("g", ":first-child")
      .attr("class", "fn-edges-group");
  }

  // Draw each edge
  edges.forEach(edge => {
    const sourcePos = findFunctionCirclePosition(edge.source);
    const targetPos = findFunctionCirclePosition(edge.target);
    if (sourcePos && targetPos) {
      edgesGroup.append("line")
        .attr("class", edge.isOutgoing ? "fn-edge fn-edge-callee" : "fn-edge fn-edge-caller")
        .attr("x1", sourcePos.x)
        .attr("y1", sourcePos.y)
        .attr("x2", targetPos.x)
        .attr("y2", targetPos.y)
        .attr("stroke", edge.isOutgoing ? "#4ade80" : "#f97316")
        .attr("stroke-width", 1.5)
        .attr("stroke-opacity", 0.6);
    }
  });
};

// Highlight all functions in a module as "source" (cyan)
// And highlight their callers (orange) and callees (green)
export const highlightModuleCallGraph_ = (moduleName) => (moduleFuncs) => (callerFuncs) => (calleeFuncs) => () => {
  // Build sets for O(1) lookup
  const moduleFuncSet = new Set(moduleFuncs);
  const callerSet = new Set(callerFuncs);
  const calleeSet = new Set(calleeFuncs);

  // Select all module-pack groups and process each declaration circle
  select("#explorer-nodes").selectAll("g.module-pack").each(function() {
    const group = select(this);
    const node = group.datum();
    if (!node) return;

    const nodeModuleName = node.name;

    // Process each declaration circle (depth 2)
    group.selectAll("circle.pack-declaration").each(function() {
      const circle = select(this);
      const d = circle.datum();
      if (!d || d.depth !== 2) return;

      const funcKey = nodeModuleName + "." + d.data_;

      // Clear existing highlight classes
      circle.classed("fn-highlight-source", false)
            .classed("fn-highlight-caller", false)
            .classed("fn-highlight-callee", false)
            .classed("fn-highlight-dimmed", false);

      // Apply highlight based on relationship
      if (moduleFuncSet.has(funcKey)) {
        circle.classed("fn-highlight-source", true);
      } else if (callerSet.has(funcKey)) {
        circle.classed("fn-highlight-caller", true);
      } else if (calleeSet.has(funcKey)) {
        circle.classed("fn-highlight-callee", true);
      } else {
        circle.classed("fn-highlight-dimmed", true);
      }
    });

    // Also dim category circles (depth 1) and module outline (depth 0)
    // except for the hovered module
    group.selectAll("circle.pack-category, circle.pack-module").each(function() {
      const circle = select(this);
      if (nodeModuleName !== moduleName) {
        circle.classed("fn-highlight-dimmed", true);
      }
    });
  });

  // Dim the neighborhood links during module highlight
  select("#explorer-links").selectAll("line").each(function() {
    select(this).classed("fn-highlight-dimmed", true);
  });
};

// Helper: find the absolute position of a function circle
function findFunctionCirclePosition(qualifiedName) {
  const parts = parseQualifiedName(qualifiedName);
  if (!parts.module) return null;

  let position = null;

  select("#explorer-nodes").selectAll("g.module-pack").each(function() {
    const group = select(this);
    const node = group.datum();
    if (!node || node.name !== parts.module) return;

    // Find the declaration circle with matching name
    group.selectAll("circle.pack-declaration").each(function() {
      const circle = select(this);
      const d = circle.datum();
      if (d && d.data_ === parts.name) {
        // Get circle position relative to group, then add group position
        const cx = parseFloat(circle.attr("cx")) || 0;
        const cy = parseFloat(circle.attr("cy")) || 0;
        position = { x: node.x + cx, y: node.y + cy };
      }
    });
  });

  return position;
}

// Helper: parse "Module.Name.func" into { module: "Module.Name", name: "func" }
function parseQualifiedName(qualifiedName) {
  const lastDot = qualifiedName.lastIndexOf(".");
  if (lastDot < 0) return { module: "", name: qualifiedName };
  return {
    module: qualifiedName.substring(0, lastDot),
    name: qualifiedName.substring(lastDot + 1)
  };
}

// Clear all call graph highlighting
export const clearCallGraphHighlight_ = () => {
  // Clear function-level highlights
  select("#explorer-nodes").selectAll("circle")
    .classed("fn-highlight-source", false)
    .classed("fn-highlight-caller", false)
    .classed("fn-highlight-callee", false)
    .classed("fn-highlight-dimmed", false);

  // Clear link dimming
  select("#explorer-links").selectAll("line")
    .classed("fn-highlight-dimmed", false);

  // Clear function edges
  clearFunctionEdges_();
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
// - onModuleHover: callback for module (outer circle) hover (moduleName) -> Effect Unit
export const renderBoundBubblePack_ = (containerSelector) => (node) => (packCircles) => (centerOffset) => (categoryColorFn) => (onDeclarationClick) => (onDeclarationHover) => (onDeclarationLeave) => (onModuleHover) => () => {
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
      if (d.depth === 0) {
        // Module outline hover - show all edges for entire module
        select(this)
          .attr("stroke", "#7dd3fc")
          .attr("stroke-width", 3);
        onModuleHover(node.name)();
      } else if (d.depth === 2) {
        select(this)
          .attr("stroke", "#000")
          .attr("stroke-width", 2);

        // Show tooltip for all declarations
        const tooltipContent = `
          <div class="tooltip-header">${d.data_}</div>
          <div class="tooltip-package">${node.name}</div>
          <div class="tooltip-metrics">
            <div class="tooltip-metric">
              <span class="metric-label">Type</span>
              <span class="metric-value">${d.category}</span>
            </div>
          </div>
        `;
        showTooltip_(tooltipContent)(event.pageX)(event.pageY)();

        // Call hover callback for value declarations (for call graph highlighting)
        if (d.category === "value") {
          onDeclarationHover(node.name)(d.data_)(d.category)();
        }
      }
    })
    .on("mouseleave", function(event, d) {
      if (d.depth === 0) {
        // Module outline leave - clear all highlights and edges
        select(this)
          .attr("stroke", "#fff")
          .attr("stroke-width", 0.5);
        onDeclarationLeave();  // Reuse same cleanup
      } else if (d.depth === 2) {
        select(this)
          .attr("stroke", "#fff")
          .attr("stroke-width", 0.5);

        // Hide tooltip
        hideTooltip_();

        // Call leave callback
        if (d.category === "value") {
          onDeclarationLeave();
        }
      }
    });
};

// Clear all bubble packs (module-pack groups) from the DOM
export const clearBubblePacks_ = () => {
  select("#explorer-nodes").selectAll("g.module-pack").remove();
};
