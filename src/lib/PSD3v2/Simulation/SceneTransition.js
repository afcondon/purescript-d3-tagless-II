// Scene Transition FFI - Bridge between declarative TransitionSpec and D3 API
//
// This module implements scene-level transitions that choreograph:
// 1. Enter behavior (how new nodes appear)
// 2. Exit behavior (how old nodes disappear)
// 3. Update behavior (how existing nodes move to new positions)
//
// All transitions are coordinated with proper timing and completion callbacks.
//
// Behaviors are passed as explicit string constants from PureScript encoding functions,
// ensuring we never rely on JavaScript decoding of ADT internals.

// Helper: Apply enter behavior to a selection
// behaviorType is a string: "FadeIn" | "ScaleUp" | "InstantEnter"
function applyEnterBehavior(selection, behaviorType, transition) {

  switch (behaviorType) {
    case "FadeIn":
      selection
        .style("opacity", 0)  // Start invisible
        .transition(transition)
        .style("opacity", 1);  // Fade to visible
      break;

    case "ScaleUp":
      selection
        .style("opacity", 0)
        .attr("transform", function(d) {
          // Scale from 0 at node position
          return `translate(${d.x || 0},${d.y || 0}) scale(0)`;
        })
        .transition(transition)
        .style("opacity", 1)
        .attr("transform", function(d) {
          return `translate(${d.x || 0},${d.y || 0}) scale(1)`;
        });
      break;

    case "InstantEnter":
      // No transition, just set to visible
      selection.style("opacity", 1);
      break;

    default:
      console.warn(`Unknown enter behavior: ${behaviorType}`);
      selection.style("opacity", 1);
  }
}

// Helper: Apply exit behavior to a selection
// behaviorType is a string: "FadeOut" | "ScaleDown" | "InstantExit"
function applyExitBehavior(selection, behaviorType, transition) {

  switch (behaviorType) {
    case "FadeOut":
      selection
        .transition(transition)
        .style("opacity", 0)  // Fade to invisible
        .remove();            // Remove after transition
      break;

    case "ScaleDown":
      selection
        .transition(transition)
        .style("opacity", 0)
        .attr("transform", function(d) {
          // Scale to 0 at node position
          return `translate(${d.x || 0},${d.y || 0}) scale(0)`;
        })
        .remove();
      break;

    case "InstantExit":
      // No transition, just remove
      selection.remove();
      break;

    default:
      console.warn(`Unknown exit behavior: ${behaviorType}`);
      selection.remove();
  }
}

// Helper: Apply update behavior to a selection
// behaviorType is a string: "TransitionMove" | "InstantMove"
// targetById is a Map from node ID to target node (for efficient lookup)
function applyUpdateBehavior(selection, behaviorType, transition, targetById) {

  switch (behaviorType) {
    case "TransitionMove":
      // Smoothly animate to new positions
      selection
        .transition(transition)
        .attr("transform", function(d) {
          // Match by ID, not array index (array order may differ from DOM order)
          const target = targetById.get(d.id);
          if (target && target.fx !== null && target.fx !== undefined &&
              target.fy !== null && target.fy !== undefined) {
            // Use pinned positions (fx/fy)
            return `translate(${target.fx},${target.fy})`;
          } else if (target) {
            // Use regular positions (x/y)
            return `translate(${target.x || 0},${target.y || 0})`;
          }
          return `translate(${d.x || 0},${d.y || 0})`;
        });
      break;

    case "InstantMove":
      // Snap to new positions immediately
      selection.attr("transform", function(d) {
        // Match by ID, not array index (array order may differ from DOM order)
        const target = targetById.get(d.id);
        if (target && target.fx !== null && target.fx !== undefined &&
            target.fy !== null && target.fy !== undefined) {
          return `translate(${target.fx},${target.fy})`;
        } else if (target) {
          return `translate(${target.x || 0},${target.y || 0})`;
        }
        return `translate(${d.x || 0},${d.y || 0})`;
      });
      break;

    default:
      console.warn(`Unknown update behavior: ${behaviorType}`);
  }
}

// Main execution function: Execute scene transition by selector
// spec contains string-encoded behaviors from PureScript
export const executeSceneTransition_Impl = (spec) => (nodeSelector) => (linkSelector) => (targetNodes) => (onComplete) => () => {
  // Select all nodes (will be split into enter/update/exit by D3's data join)
  const nodeSelection = d3.selectAll(nodeSelector);
  const linkSelection = d3.selectAll(linkSelector);

  if (nodeSelection.empty()) {
    onComplete();
    return;
  }

  // Create shared transition
  const t = d3.transition().duration(spec.duration);

  // Build ID lookup map for efficient node matching (used by both nodes and links)
  const targetById = new Map(targetNodes.map(n => [n.id, n]));

  // Note: In this simplified version, we treat all nodes as "update" nodes
  // A full implementation would need to track enter/exit selections from the GUP
  // For now, we focus on the update behavior (position transitions)
  applyUpdateBehavior(nodeSelection, spec.updateNodes, t, targetById);

  // TODO: Apply enter/exit behaviors when we have proper enter/exit tracking from GUP

  // Transition links to follow nodes (using ID lookup for efficiency)
  // Links are now path elements with 'd' attribute instead of line elements
  linkSelection
    .transition(t)
    .attr('d', function(d) {
      const source = targetById.get(d.source.id);
      const target = targetById.get(d.target.id);

      const x1 = source
        ? (source.fx !== null && source.fx !== undefined ? source.fx : source.x)
        : d.source.x;
      const y1 = source
        ? (source.fy !== null && source.fy !== undefined ? source.fy : source.y)
        : d.source.y;
      const x2 = target
        ? (target.fx !== null && target.fx !== undefined ? target.fx : target.x)
        : d.target.x;
      const y2 = target
        ? (target.fy !== null && target.fy !== undefined ? target.fy : target.y)
        : d.target.y;

      return `M${x1},${y1}L${x2},${y2}`;
    });

  // Call completion callback after last node finishes
  // Use nodeSelection.size() instead of targetNodes.length since DOM may have fewer nodes
  let completed = false;
  const lastNodeIndex = nodeSelection.size() - 1;
  nodeSelection
    .transition(t)
    .on("end", function(d, i) {
      if (!completed && i === lastNodeIndex) {
        completed = true;
        // CRITICAL: Update internal node positions to match target positions
        // This prevents the "jump" when simulation starts, because the simulation's
        // internal state now matches where the DOM ended up after the transition
        nodeSelection.each(function(d) {
          const target = targetById.get(d.id);
          if (target) {
            // Always update internal x/y positions to match target
            d.x = target.x || 0;
            d.y = target.y || 0;

            // Conditionally pin nodes based on scene configuration
            if (spec.pinAfterTransition) {
              // Pin nodes at their final positions to prevent forces from moving them
              // (Used for tree layouts and other static layouts)
              if (target.fx !== null && target.fx !== undefined) {
                d.fx = target.fx;
              } else {
                d.fx = d.x;  // Pin at final x position
              }
              if (target.fy !== null && target.fy !== undefined) {
                d.fy = target.fy;
              } else {
                d.fy = d.y;  // Pin at final y position
              }
            } else {
              // Don't pin - let forces continue to act
              // (Used for force-directed layouts like PackageGraph)
              // Only set fx/fy if target explicitly had them
              if (target.fx !== null && target.fx !== undefined) {
                d.fx = target.fx;
              }
              if (target.fy !== null && target.fy !== undefined) {
                d.fy = target.fy;
              }
            }
          }
        });

        onComplete();
      }
    });
};
