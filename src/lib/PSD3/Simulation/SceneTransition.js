// Scene Transition FFI - Bridge between declarative TransitionSpec and D3 API
//
// This module implements scene-level transitions that choreograph:
// 1. Enter behavior (how new nodes appear)
// 2. Exit behavior (how old nodes disappear)
// 3. Update behavior (how existing nodes move to new positions)
//
// All transitions are coordinated with proper timing and completion callbacks.

// Helper: Extract behavior type from PureScript ADT
// PureScript nullary constructors compile to functions with names like "FadeIn2"
// We need to extract the name and remove any compiler-added numeric suffix
function getBehaviorType(behavior) {
  // Check if it's an object with constructor field (old-style ADT)
  if (behavior && typeof behavior === 'object' && typeof behavior.constructor === 'string') {
    return behavior.constructor;
  }
  // Check if it's a function (new-style nullary constructor)
  if (typeof behavior === 'function') {
    // Remove compiler-added numeric suffix (e.g., "FadeIn2" -> "FadeIn")
    return behavior.name.replace(/\d+$/, '');
  }
  return 'Unknown';
}

// Helper: Apply enter behavior to a selection
function applyEnterBehavior(selection, behavior, transition) {
  const behaviorType = getBehaviorType(behavior);

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
function applyExitBehavior(selection, behavior, transition) {
  const behaviorType = getBehaviorType(behavior);

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
function applyUpdateBehavior(selection, behavior, transition, targetNodes) {
  const behaviorType = getBehaviorType(behavior);

  switch (behaviorType) {
    case "TransitionMove":
      // Smoothly animate to new positions
      selection
        .transition(transition)
        .attr("transform", function(d, i) {
          const target = targetNodes[i];
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
      selection.attr("transform", function(d, i) {
        const target = targetNodes[i];
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
export const executeSceneTransition_ = (spec) => (nodeSelector) => (linkSelector) => (targetNodes) => (onComplete) => () => {
  console.log(`Executing scene transition: duration=${spec.duration}ms`);
  console.log(`  Enter: ${getBehaviorType(spec.enterNodes)}`);
  console.log(`  Exit: ${getBehaviorType(spec.exitNodes)}`);
  console.log(`  Update: ${getBehaviorType(spec.updateNodes)}`);

  // Select all nodes (will be split into enter/update/exit by D3's data join)
  const nodeSelection = d3.selectAll(nodeSelector);
  const linkSelection = d3.selectAll(linkSelector);

  if (nodeSelection.empty()) {
    console.warn(`No nodes found for selector: ${nodeSelector}`);
    onComplete();
    return;
  }

  console.log(`Found ${nodeSelection.size()} nodes and ${linkSelection.size()} links`);

  // Create shared transition
  const t = d3.transition().duration(spec.duration);

  // Note: In this simplified version, we treat all nodes as "update" nodes
  // A full implementation would need to track enter/exit selections from the GUP
  // For now, we focus on the update behavior (position transitions)
  applyUpdateBehavior(nodeSelection, spec.updateNodes, t, targetNodes);

  // TODO: Apply enter/exit behaviors when we have proper enter/exit tracking from GUP

  // Transition links to follow nodes
  linkSelection
    .transition(t)
    .attr('x1', function(d) {
      const sourceIndex = targetNodes.findIndex(n => n.id === d.source.id);
      if (sourceIndex >= 0) {
        const source = targetNodes[sourceIndex];
        return source.fx !== null && source.fx !== undefined ? source.fx : source.x;
      }
      return d.source.x;
    })
    .attr('y1', function(d) {
      const sourceIndex = targetNodes.findIndex(n => n.id === d.source.id);
      if (sourceIndex >= 0) {
        const source = targetNodes[sourceIndex];
        return source.fy !== null && source.fy !== undefined ? source.fy : source.y;
      }
      return d.source.y;
    })
    .attr('x2', function(d) {
      const targetIndex = targetNodes.findIndex(n => n.id === d.target.id);
      if (targetIndex >= 0) {
        const target = targetNodes[targetIndex];
        return target.fx !== null && target.fx !== undefined ? target.fx : target.x;
      }
      return d.target.x;
    })
    .attr('y2', function(d) {
      const targetIndex = targetNodes.findIndex(n => n.id === d.target.id);
      if (targetIndex >= 0) {
        const target = targetNodes[targetIndex];
        return target.fy !== null && target.fy !== undefined ? target.fy : target.y;
      }
      return d.target.y;
    });

  // Call completion callback after last node finishes
  let completed = false;
  nodeSelection
    .transition(t)
    .on("end", function(d, i) {
      if (!completed && i === targetNodes.length - 1) {
        completed = true;
        console.log("Scene transition complete");
        onComplete();
      }
    });
};

// Advanced: Execute on specific selections (for when you have enter/update/exit already separated)
export const executeSceneTransitionOnSelections_ = (spec) => (enterSel) => (updateSel) => (exitSel) => (targetNodes) => (onComplete) => () => {
  console.log(`Executing scene transition on selections`);

  const t = d3.transition().duration(spec.duration);

  // Apply behaviors to each selection type
  applyEnterBehavior(enterSel, spec.enterNodes, t);
  applyExitBehavior(exitSel, spec.exitNodes, t);
  applyUpdateBehavior(updateSel, spec.updateNodes, t, targetNodes);

  // Call completion callback
  let completed = false;
  updateSel
    .transition(t)
    .on("end", function(d, i) {
      if (!completed && i === targetNodes.length - 1) {
        completed = true;
        console.log("Scene transition complete");
        onComplete();
      }
    });
};
