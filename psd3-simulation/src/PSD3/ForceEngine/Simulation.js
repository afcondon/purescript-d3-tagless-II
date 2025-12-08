// Simulation FFI
//
// Position mutation functions for tick-driven transitions.
// These mutate the simulation's internal node objects in place,
// which are the same objects bound to D3 selections.

// =============================================================================
// Position Updates (for transitions)
// =============================================================================

// Update node positions in place from a positions map
// positions: Object keyed by node.id (as string) -> { x, y }
// nodesRef: PureScript Ref containing Array of nodes
export function updatePositionsInPlace_(positions) {
  return function(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        const pos = positions[node.id];
        if (pos) {
          node.x = pos.x;
          node.y = pos.y;
        }
      }
    };
  };
}

// Interpolate node positions in place between start and target
// Uses lerp with given progress (0-1)
export function interpolatePositionsInPlace_(startPositions) {
  return function(targetPositions) {
    return function(progress) {
      return function(nodesRef) {
        return function() {
          const nodes = nodesRef.value;
          for (let i = 0; i < nodes.length; i++) {
            const node = nodes[i];
            const key = String(node.id);
            const start = startPositions[key];
            const target = targetPositions[key];
            if (start && target) {
              node.x = start.x + (target.x - start.x) * progress;
              node.y = start.y + (target.y - start.y) * progress;
            }
          }
        };
      };
    };
  };
}

// =============================================================================
// Pinning (fx/fy control)
// =============================================================================

// Pin all nodes at their current positions (fx = x, fy = y)
export function pinNodesInPlace_(nodesRef) {
  return function() {
    const nodes = nodesRef.value;
    for (let i = 0; i < nodes.length; i++) {
      const node = nodes[i];
      node.fx = node.x;
      node.fy = node.y;
    }
  };
}

// Unpin all nodes (fx = null, fy = null)
export function unpinNodesInPlace_(nodesRef) {
  return function() {
    const nodes = nodesRef.value;
    for (let i = 0; i < nodes.length; i++) {
      const node = nodes[i];
      node.fx = null;
      node.fy = null;
    }
  };
}

// Pin nodes at specific positions from a positions map
export function pinNodesAtPositions_(positions) {
  return function(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        const pos = positions[node.id];
        if (pos) {
          node.x = pos.x;
          node.y = pos.y;
          node.fx = pos.x;
          node.fy = pos.y;
        }
      }
    };
  };
}

// =============================================================================
// Grid Position Updates (for ForceXGrid/ForceYGrid forces)
// =============================================================================

// Update node gridX/gridY positions in place from a positions map
// This is used to change force targets before reheating
// positions: Object keyed by node.id (as string) -> { x, y }
// nodesRef: PureScript Ref containing Array of nodes
export function updateGridPositionsInPlace_(positions) {
  return function(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        const pos = positions[node.id];
        if (pos) {
          node.gridX = pos.x;
          node.gridY = pos.y;
        }
      }
    };
  };
}

// Update only gridX for all nodes using a function
// Useful for toggle animations where only X changes
// xFn: (node) -> Number (the new gridX value)
export function updateGridXWithFn_(xFn) {
  return function(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        node.gridX = xFn(node);
      }
    };
  };
}

// Update only gridY for all nodes using a function
// yFn: (node) -> Number (the new gridY value)
export function updateGridYWithFn_(yFn) {
  return function(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        node.gridY = yFn(node);
      }
    };
  };
}
