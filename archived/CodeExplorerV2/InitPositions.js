// FFI for initializing node positions

/**
 * Initialize x/y from gridXY for nodes that don't have positions
 * Mutates the node objects
 * Note: Returns an Effect thunk (a function) as required by PureScript FFI
 */
export function initPositionsFromGridXY_(nodes) {
  return function() {
    // Debug: log first node before
    if (nodes.length > 0) {
      console.log(`[InitPositions] BEFORE - First node x: ${nodes[0].x}, y: ${nodes[0].y}, has gridXY: ${!!nodes[0].gridXY}`);
    }

    let initialized = 0;
    nodes.forEach(node => {
      if (node.gridXY) {
        if (node.x == null) {
          node.x = node.gridXY.x;
          initialized++;
        }
        if (node.y == null) {
          node.y = node.gridXY.y;
        }
      }
    });

    // Debug: log first node after
    if (nodes.length > 0) {
      console.log(`[InitPositions] AFTER - First node x: ${nodes[0].x}, y: ${nodes[0].y}`);
    }

    console.log(`[InitPositions] Initialized ${initialized} nodes from gridXY (total: ${nodes.length})`);
    return nodes;
  };
}
