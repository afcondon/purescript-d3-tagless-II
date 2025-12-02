// FFI for in-place node mutations
// These functions mutate the simulation's node array directly,
// preserving the reference that D3 data binding relies on.

// Apply a PureScript transform function to all nodes in place
// transform :: SimNode -> SimNode
export function applyTransformInPlace_(transform) {
  return function(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const original = nodes[i];
        const updated = transform(original);
        // Copy all properties from updated back to original object
        // This preserves object identity (critical for D3 binding)
        Object.assign(original, updated);
      }
    };
  };
}

// Apply transform only to nodes matching a predicate
// predicate :: SimNode -> Boolean
// transform :: SimNode -> SimNode
export function applyTransformWhereInPlace_(predicate) {
  return function(transform) {
    return function(nodesRef) {
      return function() {
        const nodes = nodesRef.value;
        let matchCount = 0;
        let firstMatch = null;
        for (let i = 0; i < nodes.length; i++) {
          const original = nodes[i];
          if (predicate(original)) {
            matchCount++;
            const oldGridX = original.gridX;
            const oldGridY = original.gridY;
            const updated = transform(original);
            Object.assign(original, updated);
            if (!firstMatch && original.nodeType === 'ModuleNode') {
              firstMatch = {
                id: original.id,
                oldGridX, oldGridY,
                newGridX: original.gridX,
                newGridY: original.gridY
              };
            }
          }
        }
        if (firstMatch) {
          console.log('[ApplyInPlace] Matched', matchCount, 'nodes. First module:', firstMatch);
        }
      };
    };
  };
}
