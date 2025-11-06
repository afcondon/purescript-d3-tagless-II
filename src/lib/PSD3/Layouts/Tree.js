/**
 * FFI for PSD3.Layouts.Tree
 * Provides D3 tree layout constructors and position extraction helpers
 */

// Tree layout constructors
export function d3Tree_(unit) {
  return d3.tree();
}

export function d3Cluster_(unit) {
  return d3.cluster();
}

// Position extraction helpers
// These extract the x, y, depth properties that D3 adds to tree nodes

export function extractID(node) {
  // Try common ID field names
  if (node.data && node.data.id !== undefined) {
    return node.data.id;
  }
  if (node.id !== undefined) {
    return node.id;
  }
  // Fallback: use a stringified version of the node
  return JSON.stringify(node.data || node);
}

export function extractX(node) {
  return node.x || 0;
}

export function extractY(node) {
  return node.y || 0;
}

export function extractDepth(node) {
  return node.depth || 0;
}
