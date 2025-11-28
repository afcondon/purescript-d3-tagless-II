// Parse JSON string to TreeJson_
export function readJSON_(filecontents) {
  return JSON.parse(filecontents);
}

// =======================================================================================
// D3 Native Hierarchy FFI
// =======================================================================================

// Build D3 hierarchy from JSON
export function hierarchyFromJSON_(json) {
  return d3.hierarchy(json);
}

// Get all descendants of a tree node
export function descendants_(tree) {
  return tree.descendants();
}

// Get all links (parent-child connections) in the tree
export function links_(tree) {
  return tree.links();
}

// Run a D3 layout function on a tree
export function runLayoutFn_(layout) {
  return root => layout(root);
}

// Set node size for D3 tree layout
export function treeSetNodeSize_(tree) {
  return widthHeight => tree.nodeSize(widthHeight);
}

// Calculate min/max extents of tree coordinates
export function treeMinMax_(root) {
  let max_x = -(Infinity);
  let min_x = Infinity;
  let max_y = -(Infinity);
  let min_y = Infinity;

  root.each(d => {
    if (d.x > max_x) max_x = d.x;
    if (d.y > max_y) max_y = d.y;
    if (d.x < min_x) min_x = d.x;
    if (d.y < min_y) min_y = d.y;
  });

  return { xMin: min_x, xMax: max_x, yMin: min_y, yMax: max_y };
}

// Get height of tree node (max depth of descendants)
export function hNodeHeight_(node) {
  return node.height;
}

// =======================================================================================
// Link Path Generators
// =======================================================================================

// Horizontal tree link (smooth curve, left-to-right)
// For horizontal trees, x and y are swapped
export const horizontalLink = d3
  .linkHorizontal()
  .x(d => d.y)  // Use y coordinate for horizontal position
  .y(d => d.x); // Use x coordinate for vertical position

// Horizontal cluster link (right-angle connector for dendrograms)
// Creates stepped path with horizontal then vertical segments
export function horizontalClusterLink_(interLevel) {
  return function(d) {
    const source = d.source;
    const target = d.target;

    // Create a right-angle path:
    // 1. Start at source (parent)
    // 2. Go horizontally to the level of the target
    // 3. Go vertically to the target
    const midX = source.y + interLevel;

    return `M${source.y},${source.x} H${midX} V${target.x} H${target.y}`;
  };
}
