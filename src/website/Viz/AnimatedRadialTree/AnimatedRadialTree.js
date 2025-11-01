// FFI for AnimatedRadialTree module

// Get d3.tree() layout (Tidy Tree algorithm)
export const d3Tree_ = () => {
  console.log(`🌳 Creating d3.tree() layout`);
  return d3.tree();
};

// Get d3.cluster() layout (Dendrogram algorithm)
export const d3Cluster_ = () => {
  console.log(`🌳 Creating d3.cluster() layout`);
  return d3.cluster();
};

// Generate a stable key for tree nodes by building path from root to node
export const treeNodeKey_ = (d, i) => {
  // Build path from root to this node using names
  const path = [];
  let current = d;
  while (current) {
    if (current.data && current.data.name) {
      path.unshift(current.data.name);
    }
    current = current.parent;
  }
  const key = path.join('/');
  // Log first few keys to debug
  if (i !== undefined && i < 5) {
    console.log(`🔑 Key for NODE ${i}: "${key}" (node name: ${d.data?.name})`);
  }
  return key;
};

// Generate a stable key for tree LINKS - uses the target node's path
export const treeLinkKey_ = (d, i) => {
  // For links, d has structure { source: {...}, target: {...} }
  // We use the target node to generate the key
  const target = d.target;
  const path = [];
  let current = target;
  while (current) {
    if (current.data && current.data.name) {
      path.unshift(current.data.name);
    }
    current = current.parent;
  }
  const key = path.join('/');
  // Log first few keys to debug
  if (i !== undefined && i < 5) {
    console.log(`🔑 Key for LINK ${i}: "${key}" (target name: ${target.data?.name})`);
  }
  return key;
};

// Get the size of an array or D3 selection
export const selectionSize_ = (sel) => {
  if (Array.isArray(sel)) {
    return sel.length;
  }
  if (sel && sel.size) {
    return sel.size();
  }
  return 0;
};

// Debug helper to inspect what elements exist in a container
export const debugInspectContainer_ = (container, label) => {
  console.log(`🔍 Inspecting container "${label}":`);
  console.log(`   - Container node:`, container.node());
  console.log(`   - Size:`, container.size());

  // Try to select all 'g' elements
  const groups = container.selectAll('g');
  console.log(`   - 'g' elements found: ${groups.size()}`);

  // Log first few elements and their __data__
  groups.each(function(d, i) {
    if (i < 5) {
      console.log(`   - Group ${i}:`, this, 'has __data__:', d);
      if (d && d.data) {
        console.log(`     -> data.name: ${d.data.name}, x: ${d.x}, y: ${d.y}`);
      }
    }
  });

  return 0; // Return int for PureScript
};

// Debug helper to log first few node coordinates
export const debugLogCoordinates_ = (data, label) => {
  console.log(`📍 Coordinates for "${label}":`);
  for (let i = 0; i < Math.min(5, data.length); i++) {
    const d = data[i];
    if (d && d.data) {
      console.log(`   - Node ${i} (${d.data.name}): x=${d.x?.toFixed(4)}, y=${d.y?.toFixed(4)}`);
    }
  }
  return 0;
};
