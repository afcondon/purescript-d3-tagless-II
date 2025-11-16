// FFI for blessed JSON hierarchical data access

export const getName = (node) => node.name || "";

export const getValue = (node) => node.value || 0;

export const getChildren_ = (node) => {
  if (node.children && Array.isArray(node.children) && node.children.length > 0) {
    return node.children;
  }
  return null;
};

export const hasChildren = (node) => {
  return node.children && Array.isArray(node.children) && node.children.length > 0;
};
