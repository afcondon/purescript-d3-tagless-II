// Parse JSON string into hierarchical data
export function parseFlareJson(jsonString) {
  return JSON.parse(jsonString);
}

// FFI accessors for hierarchical JSON data
export const getName = (node) => node.name || "";
export const getValue = (node) => node.value || 0;
export const getChildren_ = (node) => {
  if (node.children && Array.isArray(node.children) && node.children.length > 0) {
    return node.children;
  }
  return null;
};
