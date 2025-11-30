// Parse JSON string into blessed hierarchical data
export function parseFlareJson(jsonString) {
  return JSON.parse(jsonString);
}

// FFI accessors for blessed JSON data
export const getName = (node) => node.name || "";

export const getValue = (node) => node.value || 0;

export const getChildren_ = (node) => {
  if (node.children && Array.isArray(node.children) && node.children.length > 0) {
    return node.children;
  }
  return null;
};

// Parse flare-imports JSON (array of {name, size, imports})
export function parseFlareImportsJson(jsonString) {
  return JSON.parse(jsonString);
}

// FFI accessors for flare-imports data
export const getImportNodeName = (node) => node.name || "";
export const getImportNodeSize = (node) => node.size || 0;
export const getImportNodeImports = (node) => node.imports || [];
