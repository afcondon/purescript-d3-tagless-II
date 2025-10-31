// FFI for shared Tree helper functions

export const unsafeFieldImpl = (field) => (obj) => obj[field];

export const unsafeCoerceImpl = (x) => x;

export const getLayout = (treeType) => {
  // TreeType constructors from PureScript: TidyTree or Dendrogram
  if (treeType.constructor.name === "TidyTree") {
    return d3.tree();
  } else {
    return d3.cluster();
  }
};

export const hasChildren_ = (node) => {
  return node.children && node.children.length > 0;
};
