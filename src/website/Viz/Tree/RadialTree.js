// FFI for RadialTree module

export const getLayout = (treeType) => {
  // TreeType constructors from PureScript: TidyTree or Dendrogram
  if (treeType.constructor.name === "TidyTree") {
    return d3.tree();
  } else {
    return d3.cluster();
  }
};
