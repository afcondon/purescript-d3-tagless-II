//pruneEmptyChildren :: Tree MetaTreeNode -> TreeJson_
export function pruneEmptyChildren(node) {
  prune(node);
  return node;
}

prune = function (node) {
  if (node.children.length == 0) {
    delete node.children;
  } else {
    node.children.forEach(child => prune(child)
    )
  };
}