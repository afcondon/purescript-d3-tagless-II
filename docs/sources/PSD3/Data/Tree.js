export function idTreeLeaf_(obj) {
  const treeObj = Object.assign({}, obj)
  treeObj.isTreeLeaf = true
  return treeObj
}
export function idTreeParent_(obj) {
  return children => {
    const treeObj = Object.assign({}, obj)
    treeObj.isTreeLeaf = false
    treeObj.children = children
    return treeObj
  }
}

export const emptyTreeJson_ = {}
