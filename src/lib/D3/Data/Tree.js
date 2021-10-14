exports.idTreeLeaf_ = obj => {
  const treeObj = Object.assign({}, obj)
  treeObj.isTreeLeaf = true
  return treeObj
}
exports.idTreeParent_ = obj => children => {
  const treeObj = Object.assign({}, obj)
  treeObj.isTreeLeaf = false
  treeObj.children = children
  return treeObj
}

exports.emptyTreeJson_ = {}
