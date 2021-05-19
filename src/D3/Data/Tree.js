exports.idTreeLeaf_ = obj => {
  const treeObj = Object.assign({}, obj)
  treeObj.isLeaf = true
  return treeObj
}
exports.idTreeParent_ = obj => children => {
  const treeObj = Object.assign({}, obj)
  treeObj.isLeaf = false
  treeObj.children = children
  return treeObj
}

exports.emptyTreeJson_ = {}
