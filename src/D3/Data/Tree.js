exports.idTreeLeaf_ = id => name => { return { name: name, id: id }}
exports.idTreeParent_ = id => name => children => { return { name: name, id: id, children: children } }