// foreign import readJSONJS               :: String -> TreeJson -- TODO no error handling at all here RN
exports.readJSON_ = filecontents => JSON.parse(filecontents)

// foreign import d3Hierarchy              :: TreeJson -> D3Hierarchical
// NB this chain of functions may not be appropriate in all cases, dates back to when only radial tree was wrapped
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
exports.hierarchy_ = json => d3.hierarchy(json).sort((a, b) => d3.ascending(a.data.name, b.data.name))

// foreign import hasChildren              :: Datum -> Boolean
exports.hasChildren_ = d => !d.children

// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.links_ = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.descendants_ = tree => tree.descendants()

// foreign import find_        :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Nullable D3HierarchicalNode_
exports.find_ = tree => filter => tree.find(filter)

// foreign import nodeSize :: D3HierarchicalNode_ -> Array Number -> D3HierarchicalNode_ -- TODO "returns this tree layout" is that node or tree config?
exports.nodeSize_ = root => dxdy => {
  return root.nodeSize(dxdy);
}