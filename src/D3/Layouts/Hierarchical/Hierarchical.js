// foreign import readJSONJS               :: String -> TreeJson -- TODO no error handling at all here RN
exports.readJSON_ = filecontents => JSON.parse(filecontents)

// foreign import d3Hierarchy              :: TreeJson -> D3Hierarchical
// NB this chain of functions may not be appropriate in all cases, dates back to when only radial tree was wrapped
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
exports.hierarchyFromJSON_ = json => d3.hierarchy(json).sort((a, b) => d3.ascending(a.data.name, b.data.name))

// foreign import hasChildren              :: Datum -> Boolean
exports.hasChildren_ = d => !d.children

// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.links_ = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.descendants_ = tree => tree.descendants()

// foreign import find_        :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Nullable D3HierarchicalNode_
exports.find_ = tree => filter => tree.find(filter)

// foreign import initTree_ :: Unit -> D3TreeLike_
exports.initTree_ = () => d3.tree()
// foreign import initTree_ :: Unit -> D3TreeLike_
exports.initCluster_ = () => d3.cluster()
// foreign import treeSetRoot_ :: D3TreeLike_ -> D3HierarchicalNode_ -> D3HierarchicalNode_
exports.treeSetRoot_ = tree => root => tree(root)
// foreign import treeSetNodeSize_ :: D3TreeLike_ -> Array Number -> D3TreeLike_
exports.treeSetNodeSize_ = tree => widthHeight => tree.nodeSize(widthHeight) 
// foreign import treeSetSize_     :: D3TreeLike_ -> Array Number -> D3TreeLike_
exports.treeSetSize_ = tree => widthHeight => tree.size(widthHeight)

exports.treeMinMax_ = root => {
  let x0 = Infinity;
  let x1 = -x0;
  let y0 = Infinity;
  let y1 = -y0;
  root.each(d => {
    if (d.x > x1) x1 = d.x;
    if (d.x < x0) x0 = d.x;
    if (d.y > y1) y1 = d.y; // don't know if we will ever need the u versions but we'll calc anyway
    if (d.y < y0) y0 = d.y;
  });
  return { xMin: x0, xMax: x1, yMin: y0, yMax: y1 };
}

exports.linkHorizontal_ = d3.linkHorizontal().x(d => d.y).y(d => d.x)
exports.linkVertical_   = d3.linkHorizontal().x(d => d.x).y(d => d.y)

// TODO this link function is actually the horizontal variant
exports.linkClusterHorizontal_ = yOffset => d => {`
  M${d.target.y}, ${d.target.x}
  C${d.source.y + yOffset / 2},${d.target.x}
   ${d.source.y + yOffset / 2},${d.source.x}
   ${d.source.y},${d.source.x}
`}

// foreign import d3LinkRadial_            :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
exports.linkRadial_ = angleFn => radiusFn => d3.linkRadial().angle(angleFn).radius(radiusFn);

// treeSetSeparation_ :: D3TreeLike_ -> (D3HierarchicalNode_ -> D3HierarchicalNode_ -> Number) -> D3TreeLike_
exports.treeSetSeparation_ = tree => separationFn => tree.separation(separationFn); 

// foreign import shareParent :: D3HierarchicalNode_ -> D3HierarchicalNode_ -> Boolean
exports.sharesParent = a => b => (a.parent == b.parent)