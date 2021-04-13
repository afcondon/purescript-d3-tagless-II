// foreign import radialSeparationJS       :: Datum -> Datum -> Int
// this REQUIRES that the data have been passed thru the d3.herarchy (and maybe d3.tree?) function
// NB because this is called directly from deep in the bowels of D3 (tree first walk to be precise) it isn't a curried function
exports.radialSeparationJS_ = (a,b) => (a.parent == b.parent ? 1 : 2) / a.depth

// foreign import readJSONJS               :: String -> TreeJson -- TODO no error handling at all here RN
exports.readJSON_ = filecontents => JSON.parse(filecontents)

// foreign import d3Hierarchy              :: TreeJson -> D3Hierarchical
// NB this chain of functions may not be appropriate in all cases, dates back to when only radial tree was wrapped
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
exports.hierarchy_ = json => d3.hierarchy(json).sort((a, b) => d3.ascending(a.data.name, b.data.name))

// foreign import d3InitTree               :: forall a. TreeConfig a -> D3Hierarchical -> D3Tree 
exports.initRadialTree_ = config => data => d3.tree().size(config.size).separation(config.separation)(data)
// TODO allow custom separations etc, either thru config with nulls or API
exports.initHorizontalTree_ = config => data => d3.tree()(data)

// foreign import hasChildren              :: Datum -> Boolean
exports.hasChildren_ = d => !d.children


// foreign import d3LinkRadial_            :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
exports.linkRadial_ = angleFn => radiusFn => d3.linkRadial().angle(angleFn).radius(radiusFn)
// foreign import d3LinkHorizontal_        :: Unit -> (Datum -> String)
exports.linkHorizontal_ = (d) => d3.linkHorizontal(d)


// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.links_ = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.descendants_ = tree => tree.descendants()

// foreign import find_        :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Nullable D3HierarchicalNode_
exports.find_ = tree => filter => tree.find(filter)

exports.horizontalTreeX0X1_ = root => {
  let x0 = Infinity;
  let x1 = -x0;
  root.each(d => {
    if (d.x > x1) x1 = d.x;
    if (d.x < x0) x0 = d.x;
  });
  return [x0, x1];
}

// foreign import nodeSize :: D3HierarchicalNode_ -> Array Number -> D3HierarchicalNode_ -- TODO "returns this tree layout" is that node or tree config?
exports.nodeSize_ = root => dxdy => {
  return root.nodeSize(dxdy);
}