// foreign import radialSeparationJS       :: Datum -> Datum -> Int
// this REQUIRES that the data have been passed thru the d3.herarchy (and maybe d3.tree?) function
// NB because this is called directly from deep in the bowels of D3 (tree first walk to be precise) it isn't a curried function
exports.radialSeparationJS_ = (a,b) => (a.parent == b.parent ? 1 : 2) / a.depth

// foreign import readJSONJS               :: String -> TreeJson -- TODO no error handling at all here RN
exports.readJSONJS_ = filecontents => JSON.parse(filecontents)

// foreign import d3Hierarchy              :: TreeJson -> D3Hierarchical
exports.d3Hierarchy_ = json => d3.hierarchy(json).sort((a, b) => d3.ascending(a.data.name, b.data.name))

// foreign import d3InitTree               :: forall a. TreeConfig a -> D3Hierarchical -> D3Tree 
exports.d3InitTree_ = config => hierarchy => d3.tree().size(config.size).separation(config.separation)(hierarchy)

// foreign import hasChildren              :: Datum -> Boolean
exports.hasChildren_ = d => !d.children



exports.d3LinkRadial_ = angleFn => radiusFn => d3.linkRadial().angle(angleFn).radius(radiusFn)


// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.d3HierarchyLinks_ = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.d3HierarchyDescendants_ = tree => tree.descendants()
