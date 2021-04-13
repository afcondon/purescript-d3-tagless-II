// foreign import radialSeparationJS       :: Datum -> Datum -> Int
// this REQUIRES that the data have been passed thru the d3.herarchy (and maybe d3.tree?) function
// NB because this is called directly from deep in the bowels of D3 (tree first walk to be precise) it isn't a curried function
exports.radialSeparationJS_ = (a,b) => (a.parent == b.parent ? 1 : 2) / a.depth
// foreign import d3InitTree               :: forall a. TreeConfig a -> D3Hierarchical -> D3Tree 
exports.initRadialTree_ = config => data => d3.tree().size(config.size).separation(config.separation)(data)
// foreign import d3LinkRadial_            :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
exports.linkRadial_ = angleFn => radiusFn => d3.linkRadial().angle(angleFn).radius(radiusFn)
