// foreign import d3InitTree:: forall a. D3Hierarchical -> Array Number -> D3HierarchicalNode_ 
exports.initHorizontalTree_ = root => rootDxDy => d3.tree().nodeSize(rootDxDy)(root)

// foreign import d3LinkHorizontal_        :: Datum -> String
// TODO this should be expressable in PureScript, not buried here in the FFI
exports.linkHorizontal_ = d3.linkHorizontal().x(d => d.y).y(d => d.x)

exports.horizontalTreeX0X1_ = root => {
  let x0 = Infinity;
  let x1 = -x0;
  root.each(d => {
    if (d.x > x1) x1 = d.x;
    if (d.x < x0) x0 = d.x;
  });
  return [x0, x1];
}
