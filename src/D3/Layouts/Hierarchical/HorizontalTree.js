// foreign import d3InitTree               :: forall a. TreeConfig a -> D3Hierarchical -> D3HierarchicalNode_ 
exports.initHorizontalTree_ = config => data => d3.tree()(data)

// foreign import d3LinkHorizontal_        :: Unit -> (Datum -> String)
exports.linkHorizontal_ = (d) => d3.linkHorizontal(d)

exports.horizontalTreeX0X1_ = root => {
  let x0 = Infinity;
  let x1 = -x0;
  root.each(d => {
    if (d.x > x1) x1 = d.x;
    if (d.x < x0) x0 = d.x;
  });
  return [x0, x1];
}
