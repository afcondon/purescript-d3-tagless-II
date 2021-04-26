exports.initRadialTree_ = config => root => 
  d3.tree()
    .size(config.size) // widthHeight
    .separation((a,b) => (a.parent == b.parent ? 1 : 2) / a.depth)
    (root)

exports.initHorizontalTree_ = root => rootDxDy => 
  d3.tree()
    .nodeSize(rootDxDy)
    (root)

// const root = d3.hierarchy(data).sort((a, b) => 
//   d3.descending(a.height, b.height) || d3.ascending(a.data.name, b.data.name));
exports.initHorizontalCluster_ = root => rootDxDy => 
  d3.cluster()
    .nodeSize(rootDxDy)
    (root)

exports.linkHorizontal_ = 
  d3.linkHorizontal()
    .x(d => d.y)
    .y(d => d.x)

exports.linkRadial_ = angleFn => radiusFn => 
  d3.linkRadial()
    .angle(angleFn)   // equivalent to link.x except the accessor returns the angle in radians, with 0 at -y (12 o’clock).
    .radius(radiusFn) // equivalent to link.y, except the accessor returns the radius: the distance from the origin ⟨0,0⟩

exports.linkClusterTree_ = d => {`
  M${d.target.y}, ${d.target.x}
  C${d.source.y + root.dy / 2},${d.target.x}
    ${d.source.y + root.dy / 2},${d.source.x}
    ${d.source.y},${d.source.x}
`}
  