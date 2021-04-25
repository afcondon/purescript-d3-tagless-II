exports.initHorizontalCluster_ = root => rootDxDy => d3.cluster().nodeSize(rootDxDy)(root)
exports.linkHorizontalCluster_ = d => {`
  M${d.target.y}, ${d.target.x}
  C${d.source.y + root.dy / 2},${d.target.x}
   ${d.source.y + root.dy / 2},${d.source.x}
   ${d.source.y},${d.source.x}
`}

