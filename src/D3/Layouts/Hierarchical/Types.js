// hNodeDepth_  :: D3HierarchicalNode_ -> Int
exports.hNodeDepth_ = node => node.depth;

// hNodeHeight_ :: D3HierarchicalNode_ -> Int
exports.hNodeHeight_ = node => node.height;

// hNodeX_      :: D3HierarchicalNode_ -> Number
exports.hNodeX_ = node => node.x;

// hNodeY_      :: D3HierarchicalNode_ -> Number
exports.hNodeY_ = node => node.y;

// nullModel_ :: forall d v. d -> v -> Model d v
exports.nullModel_ = d => v => { return { root_: null } }; 
