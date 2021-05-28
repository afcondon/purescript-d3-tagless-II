
// designed to be usable directly inside the custom force, config ignored for now
exports.forceClusterCollision_ = config => {
  forceClusterCollision()
    .radius(d => d.radius + 1)
    .strength(0.8)
    .clusterPadding(10)
}

// function taken directly from Nadia Bremer's Observable, based on earlier Mike Bostock example from D3v3
// changes:
//        d.r changed to d.radius for readability / consistency on PureScript side
function forceClusterCollision() {
  let nodes
  let radii
  let strength = 1
  let iterations = 1
  let clusterPadding = 0 //addition

  function radius(d) { return d.radius }
  function x(d) { return d.x + d.vx }
  function y(d) { return d.y + d.vy }
  function constant(x) { return function() { return x } }
  function jiggle() { return 1e-6 } //change - PLEASE no Math.random() in there ಥ﹏ಥ
  // function jiggle() { return (Math.random() - 0.5) * 1e-6 }

  function force() {
    let i
    let n = nodes.length
    let tree
    let node
    let xi
    let yi
    let ri
    let ri2

    for (let k = 0; k < iterations; ++k) {
      tree = d3.quadtree(nodes, x, y).visitAfter(prepare)
      for (i = 0; i < n; ++i) {
        node = nodes[i]
        ri = radii[node.index]
        ri2 = ri * ri
        xi = node.x + node.vx
        yi = node.y + node.vy
        tree.visit(apply)
      }//for i
    }//for k

    function apply(quad, x0, y0, x1, y1) {
      let data = quad.data
      let rj = quad.r
      let r = ri + rj + clusterPadding //change
      if (data) {
        if (data.index > node.index) {
          let x = xi - data.x - data.vx
          let y = yi - data.y - data.vy
          let l = x * x + y * y
          r = ri + rj + (node.cluster !== quad.data.cluster ? clusterPadding : 0) //addition

          if (l < r * r) {
            if (x === 0) x = jiggle(), l += x * x
            if (y === 0) y = jiggle(), l += y * y
            l = (r - (l = Math.sqrt(l))) / l * strength
            node.vx += (x *= l) * (r = (rj *= rj) / (ri2 + rj))
            node.vy += (y *= l) * r
            data.vx -= x * (r = 1 - r)
            data.vy -= y * r
          }//if
        }//if
        return
      }//if
        return x0 > xi + r || x1 < xi - r || y0 > yi + r || y1 < yi - r
      }//apply
    }//force

    function prepare(quad) {
      if (quad.data) return quad.r = radii[quad.data.index];
      for (let i = quad.r = 0; i < 4; ++i) {
        if (quad[i] && quad[i].r > quad.r) {
          quad.r = quad[i].r
        }//if
      }//for i
    }

    function initialize() {
      if (!nodes) return;
      let i, n = nodes.length, node
      radii = new Array(n)
      for (i = 0; i < n; ++i) node = nodes[i], radii[node.index] = +radius(node, i, nodes)
    }

    force.initialize = function (_) {
      nodes = _
      initialize()
      return force
    }

    force.iterations = function (_) {
      return arguments.length ? (iterations = +_, force) : iterations
    }

    //I wish strength could be a function of the node as well...
    force.strength = function (_) {
      return arguments.length ? (strength = +_, force) : strength
    }

    force.radius = function (_) {
      return arguments.length ? (radius = typeof _ === "function" ? _ : constant(+_), force) : radius
    }
    
    //addition - the actual pixels of padding
    force.clusterPadding = function (_) {
      return arguments.length ? (clusterPadding = +_, force) : clusterPadding
    }

    return force
  }//function forceCollision