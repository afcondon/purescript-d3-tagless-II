var debug = true;

//            SIMULATION functions
exports.initSimulation_ = nodes => config => {
  const simulation = 
  d3.forceSimulation(nodes)
  .alpha(config.alpha) // default is 1
  .alphaTarget(config.alphaTarget) // default is 0
  .alphaMin(config.alphaMin) // default is 0.0001
  .alphaDecay(config.alphaDecay) // default is 0.0228
  .velocityDecay(config.velocityDecay) // default is 0.4
  if(debug){ console.log(`initSimulation${simulation}`)}
  return simulation;
}
//  :: Simulation -> Array NativeNode -> Array NativeNode
exports.setNodes_ = simulation => nodes => { 
  if(debug){ console.log(`${simulation}.nodes(${nodes})`)}
  simulation.nodes(nodes)
  return simulation;
}
//  :: Simulation -> Array NativeLink -> Array NativeLink
exports.setLinks_ = simulation => links => { 
  simulation.force("links", d3.forceLink(links).id(d => d.id))
  return simulation;
}
// :: NativeSelection -> Number -> Unit
exports.setAlphaTarget_ = simulation => target => simulation.alphaTarget(target)
//  :: NativeSelection -> Unit
exports.startSimulation_ = simulation => simulation.restart()
//  :: NativeSelection -> Unit
exports.stopSimulation_ = simulation => simulation.stop()

// simulation.on("tick", () => {
exports.onTick_ = simulation => name => tickFn => {
  // if(debug){ console.log(`${simulation}.onTick(${tickFn})`)}
  return simulation.on("tick."+name, () => tickFn());
}


// default drag function, useful in probably most simulations
exports.attachDefaultDragBehavior_ = selection => simulation => {
  
  var drag = function(simulation) {
    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }
    
    function dragged(event,d) {
      d.fx = event.x;
      d.fy = event.y;
    }
    
    function dragended(event,d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }
    
    return d3.drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended);  
  }

  selection.call(drag(simulation))
}

//            FORCE functions 
// :: Simulation -> Unit
exports.forceMany_ = simulation => label => simulation.force(label, d3.forceManyBody())
// :: Simulation -> Number -> Number -> Unit
exports.forceCenter_ = simulation => label => cx => cy => simulation.force(label, d3.forceCenter(cx,cy))
// :: Simulation -> Unit
// exports.forceLinks = simulation => label => simulation.x()
// :: Simulation -> Number -> Unit
exports.forceCollide_ = simulation => label => radius => simulation.force(label, d3.forceCollide(radius))
// :: Simulation -> Number -> Unit
exports.forceX_ = simulation => label => cx => simulation.force(label, d3.forceX(cx))
// :: Simulation -> Number -> Unit
exports.forceY_ = simulation => label => cy => simulation.force(label, d3.forceY(cy))
// :: Simulation -> Number -> Number -> Unit
exports.forceRadial_ = simulation => label => cx => cy => simulation.force(label, d3.forceRadial(cx, cy))

