// Render bubble pack circles inside each node group
// Takes packed modules data and replaces single circles with nested packs
export function renderBubblePackCircles_(packedModules) {
  return function() {
    const d3 = window.d3;

    // Build a map from module name to circles
    const packMap = new Map();
    for (const pm of packedModules) {
      packMap.set(pm.name, {
        circles: pm.circles,
        radius: pm.radius
      });
    }

    console.log("renderBubblePackCircles: processing", packMap.size, "modules");

    // Select all node groups
    const nodeGroups = d3.selectAll("g.nodes > g");

    nodeGroups.each(function(d) {
      if (!d || !d.name) return;

      const packData = packMap.get(d.name);
      if (!packData) return;

      const group = d3.select(this);

      // Update the node's radius to match the packed radius
      // This allows the collision force to use the correct size
      d.r = packData.radius;

      // Remove existing circle and text
      group.selectAll("circle").remove();
      group.selectAll("text").remove();

      // Append packed circles
      group.selectAll("circle.pack")
        .data(packData.circles)
        .enter()
        .append("circle")
        .attr("class", "pack")
        .attr("cx", c => c.x)
        .attr("cy", c => c.y)
        .attr("r", c => c.r)
        .attr("fill", c => depthToFill(c.depth))
        .attr("stroke", c => depthToStroke(c.depth))
        .attr("stroke-width", c => c.depth === 0 ? 1.5 : 0.5)
        .attr("opacity", c => c.depth === 0 ? 0.3 : 0.8);

      // Add module name as label
      group.append("text")
        .attr("text-anchor", "middle")
        .attr("dy", "0.3em")
        .attr("font-size", "8px")
        .attr("fill", "#333")
        .attr("pointer-events", "none")
        .text(d.name.split(".").pop()); // Just the last part of module name
    });

    console.log("renderBubblePackCircles: done");
  };
}

function depthToFill(depth) {
  switch(depth) {
    case 0: return "#f0f0f0";  // Module (outer) - very light gray
    case 1: return "#4a90d9";  // Category - blue
    default: return "#2d5a87"; // Declaration - dark blue
  }
}

function depthToStroke(depth) {
  switch(depth) {
    case 0: return "#999";     // Module outline
    case 1: return "#3a7bc8";  // Category
    default: return "#1d4a77"; // Declaration
  }
}
