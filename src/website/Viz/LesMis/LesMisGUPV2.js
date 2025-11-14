// FFI for mutating node positions in LesMisGUPV2
import * as d3 from 'd3';

// Mutate object to set fx/fy (for pinning to positions)
export function setNodeFxFy_(node) {
  return function(fx) {
    return function(fy) {
      return function() {
        node.fx = fx;
        node.fy = fy;
        return node;
      };
    };
  };
}

// Mutate object to clear fx/fy (for unpinning)
export function clearNodeFxFy_(node) {
  return function() {
    node.fx = null;
    node.fy = null;
    return node;
  };
}

// Mutate array of nodes to set fx/fy to grid positions
export function setNodesGridPositions_(nodes) {
  return function(gridSpacing) {
    return function() {
      const nodeCount = nodes.length;
      const columns = Math.floor(Math.ceil(Math.sqrt(nodeCount)));
      const offset = -(columns * gridSpacing) / 2.0;

      nodes.forEach((node, i) => {
        const d = i;
        const c = columns;
        const x = (d % c);
        const y = Math.floor(d / c);

        node.fx = x * gridSpacing + offset;
        node.fy = y * gridSpacing + offset;
      });

      return nodes;
    };
  };
}

// Mutate array of nodes to set fx/fy to phylotaxis positions
export function setNodesPhyllotaxisPositions_(nodes) {
  return function() {
    const initialRadius = 10.0;
    const initialAngle = Math.PI * (3.0 - Math.sqrt(5.0));  // Golden angle

    nodes.forEach((node, index) => {
      const i = index;
      const radius = initialRadius * Math.sqrt(0.5 + i);
      const angle = i * initialAngle;

      node.fx = radius * Math.cos(angle);
      node.fy = radius * Math.sin(angle);
    });

    return nodes;
  };
}

// Mutate array of nodes to clear fx/fy (unpin all)
export function clearNodesFxFy_(nodes) {
  return function() {
    nodes.forEach(node => {
      node.fx = null;
      node.fy = null;
    });
    return nodes;
  };
}

// Transition nodes and links to their fx/fy positions using D3 transitions
// This creates a smooth animation to the pinned positions
export function transitionToFxFyPositions_(svgSelector) {
  return function(nodeSelector) {
    return function(linkSelector) {
      return function(nodes) {
        return function(onComplete) {
          return function() {
            const svg = d3.select(svgSelector);
            const nodeElements = svg.selectAll(nodeSelector);
            const linkElements = svg.selectAll(linkSelector);

            if (nodeElements.empty()) {
              console.error(`Could not find ${nodeSelector} elements to transition`);
              onComplete();
              return;
            }

            // Create a 1.5 second transition
            const t = d3.transition().duration(1500);

            // Transition node circles to fx/fy positions
            nodeElements
              .transition(t)
              .attr('cx', (d, i) => {
                const node = nodes[i];
                if (node && node.fx !== null && node.fx !== undefined) {
                  return node.fx;
                }
                return d.x;
              })
              .attr('cy', (d, i) => {
                const node = nodes[i];
                if (node && node.fy !== null && node.fy !== undefined) {
                  return node.fy;
                }
                return d.y;
              })
              .on('end', function(d, i) {
                // When last node finishes, call completion callback
                if (i === nodes.length - 1) {
                  onComplete();
                }
              });

            // Transition links to follow nodes to fx/fy positions
            // Note: link data is IndexedLink wrapper: { index, link: { source, target, ... } }
            linkElements
              .transition(t)
              .attr('x1', function(d) {
                // Unwrap IndexedLink to get actual link
                const actualLink = d.link || d;
                const sourceIndex = nodes.findIndex(n => n.id === actualLink.source.id);
                if (sourceIndex >= 0 && nodes[sourceIndex].fx !== null) {
                  return nodes[sourceIndex].fx;
                }
                return actualLink.source.x;
              })
              .attr('y1', function(d) {
                const actualLink = d.link || d;
                const sourceIndex = nodes.findIndex(n => n.id === actualLink.source.id);
                if (sourceIndex >= 0 && nodes[sourceIndex].fy !== null) {
                  return nodes[sourceIndex].fy;
                }
                return actualLink.source.y;
              })
              .attr('x2', function(d) {
                const actualLink = d.link || d;
                const targetIndex = nodes.findIndex(n => n.id === actualLink.target.id);
                if (targetIndex >= 0 && nodes[targetIndex].fx !== null) {
                  return nodes[targetIndex].fx;
                }
                return actualLink.target.x;
              })
              .attr('y2', function(d) {
                const actualLink = d.link || d;
                const targetIndex = nodes.findIndex(n => n.id === actualLink.target.id);
                if (targetIndex >= 0 && nodes[targetIndex].fy !== null) {
                  return nodes[targetIndex].fy;
                }
                return actualLink.target.y;
              });
          };
        };
      };
    };
  };
}
