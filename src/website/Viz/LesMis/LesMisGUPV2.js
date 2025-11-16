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
// Cache current simulation positions in sx/sy for smooth transition back
export function setNodesGridPositions_(nodes) {
  return function(gridSpacing) {
    return function() {
      const nodeCount = nodes.length;
      const columns = Math.floor(Math.ceil(Math.sqrt(nodeCount)));
      const offset = -(columns * gridSpacing) / 2.0;

      nodes.forEach((node, i) => {
        // Cache current simulation position before pinning
        node.sx = node.x;
        node.sy = node.y;

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
// Cache current simulation positions in sx/sy for smooth transition back
export function setNodesPhyllotaxisPositions_(nodes) {
  return function() {
    const initialRadius = 10.0;
    const initialAngle = Math.PI * (3.0 - Math.sqrt(5.0));  // Golden angle

    nodes.forEach((node, index) => {
      // Cache current simulation position before pinning
      node.sx = node.x;
      node.sy = node.y;

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

// Get JavaScript typeof
export function typeOf_(value) {
  return typeof value;
}

// Transition nodes back to cached simulation positions (sx, sy)
// Then clear fx/fy to unpinned them, allowing simulation to resume naturally
export function transitionToCachedPositions_(svgSelector) {
  return function(nodeSelector) {
    return function(linkSelector) {
      return function(nodes) {
        return function(links) {
          return function(onComplete) {
            return function() {
              // Wait for next animation frame to ensure DOM is ready
              requestAnimationFrame(() => {
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

              // Transition node circles to cached sx/sy positions
              nodeElements
                .transition(t)
                .attrTween('cx', function(d, i) {
                  const startX = parseFloat(d3.select(this).attr('cx'));
                  const node = nodes[i];
                  // Use cached sx if available, otherwise current x
                  const endX = (node && node.sx !== null && node.sx !== undefined)
                    ? node.sx
                    : (node && node.x !== undefined ? node.x : startX);
                  return d3.interpolate(startX, endX);
                })
                .attrTween('cy', function(d, i) {
                  const startY = parseFloat(d3.select(this).attr('cy'));
                  const node = nodes[i];
                  // Use cached sy if available, otherwise current y
                  const endY = (node && node.sy !== null && node.sy !== undefined)
                    ? node.sy
                    : (node && node.y !== undefined ? node.y : startY);
                  return d3.interpolate(startY, endY);
                })
                .on('end', function(d, i) {
                  // When last node finishes, clear fx/fy and call completion
                  if (i === nodes.length - 1) {
                    // Clear fx/fy to unpin nodes
                    nodes.forEach(node => {
                      node.fx = null;
                      node.fy = null;
                      // Also clear the cache
                      node.sx = null;
                      node.sy = null;
                    });
                    onComplete();
                  }
                });

              // Transition links to follow nodes to cached positions
              linkElements
                .transition(t)
                .attrTween('x1', function(d, i) {
                  const startX = parseFloat(d3.select(this).attr('x1'));
                  if (i >= links.length) {
                    console.warn('Link index out of bounds:', i, 'of', links.length);
                    return () => startX;
                  }
                  const link = links[i];
                  const sourceNode = nodes.find(n => n.id === link.source.id);
                  const endX = (sourceNode && sourceNode.sx !== null && sourceNode.sx !== undefined)
                    ? sourceNode.sx
                    : (sourceNode ? sourceNode.x : startX);
                  return d3.interpolate(startX, endX);
                })
                .attrTween('y1', function(d, i) {
                  const startY = parseFloat(d3.select(this).attr('y1'));
                  if (i >= links.length) return () => startY;
                  const link = links[i];
                  const sourceNode = nodes.find(n => n.id === link.source.id);
                  const endY = (sourceNode && sourceNode.sy !== null && sourceNode.sy !== undefined)
                    ? sourceNode.sy
                    : (sourceNode ? sourceNode.y : startY);
                  return d3.interpolate(startY, endY);
                })
                .attrTween('x2', function(d, i) {
                  const startX = parseFloat(d3.select(this).attr('x2'));
                  if (i >= links.length) return () => startX;
                  const link = links[i];
                  const targetNode = nodes.find(n => n.id === link.target.id);
                  const endX = (targetNode && targetNode.sx !== null && targetNode.sx !== undefined)
                    ? targetNode.sx
                    : (targetNode ? targetNode.x : startX);
                  return d3.interpolate(startX, endX);
                })
                .attrTween('y2', function(d, i) {
                  const startY = parseFloat(d3.select(this).attr('y2'));
                  if (i >= links.length) return () => startY;
                  const link = links[i];
                  const targetNode = nodes.find(n => n.id === link.target.id);
                  const endY = (targetNode && targetNode.sy !== null && targetNode.sy !== undefined)
                    ? targetNode.sy
                    : (targetNode ? targetNode.y : startY);
                  return d3.interpolate(startY, endY);
                });
              });  // Close requestAnimationFrame callback
            };
          };
        };
      };
    };
  };
}

// Transition nodes and links to their fx/fy positions using D3 transitions
// This creates a smooth animation to the pinned positions
export function transitionToFxFyPositions_(svgSelector) {
  return function(nodeSelector) {
    return function(linkSelector) {
      return function(nodes) {
        return function(links) {
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
                .attrTween('cx', function(d, i) {
                  const startX = parseFloat(d3.select(this).attr('cx'));
                  const node = nodes[i];
                  if (node && node.fx !== null && node.fx !== undefined) {
                    return d3.interpolate(startX, node.fx);
                  }
                  // If no fx, use current x position or stay at current cx
                  const endX = (node && node.x !== undefined) ? node.x : startX;
                  return d3.interpolate(startX, endX);
                })
                .attrTween('cy', function(d, i) {
                  const startY = parseFloat(d3.select(this).attr('cy'));
                  const node = nodes[i];
                  if (node && node.fy !== null && node.fy !== undefined) {
                    return d3.interpolate(startY, node.fy);
                  }
                  // If no fy, use current y position or stay at current cy
                  const endY = (node && node.y !== undefined) ? node.y : startY;
                  return d3.interpolate(startY, endY);
                })
                .on('end', function(d, i) {
                  // When last node finishes, call completion callback
                  if (i === nodes.length - 1) {
                    onComplete();
                  }
                });

              // Transition links to follow nodes to fx/fy positions
              // We get link data from the PureScript links array (not from bound data)
              // because CSS selectors create NEW selections without data bindings
              linkElements
                .transition(t)
                .attrTween('x1', function(d, i) {
                  const startX = parseFloat(d3.select(this).attr('x1'));
                  // Get link from array by index
                  if (i >= links.length) {
                    console.warn('Link index out of bounds:', i, 'of', links.length);
                    return () => startX;
                  }
                  const link = links[i];
                  const sourceNode = nodes.find(n => n.id === link.source.id);
                  const endX = (sourceNode && sourceNode.fx !== null && sourceNode.fx !== undefined)
                    ? sourceNode.fx
                    : (sourceNode ? sourceNode.x : startX);
                  return d3.interpolate(startX, endX);
                })
                .attrTween('y1', function(d, i) {
                  const startY = parseFloat(d3.select(this).attr('y1'));
                  if (i >= links.length) return () => startY;
                  const link = links[i];
                  const sourceNode = nodes.find(n => n.id === link.source.id);
                  const endY = (sourceNode && sourceNode.fy !== null && sourceNode.fy !== undefined)
                    ? sourceNode.fy
                    : (sourceNode ? sourceNode.y : startY);
                  return d3.interpolate(startY, endY);
                })
                .attrTween('x2', function(d, i) {
                  const startX = parseFloat(d3.select(this).attr('x2'));
                  if (i >= links.length) return () => startX;
                  const link = links[i];
                  const targetNode = nodes.find(n => n.id === link.target.id);
                  const endX = (targetNode && targetNode.fx !== null && targetNode.fx !== undefined)
                    ? targetNode.fx
                    : (targetNode ? targetNode.x : startX);
                  return d3.interpolate(startX, endX);
                })
                .attrTween('y2', function(d, i) {
                  const startY = parseFloat(d3.select(this).attr('y2'));
                  if (i >= links.length) return () => startY;
                  const link = links[i];
                  const targetNode = nodes.find(n => n.id === link.target.id);
                  const endY = (targetNode && targetNode.fy !== null && targetNode.fy !== undefined)
                    ? targetNode.fy
                    : (targetNode ? targetNode.y : startY);
                  return d3.interpolate(startY, endY);
                });
            };
          };
        };
      };
    };
  };
}
