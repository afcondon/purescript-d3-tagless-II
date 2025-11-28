// SimulationManager FFI
import * as d3 from 'd3';

// Store simulation in window for global access
export function setSimulationInWindow_(sim) {
  return function() {
    window._psd3_simulation_v2 = sim;
    console.log('[SimManager FFI] Simulation stored in window');
  };
}

export function getSimulationFromWindow_() {
  return window._psd3_simulation_v2;
}

// Request animation frame
export function requestAnimationFrame_(callback) {
  return function() {
    const id = requestAnimationFrame(function() {
      callback();
    });
    return function() {
      cancelAnimationFrame(id);
    };
  };
}

// Build a swizzled link from source node, target node, and raw link data
export function buildSwizzledLink_(sourceNode) {
  return function(targetNode) {
    return function(rawLink) {
      const result = {
        source: sourceNode,
        target: targetNode
      };
      for (const key in rawLink) {
        if (key !== 'source' && key !== 'target') {
          result[key] = rawLink[key];
        }
      }
      return result;
    };
  };
}

// =============================================================================
// Spago-specific force creation
// These know about the SpagoSimNode structure and create properly configured forces
// =============================================================================

// Create collision force with dynamic radius based on node.r
export function createSpagoCollision_(padding) {
  return function(strength) {
    return function(iterations) {
      return d3.forceCollide()
        .radius(d => d.r + padding)
        .strength(strength)
        .iterations(iterations);
    };
  };
}

// Create many-body force (charge)
export function createSpagoCharge_(strength) {
  return function(theta) {
    return function(distanceMin) {
      return function(distanceMax) {
        return d3.forceManyBody()
          .strength(strength)
          .theta(theta)
          .distanceMin(distanceMin)
          .distanceMax(distanceMax);
      };
    };
  };
}

// Create many-body force with filter (only applies to matching nodes)
// Returns 0 strength for filtered-out nodes
export function createSpagoChargeFiltered_(strength) {
  return function(theta) {
    return function(distanceMin) {
      return function(distanceMax) {
        return function(predicate) {
          return d3.forceManyBody()
            .strength(d => predicate(d) ? strength : 0)
            .theta(theta)
            .distanceMin(distanceMin)
            .distanceMax(distanceMax);
        };
      };
    };
  };
}

// Create center force
export function createSpagoCenter_(x) {
  return function(y) {
    return function(strength) {
      return d3.forceCenter(x, y).strength(strength);
    };
  };
}

// Create link force
export function createSpagoLink_(distance) {
  return function(strength) {
    return function(iterations) {
      return d3.forceLink()
        .distance(distance)
        .strength(strength)
        .iterations(iterations)
        .id(d => d.id);  // Use node.id for link matching
    };
  };
}

// Create X positioning force toward gridXY.x (for module clustering)
export function createSpagoClusterX_(strength) {
  return function(predicate) {
    return d3.forceX()
      .x(d => {
        if (d.gridXY && d.gridXY.x !== undefined) {
          return d.gridXY.x;
        }
        return d.x || 0;
      })
      .strength(d => predicate(d) ? strength : 0);
  };
}

// Create Y positioning force toward gridXY.y (for module clustering)
export function createSpagoClusterY_(strength) {
  return function(predicate) {
    return d3.forceY()
      .y(d => {
        if (d.gridXY && d.gridXY.y !== undefined) {
          return d.gridXY.y;
        }
        return d.y || 0;
      })
      .strength(d => predicate(d) ? strength : 0);
  };
}

// Create radial force (for orbit)
export function createSpagoRadial_(radius) {
  return function(x) {
    return function(y) {
      return function(strength) {
        return function(predicate) {
          return d3.forceRadial(radius, x, y)
            .strength(d => predicate(d) ? strength : 0);
        };
      };
    };
  };
}

// =============================================================================
// Force initialization
// =============================================================================

export function initializeForce_(force) {
  return function(nodes) {
    return function() {
      // Don't reinitialize link forces - they have links set that would be lost
      if (force._hasLinks) {
        console.log(`[SimManager FFI] Skipping reinitialize for link force (has ${force._linkCount} links)`);
        // Just update nodes, keep links
        if (force.initialize) {
          force.initialize(nodes, Math.random);
        }
        return force;
      }
      if (force.initialize) {
        force.initialize(nodes, Math.random);
      }
      return force;
    };
  };
}

export function initializeLinkForce_(force) {
  return function(nodes) {
    return function(links) {
      return function() {
        console.log(`[SimManager FFI] initializeLinkForce: ${nodes.length} nodes, ${links.length} links`);
        // Store the nodes array identity for debugging
        force._initNodes = nodes;
        force._initNodesId = nodes.map(n => n.id).slice(0, 3);
        console.log(`[SimManager FFI] initializeLinkForce nodes sample:`, force._initNodesId);

        if (force.initialize) {
          force.initialize(nodes, Math.random);
        }
        // Set links - D3 will swizzle them (replace IDs with node refs)
        force.links(links);
        // Mark this force as having links so we don't reinitialize and lose them
        force._hasLinks = true;
        force._linkCount = links.length;

        // Debug: check if swizzled correctly
        const swizzled = force.links();
        if (swizzled.length > 0) {
          const sample = swizzled[0];
          const nodeFromInit = nodes.find(n => n.id === sample.source?.id);
          console.log(`[SimManager FFI] After swizzle, link source same as init node: ${nodeFromInit === sample.source}`);
        }

        console.log(`[SimManager FFI] Link force now has ${force.links().length} links`);
        return force;
      };
    };
  };
}

// =============================================================================
// Force application
// =============================================================================

// Store nodes for identity check
let _simNodes = null;
export function setSimNodes_(nodes) {
  return function() {
    _simNodes = nodes;
    console.log(`[SimManager FFI] Stored ${nodes.length} sim nodes for identity check`);
  };
}

let _linkForceLogCount = 0;
export function applyForce_(force) {
  return function(alpha) {
    return function() {
      // Debug: check if this is a link force
      if (force._hasLinks) {
        const links = force.links();

        // Log identity check once
        if (!force._debugLogged) {
          force._debugLogged = true;
          if (links.length > 0) {
            const sample = links[0];
            const simNode = _simNodes?.find(n => n.id === sample.source?.id);
            const sameAsSimNode = simNode === sample.source;
            const initNode = force._initNodes?.find(n => n.id === sample.source?.id);
            const sameAsInitNode = initNode === sample.source;

            console.log(`[SimManager FFI] Applying link force (${links.length} links), sample:`, {
              sourceId: sample.source?.id ?? sample.source,
              targetId: sample.target?.id ?? sample.target,
              sourceType: typeof sample.source,
              sameAsSimNode: sameAsSimNode,
              sameAsInitNode: sameAsInitNode,
              simNodeSameAsInitNode: simNode === initNode,
            });
          }
        }

        // Log velocity changes from link force every 60 frames
        if (_linkForceLogCount++ % 60 === 0 && links.length > 0) {
          // Sample a few links and track velocity changes
          const sampleLinks = links.slice(0, 3);
          const beforeVx = sampleLinks.map(l => l.source.vx);
          const beforeVy = sampleLinks.map(l => l.source.vy);

          force(alpha);

          const afterVx = sampleLinks.map(l => l.source.vx);
          const afterVy = sampleLinks.map(l => l.source.vy);

          const deltaVx = sampleLinks.map((l, i) => (afterVx[i] - beforeVx[i]).toFixed(6));
          const deltaVy = sampleLinks.map((l, i) => (afterVy[i] - beforeVy[i]).toFixed(6));

          const sampleDistances = sampleLinks.map(l => {
            const dx = l.target.x - l.source.x;
            const dy = l.target.y - l.source.y;
            return Math.sqrt(dx*dx + dy*dy).toFixed(1);
          });
          const linkIds = sampleLinks.map(l => `${l.source.id}->${l.target.id}`);
          console.log(`[SimManager FFI] Link force (alpha=${alpha.toFixed(4)}): links=[${linkIds.join(', ')}], dist=[${sampleDistances.join(', ')}], deltaV=[${deltaVx.join(', ')}]`);
          return;
        }
      }
      force(alpha);
    };
  };
}

// =============================================================================
// Position integration
// =============================================================================

let _integrateLogCount = 0;
export function integratePositions_(nodes) {
  return function(velocityDecay) {
    return function() {
      // Debug: log velocity stats every 60 frames
      if (_integrateLogCount++ % 60 === 0) {
        const pinnedCount = nodes.filter(n => n.fx != null || n.fy != null).length;
        const withVelocity = nodes.filter(n => Math.abs(n.vx) > 0.001 || Math.abs(n.vy) > 0.001);
        const maxVx = Math.max(...nodes.map(n => Math.abs(n.vx || 0)));
        const maxVy = Math.max(...nodes.map(n => Math.abs(n.vy || 0)));
        console.log(`[SimManager FFI] Integrate: ${pinnedCount} pinned, ${withVelocity.length} with velocity, maxV=(${maxVx.toFixed(4)}, ${maxVy.toFixed(4)})`);
        if (withVelocity.length > 0 && withVelocity.length < 5) {
          withVelocity.forEach(n => console.log(`  ${n.name}: vx=${n.vx?.toFixed(4)}, vy=${n.vy?.toFixed(4)}`));
        }
      }

      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];

        // Skip fixed nodes
        if (node.fx != null) {
          node.x = node.fx;
          node.vx = 0;
        } else {
          node.vx *= velocityDecay;
          node.x += node.vx;
        }

        if (node.fy != null) {
          node.y = node.fy;
          node.vy = 0;
        } else {
          node.vy *= velocityDecay;
          node.y += node.vy;
        }
      }
    };
  };
}

// Initialize nodes (add index, default vx/vy)
export function initializeNodes_(nodes) {
  return function() {
    for (let i = 0; i < nodes.length; i++) {
      const node = nodes[i];
      node.index = i;
      if (node.vx === undefined) node.vx = 0;
      if (node.vy === undefined) node.vy = 0;
    }
  };
}

// =============================================================================
// Alpha management
// =============================================================================

export function decayAlpha_(alpha) {
  return function(alphaMin) {
    return function(alphaDecay) {
      return function(alphaTarget) {
        const newAlpha = alpha + (alphaTarget - alpha) * alphaDecay;
        // Debug: log first few alpha values
        if (alpha > 0.9 || newAlpha === 0) {
          console.log(`[SimManager FFI] decayAlpha: ${alpha.toFixed(4)} -> ${newAlpha.toFixed(4)} (min=${alphaMin}, decay=${alphaDecay}, target=${alphaTarget})`);
        }
        return newAlpha < alphaMin ? 0 : newAlpha;
      };
    };
  };
}

// =============================================================================
// Debug
// =============================================================================

export function logForceState_(label) {
  return function(nodes) {
    return function(alpha) {
      return function() {
        console.log(`[SimManager] ${label}:`, {
          nodeCount: nodes.length,
          alpha: alpha.toFixed(4),
          sample: nodes.slice(0, 3).map(n => ({
            name: n.name,
            x: n.x?.toFixed(1),
            y: n.y?.toFixed(1),
            vx: n.vx?.toFixed(3),
            vy: n.vy?.toFixed(3),
            fx: n.fx,
            fy: n.fy
          }))
        });
      };
    };
  };
}
