// SimulationManager FFI
import * as d3 from 'd3';

// =============================================================================
// Window Storage
// =============================================================================

export function setSimulationInWindow_(sim) {
  return function() {
    window._psd3_simulation_v2 = sim;
  };
}

export function getSimulationFromWindow_() {
  return window._psd3_simulation_v2;
}

// =============================================================================
// Animation Frame
// =============================================================================

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

// =============================================================================
// Swizzling Support
// =============================================================================

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
// Spago-specific Force Creation
// =============================================================================
// These are convenience functions that encode knowledge about SpagoSimNode structure.
// They hardcode accessors like `d.r`, `d.gridXY.x`, etc.
//
// For generic force creation, see the library module PSD3.ForceEngine.Core which
// provides functions that accept accessor functions or config objects.
//
// Example: Library's createCollide_ accepts {radius, strength, iterations} where
// radius can be a number or function. These Spago wrappers just set up the
// appropriate accessor for the Spago model.

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
        .id(d => d.id);
    };
  };
}

// Create X positioning force toward gridXY.x (for module clustering)
export function createSpagoClusterX_(strength) {
  return function(predicate) {
    return d3.forceX()
      .x(d => (d.gridXY && d.gridXY.x !== undefined) ? d.gridXY.x : (d.x || 0))
      .strength(d => predicate(d) ? strength : 0);
  };
}

// Create Y positioning force toward gridXY.y (for module clustering)
export function createSpagoClusterY_(strength) {
  return function(predicate) {
    return d3.forceY()
      .y(d => (d.gridXY && d.gridXY.y !== undefined) ? d.gridXY.y : (d.y || 0))
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
// Force Initialization
// =============================================================================

export function initializeForce_(force) {
  return function(nodes) {
    return function() {
      // Skip reinitializing link forces - they have links that would be lost
      if (force._isLinkForce && force.links().length > 0) {
        // Just update the nodes reference, keep existing links
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
        // Mark as link force so initializeForce_ knows to preserve links
        force._isLinkForce = true;

        if (force.initialize) {
          force.initialize(nodes, Math.random);
        }

        // IMPORTANT: Copy links before passing to D3!
        // D3's forceLink.links() MUTATES the link objects, replacing
        // source/target integers with node references (swizzling).
        // We copy to preserve the original links for DOM operations.
        const linksCopy = links.map(l => ({...l}));
        force.links(linksCopy);

        return force;
      };
    };
  };
}

// =============================================================================
// Force Application
// =============================================================================

export function applyForce_(force) {
  return function(alpha) {
    return function() {
      force(alpha);
    };
  };
}

// =============================================================================
// Position Integration
// =============================================================================

export function integratePositions_(nodes) {
  return function(velocityDecay) {
    return function() {
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];

        // Handle fixed nodes
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
// Alpha Management
// =============================================================================

// PURE FUNCTION - D3's alpha decay formula: alpha += (alphaTarget - alpha) * alphaDecay
// Returns 0 if below alphaMin (simulation should stop).
// Kept in JS for performance (called every frame). Equivalent PureScript:
//   decayAlpha :: Number -> Number -> Number -> Number -> Number
//   decayAlpha alpha alphaMin alphaDecay alphaTarget =
//     let newAlpha = alpha + (alphaTarget - alpha) * alphaDecay
//     in if newAlpha < alphaMin then 0.0 else newAlpha
export function decayAlpha_(alpha) {
  return function(alphaMin) {
    return function(alphaDecay) {
      return function(alphaTarget) {
        const newAlpha = alpha + (alphaTarget - alpha) * alphaDecay;
        return newAlpha < alphaMin ? 0 : newAlpha;
      };
    };
  };
}
