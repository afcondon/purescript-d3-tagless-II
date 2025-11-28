// Pure Force Engine FFI
//
// This module extracts ONLY the force calculation functions from D3.
// No simulation wrapper, no timers, no lifecycle management.
//
// Each force is just: initialize(nodes) -> force(alpha) -> mutates vx/vy

import * as d3 from 'd3';

// =============================================================================
// Force Creation (returns D3 force objects)
// =============================================================================

// Create a many-body (charge) force
export function createManyBody_(config) {
  const force = d3.forceManyBody()
    .strength(config.strength)
    .theta(config.theta)
    .distanceMin(config.distanceMin)
    .distanceMax(config.distanceMax);
  return force;
}

// Create a collision force
// Note: radius and strength can be numbers or functions
export function createCollide_(config) {
  const force = d3.forceCollide()
    .radius(config.radius)
    .strength(config.strength)
    .iterations(config.iterations);
  return force;
}

// Create a collision force with dynamic radius function
export function createCollideWithRadius_(radiusFn) {
  return function(strength) {
    return function(iterations) {
      const force = d3.forceCollide()
        .radius(function(d, i) { return radiusFn(d)(i)(); })
        .strength(strength)
        .iterations(iterations);
      return force;
    };
  };
}

// Create a link force
export function createLink_(config) {
  const force = d3.forceLink()
    .distance(config.distance)
    .strength(config.strength)
    .iterations(config.iterations);
  return force;
}

// Create a center force
export function createCenter_(config) {
  const force = d3.forceCenter(config.x, config.y)
    .strength(config.strength);
  return force;
}

// Create an X positioning force
export function createForceX_(config) {
  const force = d3.forceX(config.x)
    .strength(config.strength);
  return force;
}

// Create a Y positioning force
export function createForceY_(config) {
  const force = d3.forceY(config.y)
    .strength(config.strength);
  return force;
}

// Create a radial force
export function createRadial_(config) {
  const force = d3.forceRadial(config.radius, config.x, config.y)
    .strength(config.strength);
  return force;
}

// =============================================================================
// Force Initialization
// =============================================================================

// Initialize a force with nodes
// This must be called before applying the force
// D3 forces expect (nodes, random) - random is used for jiggle
export function initializeForce_(force) {
  return function(nodes) {
    return function() {
      if (force.initialize) {
        force.initialize(nodes, Math.random);
      }
      return force;
    };
  };
}

// Initialize a link force with links
// Link forces need both nodes (via initialize) and links (via links())
// Note: We COPY links before passing to D3 because forceLink.links() MUTATES
// the link objects, replacing source/target integers with node references.
export function initializeLinkForce_(force) {
  return function(nodes) {
    return function(links) {
      return function() {
        if (force.initialize) {
          force.initialize(nodes, Math.random);
        }
        // Copy links to preserve originals for DOM operations
        const linksCopy = links.map(l => ({...l}));
        force.links(linksCopy);
        return force;
      };
    };
  };
}

// =============================================================================
// Force Application (the actual calculation)
// =============================================================================

// Apply a force to nodes
// This is the core calculation - it mutates vx/vy on nodes
export function applyForce_(force) {
  return function(alpha) {
    return function() {
      // D3 forces are callable - force(alpha) applies the force
      force(alpha);
    };
  };
}

// =============================================================================
// Velocity & Position Update
// =============================================================================

// Update velocities with decay (friction)
// This should be called once per tick, after all forces
export function applyVelocityDecay_(nodes) {
  return function(velocityDecay) {
    return function() {
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        node.vx *= velocityDecay;
        node.vy *= velocityDecay;
      }
    };
  };
}

// Update positions from velocities
// This should be called once per tick, after velocity decay
export function updatePositions_(nodes) {
  return function() {
    for (let i = 0; i < nodes.length; i++) {
      const node = nodes[i];
      node.x += node.vx;
      node.y += node.vy;
    }
  };
}

// Combined: apply velocity decay and update positions
// Respects fixed nodes (fx/fy) - if set, node is pinned there
export function integratePositions_(nodes) {
  return function(velocityDecay) {
    return function() {
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];

        // Handle X: pinned or free
        if (node.fx != null) {
          node.x = node.fx;
          node.vx = 0;
        } else {
          node.vx *= velocityDecay;
          node.x += node.vx;
        }

        // Handle Y: pinned or free
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

// =============================================================================
// Alpha Management
// =============================================================================

// Calculate new alpha value (the "cooling" step)
export function decayAlpha_(alpha) {
  return function(alphaMin) {
    return function(alphaDecay) {
      return function(alphaTarget) {
        // D3's formula: alpha += (alphaTarget - alpha) * alphaDecay
        const newAlpha = alpha + (alphaTarget - alpha) * alphaDecay;
        // Return 0 if below minimum (simulation should stop)
        return newAlpha < alphaMin ? 0.0 : newAlpha;
      };
    };
  };
}

// =============================================================================
// Node Initialization
// =============================================================================

// Initialize node indices and default velocities
export function initializeNodes_(nodes) {
  return function() {
    for (let i = 0; i < nodes.length; i++) {
      const node = nodes[i];
      node.index = i;
      if (node.vx === undefined) node.vx = 0;
      if (node.vy === undefined) node.vy = 0;
      // If no position, use random (D3's default behavior)
      if (node.x === undefined) node.x = Math.random() * 100 - 50;
      if (node.y === undefined) node.y = Math.random() * 100 - 50;
    }
  };
}

// =============================================================================
// Animation Frame
// =============================================================================

// Request animation frame - returns a cancel function
export function requestAnimationFrame_(callback) {
  return function() {
    const id = requestAnimationFrame(function(timestamp) {
      callback(timestamp)();
    });
    return function() {
      cancelAnimationFrame(id);
    };
  };
}

// =============================================================================
// Force Parameter Updates (for live tuning)
// =============================================================================

export function setManyBodyStrength_(force) {
  return function(value) {
    return function() {
      force.strength(value);
    };
  };
}

export function setManyBodyTheta_(force) {
  return function(value) {
    return function() {
      force.theta(value);
    };
  };
}

export function setManyBodyDistanceMin_(force) {
  return function(value) {
    return function() {
      force.distanceMin(value);
    };
  };
}

export function setManyBodyDistanceMax_(force) {
  return function(value) {
    return function() {
      force.distanceMax(value);
    };
  };
}

export function setCollideRadius_(force) {
  return function(value) {
    return function() {
      force.radius(value);
    };
  };
}

export function setCollideStrength_(force) {
  return function(value) {
    return function() {
      force.strength(value);
    };
  };
}

export function setLinkDistance_(force) {
  return function(value) {
    return function() {
      force.distance(value);
    };
  };
}

export function setLinkStrength_(force) {
  return function(value) {
    return function() {
      force.strength(value);
    };
  };
}

// =============================================================================
// Mutable Cancel Ref (for animation loop)
// =============================================================================

export function newCancelRef() {
  return function() {
    return { cancel: function() {} };
  };
}

export function setCancelRef(ref) {
  return function(cancel) {
    return function() {
      ref.cancel = cancel;
    };
  };
}

export function getCancelRef(ref) {
  return function() {
    return ref.cancel;
  };
}

// =============================================================================
// Drag Behavior
// =============================================================================

// Attach simulation-aware drag to an array of DOM elements
// Takes a PureScript Effect Unit callback for reheating
// Works with ForceEngine's PureScript-based simulation
export function attachDragWithReheat_(elements) {
  return function(reheatCallback) {
    return function() {
      function dragstarted(event) {
        // Reheat the simulation
        reheatCallback();
        // Set fixed position
        event.subject.fx = event.subject.x;
        event.subject.fy = event.subject.y;
      }

      function dragged(event) {
        // Update fixed position to follow mouse
        event.subject.fx = event.x;
        event.subject.fy = event.y;
      }

      function dragended(event) {
        // Release fixed position (unless we want sticky drag)
        event.subject.fx = null;
        event.subject.fy = null;
      }

      const drag = d3.drag()
        .on('start', dragstarted)
        .on('drag', dragged)
        .on('end', dragended);

      // Apply drag to each element
      elements.forEach(function(el) {
        d3.select(el)
          .call(drag)
          .style('cursor', 'grab');
      });
    };
  };
}

// =============================================================================
// Debug Helpers
// =============================================================================

export function logNodes_(label) {
  return function(nodes) {
    return function() {
      console.log(`[ForceEngine] ${label}:`, {
        count: nodes.length,
        sample: nodes.slice(0, 3).map(n => ({
          x: n.x?.toFixed(2),
          y: n.y?.toFixed(2),
          vx: n.vx?.toFixed(4),
          vy: n.vy?.toFixed(4)
        }))
      });
    };
  };
}
