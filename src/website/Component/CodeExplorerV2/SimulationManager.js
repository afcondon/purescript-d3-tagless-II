// SimulationManager FFI
import * as d3 from 'd3';

// =============================================================================
// Window Storage
// =============================================================================

export function setSimulationInWindow_(sim) {
  return function() {
    window._psd3_simulation_v2 = sim;
    // Also initialize forces object for control panel access
    window._psd3_forces_v2 = window._psd3_forces_v2 || {};
  };
}

export function getSimulationFromWindow_() {
  return window._psd3_simulation_v2;
}

// =============================================================================
// Force Registry (for control panel access)
// =============================================================================

// Register a force by name (called from PureScript addForce)
export function registerForce_(name) {
  return function(force) {
    return function() {
      window._psd3_forces_v2 = window._psd3_forces_v2 || {};
      window._psd3_forces_v2[name] = force;
    };
  };
}

// Unregister a force
export function unregisterForce_(name) {
  return function() {
    if (window._psd3_forces_v2) {
      delete window._psd3_forces_v2[name];
    }
  };
}

// Clear all forces from registry
export function clearForceRegistry_() {
  window._psd3_forces_v2 = {};
}

// Store restart callback (called from PureScript)
export function setRestartCallback_(callback) {
  return function() {
    window._psd3_restart_v2 = callback;
  };
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

// Apply all forces from the window registry (source of truth for active forces)
export function applyAllRegisteredForces_(alpha) {
  return function() {
    const forces = window._psd3_forces_v2 || {};
    for (const name in forces) {
      const force = forces[name];
      if (typeof force === 'function') {
        force(alpha);
      }
    }
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

// =============================================================================
// Control Panel Helpers (plain JS functions, not curried)
// =============================================================================

// Get all registered force names
export function getForceNamesV2() {
  const forces = window._psd3_forces_v2 || {};
  return Object.keys(forces);
}

// Get a force by name
export function getForceV2(name) {
  const forces = window._psd3_forces_v2 || {};
  return forces[name] || null;
}

// Update a force parameter and reheat
export function updateForceParamV2(forceName, paramName, value) {
  const force = getForceV2(forceName);
  if (!force) {
    console.warn(`Force not found: ${forceName}`);
    return;
  }

  if (typeof force[paramName] === 'function') {
    force[paramName](value);
    console.log(`Set ${forceName}.${paramName} = ${value}`);
    // Bump alpha and restart if needed
    const ref = window._psd3_simulation_v2;
    if (ref && ref.value) {
      ref.value.alpha = Math.max(ref.value.alpha, 0.3);
    }
    if (window._psd3_restart_v2) {
      window._psd3_restart_v2();
    }
  } else {
    console.warn(`Parameter not found: ${forceName}.${paramName}`);
  }
}

// Start simulation (set alpha and call restart callback)
export function startSimulationV2() {
  const ref = window._psd3_simulation_v2;
  if (!ref) return;

  const state = ref.value;
  if (state) {
    state.alpha = 0.3;
    console.log('[ControlPanel] Set alpha=0.3');
    // Call the restart callback to start the loop
    if (window._psd3_restart_v2) {
      window._psd3_restart_v2();
    }
  }
}

// Stop simulation
export function stopSimulationV2() {
  const ref = window._psd3_simulation_v2;
  if (!ref) return;

  const state = ref.value;
  if (state) {
    state.running = false;
    console.log('[ControlPanel] Set running=false');
  }
}

// Reheat simulation (set alpha to 1.0 and restart)
export function reheatSimulationV2() {
  const ref = window._psd3_simulation_v2;
  if (!ref) return;

  const state = ref.value;
  if (state) {
    state.alpha = 1.0;
    console.log('[ControlPanel] Set alpha=1.0');
    // Call the restart callback to ensure loop is running
    if (window._psd3_restart_v2) {
      window._psd3_restart_v2();
    }
  }
}

// Toggle a force on/off (removes/re-adds from registry)
export function toggleForceV2(forceName, enabled) {
  window._psd3_disabled_forces_v2 = window._psd3_disabled_forces_v2 || {};
  const forces = window._psd3_forces_v2 || {};

  if (enabled) {
    // Re-enable: restore from disabled storage
    if (window._psd3_disabled_forces_v2[forceName]) {
      forces[forceName] = window._psd3_disabled_forces_v2[forceName];
      delete window._psd3_disabled_forces_v2[forceName];
      console.log(`Enabled force: ${forceName}`);
    }
  } else {
    // Disable: move to disabled storage
    if (forces[forceName]) {
      window._psd3_disabled_forces_v2[forceName] = forces[forceName];
      delete forces[forceName];
      console.log(`Disabled force: ${forceName}`);
    }
  }
  // Bump alpha and restart
  const ref = window._psd3_simulation_v2;
  if (ref && ref.value) {
    ref.value.alpha = 0.5;
  }
  if (window._psd3_restart_v2) {
    window._psd3_restart_v2();
  }
}

// Get force parameter value
export function getForceParamV2(forceName, paramName) {
  const force = getForceV2(forceName);
  if (!force) return null;

  if (typeof force[paramName] === 'function') {
    const val = force[paramName]();
    // Handle functions that return functions (like radius)
    if (typeof val === 'function') {
      return null;
    }
    return val;
  }
  return null;
}

// Get all force settings as string
export function getForceSettingsV2() {
  const forces = window._psd3_forces_v2 || {};
  const forceNames = Object.keys(forces);

  if (forceNames.length === 0) {
    return "No forces registered";
  }

  const paramsByType = {
    charge: ['strength', 'theta', 'distanceMin', 'distanceMax'],
    collide: ['strength', 'iterations'],
    center: ['strength', 'x', 'y'],
    link: ['distance', 'strength', 'iterations'],
    radial: ['strength', 'radius'],
    position: ['strength']
  };

  const getForceType = (name) => {
    if (name.includes('charge') || name.includes('Charge')) return 'charge';
    if (name.includes('collid') || name.includes('Collid') || name.includes('collision')) return 'collide';
    if (name.includes('center')) return 'center';
    if (name.includes('link')) return 'link';
    if (name.includes('Orbit') || name.includes('radial')) return 'radial';
    if (name.includes('cluster') || name.includes('Cluster')) return 'position';
    return 'unknown';
  };

  let output = '-- Force Settings (V2) --\n';

  for (const name of forceNames) {
    const force = forces[name];
    output += `\n${name}:\n`;
    const forceType = getForceType(name);
    const params = paramsByType[forceType] || [];

    for (const param of params) {
      if (typeof force[param] === 'function') {
        const val = force[param]();
        if (typeof val === 'number') {
          output += `  ${param}: ${val}\n`;
        } else if (typeof val === 'function') {
          output += `  ${param}: <function>\n`;
        }
      }
    }
  }

  return output;
}
