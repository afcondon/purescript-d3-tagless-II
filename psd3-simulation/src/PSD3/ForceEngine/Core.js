// Pure Force Engine FFI
//
// This module extracts ONLY the force calculation functions from D3.
// No simulation wrapper, no timers, no lifecycle management.
//
// Each force is just: initialize(nodes) -> force(alpha) -> mutates vx/vy
//
// D3 dependencies: d3-force, d3-selection, d3-drag
import {
  forceManyBody, forceCollide, forceLink, forceCenter,
  forceX, forceY, forceRadial
} from "d3-force";
import { select, pointer } from "d3-selection";
import { drag } from "d3-drag";

// =============================================================================
// Force Creation (returns D3 force objects)
// =============================================================================

// Create a many-body (charge) force
export function createManyBody_(config) {
  const force = forceManyBody()
    .strength(config.strength)
    .theta(config.theta)
    .distanceMin(config.distanceMin)
    .distanceMax(config.distanceMax);
  return force;
}

// Create a collision force
// Note: radius and strength can be numbers or functions
export function createCollide_(config) {
  const force = forceCollide()
    .radius(config.radius)
    .strength(config.strength)
    .iterations(config.iterations);
  return force;
}

// Create a collision force with dynamic radius function
export function createCollideWithRadius_(radiusFn) {
  return function(strength) {
    return function(iterations) {
      const force = forceCollide()
        .radius(function(d, i) { return radiusFn(d)(i)(); })
        .strength(strength)
        .iterations(iterations);
      return force;
    };
  };
}

// Create a link force
// IMPORTANT: Uses .id(d => d.id) so that link source/target values are matched
// against node.id, not array index. This is essential for GUP (General Update Pattern)
// where nodes are filtered and array indices change.
export function createLink_(config) {
  const force = forceLink()
    .id(d => d.id)  // Use node.id for lookup, not array index
    .distance(config.distance)
    .strength(config.strength)
    .iterations(config.iterations);
  return force;
}

// Create a link force with dynamic strength per-link
// strengthAccessor is called for each link to get strength (typically from link.weight)
// Uses .id(d => d.id) for same reason as createLink_
export function createLinkDynamic_(config) {
  const force = forceLink()
    .id(d => d.id)  // Use node.id for lookup, not array index
    .distance(config.distance)
    .strength(function(link) {
      return config.strengthAccessor(link);
    })
    .iterations(config.iterations);
  return force;
}

// Create a center force
export function createCenter_(config) {
  const force = forceCenter(config.x, config.y)
    .strength(config.strength);
  return force;
}

// Create an X positioning force
export function createForceX_(config) {
  const force = forceX(config.x)
    .strength(config.strength);
  return force;
}

// Create a Y positioning force
export function createForceY_(config) {
  const force = forceY(config.y)
    .strength(config.strength);
  return force;
}

// Create a radial force
export function createRadial_(config) {
  const force = forceRadial(config.radius, config.x, config.y)
    .strength(config.strength);
  return force;
}

// =============================================================================
// Filtered/Dynamic Force Creation
// =============================================================================

// Create a many-body force with filter predicate
// Only applies charge to nodes where filter(node) returns true
export function createManyBodyFiltered_(config) {
  const force = forceManyBody()
    .strength(function(d) {
      return config.filter(d) ? config.strength : 0;
    })
    .theta(config.theta)
    .distanceMin(config.distanceMin)
    .distanceMax(config.distanceMax);
  return force;
}

// Create a radial force with filter predicate
export function createRadialFiltered_(config) {
  const force = forceRadial(config.radius, config.x, config.y)
    .strength(function(d) {
      return config.filter(d) ? config.strength : 0;
    });
  return force;
}

// Create a collision force with dynamic radius accessor
// radiusAccessor is called per-node to get collision radius
export function createCollideDynamic_(config) {
  const force = forceCollide()
    .radius(function(d) {
      return config.radiusAccessor(d);
    })
    .strength(config.strength)
    .iterations(config.iterations);
  return force;
}

// Create an X positioning force with dynamic target accessor
// xAccessor is called per-node to get target X position
let forceXDebugCount = 0;
export function createForceXDynamic_(config) {
  const force = forceX()
    .x(function(d) {
      const target = config.xAccessor(d);
      // Only log modules (id >= 93), not packages
      if (forceXDebugCount < 5 && d.id >= 93) {
        const dist = Math.abs(d.x - target);
        console.log(`[ForceXDynamic] module ${d.id} (${d.name?.substring(0,20)}) -> gridX=${target?.toFixed(0)}, x=${d.x?.toFixed(0)}, dist=${dist.toFixed(0)}`);
        forceXDebugCount++;
      }
      return target;
    })
    .strength(config.strength);
  return force;
}

// Create a Y positioning force with dynamic target accessor
export function createForceYDynamic_(config) {
  const force = forceY()
    .y(function(d) {
      return config.yAccessor(d);
    })
    .strength(config.strength);
  return force;
}

// =============================================================================
// Optimized Grid Forces (no FFI callbacks - reads node.gridX/gridY directly)
// =============================================================================

// Create ForceX that reads node.gridX directly (no PureScript callback)
export function createForceXGrid_(strength) {
  return forceX()
    .x(d => d.gridX)
    .strength(strength);
}

// Create ForceY that reads node.gridY directly (no PureScript callback)
export function createForceYGrid_(strength) {
  return forceY()
    .y(d => d.gridY)
    .strength(strength);
}

// Create collision force that reads node.r directly (no PureScript callback)
// Adds padding to node radius
export function createCollideGrid_(padding) {
  return function(strength) {
    return function(iterations) {
      return forceCollide()
        .radius(d => d.r + padding)
        .strength(strength)
        .iterations(iterations);
    };
  };
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
      const start = performance.now();
      if (force.initialize) {
        force.initialize(nodes, Math.random);
      }
      const elapsed = performance.now() - start;
      if (elapsed > 1) {
        console.log(`[InitForce] Took ${elapsed.toFixed(1)}ms for ${nodes.length} nodes`);
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
      const start = performance.now();
      force(alpha);
      const elapsed = performance.now() - start;
      // if (elapsed > 5) {
      //   console.log(`[Force] ${force.name || 'unknown'} took ${elapsed.toFixed(1)}ms`);
      // }
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
let integrateDebugCount = 0;
export function integratePositions_(nodes) {
  return function(velocityDecay) {
    return function() {
      const start = performance.now();

      // Debug: log first FREE node (fx == null)
      if (integrateDebugCount % 100 === 0) {
        const freeNode = nodes.find(n => n.fx == null);
        if (freeNode) {
          console.log(`[IntegrateDebug tick=${integrateDebugCount}] free node ${freeNode.id}: x=${freeNode.x?.toFixed(1)}, y=${freeNode.y?.toFixed(1)}, vx=${freeNode.vx?.toFixed(3)}, vy=${freeNode.vy?.toFixed(3)}`);
        } else {
          console.log(`[IntegrateDebug tick=${integrateDebugCount}] NO FREE NODES! All ${nodes.length} nodes have fx set`);
        }
      }
      integrateDebugCount++;

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
      const elapsed = performance.now() - start;
      if (elapsed > 2) {
        console.log(`[Integrate] Took ${elapsed.toFixed(1)}ms for ${nodes.length} nodes`);
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

      // DEBUG: Mark node 93 (first module) with a unique marker
      if (node.id === 93) {
        node.__simMarker__ = true;
        console.log(`[InitNodes] Marked node 93 with __simMarker__, x=${node.x?.toFixed(1)}`);
      }
    }
  };
}

// =============================================================================
// Animation Frame
// =============================================================================

// Tick timing stats
let tickStats = { count: 0, totalTime: 0, maxTime: 0, lastReport: Date.now() };
let lastTickEnd = performance.now();
let gapStats = { total: 0, max: 0, count: 0 };

// Use setTimeout instead of rAF to avoid browser throttling
// Set to true to use setTimeout instead of requestAnimationFrame
const USE_SETTIMEOUT = true;
const FRAME_TIME = 8; // Target ~60fps (accounting for ~8ms work time)

// Request animation frame - returns a cancel function
export function requestAnimationFrame_(callback) {
  return function() {
    const scheduleTime = performance.now();
    const gapSinceLastTick = scheduleTime - lastTickEnd;

    let id;
    const handler = function(timestamp) {
      const fireTime = performance.now();
      const waitTime = fireTime - scheduleTime;

      const start = performance.now();
      callback(timestamp || fireTime)();
      const elapsed = performance.now() - start;
      lastTickEnd = performance.now();

      tickStats.count++;
      tickStats.totalTime += elapsed;
      tickStats.maxTime = Math.max(tickStats.maxTime, elapsed);
      gapStats.total += waitTime;
      gapStats.max = Math.max(gapStats.max, waitTime);
      gapStats.count++;

      const now = Date.now();
      if (now - tickStats.lastReport > 2000) {
        const avg = tickStats.totalTime / tickStats.count;
        const avgGap = gapStats.total / gapStats.count;
        console.log(`[AnimLoop] ${tickStats.count} ticks, work=${avg.toFixed(1)}ms, wait=${avgGap.toFixed(0)}ms (max=${gapStats.max.toFixed(0)}ms) [${USE_SETTIMEOUT ? 'setTimeout' : 'rAF'}]`);
        tickStats = { count: 0, totalTime: 0, maxTime: 0, lastReport: now };
        gapStats = { total: 0, max: 0, count: 0 };
      }
    };

    if (USE_SETTIMEOUT) {
      id = setTimeout(handler, FRAME_TIME);
      return function() { clearTimeout(id); };
    } else {
      id = requestAnimationFrame(handler);
      return function() { cancelAnimationFrame(id); };
    }
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

      const dragBehavior = drag()
        .on('start', dragstarted)
        .on('drag', dragged)
        .on('end', dragended);

      // Apply drag to each element
      elements.forEach(function(el) {
        select(el)
          .call(dragBehavior)
          .style('cursor', 'grab');
      });
    };
  };
}

// Attach drag to transformed group elements (like bubble packs)
// Uses a container selector to get pointer coordinates in the right space
// containerSelector: CSS selector for the coordinate reference container (e.g., "#zoom-group" or "svg")
export function attachGroupDragWithReheat_(elements) {
  return function(containerSelector) {
    return function(reheatCallback) {
      return function() {
        const container = select(containerSelector).node();
        if (!container) {
          console.warn('[attachGroupDragWithReheat] Container not found:', containerSelector);
          return;
        }

        const dragBehavior = drag()
          // Custom subject: walk up DOM from click target to find .module-pack group's data
          .subject(function(event) {
            let el = event.sourceEvent.target;
            while (el && !el.classList?.contains('module-pack')) {
              el = el.parentElement;
            }
            return el?.__data__;
          })
          .on('start', function(event) {
            reheatCallback();
            event.subject.fx = event.subject.x;
            event.subject.fy = event.subject.y;
          })
          .on('drag', function(event) {
            const [px, py] = pointer(event, container);
            event.subject.fx = px;
            event.subject.fy = py;
          })
          .on('end', function(event) {
            event.subject.fx = null;
            event.subject.fy = null;
          });

        // Apply drag to each element
        elements.forEach(function(el) {
          select(el)
            .call(dragBehavior)
            .style('cursor', 'grab');
        });
      };
    };
  };
}

// Attach drag with toggle-pinning behavior
// First drag pins the node where you drop it, second drag unpins it
// The node stays pinned between drags (fx/fy remain set)
export function attachPinningDrag_(elements) {
  return function(reheatCallback) {
    return function() {
      function dragstarted(event) {
        // Reheat the simulation
        reheatCallback();
        // Set fixed position to current location
        event.subject.fx = event.subject.x;
        event.subject.fy = event.subject.y;
      }

      function dragged(event) {
        // Update fixed position to follow mouse
        event.subject.fx = event.x;
        event.subject.fy = event.y;
      }

      function dragended(event) {
        // Check if we moved significantly - if not, treat as "unpin" click
        const dx = event.subject.fx - event.subject.x;
        const dy = event.subject.fy - event.subject.y;
        const distMoved = Math.sqrt(dx * dx + dy * dy);

        // If node was already pinned and barely moved, unpin it
        // We detect "was pinned" by checking if x/y matched fx/fy at drag start
        // For simplicity: if moved less than 3px total, toggle pin state
        if (distMoved < 3) {
          // Toggle: if was at exact pinned position, unpin
          if (event.subject._wasPinned) {
            event.subject.fx = null;
            event.subject.fy = null;
            delete event.subject._wasPinned;
          }
        }
        // Otherwise keep it pinned where we dropped it
        // Mark as pinned for next drag detection
        if (event.subject.fx != null) {
          event.subject._wasPinned = true;
        }
      }

      const dragBehavior = drag()
        .on('start', dragstarted)
        .on('drag', dragged)
        .on('end', dragended);

      // Apply drag to each element
      elements.forEach(function(el) {
        select(el)
          .call(dragBehavior)
          .style('cursor', 'grab');
      });
    };
  };
}

// =============================================================================
// DOM Utilities
// =============================================================================

// Query elements by CSS selector
// Returns an array of Element (Web.DOM.Element compatible)
export function querySelectorElements_(selector) {
  return function() {
    const nodeList = document.querySelectorAll(selector);
    return Array.from(nodeList);
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
