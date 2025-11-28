// OrchestrationV2 FFI
import * as d3 from 'd3';

// =============================================================================
// Orchestra State (module-level)
// =============================================================================

let orchestraState = null;

export function setOrchestraState_(state) {
  return function() {
    orchestraState = state;
    console.log('[OrchV2 FFI] Orchestra state set');
  };
}

export function getOrchestraState_() {
  return orchestraState;
}

// =============================================================================
// SVG Structure
// =============================================================================

let svg = null;
let innerGroup = null;
let nodesGroup = null;
let linksGroup = null;

export function createSvgStructure_(selector) {
  return function(width) {
    return function(height) {
      return function() {
        console.log(`[OrchV2 FFI] Creating SVG structure in ${selector}`);

        // Clear existing
        d3.select(selector).selectAll('*').remove();

        // Create SVG
        svg = d3.select(selector)
          .append('svg')
          .attr('viewBox', `${-width/2} ${-height/2} ${width} ${height}`)
          .attr('width', '100%')
          .attr('height', '100%');

        // Create inner group for zoom/pan
        innerGroup = svg.append('g').attr('class', 'inner');

        // Add zoom behavior
        const zoom = d3.zoom()
          .scaleExtent([0.1, 4])
          .on('zoom', (event) => {
            innerGroup.attr('transform', event.transform);
          });
        svg.call(zoom);

        // Create groups for links and nodes (links below nodes)
        linksGroup = innerGroup.append('g').attr('class', 'links');
        nodesGroup = innerGroup.append('g').attr('class', 'nodes');

        console.log('[OrchV2 FFI] SVG structure created');
      };
    };
  };
}

// =============================================================================
// Node DOM Updates
// =============================================================================

export function joinNodesToDOM_(nodes) {
  return function() {
    if (!nodesGroup) {
      console.error('[OrchV2 FFI] nodesGroup not initialized');
      return;
    }

    console.log(`[OrchV2 FFI] Joining ${nodes.length} nodes to DOM`);

    // Join data to groups
    const nodeGroups = nodesGroup.selectAll('g.node')
      .data(nodes, d => d.id);

    // Enter: create new groups
    const enter = nodeGroups.enter()
      .append('g')
      .attr('class', 'node')
      .attr('transform', d => `translate(${d.x || 0}, ${d.y || 0})`);

    // Add circle to each group
    enter.append('circle')
      .attr('r', d => d.r || 5)
      .attr('fill', d => getNodeColor(d))
      .attr('stroke', '#fff')
      .attr('stroke-width', 1);

    // Add text label
    enter.append('text')
      .attr('dy', '0.35em')
      .attr('text-anchor', 'middle')
      .attr('font-size', d => Math.min(d.r * 0.8, 10))
      .attr('fill', '#fff')
      .text(d => d.name ? d.name.split('.').pop() : '');

    // Add drag behavior
    enter.call(d3.drag()
      .on('start', dragStarted)
      .on('drag', dragged)
      .on('end', dragEnded));

    // Update: update existing groups
    nodeGroups
      .attr('transform', d => `translate(${d.x || 0}, ${d.y || 0})`);

    // Exit: remove old groups
    nodeGroups.exit().remove();

    console.log('[OrchV2 FFI] Nodes joined');
  };
}

export function updateNodeTransforms_(nodes) {
  return function() {
    if (!nodesGroup) return;

    nodesGroup.selectAll('g.node')
      .data(nodes, d => d.id)
      .attr('transform', d => `translate(${d.x || 0}, ${d.y || 0})`);
  };
}

// Node color based on cluster (uses d3.schemeCategory10 like the PureScript version)
function getNodeColor(d) {
  // Use cluster property for consistent coloring across scenes
  const cluster = d.cluster !== undefined ? d.cluster : 0;
  return d3.schemeCategory10[cluster % 10];
}

// =============================================================================
// Link DOM Updates
// =============================================================================

export function joinLinksToDOM_(links) {
  return function() {
    if (!linksGroup) {
      console.error('[OrchV2 FFI] linksGroup not initialized');
      return;
    }

    console.log(`[OrchV2 FFI] Joining ${links.length} links to DOM`);

    // Join data to paths
    const linkPaths = linksGroup.selectAll('path.link')
      .data(links, d => `${d.source}-${d.target}`);

    // Enter: create new paths (initially invisible, will be updated by tick)
    linkPaths.enter()
      .append('path')
      .attr('class', 'link')
      .attr('fill', 'none')
      .attr('stroke', '#666')
      .attr('stroke-width', 1)
      .attr('stroke-opacity', 0.6);

    // Exit: remove old paths
    linkPaths.exit().remove();

    console.log('[OrchV2 FFI] Links joined');
  };
}

export function updateLinkPaths_(swizzledLinks) {
  return function() {
    if (!linksGroup) return;

    linksGroup.selectAll('path.link')
      .data(swizzledLinks, d => `${d.source.id}-${d.target.id}`)
      .attr('d', d => {
        const sx = d.source.x || 0;
        const sy = d.source.y || 0;
        const tx = d.target.x || 0;
        const ty = d.target.y || 0;
        return `M${sx},${sy}L${tx},${ty}`;
      });
  };
}

export function clearLinks_() {
  if (linksGroup) {
    linksGroup.selectAll('*').remove();
    console.log('[OrchV2 FFI] Links cleared');
  }
}

// =============================================================================
// Drag Handlers
// =============================================================================

function dragStarted(event, d) {
  // Reheat simulation
  if (orchestraState && orchestraState.simRef) {
    // We can't call PureScript from here directly, but the simulation
    // should respond to node movement
  }
  d.fx = d.x;
  d.fy = d.y;
}

function dragged(event, d) {
  d.fx = event.x;
  d.fy = event.y;
  d.x = event.x;
  d.y = event.y;
}

function dragEnded(event, d) {
  d.fx = null;
  d.fy = null;
}

// =============================================================================
// Window Size
// =============================================================================

export function getWindowSize_() {
  return {
    width: window.innerWidth || 1200,
    height: window.innerHeight || 800
  };
}

// =============================================================================
// Tree Transition (staged D3 animation)
// =============================================================================

const treeDepthMultiplier = 180;  // Radial spacing per depth level

// Calculate target tree position from treeXY.x (angle) and treeDepth (integer depth)
function getTreePosition(node) {
  if (node.name === 'PSD3.Main') {
    return { x: 0, y: 0 };
  }
  if (node.treeXY) {
    const angle = node.treeXY.x;
    // Use integer treeDepth for radius, NOT treeXY.y (which is D3's raw layout value)
    const depth = node.treeDepth !== undefined ? node.treeDepth : 1;
    const radius = depth * treeDepthMultiplier;
    return {
      x: radius * Math.cos(angle),
      y: radius * Math.sin(angle)
    };
  }
  return { x: node.x || 0, y: node.y || 0 };
}

// Get tree depth from node.treeDepth (0 = root, 1 = first level, etc)
function getTreeDepth(node) {
  if (node.treeDepth !== undefined) return node.treeDepth;
  if (node.name === 'PSD3.Main') return 0;
  return 999;  // Non-tree nodes go last
}

/**
 * Animate nodes to tree positions with staggered timing
 * Phase 1: D3 transition with layer-based stagger + progressive link appearance
 * Phase 2: Pin nodes at final positions
 * Phase 3: After pause, swap to straight links and engage simulation
 *
 * @param nodes - Array of nodes with treeXY data
 * @param links - Tree links to display
 * @param onComplete - Callback when ready for simulation
 */
export function transitionToTreePositions_(nodes) {
  return function(links) {
    return function(onComplete) {
      return function() {
        if (!nodesGroup || !linksGroup) {
          console.error('[OrchV2 FFI] Groups not initialized');
          return;
        }

        console.log('[OrchV2 FFI] Starting tree transition');

        // Clear existing links
        linksGroup.selectAll('*').remove();

        // Build node lookup for links
        const nodeMap = new Map(nodes.map(n => [n.id, n]));

        // Only include nodes that have tree positions (filter out packages, etc.)
        const treeNodes = nodes.filter(n => n.treeXY !== undefined && n.treeXY !== null);
        const nonTreeNodes = nodes.filter(n => n.treeXY === undefined || n.treeXY === null);

        console.log(`[OrchV2 FFI] Tree nodes: ${treeNodes.length}, non-tree nodes: ${nonTreeNodes.length}`);

        // Group tree nodes by depth for staggering
        const nodesByDepth = new Map();
        treeNodes.forEach(node => {
          const depth = getTreeDepth(node);
          if (!nodesByDepth.has(depth)) {
            nodesByDepth.set(depth, []);
          }
          nodesByDepth.get(depth).push(node);
        });

        // Debug: log any non-numeric depths
        const allDepths = Array.from(nodesByDepth.keys());
        const nonNumericDepths = allDepths.filter(d => typeof d !== 'number' || isNaN(d));
        if (nonNumericDepths.length > 0) {
          console.warn(`[OrchV2 FFI] WARNING: Found non-numeric depths:`, nonNumericDepths);
          // Log some example nodes with non-numeric depths
          nonNumericDepths.forEach(badDepth => {
            const examples = nodesByDepth.get(badDepth).slice(0, 3);
            console.warn(`[OrchV2 FFI] Nodes with depth=${badDepth}:`, examples.map(n => ({
              name: n.name,
              treeDepth: n.treeDepth,
              treeXY: n.treeXY,
              nodetype: n.nodetype
            })));
          });
        }

        // Sort depths (filter out any non-numeric)
        const depths = allDepths
          .filter(d => typeof d === 'number' && !isNaN(d))
          .sort((a, b) => a - b);
        console.log(`[OrchV2 FFI] Tree depths: ${depths.join(', ')}`);

        // Calculate timing
        const layerDuration = 400;  // ms per layer
        const withinLayerStagger = 15;  // ms between nodes in same layer
        const totalDuration = depths.length * layerDuration +
          Math.max(...Array.from(nodesByDepth.values()).map(arr => arr.length)) * withinLayerStagger;

        // Create radial link path using cubic bezier (matches PureScript radialLinkPath)
        const makeTreeLinkPath = (source, target) => {
          // Get angles and depths
          const srcAngle = source.treeXY ? source.treeXY.x : 0;
          const tgtAngle = target.treeXY ? target.treeXY.x : 0;
          const srcDepth = source.treeDepth !== undefined ? source.treeDepth : 0;
          const tgtDepth = target.treeDepth !== undefined ? target.treeDepth : 0;
          const srcRadius = srcDepth * treeDepthMultiplier;
          const tgtRadius = tgtDepth * treeDepthMultiplier;

          // Source and target positions
          const sx = srcRadius * Math.cos(srcAngle);
          const sy = srcRadius * Math.sin(srcAngle);
          const tx = tgtRadius * Math.cos(tgtAngle);
          const ty = tgtRadius * Math.sin(tgtAngle);

          // Control points at intermediate radius
          const cr = (srcRadius + tgtRadius) / 2;
          const cx1 = cr * Math.cos(srcAngle);
          const cy1 = cr * Math.sin(srcAngle);
          const cx2 = cr * Math.cos(tgtAngle);
          const cy2 = cr * Math.sin(tgtAngle);

          return `M${sx},${sy}C${cx1},${cy1} ${cx2},${cy2} ${tx},${ty}`;
        };

        // Animate tree nodes
        const nodeSelection = nodesGroup.selectAll('g.node')
          .data(nodes, d => d.id);

        nodeSelection
          .filter(d => d.treeXY)  // Only animate nodes with tree positions
          .transition()
          .duration(layerDuration)
          .delay(d => {
            const depth = getTreeDepth(d);
            const depthIndex = depths.indexOf(depth);
            const nodesAtDepth = nodesByDepth.get(depth) || [];
            const indexInLayer = nodesAtDepth.indexOf(d);
            return depthIndex * layerDuration + indexInLayer * withinLayerStagger;
          })
          .ease(d3.easeCubicOut)
          .attr('transform', d => {
            const pos = getTreePosition(d);
            return `translate(${pos.x}, ${pos.y})`;
          })
          .on('start', function(event, d) {
            // D3 v6+: callback is (event, d) - but for selection.transition().on(),
            // the signature is actually (event, d) where d is the datum
            // However, `this` is the DOM element, so we can get data via d3.select(this).datum()
            const datum = d3.select(this).datum();
            if (!datum) return;

            // When a node starts transitioning, draw links TO it (from parent)
            const incomingLinks = links.filter(l => l.target === datum.id);
            incomingLinks.forEach(link => {
              const source = nodeMap.get(link.source);
              const target = nodeMap.get(link.target);
              if (source && target && source.treeXY && target.treeXY) {
                // Add the link with a fade-in using our custom path generator
                const path = linksGroup.append('path')
                  .attr('class', 'link link--tree')
                  .attr('data-source', link.source)
                  .attr('data-target', link.target)
                  .attr('fill', 'none')
                  .attr('stroke', '#888')
                  .attr('stroke-width', 1.5)
                  .attr('stroke-opacity', 0)
                  .attr('d', makeTreeLinkPath(source, target));

                path.transition()
                  .duration(300)
                  .attr('stroke-opacity', 0.6);
              }
            });
          })
          .on('end', function(event, d) {
            // Update node data with final position
            const datum = d3.select(this).datum();
            if (!datum) return;
            const pos = getTreePosition(datum);
            datum.x = pos.x;
            datum.y = pos.y;
          });

        // After all transitions complete
        setTimeout(() => {
          console.log('[OrchV2 FFI] Tree transition complete, pinning nodes');

          // Phase 2: Pin all nodes at their current/tree positions
          nodes.forEach(node => {
            if (node.treeXY) {
              const pos = getTreePosition(node);
              node.x = pos.x;
              node.y = pos.y;
              node.fx = pos.x;
              node.fy = pos.y;
            }
            node.vx = 0;
            node.vy = 0;
          });

          // Phase 3: After a pause, swap to straight links and engage simulation
          setTimeout(() => {
            console.log('[OrchV2 FFI] Swapping to straight links, engaging simulation');
            console.log(`[OrchV2 FFI] Links to add: ${links.length}`);
            console.log(`[OrchV2 FFI] Tree links in DOM before swap: ${linksGroup.selectAll('.link--tree').size()}`);

            // Fade out tree links, then add straight links
            linksGroup.selectAll('.link--tree')
              .transition()
              .duration(300)
              .attr('stroke-opacity', 0)
              .remove();

            // After fade out, add straight links (pass nodes for initial positions)
            setTimeout(() => {
              joinStraightLinksToDOM(links, nodes);
              console.log(`[OrchV2 FFI] Force links in DOM after add: ${linksGroup.selectAll('.link--force').size()}`);

              // Call completion callback (will unpin and start simulation)
              onComplete();
            }, 350);

          }, 1000);

        }, totalDuration + 500);  // Wait for all transitions plus buffer
      };
    };
  };
}

// Join bezier tree links (curved, hierarchical look)
function joinTreeLinksToDOM(nodes, links) {
  if (!linksGroup) return;

  // Build node lookup
  const nodeMap = new Map(nodes.map(n => [n.id, n]));

  // Create link generator for radial tree
  // Use treeDepth (integer) for radius, treeXY.x for angle
  const linkGen = d3.linkRadial()
    .angle(d => d.treeXY ? d.treeXY.x : 0)
    .radius(d => (d.treeDepth !== undefined ? d.treeDepth : 0) * treeDepthMultiplier);

  const linkPaths = linksGroup.selectAll('path.tree-link')
    .data(links, d => `${d.source}-${d.target}`);

  linkPaths.enter()
    .append('path')
    .attr('class', 'tree-link')
    .attr('fill', 'none')
    .attr('stroke', '#666')
    .attr('stroke-width', 1)
    .attr('stroke-opacity', 0.6)
    .attr('d', d => {
      const source = nodeMap.get(d.source);
      const target = nodeMap.get(d.target);
      if (!source || !target) return '';
      return linkGen({ source, target });
    });

  console.log(`[OrchV2 FFI] Added ${links.length} tree links`);
}

// Join straight simulation links (uses raw links with source/target as IDs)
// Also draws initial positions based on current node positions
function joinStraightLinksToDOM(links, nodes) {
  if (!linksGroup) return;

  console.log(`[OrchV2 FFI] joinStraightLinksToDOM called with ${links.length} links`);

  // Build node map for initial positions
  const nodeMap = nodes ? new Map(nodes.map(n => [n.id, n])) : null;

  // Use source-target IDs as key - use link--force class
  const linkPaths = linksGroup.selectAll('path.link--force')
    .data(links, d => `${d.source}-${d.target}`);

  const entered = linkPaths.enter()
    .append('path')
    .attr('class', 'link link--force')
    .attr('data-source', d => d.source)
    .attr('data-target', d => d.target)
    .attr('fill', 'none')
    .attr('stroke', '#4a9')
    .attr('stroke-width', 1)
    .attr('stroke-opacity', 0.6)
    .attr('d', d => {
      // Draw initial position if we have nodes
      if (!nodeMap) return '';
      const source = nodeMap.get(d.source);
      const target = nodeMap.get(d.target);
      if (!source || !target) return '';
      return `M${source.x || 0},${source.y || 0}L${target.x || 0},${target.y || 0}`;
    });

  linkPaths.exit().remove();

  console.log(`[OrchV2 FFI] Added ${entered.size()} straight links to DOM (link--force class)`);
}

// Update link paths for force simulation links (link--force class)
export function updateLinkPathsFromRaw_(nodes) {
  return function(rawLinks) {
    return function() {
      if (!linksGroup) return;

      // Build node map for position lookup
      const nodeMap = new Map(nodes.map(n => [n.id, n]));

      // Update all link paths using data-source/data-target attributes
      linksGroup.selectAll('path.link--force').each(function() {
        const el = d3.select(this);
        const sourceId = +el.attr('data-source');
        const targetId = +el.attr('data-target');
        const source = nodeMap.get(sourceId);
        const target = nodeMap.get(targetId);
        if (source && target) {
          el.attr('d', `M${source.x},${source.y}L${target.x},${target.y}`);
        }
      });
    };
  };
}

// =============================================================================
// Node Pinning
// =============================================================================

export function unpinNodes_(nodes) {
  return function() {
    console.log(`[OrchV2 FFI] Unpinning ${nodes.length} nodes`);
    nodes.forEach(node => {
      node.fx = null;
      node.fy = null;
    });
  };
}
