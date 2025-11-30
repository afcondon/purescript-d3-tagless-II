// OrchestrationV2 FFI
// D3 dependencies: via psd3-selection library (path relative to output/ after PureScript compile)
import { select, zoom, drag, easeCubicOut, schemeCategory10 } from "../PSD3.Internal.FFI/foreign.js";

// =============================================================================
// Orchestra State (module-level)
// =============================================================================

let orchestraState = null;

export function setOrchestraState_(state) {
  return function() {
    orchestraState = state;
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
        // Clear existing
        select(selector).selectAll('*').remove();

        // Create SVG
        svg = select(selector)
          .append('svg')
          .attr('viewBox', `${-width/2} ${-height/2} ${width} ${height}`)
          .attr('width', '100%')
          .attr('height', '100%');

        // Create inner group for zoom/pan
        innerGroup = svg.append('g').attr('class', 'inner');

        // Add zoom behavior
        const zoomBehavior = zoom()
          .scaleExtent([0.1, 4])
          .on('zoom', (event) => {
            innerGroup.attr('transform', event.transform);
          });
        svg.call(zoomBehavior);

        // Create groups for links and nodes (links below nodes)
        linksGroup = innerGroup.append('g').attr('class', 'links');
        nodesGroup = innerGroup.append('g').attr('class', 'nodes');
      };
    };
  };
}

// =============================================================================
// Node DOM Updates
// =============================================================================

export function joinNodesToDOM_(nodes) {
  return function() {
    if (!nodesGroup) return;

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
    enter.call(drag()
      .on('start', dragStarted)
      .on('drag', dragged)
      .on('end', dragEnded));

    // Update: update existing groups
    nodeGroups
      .attr('transform', d => `translate(${d.x || 0}, ${d.y || 0})`);

    // Exit: remove old groups
    nodeGroups.exit().remove();
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

// Node color based on cluster (uses schemeCategory10)
function getNodeColor(d) {
  const cluster = d.cluster !== undefined ? d.cluster : 0;
  return schemeCategory10[cluster % 10];
}

// =============================================================================
// Link DOM Updates
// =============================================================================

// Join raw links (with integer source/target) to DOM
// IMPORTANT: This must be called BEFORE initializing the D3 link force,
// because D3's forceLink mutates links in place (swizzles IDs to node refs)
export function joinLinksToDOM_(links) {
  return function() {
    if (!linksGroup) return;

    // Join data to paths - use link--force class so tick callback updates them
    const linkPaths = linksGroup.selectAll('path.link--force')
      .data(links, d => `${d.source}-${d.target}`);

    // Enter: create new paths with data attributes for tick updates
    linkPaths.enter()
      .append('path')
      .attr('class', 'link link--force')
      .attr('data-source', d => d.source)
      .attr('data-target', d => d.target)
      .attr('fill', 'none')
      .attr('stroke', '#4a9')
      .attr('stroke-width', 1)
      .attr('stroke-opacity', 0.6);

    // Exit: remove old paths
    linkPaths.exit().remove();
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
  }
}

// =============================================================================
// Drag Handlers
// =============================================================================

function dragStarted(event, d) {
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
//
// PURE FUNCTION - This could be moved to PureScript, but is kept here because:
// 1. It's called from within D3 transition callbacks (would need FFI round-trip)
// 2. The math is simple and self-contained
// 3. Performance: avoiding FFI overhead in animation code
//
// Equivalent PureScript:
//   getTreePosition :: SpagoSimNode -> { x :: Number, y :: Number }
//   getTreePosition node = case node.treeXY of
//     Just { x: angle } ->
//       let depth = fromMaybe 1 node.treeDepth
//           radius = toNumber depth * treeDepthMultiplier
//       in { x: radius * cos angle, y: radius * sin angle }
//     Nothing -> { x: fromMaybe 0.0 node.x, y: fromMaybe 0.0 node.y }
function getTreePosition(node) {
  if (node.name === 'PSD3.Main') {
    return { x: 0, y: 0 };
  }
  if (node.treeXY) {
    const angle = node.treeXY.x;
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
// PURE FUNCTION - kept in JS for same reasons as getTreePosition (callback context)
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
 */
export function transitionToTreePositions_(nodes) {
  return function(links) {
    return function(onComplete) {
      return function() {
        if (!nodesGroup || !linksGroup) return;

        // Clear existing links
        linksGroup.selectAll('*').remove();

        // Build node lookup for links
        const nodeMap = new Map(nodes.map(n => [n.id, n]));

        // Only include nodes that have tree positions (filter out packages, etc.)
        const treeNodes = nodes.filter(n => n.treeXY !== undefined && n.treeXY !== null);

        // Group tree nodes by depth for staggering
        const nodesByDepth = new Map();
        treeNodes.forEach(node => {
          const depth = getTreeDepth(node);
          if (!nodesByDepth.has(depth)) {
            nodesByDepth.set(depth, []);
          }
          nodesByDepth.get(depth).push(node);
        });

        // Sort depths (filter out any non-numeric)
        const depths = Array.from(nodesByDepth.keys())
          .filter(d => typeof d === 'number' && !isNaN(d))
          .sort((a, b) => a - b);

        // Calculate timing
        const layerDuration = 400;  // ms per layer
        const withinLayerStagger = 15;  // ms between nodes in same layer
        const totalDuration = depths.length * layerDuration +
          Math.max(...Array.from(nodesByDepth.values()).map(arr => arr.length)) * withinLayerStagger;

        // Create radial link path using cubic bezier
        const makeTreeLinkPath = (source, target) => {
          const srcAngle = source.treeXY ? source.treeXY.x : 0;
          const tgtAngle = target.treeXY ? target.treeXY.x : 0;
          const srcDepth = source.treeDepth !== undefined ? source.treeDepth : 0;
          const tgtDepth = target.treeDepth !== undefined ? target.treeDepth : 0;
          const srcRadius = srcDepth * treeDepthMultiplier;
          const tgtRadius = tgtDepth * treeDepthMultiplier;

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
          .filter(d => d.treeXY)
          .transition()
          .duration(layerDuration)
          .delay(d => {
            const depth = getTreeDepth(d);
            const depthIndex = depths.indexOf(depth);
            const nodesAtDepth = nodesByDepth.get(depth) || [];
            const indexInLayer = nodesAtDepth.indexOf(d);
            return depthIndex * layerDuration + indexInLayer * withinLayerStagger;
          })
          .ease(easeCubicOut)
          .attr('transform', d => {
            const pos = getTreePosition(d);
            return `translate(${pos.x}, ${pos.y})`;
          })
          .on('start', function() {
            const datum = select(this).datum();
            if (!datum) return;

            // When a node starts transitioning, draw links TO it (from parent)
            const incomingLinks = links.filter(l => l.target === datum.id);
            incomingLinks.forEach(link => {
              const source = nodeMap.get(link.source);
              const target = nodeMap.get(link.target);
              if (source && target && source.treeXY && target.treeXY) {
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
          .on('end', function() {
            const datum = select(this).datum();
            if (!datum) return;
            const pos = getTreePosition(datum);
            datum.x = pos.x;
            datum.y = pos.y;
          });

        // After all transitions complete
        setTimeout(() => {
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
            // Fade out tree links, then add straight links
            linksGroup.selectAll('.link--tree')
              .transition()
              .duration(300)
              .attr('stroke-opacity', 0)
              .remove();

            // After fade out, add straight links
            setTimeout(() => {
              joinStraightLinksToDOM(links, nodes);

              // Fade out non-tree nodes (packages and modules without treeXY)
              nodesGroup.selectAll('g.node')
                .filter(d => !d.treeXY)
                .transition()
                .duration(500)
                .style('opacity', 0)
                .on('end', function() {
                  select(this).classed('hidden-node', true);
                });

              onComplete();
            }, 350);

          }, 1000);

        }, totalDuration + 500);
      };
    };
  };
}

// Join straight simulation links (uses raw links with source/target as IDs)
function joinStraightLinksToDOM(links, nodes) {
  if (!linksGroup) return;

  // Build node map for initial positions
  const nodeMap = nodes ? new Map(nodes.map(n => [n.id, n])) : null;

  // Use source-target IDs as key - use link--force class
  const linkPaths = linksGroup.selectAll('path.link--force')
    .data(links, d => `${d.source}-${d.target}`);

  linkPaths.enter()
    .append('path')
    .attr('class', 'link link--force')
    .attr('data-source', d => d.source)
    .attr('data-target', d => d.target)
    .attr('fill', 'none')
    .attr('stroke', '#4a9')
    .attr('stroke-width', 1)
    .attr('stroke-opacity', 0.6)
    .attr('d', d => {
      if (!nodeMap) return '';
      const source = nodeMap.get(d.source);
      const target = nodeMap.get(d.target);
      if (!source || !target) return '';
      return `M${source.x || 0},${source.y || 0}L${target.x || 0},${target.y || 0}`;
    });

  linkPaths.exit().remove();
}

// Update link paths for force simulation (uses data-source/data-target attributes)
export function updateLinkPathsFromRaw_(nodes) {
  return function(_rawLinks) {
    return function() {
      if (!linksGroup) return;

      const nodeMap = new Map(nodes.map(n => [n.id, n]));
      const linkElements = linksGroup.selectAll('path.link--force');

      linkElements.each(function() {
        const el = select(this);
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
    nodes.forEach(node => {
      node.fx = null;
      node.fy = null;
    });
  };
}

// Unpin only tree nodes, keep non-tree nodes pinned (so they don't move)
export function unpinTreeNodesOnly_(nodes) {
  return function() {
    nodes.forEach(node => {
      if (node.treeXY) {
        // Tree node - unpin so it can move
        node.fx = null;
        node.fy = null;
      } else {
        // Non-tree node - pin at current position (or offscreen)
        node.fx = node.x || -9999;
        node.fy = node.y || -9999;
      }
    });
  };
}
