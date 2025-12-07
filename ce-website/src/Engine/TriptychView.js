// FFI for TriptychView - renders all three neighborhood views in a triangular layout
// Layout: Equilateral triangle with Bubbles at top, Chord bottom-left, Matrix bottom-right
// All rendering goes into explorer-zoom-group for pan/zoom support

import { select } from "d3-selection";
import { pack, hierarchy } from "d3-hierarchy";
import { scaleOrdinal } from "d3-scale";
import { schemeCategory10 } from "d3-scale-chromatic";
import { chord as d3Chord, ribbon } from "d3-chord";
import { arc } from "d3-shape";

// Panel configuration
// Each panel should be ~40% of viewport height (viewBox is 1600 tall, so ~640 units)
const PANEL_SIZE = 640;  // Size of each panel's bounding box (40% of viewport height)
const PANEL_RADIUS = PANEL_SIZE / 2;  // Radius for circular background (320)

// Triangle layout - spread panels out more to avoid overlap
const TRIANGLE_RADIUS = 500;  // Distance from center to vertices
const TOP_VERTEX = { x: 0, y: -TRIANGLE_RADIUS * 0.75 };  // Bubbles (top)
const BOTTOM_LEFT = { x: -TRIANGLE_RADIUS * 1.0, y: TRIANGLE_RADIUS * 0.6 };  // Chord (bottom-left)
const BOTTOM_RIGHT = { x: TRIANGLE_RADIUS * 1.0, y: TRIANGLE_RADIUS * 0.6 };  // Matrix (bottom-right)

// Uniform scale for all panels - scale content to fit within circular background
const PANEL_SCALE = 0.85;

// Background styling
const PANEL_BG_COLOR = "#0f172a";  // Dark slate background
const PANEL_BG_OPACITY = 0.95;
const PANEL_BORDER_COLOR = "#334155";
const PANEL_BORDER_WIDTH = 3;

// Clear all triptych content from the zoom group
export const clearTriptych_ = () => {
  select("#explorer-zoom-group").selectAll(".triptych-panel").remove();
  select("#explorer-zoom-group").selectAll(".triptych-label").remove();
  select("#explorer-zoom-group").selectAll(".triptych-connector").remove();
  select("#explorer-zoom-group").selectAll(".triptych-bg").remove();
  // Reset transform on explorer-nodes when leaving triptych
  select("#explorer-nodes").attr("transform", null);
  select("#explorer-links").attr("transform", null);
};

// Helper to create a panel background circle that blocks treemap interactions
const createPanelBackground = (container, pos, label) => {
  const bgGroup = container.append("g")
    .attr("class", "triptych-bg")
    .attr("transform", `translate(${pos.x},${pos.y})`);

  // Circular background that blocks mouse events
  bgGroup.append("circle")
    .attr("cx", 0)
    .attr("cy", 0)
    .attr("r", PANEL_RADIUS)
    .attr("fill", PANEL_BG_COLOR)
    .attr("fill-opacity", PANEL_BG_OPACITY)
    .attr("stroke", PANEL_BORDER_COLOR)
    .attr("stroke-width", PANEL_BORDER_WIDTH)
    .style("pointer-events", "all");  // Explicitly capture all mouse events

  // Panel label inside the background
  bgGroup.append("text")
    .attr("x", 0)
    .attr("y", -PANEL_RADIUS + 25)
    .attr("text-anchor", "middle")
    .attr("font-size", "20px")
    .attr("font-weight", "600")
    .attr("fill", "#94a3b8")
    .text(label);

  return bgGroup;
};

// Render panel backgrounds and labels
export const renderTriptychLabels_ = (panelWidth) => (viewBoxHeight) => () => {
  const container = select("#explorer-zoom-group");
  if (container.empty()) return;

  // Get references to explorer-links and explorer-nodes
  const explorerLinks = select("#explorer-links").node();
  const explorerNodes = select("#explorer-nodes").node();

  // Create backgrounds for each panel
  const topBg = createPanelBackground(container, TOP_VERTEX, "Force Graph");
  createPanelBackground(container, BOTTOM_LEFT, "Chord Diagram");
  createPanelBackground(container, BOTTOM_RIGHT, "Adjacency Matrix");

  // Move Force Graph background to be before explorer-links (so links and nodes appear on top)
  // SVG z-order: backgrounds first, then links, then nodes
  const insertBefore = explorerLinks || explorerNodes;
  if (insertBefore && topBg.node()) {
    container.node().insertBefore(topBg.node(), insertBefore);
  }
};

// Wrap explorer-nodes and explorer-links with a transform to position at top vertex
// This is called AFTER bubble packs are already rendered
export const wrapExplorerNodesForTriptych_ = (nodes) => () => {
  const container = select("#explorer-zoom-group");
  if (container.empty()) return;

  const existingNodes = select("#explorer-nodes");
  const existingLinks = select("#explorer-links");

  if (existingNodes.empty()) {
    console.log("[TriptychView] No explorer-nodes found to wrap");
    return;
  }

  // Calculate bounding box of all nodes to determine centering
  let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
  nodes.forEach(n => {
    const r = n.r || 50;
    if (n.x - r < minX) minX = n.x - r;
    if (n.x + r > maxX) maxX = n.x + r;
    if (n.y - r < minY) minY = n.y - r;
    if (n.y + r > maxY) maxY = n.y + r;
  });

  const rangeX = maxX - minX || 1;
  const rangeY = maxY - minY || 1;
  const centerX = (minX + maxX) / 2;
  const centerY = (minY + maxY) / 2;

  // Scale to fit within panel circle (use ~90% of panel diameter for content)
  const targetSize = PANEL_SIZE * 0.85;
  const fitScale = Math.min(targetSize / rangeX, targetSize / rangeY, 0.8);

  // Apply transform to the existing nodes and links containers
  // Position at top vertex with scaling
  const transform = `translate(${TOP_VERTEX.x},${TOP_VERTEX.y}) scale(${fitScale}) translate(${-centerX},${-centerY})`;

  existingNodes.attr("transform", transform);
  if (!existingLinks.empty()) {
    existingLinks.attr("transform", transform);
  }

  // Add synchronized hover handlers to bubble pack module groups
  // The module-pack groups have SimNode data bound to them
  existingNodes.selectAll("g.module-pack")
    .style("cursor", "pointer")
    .on("mouseenter.triptych", function() {
      const data = select(this).datum();
      if (data && data.name) {
        highlightModuleAcrossPanels(data.name);
      }
    })
    .on("mouseleave.triptych", clearHighlightsAcrossPanels);

  console.log(`[TriptychView] Wrapped bubble packs at top vertex, scale=${fitScale.toFixed(2)}`);
};

// For backwards compatibility - simplified bubbles rendering (not used in new flow)
export const renderBubblesIntoPanel_ = (nodes) => () => {
  // This is a no-op now - bubbles are rendered via renderBubblePackView and wrapped
  console.log("[TriptychView] renderBubblesIntoPanel_ called (legacy)");
};

// Build chord diagram data from nodes
const buildChordData = (centralName, nodes) => {
  const allNames = [centralName, ...nodes.filter(n => n.name !== centralName).map(n => n.name)];
  const nameToIdx = new Map(allNames.map((n, i) => [n, i]));
  const idToName = new Map(nodes.map(n => [n.id, n.name]));

  const n = allNames.length;
  const matrix = Array(n).fill(null).map(() => Array(n).fill(0));

  // Track imports and dependents for color coding
  // imports = modules that central imports (targets of central)
  // dependents = modules that import central (sources of central)
  const importSet = new Set();
  const dependentSet = new Set();

  // Find the central node to get its relationships
  const centralNode = nodes.find(n => n.name === centralName);
  if (centralNode) {
    // Central's targets are modules it imports
    (centralNode.targets || []).forEach(targetId => {
      const name = idToName.get(targetId);
      if (name) importSet.add(name);
    });
    // Central's sources are modules that depend on it
    (centralNode.sources || []).forEach(sourceId => {
      const name = idToName.get(sourceId);
      if (name) dependentSet.add(name);
    });
  }

  nodes.forEach(node => {
    const nodeIdx = nameToIdx.get(node.name);
    if (nodeIdx === undefined) return;

    (node.targets || []).forEach(targetId => {
      const targetName = idToName.get(targetId);
      if (targetName) {
        const targetIdx = nameToIdx.get(targetName);
        if (targetIdx !== undefined) {
          matrix[nodeIdx][targetIdx] = 1;
        }
      }
    });

    (node.sources || []).forEach(sourceId => {
      const sourceName = idToName.get(sourceId);
      if (sourceName) {
        const sourceIdx = nameToIdx.get(sourceName);
        if (sourceIdx !== undefined) {
          matrix[sourceIdx][nodeIdx] = 1;
        }
      }
    });
  });

  return { matrix, names: allNames, importSet, dependentSet };
};

// Render chord diagram at bottom-left vertex
export const renderChordWithOffset_ = (centralName) => (nodes) => (offsetX) => (offsetY) => (scale) => (viewBoxWidth) => (viewBoxHeight) => () => {
  const container = select("#explorer-zoom-group");
  if (container.empty()) return;

  const { matrix, names, importSet, dependentSet } = buildChordData(centralName, nodes);
  if (names.length < 2) return;

  // Use BOTTOM_LEFT position
  const panelX = BOTTOM_LEFT.x;
  const panelY = BOTTOM_LEFT.y;

  const panelGroup = container.append("g")
    .attr("class", "triptych-panel triptych-chord-panel")
    .attr("transform", `translate(${panelX},${panelY}) scale(${PANEL_SCALE})`);

  const chordLayout = d3Chord()
    .padAngle(0.04)
    .sortSubgroups((a, b) => b - a);

  const chordData = chordLayout(matrix);

  // Size chord to fit within circular panel (PANEL_RADIUS * PANEL_SCALE * 0.8 for labels)
  const outerR = PANEL_RADIUS * 0.75;  // ~240 with current settings
  const innerR = outerR - 10;

  const arcGen = arc()
    .innerRadius(innerR)
    .outerRadius(outerR);

  const ribbonGen = ribbon()
    .radius(innerR);

  // Colors matching ChordDiagram.purs
  const centralColor = "#4299e1";       // Blue for central module
  const importColor = "#48bb78";        // Green for modules we import
  const dependentColor = "#ed8936";     // Orange for modules that depend on us

  const getColor = (idx) => {
    const name = names[idx];
    if (name === centralName) return centralColor;
    if (importSet.has(name)) return importColor;
    if (dependentSet.has(name)) return dependentColor;
    return importColor;  // Default to green
  };

  // Ribbons with data attributes for hover synchronization
  panelGroup.selectAll("path.triptych-ribbon")
    .data(chordData)
    .enter()
    .append("path")
    .attr("class", "triptych-ribbon")
    .attr("d", ribbonGen)
    .attr("fill", d => getColor(d.source.index))
    .attr("fill-opacity", 0.65)
    .attr("stroke", "#000")
    .attr("stroke-width", 0.5)
    .attr("stroke-opacity", 0.2)
    .attr("data-source", d => names[d.source.index])
    .attr("data-target", d => names[d.target.index])
    .style("cursor", "pointer")
    .on("mouseenter", function(event, d) {
      highlightModuleAcrossPanels(names[d.source.index]);
    })
    .on("mouseleave", clearHighlightsAcrossPanels);

  // Arcs with hover handlers
  panelGroup.selectAll("path.triptych-arc")
    .data(chordData.groups)
    .enter()
    .append("path")
    .attr("class", "triptych-arc")
    .attr("d", arcGen)
    .attr("fill", d => getColor(d.index))
    .attr("stroke", "#fff")
    .attr("stroke-width", 2)
    .attr("data-module", d => names[d.index])
    .style("cursor", "pointer")
    .on("mouseenter", function(event, d) {
      highlightModuleAcrossPanels(names[d.index]);
    })
    .on("mouseleave", clearHighlightsAcrossPanels);

  // Labels (only show first 15 to avoid clutter)
  const labelR = outerR + 15;
  const visibleGroups = chordData.groups.slice(0, 15);

  panelGroup.selectAll("text.triptych-chord-label")
    .data(visibleGroups)
    .enter()
    .append("text")
    .attr("class", "triptych-chord-label")
    .attr("data-module", d => names[d.index])
    .each(function(d) {
      const angle = (d.startAngle + d.endAngle) / 2;
      const x = labelR * Math.cos(angle - Math.PI / 2);
      const y = labelR * Math.sin(angle - Math.PI / 2);
      select(this)
        .attr("x", x)
        .attr("y", y)
        .attr("text-anchor", angle > Math.PI ? "end" : "start")
        .attr("dominant-baseline", "middle")
        .attr("font-size", "10px")
        .attr("fill", "#e2e8f0")
        .attr("transform", () => {
          const rotation = (angle * 180 / Math.PI) - 90;
          if (rotation > 90 || rotation < -90) {
            return `rotate(${rotation + 180},${x},${y})`;
          }
          return `rotate(${rotation},${x},${y})`;
        })
        .text(shortenName(names[d.index]));
    })
    .style("cursor", "pointer")
    .on("mouseenter", function(event, d) {
      highlightModuleAcrossPanels(names[d.index]);
    })
    .on("mouseleave", clearHighlightsAcrossPanels);
};

// Build matrix data from nodes
const buildMatrixData = (centralName, nodes) => {
  const allNames = [centralName, ...nodes.filter(n => n.name !== centralName).map(n => n.name)];
  const nameToIdx = new Map(allNames.map((n, i) => [n, i]));
  const idToName = new Map(nodes.map(n => [n.id, n.name]));

  const n = allNames.length;
  const outbound = Array(n).fill(null).map(() => Array(n).fill(0));
  const inbound = Array(n).fill(null).map(() => Array(n).fill(0));

  nodes.forEach(node => {
    const nodeIdx = nameToIdx.get(node.name);
    if (nodeIdx === undefined) return;

    (node.targets || []).forEach(targetId => {
      const targetName = idToName.get(targetId);
      if (targetName) {
        const targetIdx = nameToIdx.get(targetName);
        if (targetIdx !== undefined) {
          outbound[nodeIdx][targetIdx] = 1;
        }
      }
    });

    (node.sources || []).forEach(sourceId => {
      const sourceName = idToName.get(sourceId);
      if (sourceName) {
        const sourceIdx = nameToIdx.get(sourceName);
        if (sourceIdx !== undefined) {
          inbound[nodeIdx][sourceIdx] = 1;
        }
      }
    });
  });

  return { outbound, inbound, names: allNames };
};

// Render matrix at bottom-right vertex
export const renderMatrixWithOffset_ = (centralName) => (nodes) => (offsetX) => (offsetY) => (scale) => (viewBoxWidth) => (viewBoxHeight) => () => {
  const container = select("#explorer-zoom-group");
  if (container.empty()) return;

  const { outbound, inbound, names } = buildMatrixData(centralName, nodes);
  if (names.length < 2) return;

  // Use BOTTOM_RIGHT position
  const panelX = BOTTOM_RIGHT.x;
  const panelY = BOTTOM_RIGHT.y;

  const n = names.length;
  const maxCells = 25;  // Limit for readability
  const displayN = Math.min(n, maxCells);
  // Size matrix to fit within circular panel (use ~70% of panel radius for grid)
  const maxGridSize = PANEL_RADIUS * 1.3;  // ~416 with current settings
  const cellSize = Math.min(18, maxGridSize / displayN);
  const gridSize = displayN * cellSize;

  const panelGroup = container.append("g")
    .attr("class", "triptych-panel triptych-matrix-panel")
    .attr("transform", `translate(${panelX},${panelY}) scale(${PANEL_SCALE})`);

  const gridOffsetX = -gridSize / 2;
  const gridOffsetY = -gridSize / 2;

  // Draw cells (only up to displayN)
  const cells = [];
  for (let i = 0; i < displayN; i++) {
    for (let j = 0; j < displayN; j++) {
      cells.push({
        row: i,
        col: j,
        outbound: outbound[i][j],
        inbound: inbound[i][j],
        x: gridOffsetX + j * cellSize,
        y: gridOffsetY + i * cellSize
      });
    }
  }

  // Background cells with data attributes for hover synchronization
  panelGroup.selectAll("rect.triptych-cell")
    .data(cells)
    .enter()
    .append("rect")
    .attr("class", "triptych-cell")
    .attr("x", d => d.x)
    .attr("y", d => d.y)
    .attr("width", cellSize)
    .attr("height", cellSize)
    .attr("fill", "#1a202c")
    .attr("stroke", "#2d3748")
    .attr("stroke-width", 0.5)
    .attr("data-row", d => names[d.row])
    .attr("data-col", d => names[d.col])
    .style("cursor", d => (d.outbound > 0 || d.inbound > 0) ? "pointer" : "default")
    .on("mouseenter", function(event, d) {
      // Highlight both row and column modules
      highlightModuleAcrossPanels(names[d.row]);
    })
    .on("mouseleave", clearHighlightsAcrossPanels);

  // Upper-left triangles (outbound)
  panelGroup.selectAll("path.triptych-outbound")
    .data(cells.filter(d => d.outbound > 0))
    .enter()
    .append("path")
    .attr("class", "triptych-outbound")
    .attr("d", d => `M${d.x},${d.y} L${d.x + cellSize},${d.y} L${d.x},${d.y + cellSize} Z`)
    .attr("fill", "#48bb78")
    .attr("fill-opacity", 0.8);

  // Lower-right triangles (inbound)
  panelGroup.selectAll("path.triptych-inbound")
    .data(cells.filter(d => d.inbound > 0))
    .enter()
    .append("path")
    .attr("class", "triptych-inbound")
    .attr("d", d => `M${d.x + cellSize},${d.y} L${d.x + cellSize},${d.y + cellSize} L${d.x},${d.y + cellSize} Z`)
    .attr("fill", "#ed8936")
    .attr("fill-opacity", 0.8);

  // Row labels (left) - show every Nth for large matrices
  const labelStep = displayN > 20 ? 3 : (displayN > 10 ? 2 : 1);
  const rowLabelData = names.slice(0, displayN).filter((_, i) => i % labelStep === 0);

  panelGroup.selectAll("text.triptych-row-label")
    .data(rowLabelData)
    .enter()
    .append("text")
    .attr("class", "triptych-row-label")
    .attr("x", gridOffsetX - 5)
    .attr("y", (d, i) => gridOffsetY + (i * labelStep) * cellSize + cellSize / 2)
    .attr("text-anchor", "end")
    .attr("dominant-baseline", "middle")
    .attr("font-size", "8px")
    .attr("fill", "#e2e8f0")
    .attr("data-module", d => d)
    .text(d => shortenName(d))
    .style("cursor", "pointer")
    .on("mouseenter", function(event, d) {
      highlightModuleAcrossPanels(d);
    })
    .on("mouseleave", clearHighlightsAcrossPanels);

  // Column labels (top, rotated)
  const colLabelData = names.slice(0, displayN).filter((_, i) => i % labelStep === 0);

  panelGroup.selectAll("text.triptych-col-label")
    .data(colLabelData)
    .enter()
    .append("text")
    .attr("class", "triptych-col-label")
    .attr("x", (d, i) => gridOffsetX + (i * labelStep) * cellSize + cellSize / 2)
    .attr("y", gridOffsetY - 5)
    .attr("text-anchor", "start")
    .attr("font-size", "8px")
    .attr("fill", "#e2e8f0")
    .attr("transform", (d, i) => {
      const x = gridOffsetX + (i * labelStep) * cellSize + cellSize / 2;
      const y = gridOffsetY - 5;
      return `rotate(-45,${x},${y})`;
    })
    .attr("data-module", d => d)
    .text(d => shortenName(d))
    .style("cursor", "pointer")
    .on("mouseenter", function(event, d) {
      highlightModuleAcrossPanels(d);
    })
    .on("mouseleave", clearHighlightsAcrossPanels);

  // If truncated, show indicator
  if (n > maxCells) {
    panelGroup.append("text")
      .attr("class", "triptych-truncated")
      .attr("x", 0)
      .attr("y", gridOffsetY + gridSize + 20)
      .attr("text-anchor", "middle")
      .attr("font-size", "10px")
      .attr("fill", "#94a3b8")
      .text(`(showing ${maxCells} of ${n} modules)`);
  }
};

// Shorten module name (take last segment)
const shortenName = (name) => {
  const parts = name.split(".");
  return parts[parts.length - 1];
};

// ============================================================================
// Synchronized hover across all three panels
// ============================================================================

// Highlight a module across all three triptych panels
const highlightModuleAcrossPanels = (moduleName) => {
  const container = select("#explorer-zoom-group");
  if (container.empty()) return;

  // 1. Highlight in Force Graph (bubble pack)
  // The bubble packs have module-pack class with data bound
  container.selectAll("g.module-pack").each(function() {
    const group = select(this);
    const data = group.datum();
    const isMatch = data && data.name === moduleName;

    // Highlight the outer circle (pack-module, depth 0)
    group.select("circle.pack-module")
      .attr("stroke", isMatch ? "#ffffff" : "#666")
      .attr("stroke-width", isMatch ? 4 : 2);

    // Dim/highlight the whole group
    group.style("opacity", isMatch ? 1 : 0.4);
  });

  // Also highlight links connected to this module
  container.selectAll("#explorer-links line").each(function() {
    const line = select(this);
    const d = line.datum();
    if (d) {
      const isConnected = (d.source && d.source.name === moduleName) ||
                          (d.target && d.target.name === moduleName);
      line.attr("stroke-opacity", isConnected ? 1 : 0.1);
    }
  });

  // 2. Highlight in Chord diagram
  container.selectAll("path.triptych-arc")
    .attr("stroke-width", function(d) {
      const name = select(this).attr("data-module");
      return name === moduleName ? 4 : 2;
    })
    .attr("stroke", function(d) {
      const name = select(this).attr("data-module");
      return name === moduleName ? "#ffffff" : "#fff";
    });

  container.selectAll("path.triptych-ribbon")
    .attr("fill-opacity", function(d) {
      const source = select(this).attr("data-source");
      const target = select(this).attr("data-target");
      return (source === moduleName || target === moduleName) ? 0.9 : 0.3;
    });

  container.selectAll("text.triptych-chord-label")
    .attr("font-weight", function(d) {
      const name = select(this).attr("data-module");
      return name === moduleName ? "bold" : "normal";
    })
    .attr("fill", function(d) {
      const name = select(this).attr("data-module");
      return name === moduleName ? "#ffffff" : "#e2e8f0";
    });

  // 3. Highlight in Matrix
  container.selectAll("text.triptych-row-label, text.triptych-col-label")
    .attr("font-weight", function() {
      const name = select(this).attr("data-module");
      return name === moduleName ? "bold" : "normal";
    })
    .attr("fill", function() {
      const name = select(this).attr("data-module");
      return name === moduleName ? "#ffffff" : "#e2e8f0";
    });

  // Highlight row and column cells
  container.selectAll("rect.triptych-cell")
    .attr("stroke", function(d) {
      const row = select(this).attr("data-row");
      const col = select(this).attr("data-col");
      return (row === moduleName || col === moduleName) ? "#ffffff" : "#2d3748";
    })
    .attr("stroke-width", function(d) {
      const row = select(this).attr("data-row");
      const col = select(this).attr("data-col");
      return (row === moduleName || col === moduleName) ? 1.5 : 0.5;
    });
};

// Clear all highlights across panels
const clearHighlightsAcrossPanels = () => {
  const container = select("#explorer-zoom-group");
  if (container.empty()) return;

  // Reset Force Graph bubble packs
  container.selectAll("g.module-pack")
    .style("opacity", 1);

  container.selectAll("g.module-pack circle.pack-module")
    .attr("stroke", "#666")
    .attr("stroke-width", 2);

  // Reset links
  container.selectAll("#explorer-links line")
    .attr("stroke-opacity", 0.6);

  // Reset Chord diagram
  container.selectAll("path.triptych-arc")
    .attr("stroke", "#fff")
    .attr("stroke-width", 2);

  container.selectAll("path.triptych-ribbon")
    .attr("fill-opacity", 0.65);

  container.selectAll("text.triptych-chord-label")
    .attr("font-weight", "normal")
    .attr("fill", "#e2e8f0");

  // Reset Matrix
  container.selectAll("text.triptych-row-label, text.triptych-col-label")
    .attr("font-weight", "normal")
    .attr("fill", "#e2e8f0");

  container.selectAll("rect.triptych-cell")
    .attr("stroke", "#2d3748")
    .attr("stroke-width", 0.5);
};
