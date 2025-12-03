// FFI for AtomicView - force-directed call graph visualization
// Shows a declaration's incoming/outgoing calls in a floating panel

import { select } from "d3-selection";
import { forceSimulation, forceLink, forceManyBody, forceX, forceY, forceCollide } from "d3-force";
import { drag } from "d3-drag";
import { scaleOrdinal } from "d3-scale";
import { schemeCategory10 } from "d3-scale-chromatic";

// Color scale for modules
const moduleColorScale = scaleOrdinal(schemeCategory10);

// Active simulation reference (for cleanup)
let activeSimulation = null;

// Clear any existing atomic view
export const clearAtomicView_ = () => {
  if (activeSimulation) {
    activeSimulation.stop();
    activeSimulation = null;
  }
  const existing = document.getElementById("atomic-view-panel");
  if (existing) existing.remove();
};

// Render the atomic view panel
// Parameters:
// - declarationInfo: { module, name, calls: [{target, targetModule, identifier}], calledBy: [string] }
export const renderAtomicView_ = (declarationInfo) => () => {
  // Clear any existing view
  clearAtomicView_();

  const { module: moduleName, name: declName, calls, calledBy } = declarationInfo;

  // Create floating panel
  const panel = document.createElement("div");
  panel.id = "atomic-view-panel";
  panel.style.cssText = `
    position: fixed;
    top: 60px;
    left: 50%;
    transform: translateX(-50%);
    width: 800px;
    height: 500px;
    background: rgba(20, 20, 30, 0.95);
    border: 1px solid #444;
    border-radius: 12px;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5);
    z-index: 2000;
    overflow: hidden;
  `;

  // Header with title and close button
  const header = document.createElement("div");
  header.style.cssText = `
    padding: 12px 16px;
    background: rgba(40, 40, 50, 0.9);
    border-bottom: 1px solid #444;
    display: flex;
    justify-content: space-between;
    align-items: center;
  `;

  const title = document.createElement("div");
  title.innerHTML = `
    <span style="color: #888; font-size: 12px;">Function Call Graph</span><br/>
    <span style="color: #fff; font-size: 16px; font-weight: 500;">${moduleName}.<span style="color: #7dd3fc;">${declName}</span></span>
  `;
  header.appendChild(title);

  const closeBtn = document.createElement("button");
  closeBtn.textContent = "×";
  closeBtn.style.cssText = `
    background: none;
    border: none;
    color: #888;
    font-size: 24px;
    cursor: pointer;
    padding: 0 8px;
    line-height: 1;
  `;
  closeBtn.onclick = clearAtomicView_;
  closeBtn.onmouseenter = () => closeBtn.style.color = "#fff";
  closeBtn.onmouseleave = () => closeBtn.style.color = "#888";
  header.appendChild(closeBtn);

  panel.appendChild(header);

  // Stats bar
  const statsBar = document.createElement("div");
  statsBar.style.cssText = `
    padding: 8px 16px;
    background: rgba(30, 30, 40, 0.9);
    border-bottom: 1px solid #333;
    display: flex;
    gap: 24px;
    font-size: 12px;
  `;
  statsBar.innerHTML = `
    <div><span style="color: #888;">Calls:</span> <span style="color: #4ade80;">${calls.length}</span></div>
    <div><span style="color: #888;">Called by:</span> <span style="color: #f97316;">${calledBy.length}</span></div>
    <div style="margin-left: auto; color: #666;">Drag nodes to rearrange • Click outside to close</div>
  `;
  panel.appendChild(statsBar);

  // SVG container
  const svgContainer = document.createElement("div");
  svgContainer.id = "atomic-view-svg";
  svgContainer.style.cssText = `
    width: 100%;
    height: calc(100% - 90px);
  `;
  panel.appendChild(svgContainer);

  document.body.appendChild(panel);

  // Close on click outside panel
  const handleOutsideClick = (e) => {
    if (!panel.contains(e.target)) {
      clearAtomicView_();
      document.removeEventListener("click", handleOutsideClick);
    }
  };
  // Delay adding listener so current click doesn't immediately close
  setTimeout(() => document.addEventListener("click", handleOutsideClick), 100);

  // Build nodes and links for force graph
  const nodes = [];
  const links = [];

  // Center node (the clicked declaration)
  const centerNode = {
    id: `${moduleName}.${declName}`,
    label: declName,
    module: moduleName,
    type: "center",
    fx: 400, // Fixed at center x
    fy: 205  // Fixed at center y
  };
  nodes.push(centerNode);

  // Add outgoing calls (what this function calls)
  const seenCalls = new Set();
  calls.forEach((call, i) => {
    const nodeId = `${call.targetModule}.${call.identifier}`;
    if (!seenCalls.has(nodeId) && nodeId !== centerNode.id) {
      seenCalls.add(nodeId);
      nodes.push({
        id: nodeId,
        label: call.identifier,
        module: call.targetModule,
        type: "outgoing"
      });
      links.push({
        source: centerNode.id,
        target: nodeId,
        type: "outgoing"
      });
    }
  });

  // Add callers (what calls this function)
  const seenCallers = new Set();
  calledBy.forEach((caller, i) => {
    if (!seenCallers.has(caller) && caller !== centerNode.id) {
      seenCallers.add(caller);
      // Parse caller string "Module.name"
      const lastDot = caller.lastIndexOf(".");
      const callerModule = lastDot > 0 ? caller.substring(0, lastDot) : "Unknown";
      const callerName = lastDot > 0 ? caller.substring(lastDot + 1) : caller;

      nodes.push({
        id: caller,
        label: callerName,
        module: callerModule,
        type: "incoming"
      });
      links.push({
        source: caller,
        target: centerNode.id,
        type: "incoming"
      });
    }
  });

  // Create SVG
  const width = 800;
  const height = 410;

  const svg = select("#atomic-view-svg")
    .append("svg")
    .attr("width", "100%")
    .attr("height", "100%")
    .attr("viewBox", `0 0 ${width} ${height}`);

  // Arrow markers for directed edges
  const defs = svg.append("defs");

  // Outgoing arrow (green)
  defs.append("marker")
    .attr("id", "arrow-outgoing")
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 20)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("fill", "#4ade80")
    .attr("d", "M0,-5L10,0L0,5");

  // Incoming arrow (orange)
  defs.append("marker")
    .attr("id", "arrow-incoming")
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 20)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("fill", "#f97316")
    .attr("d", "M0,-5L10,0L0,5");

  // Create links
  const linkGroup = svg.append("g").attr("class", "links");
  const linkElements = linkGroup.selectAll("line")
    .data(links)
    .enter()
    .append("line")
    .attr("stroke", d => d.type === "outgoing" ? "#4ade80" : "#f97316")
    .attr("stroke-width", 1.5)
    .attr("stroke-opacity", 0.6)
    .attr("marker-end", d => d.type === "outgoing" ? "url(#arrow-outgoing)" : "url(#arrow-incoming)");

  // Create node groups
  const nodeGroup = svg.append("g").attr("class", "nodes");
  const nodeElements = nodeGroup.selectAll("g")
    .data(nodes)
    .enter()
    .append("g")
    .attr("class", "node")
    .style("cursor", "grab");

  // Node circles
  nodeElements.append("circle")
    .attr("r", d => d.type === "center" ? 20 : 12)
    .attr("fill", d => {
      if (d.type === "center") return "#7dd3fc";
      return moduleColorScale(d.module);
    })
    .attr("stroke", d => d.type === "center" ? "#fff" : "#333")
    .attr("stroke-width", d => d.type === "center" ? 3 : 1);

  // Node labels (function name)
  nodeElements.append("text")
    .text(d => d.label)
    .attr("dy", d => d.type === "center" ? 35 : 25)
    .attr("text-anchor", "middle")
    .attr("fill", "#fff")
    .attr("font-size", d => d.type === "center" ? "14px" : "11px")
    .attr("font-weight", d => d.type === "center" ? "bold" : "normal");

  // Module labels (smaller, below)
  nodeElements.append("text")
    .text(d => d.module.split(".").pop()) // Just last part of module name
    .attr("dy", d => d.type === "center" ? 50 : 38)
    .attr("text-anchor", "middle")
    .attr("fill", "#888")
    .attr("font-size", "9px");

  // Set up force simulation
  const simulation = forceSimulation(nodes)
    .force("link", forceLink(links)
      .id(d => d.id)
      .distance(120)
      .strength(0.5))
    .force("charge", forceManyBody().strength(-300))
    .force("x", forceX()
      .x(d => {
        if (d.type === "center") return width / 2;
        if (d.type === "incoming") return width * 0.2;  // Left side for callers
        return width * 0.8;  // Right side for callees
      })
      .strength(0.3))
    .force("y", forceY(height / 2).strength(0.1))
    .force("collide", forceCollide().radius(40));

  activeSimulation = simulation;

  // Drag behavior
  const dragBehavior = drag()
    .on("start", (event, d) => {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    })
    .on("drag", (event, d) => {
      d.fx = event.x;
      d.fy = event.y;
    })
    .on("end", (event, d) => {
      if (!event.active) simulation.alphaTarget(0);
      // Keep center node fixed, release others
      if (d.type !== "center") {
        d.fx = null;
        d.fy = null;
      }
    });

  nodeElements.call(dragBehavior);

  // Update positions on tick
  simulation.on("tick", () => {
    linkElements
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);

    nodeElements.attr("transform", d => `translate(${d.x},${d.y})`);
  });
};
