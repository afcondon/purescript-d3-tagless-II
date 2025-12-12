// TreeBuilder App FFI
import { select } from 'd3-selection';
import { sankey, sankeyLinkHorizontal } from 'd3-sankey';

export const clearPreviewContainer = (selector) => () => {
  const container = document.querySelector(selector);
  if (container) {
    container.innerHTML = '';
  }
};

// Render a Sankey watermark showing Tree → D3/Quine → DOM/Code data flow
export const renderSankeyWatermarkImpl = (selector) => () => {
  const container = document.querySelector(selector);
  if (!container) return;

  // Clear any existing content
  container.innerHTML = '';

  // Get container dimensions for full-width rendering
  const containerRect = container.getBoundingClientRect();
  const width = Math.max(800, containerRect.width - 40);
  const height = Math.max(300, containerRect.height - 40);
  const nodeWidth = 24;
  const nodePadding = 40;

  // Define nodes for the data flow diagram
  // 0: Tree, 1: Data, 2: D3 Interp, 3: Quine, 4: DOM, 5: Code
  const nodes = [
    { name: "Tree" },      // 0
    { name: "Data" },      // 1
    { name: "D3 Interp" }, // 2
    { name: "Quine" },     // 3
    { name: "DOM" },       // 4
    { name: "Code" }       // 5
  ];

  // Define links (source, target, value)
  // Tree → D3 Interp, Tree → Quine
  // Data → D3 Interp, Data → Quine
  // D3 Interp → DOM, Quine → Code
  const links = [
    { source: 0, target: 2, value: 3 },  // Tree → D3 Interp
    { source: 0, target: 3, value: 2 },  // Tree → Quine
    { source: 1, target: 2, value: 2 },  // Data → D3 Interp
    { source: 1, target: 3, value: 1 },  // Data → Quine
    { source: 2, target: 4, value: 5 },  // D3 Interp → DOM
    { source: 3, target: 5, value: 3 }   // Quine → Code
  ];

  // Create sankey generator
  const sankeyGenerator = sankey()
    .nodeWidth(nodeWidth)
    .nodePadding(nodePadding)
    .extent([[1, 1], [width - 1, height - 1]]);

  // Compute the sankey layout
  const graph = sankeyGenerator({
    nodes: nodes.map(d => Object.assign({}, d)),
    links: links.map(d => Object.assign({}, d))
  });

  // Create SVG
  const svg = select(container)
    .append('svg')
    .attr('width', width)
    .attr('height', height)
    .attr('viewBox', [0, 0, width, height])
    .style('overflow', 'visible');

  // Color scale - editorial palette with bright outputs
  const colors = {
    "Tree": "#E63946",     // red accent
    "Data": "#5A8A8A",     // teal
    "D3 Interp": "#D4C9A8", // beige
    "Quine": "#D4C9A8",    // beige
    "DOM": "#1d4ed8",      // bright blue
    "Code": "#16a34a"      // bright green
  };

  // Draw links - color by target for output nodes (DOM, Code)
  svg.append('g')
    .attr('fill', 'none')
    .selectAll('path')
    .data(graph.links)
    .join('path')
    .attr('d', sankeyLinkHorizontal())
    .attr('stroke', d => {
      // Color output links by their target
      if (d.target.name === 'DOM' || d.target.name === 'Code') {
        return colors[d.target.name];
      }
      return colors[d.source.name];
    })
    .attr('stroke-width', d => Math.max(1, d.width))
    .attr('stroke-opacity', 0.5);

  // Draw nodes
  svg.append('g')
    .selectAll('rect')
    .data(graph.nodes)
    .join('rect')
    .attr('x', d => d.x0)
    .attr('y', d => d.y0)
    .attr('height', d => d.y1 - d.y0)
    .attr('width', d => d.x1 - d.x0)
    .attr('fill', d => colors[d.name])
    .attr('stroke', '#333')
    .attr('stroke-width', 0.5);

  // Add node labels
  svg.append('g')
    .style('font-family', "'Courier New', monospace")
    .style('font-size', '16px')
    .style('font-weight', 'bold')
    .style('letter-spacing', '0.05em')
    .selectAll('text')
    .data(graph.nodes)
    .join('text')
    .attr('x', d => d.x0 < width / 2 ? d.x1 + 12 : d.x0 - 12)
    .attr('y', d => (d.y0 + d.y1) / 2)
    .attr('dy', '0.35em')
    .attr('text-anchor', d => d.x0 < width / 2 ? 'start' : 'end')
    .attr('fill', '#333')
    .text(d => d.name);
};
