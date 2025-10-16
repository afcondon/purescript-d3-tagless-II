import * as d3 from "d3";
import { sankey, sankeyLinkHorizontal } from "d3-sankey";

// Apply Sankey layout to data
export const sankeySetData_ = (data) => (width) => (height) => () => {
  // Create Sankey generator
  const sankeyGenerator = sankey()
    .nodeWidth(15)
    .nodePadding(10)
    .extent([[1, 1], [width - 1, height - 5]]);

  // Deep clone the data to avoid mutation
  const dataCopy = {
    nodes: data.nodes.map(n => ({ ...n })),
    links: data.links.map(l => ({ ...l }))
  };

  // Apply the layout - this mutates the data by adding computed properties
  const graph = sankeyGenerator(dataCopy);

  // Add color to nodes based on their name (using D3 color scheme)
  const colorScale = d3.scaleOrdinal(d3.schemeCategory10);
  graph.nodes.forEach(node => {
    node.color = colorScale(node.name);
  });

  // Add visual properties to links
  graph.links.forEach(link => {
    link.width = link.width || Math.max(1, link.value);
    // Color links based on source node
    link.color = link.source.color || colorScale(link.source.name);
  });

  return graph;
};

// Generate SVG path data for a Sankey link using D3's sankeyLinkHorizontal
export const sankeyLinkPath_ = sankeyLinkHorizontal();
