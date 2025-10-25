import * as d3 from "d3";
import {
  sankey,
  sankeyLinkHorizontal,
  sankeyJustify,
  sankeyLeft,
  sankeyRight,
  sankeyCenter
} from "d3-sankey";

// Map alignment string to d3-sankey alignment function
const getAlignmentFunction = (alignment) => {
  switch (alignment) {
    case "left": return sankeyLeft;
    case "right": return sankeyRight;
    case "center": return sankeyCenter;
    case "justify":
    default: return sankeyJustify;
  }
};

// Apply Sankey layout to data
export const sankeySetDataWithConfig_ = (data) => (width) => (height) => (config) => () => {
  // Create Sankey generator with configuration
  const sankeyGenerator = sankey()
    .nodeWidth(config.nodeWidth)
    .nodePadding(config.nodePadding)
    .nodeAlign(getAlignmentFunction(config.alignment))
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

  // Add visual properties to links based on color mode
  graph.links.forEach(link => {
    link.width = link.width || Math.max(1, link.value);

    // Apply color based on linkColorMode
    switch (config.linkColorMode) {
      case "target":
        link.color = link.target.color || colorScale(link.target.name);
        break;
      case "source-target":
        // For source-target, interpolate between source and target colors
        const sourceColor = d3.color(link.source.color || colorScale(link.source.name));
        const targetColor = d3.color(link.target.color || colorScale(link.target.name));
        // Blend the colors (simple average)
        const blended = d3.rgb(
          (sourceColor.r + targetColor.r) / 2,
          (sourceColor.g + targetColor.g) / 2,
          (sourceColor.b + targetColor.b) / 2
        );
        link.color = blended.toString();
        break;
      case "static":
        link.color = "#ccc";
        break;
      case "source":
      default:
        link.color = link.source.color || colorScale(link.source.name);
        break;
    }
  });

  return graph;
};

// Backwards compatible version with default settings
export const sankeySetData_ = (data) => (width) => (height) => () => {
  return sankeySetDataWithConfig_(data)(width)(height)({
    alignment: "justify",
    linkColorMode: "source",
    nodeWidth: 15,
    nodePadding: 10
  })();
};

// Generate SVG path data for a Sankey link using D3's sankeyLinkHorizontal
export const sankeyLinkPath_ = sankeyLinkHorizontal();
