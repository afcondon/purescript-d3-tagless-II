import { sankey } from "d3-sankey";

// Create an initial Sankey layout generator
// This is a stateless generator function that can be called multiple times
export const initialSankeyLayoutState_ = sankey()
  .nodeWidth(15)
  .nodePadding(10);

// Property accessors for SankeyNode_
export const nodeX0_ = (node) => node.x0;
export const nodeY0_ = (node) => node.y0;
export const nodeX1_ = (node) => node.x1;
export const nodeY1_ = (node) => node.y1;
export const nodeValue_ = (node) => node.value;
export const nodeDepth_ = (node) => node.depth;
export const nodeName_ = (node) => node.name;

// Property accessors for SankeyLink_
export const linkValue_ = (link) => link.value;
export const linkWidth_ = (link) => link.width;
export const linkSourceIndex_ = (link) => link.source.index;
export const linkTargetIndex_ = (link) => link.target.index;
