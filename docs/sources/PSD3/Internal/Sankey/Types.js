import { sankey } from "d3-sankey";

// Create an initial Sankey layout generator
// This is a stateless generator function that can be called multiple times
export const initialSankeyLayoutState_ = sankey()
  .nodeWidth(15)
  .nodePadding(10);
