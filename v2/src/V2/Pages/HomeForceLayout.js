// FFI for HomeForceLayout component

/**
 * Initialize an empty force simulation in the specified container
 * @param {string} selector - CSS selector for the SVG container
 * @param {number} width - Viewport width
 * @param {number} height - Viewport height
 */
export function initializeEmptyForceLayout(selector) {
  return function(width) {
    return function(height) {
      return function() {
        // Remove any existing SVG
        d3.select(selector).selectAll("svg").remove();

        // Create SVG
        const svg = d3.select(selector)
          .append("svg")
          .attr("width", width)
          .attr("height", height)
          .attr("viewBox", [0, 0, width, height]);

        // Add a background for debugging (subtle)
        svg.append("rect")
          .attr("width", width)
          .attr("height", height)
          .attr("fill", "#fafafa");

        // Create empty force simulation
        const simulation = d3.forceSimulation([])
          .force("charge", d3.forceManyBody().strength(-300))
          .force("center", d3.forceCenter(width / 2, height / 2))
          .force("collision", d3.forceCollide().radius(50));

        // Create container groups for links and nodes
        const linkGroup = svg.append("g").attr("class", "links");
        const nodeGroup = svg.append("g").attr("class", "nodes");

        console.log("Empty force layout initialized:", { selector, width, height });

        // Return the simulation and svg for later use
        return {
          simulation: simulation,
          svg: svg,
          linkGroup: linkGroup,
          nodeGroup: nodeGroup
        };
      };
    };
  };
}

/**
 * Get current window dimensions
 */
export function getWindowDimensions() {
  return {
    width: window.innerWidth,
    height: window.innerHeight
  };
}
