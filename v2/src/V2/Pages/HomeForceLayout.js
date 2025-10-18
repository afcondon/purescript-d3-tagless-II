// FFI for HomeForceLayout component

// Navigation data structure with hierarchical relationships
const navigationData = {
  nodes: [
    { id: "gallery", label: "Gallery", type: "root", expanded: false, children: [
      "line-chart", "bar-chart", "scatter-plot", "scatter-quartet",
      "chord-diagram", "bubble-chart", "sankey",
      "tree", "tree-horizontal", "tree-vertical", "tree-radial"
    ]},
    { id: "spago", label: "Spago Explorer", type: "root", url: "#/spago" },
    { id: "interpreters", label: "Interpreters", type: "root", expanded: false, children: [
      "meta-tree", "print-tree"
    ]},
    { id: "github", label: "GitHub", type: "root", url: "https://github.com/afcondon/purescript-d3-tagless", external: true },

    // Gallery children
    { id: "line-chart", label: "Line Chart", type: "example", parent: "gallery" },
    { id: "bar-chart", label: "Bar Chart", type: "example", parent: "gallery" },
    { id: "scatter-plot", label: "Scatter Plot", type: "example", parent: "gallery" },
    { id: "scatter-quartet", label: "Anscombe's Quartet", type: "example", parent: "gallery" },
    { id: "chord-diagram", label: "Chord Diagram", type: "example", parent: "gallery" },
    { id: "bubble-chart", label: "Bubble Chart", type: "example", parent: "gallery" },
    { id: "sankey", label: "Sankey Diagram", type: "example", parent: "gallery" },
    { id: "tree", label: "Tree Layout", type: "example", parent: "gallery" },
    { id: "tree-horizontal", label: "Horizontal Tree", type: "example", parent: "gallery" },
    { id: "tree-vertical", label: "Vertical Tree", type: "example", parent: "gallery" },
    { id: "tree-radial", label: "Radial Tree", type: "example", parent: "gallery" },

    // Interpreters children
    { id: "meta-tree", label: "MetaTree Visualizer", type: "example", parent: "interpreters" },
    { id: "print-tree", label: "String Generator", type: "example", parent: "interpreters" }
  ]
};

/**
 * Initialize force simulation with navigation nodes
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

        // Add a background
        svg.append("rect")
          .attr("width", width)
          .attr("height", height)
          .attr("fill", "#fafafa");

        // State: currently visible nodes and links
        let visibleNodes = navigationData.nodes.filter(n => n.type === "root");
        let visibleLinks = [];

        // Create force simulation
        const simulation = d3.forceSimulation(visibleNodes)
          .force("charge", d3.forceManyBody().strength(-800))
          .force("center", d3.forceCenter(width / 2, height / 2))
          .force("collision", d3.forceCollide().radius(80))
          .force("link", d3.forceLink(visibleLinks).id(d => d.id).distance(150));

        // Create container groups
        const linkGroup = svg.append("g").attr("class", "links");
        const nodeGroup = svg.append("g").attr("class", "nodes");

        // Function to update visualization
        function update() {
          // Update links
          const link = linkGroup.selectAll("line")
            .data(visibleLinks, d => `${d.source.id}-${d.target.id}`);

          link.exit().remove();

          const linkEnter = link.enter()
            .append("line")
            .attr("stroke", "#999")
            .attr("stroke-opacity", 0.6)
            .attr("stroke-width", 2);

          // Update nodes
          const node = nodeGroup.selectAll("g")
            .data(visibleNodes, d => d.id);

          node.exit().remove();

          const nodeEnter = node.enter()
            .append("g")
            .attr("class", "node")
            .call(d3.drag()
              .on("start", dragstarted)
              .on("drag", dragged)
              .on("end", dragended));

          // Add circles
          nodeEnter.append("circle")
            .attr("r", d => d.type === "root" ? 50 : 35)
            .attr("fill", d => {
              if (d.type === "root") return d.expanded ? "#2563eb" : "#3b82f6";
              return "#10b981";
            })
            .attr("stroke", "#fff")
            .attr("stroke-width", 3);

          // Add labels
          nodeEnter.append("text")
            .attr("dy", "0.35em")
            .attr("text-anchor", "middle")
            .attr("fill", "#fff")
            .attr("font-size", d => d.type === "root" ? "14px" : "11px")
            .attr("font-weight", "600")
            .attr("pointer-events", "none")
            .text(d => d.label);

          // Click handler
          nodeEnter.on("click", function(event, d) {
            event.stopPropagation();

            if (d.type === "root" && d.children) {
              // Toggle expansion
              d.expanded = !d.expanded;

              if (d.expanded) {
                // Add children
                const children = navigationData.nodes.filter(n => d.children.includes(n.id));
                children.forEach(child => {
                  if (!visibleNodes.find(n => n.id === child.id)) {
                    visibleNodes.push(child);
                    visibleLinks.push({ source: d.id, target: child.id });
                  }
                });
              } else {
                // Remove children
                visibleLinks = visibleLinks.filter(l => l.source.id !== d.id && l.target.id !== d.id);
                visibleNodes = visibleNodes.filter(n => !d.children.includes(n.id));
              }

              update();
            } else if (d.type === "example") {
              // Navigate to example
              window.location.hash = `#/example/${d.id}`;
            } else if (d.url) {
              // Navigate to URL
              if (d.external) {
                window.open(d.url, '_blank');
              } else {
                window.location.hash = d.url.replace('#', '');
              }
            }
          });

          // Update simulation
          simulation.nodes(visibleNodes);
          simulation.force("link").links(visibleLinks);
          simulation.alpha(0.3).restart();
        }

        // Tick function
        simulation.on("tick", () => {
          linkGroup.selectAll("line")
            .attr("x1", d => d.source.x)
            .attr("y1", d => d.source.y)
            .attr("x2", d => d.target.x)
            .attr("y2", d => d.target.y);

          nodeGroup.selectAll("g")
            .attr("transform", d => `translate(${d.x},${d.y})`);
        });

        // Drag functions
        function dragstarted(event, d) {
          if (!event.active) simulation.alphaTarget(0.3).restart();
          d.fx = d.x;
          d.fy = d.y;
        }

        function dragged(event, d) {
          d.fx = event.x;
          d.fy = event.y;
        }

        function dragended(event, d) {
          if (!event.active) simulation.alphaTarget(0);
          d.fx = null;
          d.fy = null;
        }

        // Initial render
        update();

        console.log("Force layout initialized with navigation nodes");
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
