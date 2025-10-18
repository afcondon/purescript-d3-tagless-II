// FFI for HomeForceLayout component

// Category colors
const categoryColors = {
  "basic-chart": "#3b82f6",      // blue
  "advanced-layout": "#8b5cf6",  // purple
  "interactive": "#10b981",      // green
  "interpreter": "#f59e0b",      // amber
  "application": "#ef4444"       // red
};

// Type colors
const typeColors = {
  "center": "#1e40af",     // dark blue
  "section": "#3b82f6",    // blue
  "example": "#10b981",    // green (default, overridden by category)
  "feature": "#ec4899"     // pink
};

// Navigation data structure with hierarchical relationships
const navigationData = {
  nodes: [
    // Central root
    { id: "purescript-d3", label: "PureScript D3", type: "center", expanded: true, children: [
      "gallery", "about", "spago", "interpreters", "github"
    ]},

    // Main sections
    { id: "gallery", label: "Gallery", type: "section", expanded: false, parent: "purescript-d3", children: [
      "line-chart", "bar-chart", "scatter-plot", "scatter-quartet",
      "chord-diagram", "bubble-chart", "sankey",
      "tree", "tree-horizontal", "tree-vertical", "tree-radial",
      "three-little-circles", "gup", "les-mis"
    ]},
    { id: "about", label: "About", type: "section", expanded: false, parent: "purescript-d3", children: [
      "type-safe", "composable", "interpreters", "d3-powered", "interactive", "documented"
    ]},
    { id: "spago", label: "Spago Explorer", type: "section", parent: "purescript-d3", url: "#/spago" },
    { id: "interpreters", label: "Interpreters", type: "section", expanded: false, parent: "purescript-d3", children: [
      "meta-tree", "print-tree"
    ]},
    { id: "github", label: "GitHub", type: "section", parent: "purescript-d3", url: "https://github.com/afcondon/purescript-d3-tagless", external: true },

    // Gallery children - Basic Charts
    { id: "line-chart", label: "Line Chart", type: "example", category: "basic-chart", parent: "gallery" },
    { id: "bar-chart", label: "Bar Chart", type: "example", category: "basic-chart", parent: "gallery" },
    { id: "scatter-plot", label: "Scatter Plot", type: "example", category: "basic-chart", parent: "gallery" },
    { id: "scatter-quartet", label: "Anscombe's Quartet", type: "example", category: "basic-chart", parent: "gallery" },

    // Gallery children - Advanced Layouts
    { id: "chord-diagram", label: "Chord Diagram", type: "example", category: "advanced-layout", parent: "gallery" },
    { id: "bubble-chart", label: "Bubble Chart", type: "example", category: "advanced-layout", parent: "gallery" },
    { id: "sankey", label: "Sankey Diagram", type: "example", category: "advanced-layout", parent: "gallery" },
    { id: "tree", label: "Tree Layout", type: "example", category: "advanced-layout", parent: "gallery" },
    { id: "tree-horizontal", label: "Horizontal Tree", type: "example", category: "advanced-layout", parent: "gallery" },
    { id: "tree-vertical", label: "Vertical Tree", type: "example", category: "advanced-layout", parent: "gallery" },
    { id: "tree-radial", label: "Radial Tree", type: "example", category: "advanced-layout", parent: "gallery" },

    // Gallery children - Interactive
    { id: "three-little-circles", label: "Three Little Circles", type: "example", category: "interactive", parent: "gallery" },
    { id: "gup", label: "General Update Pattern", type: "example", category: "interactive", parent: "gallery" },
    { id: "les-mis", label: "Les Misérables Network", type: "example", category: "interactive", parent: "gallery" },

    // Interpreters children
    { id: "meta-tree", label: "MetaTree Visualizer", type: "example", category: "interpreter", parent: "interpreters" },
    { id: "print-tree", label: "String Generator", type: "example", category: "interpreter", parent: "interpreters" },

    // About children (features)
    { id: "type-safe", label: "Type-Safe", description: "Strong type safety with PureScript", type: "feature", parent: "about" },
    { id: "composable", label: "Composable", description: "Build complex visualizations from simple components", type: "feature", parent: "about" },
    { id: "interpreters", label: "Multiple Interpreters", description: "Finally Tagless pattern enables different interpretations", type: "feature", parent: "about" },
    { id: "d3-powered", label: "D3-Powered", description: "Leverages D3.js for battle-tested rendering", type: "feature", parent: "about" },
    { id: "interactive", label: "Interactive", description: "Support for drag, zoom, and other behaviors", type: "feature", parent: "about" },
    { id: "documented", label: "Well-Documented", description: "Comprehensive examples with comparisons", type: "feature", parent: "about" }
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
        // Start with central node and its immediate children (main sections)
        const centerNode = navigationData.nodes.find(n => n.type === "center");
        const sectionNodes = navigationData.nodes.filter(n => n.type === "section");
        let visibleNodes = [centerNode, ...sectionNodes];
        let visibleLinks = sectionNodes.map(s => ({ source: "purescript-d3", target: s.id }));

        // Fix center node to viewport center
        centerNode.fx = width / 2;
        centerNode.fy = height / 2;

        // Boundary force to keep nodes in viewport
        function boundaryForce() {
          const padding = 80;
          for (let node of visibleNodes) {
            node.x = Math.max(padding, Math.min(width - padding, node.x));
            node.y = Math.max(padding, Math.min(height - padding, node.y));
          }
        }

        // Create force simulation
        const simulation = d3.forceSimulation(visibleNodes)
          .force("charge", d3.forceManyBody().strength(-800))
          .force("collision", d3.forceCollide().radius(80))
          .force("link", d3.forceLink(visibleLinks).id(d => d.id).distance(150))
          .force("boundary", boundaryForce);

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
            .attr("r", d => {
              if (d.type === "center") return 60;
              if (d.type === "section") return 50;
              if (d.type === "feature") return 30;
              return 35;
            })
            .attr("fill", d => {
              if (d.type === "center") return typeColors.center;
              if (d.type === "section") return d.expanded ? "#2563eb" : typeColors.section;
              if (d.type === "feature") return typeColors.feature;
              // Example nodes use category colors
              return categoryColors[d.category] || typeColors.example;
            })
            .attr("stroke", "#fff")
            .attr("stroke-width", 3);

          // Add labels
          nodeEnter.append("text")
            .attr("dy", "0.35em")
            .attr("text-anchor", "middle")
            .attr("fill", "#fff")
            .attr("font-size", d => {
              if (d.type === "center") return "16px";
              if (d.type === "section") return "14px";
              return "11px";
            })
            .attr("font-weight", "600")
            .attr("pointer-events", "none")
            .text(d => d.label);

          // Click handler
          nodeEnter.on("click", function(event, d) {
            event.stopPropagation();

            // Handle expandable nodes (center and section nodes with children)
            if ((d.type === "center" || d.type === "section") && d.children) {
              // Toggle expansion
              d.expanded = !d.expanded;

              if (d.expanded) {
                // Add children
                const children = navigationData.nodes.filter(n => d.children.includes(n.id));
                children.forEach(child => {
                  if (!visibleNodes.find(n => n.id === child.id)) {
                    visibleNodes.push(child); // children should be entered at parent's position initially
                    visibleLinks.push({ source: d.id, target: child.id }); 
                  }
                });
              } else {
                // Remove children (and their children recursively)
                function removeDescendants(nodeId) {
                  const node = navigationData.nodes.find(n => n.id === nodeId);
                  if (node && node.children) {
                    node.children.forEach(childId => {
                      removeDescendants(childId);
                      visibleNodes = visibleNodes.filter(n => n.id !== childId);
                    });
                    node.expanded = false;
                  }
                  // Remove links where this node is the source (but keep parent link)
                  visibleLinks = visibleLinks.filter(l => l.source.id !== nodeId);
                }

                d.children.forEach(childId => removeDescendants(childId));
                visibleNodes = visibleNodes.filter(n => !d.children.includes(n.id));
                // Remove child links but keep the parent link to this node
                visibleLinks = visibleLinks.filter(l => {
                  // Keep link if this node is the target (parent link)
                  if (l.target.id === d.id) return true;
                  // Keep link if this node is not the source
                  if (l.source.id !== d.id) return true;
                  // Remove link (this node is source and we're collapsing)
                  return false;
                });
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
