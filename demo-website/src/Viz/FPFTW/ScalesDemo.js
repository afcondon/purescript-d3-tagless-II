// ScalesDemo FFI - D3 rendering for scale visualizations
import * as d3 from "d3";

// Draw a horizontal gradient strip with label
export function drawGradientStrip_(selector) {
  return function(label) {
    return function(colors) {
      return function() {
        const container = d3.select(selector);
        if (container.empty()) {
          console.warn("ScalesDemo: selector not found:", selector);
          return;
        }

        const width = 400;
        const height = 30;
        const labelWidth = 120;

        // Create SVG
        const svg = container.append("svg")
          .attr("width", width + labelWidth)
          .attr("height", height + 10)
          .style("display", "block")
          .style("margin", "5px auto");

        // Add label
        svg.append("text")
          .attr("x", 0)
          .attr("y", height / 2 + 5)
          .attr("font-size", "12px")
          .attr("font-family", "monospace")
          .attr("fill", "#666")
          .text(label);

        // Draw gradient rectangles
        const rectWidth = width / colors.length;
        svg.selectAll("rect")
          .data(colors)
          .enter()
          .append("rect")
          .attr("x", (d, i) => labelWidth + i * rectWidth)
          .attr("y", 0)
          .attr("width", rectWidth + 1) // +1 to prevent gaps
          .attr("height", height)
          .attr("fill", d => d);
      };
    };
  };
}

// Draw a pipeline diagram showing data transformation
export function drawPipelineDiagram_(selector) {
  return function(data) {
    return function() {
      const container = d3.select(selector);
      if (container.empty()) {
        console.warn("ScalesDemo: selector not found:", selector);
        return;
      }

      const width = 500;
      const height = 120;
      const margin = { top: 20, right: 20, bottom: 40, left: 20 };
      const innerWidth = width - margin.left - margin.right;

      // Create SVG
      const svg = container.append("svg")
        .attr("width", width)
        .attr("height", height)
        .style("display", "block")
        .style("margin", "20px auto");

      const g = svg.append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

      // X scale for positioning
      const xScale = d3.scaleLinear()
        .domain(d3.extent(data, d => d.x))
        .range([0, innerWidth]);

      // Draw pipeline arrow
      g.append("line")
        .attr("x1", 0)
        .attr("x2", innerWidth)
        .attr("y1", 25)
        .attr("y2", 25)
        .attr("stroke", "#ccc")
        .attr("stroke-width", 2)
        .attr("marker-end", "url(#arrow)");

      // Arrow marker
      svg.append("defs").append("marker")
        .attr("id", "arrow")
        .attr("viewBox", "0 -5 10 10")
        .attr("refX", 8)
        .attr("refY", 0)
        .attr("markerWidth", 6)
        .attr("markerHeight", 6)
        .attr("orient", "auto")
        .append("path")
        .attr("d", "M0,-5L10,0L0,5")
        .attr("fill", "#ccc");

      // Draw data points
      const nodes = g.selectAll(".node")
        .data(data)
        .enter()
        .append("g")
        .attr("class", "node")
        .attr("transform", d => `translate(${xScale(d.x)},25)`);

      // Colored circles
      nodes.append("circle")
        .attr("r", 15)
        .attr("fill", d => d.color)
        .attr("stroke", "#fff")
        .attr("stroke-width", 2);

      // Labels below
      nodes.append("text")
        .attr("y", 35)
        .attr("text-anchor", "middle")
        .attr("font-size", "11px")
        .attr("font-family", "monospace")
        .attr("fill", "#666")
        .text(d => d.label);

      // Title
      g.append("text")
        .attr("x", innerWidth / 2)
        .attr("y", -5)
        .attr("text-anchor", "middle")
        .attr("font-size", "12px")
        .attr("font-weight", "bold")
        .attr("fill", "#333")
        .text("Temperature → Normalize → Color");
    };
  };
}

// Draw scale composition diagram
export function drawScaleComposition_(selector) {
  return function() {
    const container = d3.select(selector);
    if (container.empty()) {
      console.warn("ScalesDemo: selector not found:", selector);
      return;
    }

    const width = 600;
    const height = 150;

    const svg = container.append("svg")
      .attr("width", width)
      .attr("height", height)
      .style("display", "block")
      .style("margin", "20px auto");

    // Box style
    const boxStyle = {
      fill: "#f8f9fa",
      stroke: "#dee2e6",
      rx: 8
    };

    // Draw three boxes: Domain → Scale → Range
    const boxes = [
      { x: 50, label: "Domain", sublabel: "[-10, 40]", color: "#4dabf7" },
      { x: 250, label: "Scale", sublabel: "normalize `andThen` color", color: "#69db7c" },
      { x: 450, label: "Range", sublabel: "Colors", color: "#ffa94d" }
    ];

    // Boxes
    boxes.forEach(box => {
      const g = svg.append("g")
        .attr("transform", `translate(${box.x},40)`);

      g.append("rect")
        .attr("x", -50)
        .attr("y", 0)
        .attr("width", 100)
        .attr("height", 60)
        .attr("fill", boxStyle.fill)
        .attr("stroke", box.color)
        .attr("stroke-width", 2)
        .attr("rx", boxStyle.rx);

      g.append("text")
        .attr("y", 25)
        .attr("text-anchor", "middle")
        .attr("font-size", "14px")
        .attr("font-weight", "bold")
        .attr("fill", "#333")
        .text(box.label);

      g.append("text")
        .attr("y", 45)
        .attr("text-anchor", "middle")
        .attr("font-size", "10px")
        .attr("font-family", "monospace")
        .attr("fill", "#666")
        .text(box.sublabel);
    });

    // Arrows between boxes
    [[150, 200], [350, 400]].forEach(([x1, x2]) => {
      svg.append("line")
        .attr("x1", x1)
        .attr("x2", x2)
        .attr("y1", 70)
        .attr("y2", 70)
        .attr("stroke", "#adb5bd")
        .attr("stroke-width", 2)
        .attr("marker-end", "url(#arrow2)");
    });

    // Arrow marker
    svg.append("defs").append("marker")
      .attr("id", "arrow2")
      .attr("viewBox", "0 -5 10 10")
      .attr("refX", 8)
      .attr("refY", 0)
      .attr("markerWidth", 6)
      .attr("markerHeight", 6)
      .attr("orient", "auto")
      .append("path")
      .attr("d", "M0,-5L10,0L0,5")
      .attr("fill", "#adb5bd");

    // Caption
    svg.append("text")
      .attr("x", width / 2)
      .attr("y", 130)
      .attr("text-anchor", "middle")
      .attr("font-size", "12px")
      .attr("fill", "#666")
      .text("Scales compose: normalize `andThen` interpolateRdYlGn");
  };
}
