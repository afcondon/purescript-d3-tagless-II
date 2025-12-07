// FFI for Explorer - string manipulation utilities

export const charAtFFI = (idx) => (str) => {
  return idx < str.length ? str.charAt(idx) : "";
};

export const substringFFI = (start) => (end) => (str) => {
  return str.substring(start, end);
};

export const stringLengthFFI = (str) => str.length;

// Render a package node in the package neighborhood view
// Creates a group with circle + layer indicator + label
export const renderPackageNodeFFI = (x) => (y) => (name) => (layer) => (radius) => (fillColor) => (strokeColor) => (strokeWidth) => (fontWeight) => () => {
  const nodesGroup = document.querySelector("#explorer-nodes");
  if (!nodesGroup) return;

  // Create SVG namespace elements
  const svgNS = "http://www.w3.org/2000/svg";

  // Create group with transform
  const g = document.createElementNS(svgNS, "g");
  g.setAttribute("transform", `translate(${x},${y})`);
  g.setAttribute("class", "package-neighborhood-node");

  // Circle
  const circle = document.createElementNS(svgNS, "circle");
  circle.setAttribute("r", radius);
  circle.setAttribute("fill", fillColor);
  circle.setAttribute("stroke", strokeColor);
  circle.setAttribute("stroke-width", strokeWidth);
  g.appendChild(circle);

  // Layer indicator (small text above circle)
  const layerText = document.createElementNS(svgNS, "text");
  layerText.setAttribute("y", -(radius + 8));
  layerText.setAttribute("text-anchor", "middle");
  layerText.setAttribute("fill", "#94a3b8");
  layerText.setAttribute("font-size", "10px");
  layerText.textContent = `L${layer}`;
  g.appendChild(layerText);

  // Package name label (below circle)
  const label = document.createElementNS(svgNS, "text");
  label.setAttribute("y", radius + 16);
  label.setAttribute("text-anchor", "middle");
  label.setAttribute("fill", "#e2e8f0");
  label.setAttribute("font-size", "12px");
  label.setAttribute("font-weight", fontWeight);
  label.textContent = name;
  g.appendChild(label);

  nodesGroup.appendChild(g);
};
