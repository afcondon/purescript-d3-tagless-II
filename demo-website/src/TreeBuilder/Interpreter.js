// TreeBuilder Interpreter FFI
import { select } from "d3-selection";

// SVG namespace
const SVG_NS = "http://www.w3.org/2000/svg";

// Elements that need SVG namespace
const SVG_ELEMENTS = new Set(['svg', 'circle', 'rect', 'line', 'path', 'text', 'g', 'group']);

export const renderPreviewFFI = (containerSelector) => (instructions) => () => {
  const container = select(containerSelector);
  container.selectAll('*').remove();

  // Render each root instruction
  instructions.forEach(instr => {
    renderInstruction(container.node(), instr);
  });
};

function renderInstruction(parent, instruction) {
  const elemType = normalizeElementType(instruction.elemType);

  // Create element with appropriate namespace
  let elem;
  if (SVG_ELEMENTS.has(elemType)) {
    elem = document.createElementNS(SVG_NS, elemType);
  } else {
    elem = document.createElement(elemType);
  }

  // Apply attributes
  instruction.attrs.forEach(attr => {
    if (attr.name === 'text') {
      // Special handling for text content
      elem.textContent = attr.value;
    } else {
      elem.setAttribute(attr.name, attr.value);
    }
  });

  // Add to parent
  parent.appendChild(elem);

  // Render children
  instruction.children.forEach(child => {
    renderInstruction(elem, child);
  });

  return elem;
}

function normalizeElementType(type) {
  // Map our element names to actual SVG/HTML element names
  switch (type) {
    case 'group': return 'g';
    default: return type;
  }
}
