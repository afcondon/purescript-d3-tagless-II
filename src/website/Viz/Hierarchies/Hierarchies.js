// FFI functions for Hierarchies visualization

// Clear all contents of a container selected by D3
export function clearContainer_(selector) {
  const node = d3.select(selector).node();
  if (node) {
    node.innerHTML = "";
  }
}
