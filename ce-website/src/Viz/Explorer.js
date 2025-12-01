// FFI for efficient tick updates
// Uses D3 selection directly instead of PureScript traversal
import { selectAll } from "d3-selection";

let tickCount = 0;
let lastLog = Date.now();

// Global busy-check: log if main thread is blocked
let lastBusyCheck = performance.now();
setInterval(() => {
  const now = performance.now();
  const gap = now - lastBusyCheck;
  if (gap > 200) { // Should be ~100ms, warn if significantly longer
    console.warn(`[MainThread] Blocked for ${gap.toFixed(0)}ms`);
  }
  lastBusyCheck = now;
}, 100);

// Update node positions directly via D3 selection
// Much faster than traversing in PureScript
export const updateNodePositions = _nodeSelection => () => {
  const start = performance.now();
  tickCount++;
  const now = Date.now();
  if (now - lastLog > 1000) {
    console.log(`[Tick] ${tickCount} ticks in last second`);
    tickCount = 0;
    lastLog = now;
  }

  // Use direct d3.selectAll instead of the wrapped selection
  const circles = selectAll("#ce-nodes circle");
  circles
    .attr("cx", d => d.x)
    .attr("cy", d => d.y);

  const elapsed = performance.now() - start;
  if (elapsed > 5) {
    console.log(`[DOM] Updated ${circles.size()} nodes in ${elapsed.toFixed(1)}ms`);
  }
};

// Update link positions
export const updateLinkPositions = _linkSelection => () => {
  selectAll("#ce-links line")
    .attr("x1", d => d.source.x)
    .attr("y1", d => d.source.y)
    .attr("x2", d => d.target.x)
    .attr("y2", d => d.target.y);
};
