// FFI for Call Graph Popup
// Show/hide popup and fetch function call data

const API_BASE = "http://localhost:8080";

// Show the call graph popup with function call data
export const showCallGraphPopup_ = (moduleName) => (declarationName) => () => {
  const popup = document.getElementById("call-graph-popup");
  const title = document.getElementById("call-graph-title");
  const callersList = document.getElementById("call-graph-callers-list");
  const sourceCode = document.getElementById("call-graph-source-code");
  const calleesList = document.getElementById("call-graph-callees-list");

  if (!popup) {
    console.error("[CallGraphPopup] Popup element not found");
    return;
  }

  // Update title
  title.textContent = `${declarationName} (${moduleName})`;

  // Show loading state
  callersList.innerHTML = '<div class="call-graph-loading">Loading...</div>';
  sourceCode.innerHTML = '<div class="call-graph-loading">Loading source code...</div>';
  calleesList.innerHTML = '<div class="call-graph-loading">Loading...</div>';

  // Show popup
  popup.style.display = "flex";

  // Fetch function call data
  fetchCallGraphData(moduleName, declarationName)
    .then(data => {
      // Update callers
      if (data.callers && data.callers.length > 0) {
        callersList.innerHTML = data.callers
          .map(fn => {
            // Handle both string format and object format
            const displayName = typeof fn === 'string' ? fn : fn.target || fn;
            return `<div class="call-graph-list-item">${escapeHtml(displayName)}</div>`;
          })
          .join("");
      } else {
        callersList.innerHTML = '<div class="call-graph-empty">No callers found</div>';
      }

      // Update callees
      if (data.callees && data.callees.length > 0) {
        calleesList.innerHTML = data.callees
          .map(fn => {
            // Handle both string format and object format
            const displayName = typeof fn === 'string' ? fn : fn.target || fn;
            return `<div class="call-graph-list-item">${escapeHtml(displayName)}</div>`;
          })
          .join("");
      } else {
        calleesList.innerHTML = '<div class="call-graph-empty">No calls found</div>';
      }

      // Update source code
      if (data.sourceCode) {
        sourceCode.textContent = data.sourceCode;
      } else {
        sourceCode.innerHTML = '<div class="call-graph-empty">Source code not available</div>';
      }
    })
    .catch(error => {
      console.error("[CallGraphPopup] Error fetching call graph data:", error);
      callersList.innerHTML = '<div class="call-graph-error">Error loading callers</div>';
      sourceCode.innerHTML = '<div class="call-graph-error">Error loading source code</div>';
      calleesList.innerHTML = '<div class="call-graph-error">Error loading callees</div>';
    });
};

// Hide the call graph popup
export const hideCallGraphPopup_ = () => {
  const popup = document.getElementById("call-graph-popup");
  if (popup) {
    popup.style.display = "none";
  }
};

// Fetch call graph data from API
async function fetchCallGraphData(moduleName, declarationName) {
  try {
    // Get current snapshot ID from URL or use default
    const urlParams = new URLSearchParams(window.location.search);
    const snapshotId = urlParams.get('snapshot') || '1';

    // Fetch function calls for the entire module
    const callsResponse = await fetch(`${API_BASE}/api/module-function-calls/${encodeURIComponent(moduleName)}?snapshot=${snapshotId}`);
    const callsData = await callsResponse.json();

    // Extract data for the specific declaration
    const functionData = callsData.functions?.[declarationName];

    if (!functionData) {
      console.warn(`[CallGraphPopup] No data found for ${moduleName}.${declarationName}`);
      return {
        callers: [],
        callees: [],
        sourceCode: null
      };
    }

    // Fetch declaration info (comments) which may contain some code info
    const declResponse = await fetch(`${API_BASE}/api/module-declarations/${encodeURIComponent(moduleName)}?snapshot=${snapshotId}`);
    const declData = await declResponse.json();

    // Find the specific declaration
    const declaration = declData.declarations?.find(d => d.title === declarationName);
    const sourceInfo = declaration
      ? `${declaration.kind}: ${declaration.title}\n\n(Source code display coming soon)`
      : `${declarationName}\n\n(Source code display coming soon)`;

    return {
      callers: functionData.calledBy || [],
      callees: functionData.calls || [],
      sourceCode: sourceInfo
    };
  } catch (error) {
    console.error("[CallGraphPopup] Fetch error:", error);
    throw error;
  }
}

// Escape HTML to prevent XSS
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

// Initialize event listeners on page load
if (typeof document !== 'undefined') {
  document.addEventListener('DOMContentLoaded', () => {
    // Close button
    const closeBtn = document.getElementById("call-graph-close");
    if (closeBtn) {
      closeBtn.addEventListener('click', hideCallGraphPopup_);
    }

    // Click outside to close
    const popup = document.getElementById("call-graph-popup");
    if (popup) {
      popup.addEventListener('click', (e) => {
        if (e.target === popup) {
          hideCallGraphPopup_();
        }
      });
    }

    // ESC key to close
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape') {
        const popup = document.getElementById("call-graph-popup");
        if (popup && popup.style.display !== 'none') {
          hideCallGraphPopup_();
        }
      }
    });
  });
}
