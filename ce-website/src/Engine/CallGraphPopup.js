// FFI for Call Graph Popup
// DOM-only operations - all data fetching is done in PureScript

// =============================================================================
// Show Loading State
// =============================================================================

// Show the popup with loading indicators (called before data is fetched)
export const showPopupLoading_ = (moduleName) => (declarationName) => () => {
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
};

// =============================================================================
// Render Popup Data
// =============================================================================

// Render the popup with fetched data from PureScript
export const renderPopupData_ = (data) => () => {
  const callersList = document.getElementById("call-graph-callers-list");
  const sourceCode = document.getElementById("call-graph-source-code");
  const calleesList = document.getElementById("call-graph-callees-list");

  if (!callersList || !sourceCode || !calleesList) {
    console.error("[CallGraphPopup] Popup elements not found");
    return;
  }

  // Update callers with clickable links
  if (data.callers && data.callers.length > 0) {
    callersList.innerHTML = data.callers
      .map(fn => {
        const displayName = fn.target;
        const targetModule = fn.targetModule;
        if (targetModule && displayName) {
          return `<div class="call-graph-list-item call-graph-link" data-module="${escapeHtml(targetModule)}" data-function="${escapeHtml(displayName)}">${escapeHtml(displayName)}</div>`;
        } else {
          return `<div class="call-graph-list-item">${escapeHtml(displayName)}</div>`;
        }
      })
      .join("");
  } else {
    callersList.innerHTML = '<div class="call-graph-empty">No callers found</div>';
  }

  // Update callees with clickable links
  if (data.callees && data.callees.length > 0) {
    calleesList.innerHTML = data.callees
      .map(fn => {
        const displayName = fn.target;
        const targetModule = fn.targetModule;
        if (targetModule && displayName) {
          return `<div class="call-graph-list-item call-graph-link" data-module="${escapeHtml(targetModule)}" data-function="${escapeHtml(displayName)}">${escapeHtml(displayName)}</div>`;
        } else {
          return `<div class="call-graph-list-item">${escapeHtml(displayName)}</div>`;
        }
      })
      .join("");
  } else {
    calleesList.innerHTML = '<div class="call-graph-empty">No calls found</div>';
  }

  // Update source code with git metrics
  let sourceHtml = '';

  // Add git metrics section if available
  if (data.hasGitMetrics) {
    const m = data.gitMetrics;
    sourceHtml += '<div class="call-graph-git-metrics">';
    sourceHtml += `<div class="git-metric"><strong>Commits:</strong> ${m.commitCount || 0}</div>`;
    sourceHtml += `<div class="git-metric"><strong>Last modified:</strong> ${m.daysSinceModified || 0} days ago</div>`;
    sourceHtml += `<div class="git-metric"><strong>Authors:</strong> ${m.authorCount || 0}</div>`;
    if (m.authors && m.authors.length > 0) {
      sourceHtml += `<div class="git-metric"><strong>Contributors:</strong> ${m.authors.slice(0, 3).join(', ')}${m.authors.length > 3 ? '...' : ''}</div>`;
    }
    sourceHtml += '</div>';
  }

  // Add source code
  if (data.sourceCode) {
    sourceHtml += `<pre class="call-graph-source">${escapeHtml(data.sourceCode)}</pre>`;
  } else {
    const kindText = data.declarationKind || 'Declaration';
    sourceHtml += `<div class="call-graph-empty">${kindText}: ${data.declarationName}<br><br>(Source code not available)</div>`;
  }

  sourceCode.innerHTML = sourceHtml;

  // Add click handlers to all links (for navigation between functions)
  document.querySelectorAll('.call-graph-link').forEach(link => {
    link.addEventListener('click', () => {
      const module = link.getAttribute('data-module');
      const func = link.getAttribute('data-function');
      if (module && func) {
        // Call the PureScript showCallGraphPopup to navigate
        // This is imported from the PureScript module
        if (typeof window.showCallGraphPopup === 'function') {
          window.showCallGraphPopup(module)(func)();
        } else {
          console.warn("[CallGraphPopup] showCallGraphPopup not available on window");
        }
      }
    });
  });
};

// =============================================================================
// Show Error State
// =============================================================================

export const showPopupError_ = (errorMessage) => () => {
  const callersList = document.getElementById("call-graph-callers-list");
  const sourceCode = document.getElementById("call-graph-source-code");
  const calleesList = document.getElementById("call-graph-callees-list");

  if (callersList) {
    callersList.innerHTML = '<div class="call-graph-error">Error loading callers</div>';
  }
  if (sourceCode) {
    sourceCode.innerHTML = `<div class="call-graph-error">Error: ${escapeHtml(errorMessage)}</div>`;
  }
  if (calleesList) {
    calleesList.innerHTML = '<div class="call-graph-error">Error loading callees</div>';
  }
};

// =============================================================================
// Hide Popup
// =============================================================================

export const hideCallGraphPopup_ = () => {
  const popup = document.getElementById("call-graph-popup");
  if (popup) {
    popup.style.display = "none";
  }
};

// =============================================================================
// Utilities
// =============================================================================

// Escape HTML to prevent XSS
function escapeHtml(text) {
  if (!text) return '';
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

// =============================================================================
// Event Listeners (initialize once on page load)
// =============================================================================

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
