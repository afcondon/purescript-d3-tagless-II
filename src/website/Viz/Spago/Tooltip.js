// Tooltip module for Code Explorer
// Shows node information including git metrics on hover

let tooltipElement = null;

// Ensure tooltip element exists
function ensureTooltip() {
  if (!tooltipElement) {
    tooltipElement = document.createElement('div');
    tooltipElement.className = 'node-tooltip';
    tooltipElement.style.display = 'none';
    document.body.appendChild(tooltipElement);
  }
  return tooltipElement;
}

// Show tooltip with content at position
export function showTooltip_(content) {
  return function(x) {
    return function(y) {
      return function() {
        const tooltip = ensureTooltip();
        tooltip.innerHTML = content;
        tooltip.style.display = 'block';

        // Position tooltip near cursor but offset to avoid overlap
        const offsetX = 15;
        const offsetY = 10;

        // Get viewport dimensions to keep tooltip on screen
        const viewportWidth = window.innerWidth;
        const viewportHeight = window.innerHeight;

        // Calculate position
        let posX = x + offsetX;
        let posY = y + offsetY;

        // Adjust if tooltip would go off screen
        const tooltipRect = tooltip.getBoundingClientRect();
        if (posX + tooltipRect.width > viewportWidth) {
          posX = x - tooltipRect.width - offsetX;
        }
        if (posY + tooltipRect.height > viewportHeight) {
          posY = y - tooltipRect.height - offsetY;
        }

        tooltip.style.left = posX + 'px';
        tooltip.style.top = posY + 'px';
      };
    };
  };
}

// Hide tooltip
export function hideTooltip_() {
  const tooltip = ensureTooltip();
  tooltip.style.display = 'none';
}

// Format git metrics for display
export function formatMetricsContent_(node) {
  return function(metricsData) {
    const name = node.name || 'Unknown';
    const nodeType = node.nodetype ? (node.nodetype.tag === 'IsPackage' ? 'Package' : 'Module') : 'Node';
    const cluster = node.containerName || node.cluster || '';

    let html = `<div class="tooltip-header">${name}</div>`;
    html += `<div class="tooltip-type">${nodeType}${cluster ? ' in ' + cluster : ''}</div>`;

    // Add git metrics if available
    if (metricsData && metricsData.modules) {
      // Try to find the module in metrics data
      let moduleMetrics = metricsData.modules[name];

      // Try common prefixes if not found
      if (!moduleMetrics) {
        const prefixes = ['D3.Viz.', 'PSD3.', 'Component.'];
        for (const prefix of prefixes) {
          if (name.startsWith(prefix)) {
            moduleMetrics = metricsData.modules[name.slice(prefix.length)];
            if (moduleMetrics) break;
          }
        }
      }

      if (moduleMetrics) {
        html += '<div class="tooltip-metrics">';
        html += '<div class="tooltip-metrics-title">Git Metrics</div>';

        if (moduleMetrics.commitCount !== undefined) {
          html += `<div class="tooltip-metric"><span class="metric-label">Commits:</span> <span class="metric-value">${moduleMetrics.commitCount}</span></div>`;
        }
        if (moduleMetrics.daysSinceModified !== undefined) {
          html += `<div class="tooltip-metric"><span class="metric-label">Last modified:</span> <span class="metric-value">${moduleMetrics.daysSinceModified} days ago</span></div>`;
        }
        if (moduleMetrics.ageInDays !== undefined) {
          html += `<div class="tooltip-metric"><span class="metric-label">Age:</span> <span class="metric-value">${moduleMetrics.ageInDays} days</span></div>`;
        }
        if (moduleMetrics.authorCount !== undefined) {
          html += `<div class="tooltip-metric"><span class="metric-label">Authors:</span> <span class="metric-value">${moduleMetrics.authorCount}</span></div>`;
        }
        if (moduleMetrics.linesChanged !== undefined) {
          html += `<div class="tooltip-metric"><span class="metric-label">Churn:</span> <span class="metric-value">${moduleMetrics.linesChanged} lines</span></div>`;
        }
        if (moduleMetrics.lineCount !== undefined) {
          html += `<div class="tooltip-metric"><span class="metric-label">Size:</span> <span class="metric-value">${moduleMetrics.lineCount} lines</span></div>`;
        }

        html += '</div>';
      }
    }

    return html;
  };
}

// Get the current metrics data (loaded by GitMetrics module)
export function getMetricsData_() {
  // Access the metricsData from GitMetrics module
  // This relies on the module being loaded first
  return window.__metricsData || null;
}
