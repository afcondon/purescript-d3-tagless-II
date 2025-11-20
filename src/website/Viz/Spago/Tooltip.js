// Tooltip module for Code Explorer
// Shows node information including git metrics on hover
// Uses PSD3v2.Tooltip library for show/hide, this module provides formatting

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
