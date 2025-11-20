// Git metrics module - loads and provides access to module metrics

let metricsData = null;

export function loadGitMetrics_() {
  // Fire and forget - load metrics asynchronously
  fetch('./data/module-metrics.json')
    .then(response => response.json())
    .then(data => {
      metricsData = data;
      // Expose globally for tooltip access
      window.__metricsData = data;
      console.log('Loaded git metrics for', Object.keys(data.modules).length, 'modules');
    })
    .catch(err => {
      console.warn('Failed to load git metrics:', err);
    });
}

export function getModuleMetric_(moduleName) {
  return function(metricType) {
    if (!metricsData || !metricsData.modules) return 0;

    // Try direct lookup first
    let module = metricsData.modules[moduleName];

    // If not found, try common prefixes
    if (!module) {
      // Module names in Code Explorer might have prefixes like "D3.Viz." or "PSD3."
      // Try stripping common prefixes
      const prefixes = ['D3.Viz.', 'PSD3.', 'Component.'];
      for (const prefix of prefixes) {
        if (moduleName.startsWith(prefix)) {
          module = metricsData.modules[moduleName.slice(prefix.length)];
          if (module) break;
        }
      }
    }

    // Still not found? Try matching by just the last part
    if (!module) {
      const parts = moduleName.split('.');
      const lastName = parts[parts.length - 1];
      // Search for any module ending with this name
      for (const key of Object.keys(metricsData.modules)) {
        if (key.endsWith('.' + lastName) || key === lastName) {
          module = metricsData.modules[key];
          break;
        }
      }
    }

    if (!module || !module.normalized) return 0;
    return module.normalized[metricType] || 0;
  };
}

// Check if metrics are loaded
export function metricsLoaded_() {
  return metricsData !== null;
}
