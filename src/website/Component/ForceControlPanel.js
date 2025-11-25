// Force Control Panel FFI

// Get the current simulation
export function getSimulation_() {
  return window._psd3_simulation || null;
}

// Get list of active force names
export function getForceNames_() {
  const sim = window._psd3_simulation;
  if (!sim) {
    return [];
  }

  // D3 simulations store forces in an internal Map
  // We need to access the force names that have been registered
  // Try to get force names from the simulation's internal state
  const activeForces = [];

  // D3 stores forces internally - we can probe for them
  // Check both the common names and try to access the internal map
  const knownForces = [
    'centerStrong', 'center',
    'charge', 'charge2', 'chargePack', 'chargetree',
    'collision', 'collide2', 'collidePack',
    'link', 'links',  // D3 force link can be named either
    'clusterX_M', 'clusterY_M',
    'packageOrbit', 'moduleOrbit'
  ];

  for (const name of knownForces) {
    if (sim.force(name)) {
      activeForces.push(name);
    }
  }

  return activeForces;
}

// Update a force parameter
export function updateForceParam_(forceName) {
  return function(paramName) {
    return function(value) {
      return function() {
        const sim = window._psd3_simulation;
        if (!sim) {
          console.warn('No simulation found');
          return;
        }

        const force = sim.force(forceName);
        if (!force) {
          console.warn(`Force not found: ${forceName}`);
          return;
        }

        if (typeof force[paramName] === 'function') {
          force[paramName](value);
          console.log(`Set ${forceName}.${paramName} = ${value}`);
          sim.alpha(0.3).restart();
        } else {
          console.warn(`Parameter not found: ${forceName}.${paramName}`);
        }
      };
    };
  };
}

// Toggle a force on/off
export function toggleForce_(forceName) {
  return function(enabled) {
    return function() {
      const sim = window._psd3_simulation;
      if (!sim) return;

      window._psd3_disabled_forces = window._psd3_disabled_forces || {};

      if (enabled) {
        // Re-enable
        if (window._psd3_disabled_forces[forceName]) {
          sim.force(forceName, window._psd3_disabled_forces[forceName]);
          delete window._psd3_disabled_forces[forceName];
          console.log(`Enabled force: ${forceName}`);
        }
      } else {
        // Disable
        const force = sim.force(forceName);
        if (force) {
          window._psd3_disabled_forces[forceName] = force;
          sim.force(forceName, null);
          console.log(`Disabled force: ${forceName}`);
        }
      }

      sim.alpha(0.3).restart();
    };
  };
}

// Start simulation
export function startSimulation_() {
  const sim = window._psd3_simulation;
  if (sim) {
    sim.alpha(0.3).restart();
  }
}

// Stop simulation
export function stopSimulation_() {
  const sim = window._psd3_simulation;
  if (sim) {
    sim.stop();
  }
}

// Reheat simulation
export function reheatSimulation_() {
  const sim = window._psd3_simulation;
  if (sim) {
    sim.alpha(1).restart();
  }
}

// Get current parameter value
export function getForceParam_(forceName) {
  return function(paramName) {
    return function() {
      const sim = window._psd3_simulation;
      if (!sim) return null;

      const force = sim.force(forceName);
      if (!force) return null;

      if (typeof force[paramName] === 'function') {
        const val = force[paramName]();
        // Handle functions that return functions (like radius)
        if (typeof val === 'function') {
          return null; // Can't serialize a function
        }
        return val;
      }
      return null;
    };
  };
}

// Parse float from string
export function unsafeParseFloat(str) {
  return parseFloat(str);
}

export function isNaN(n) {
  return Number.isNaN(n);
}

export function round(n) {
  return Math.round(n);
}

// Delay function for Aff
export function effectDelay(ms) {
  return function() {
    return new Promise(resolve => setTimeout(resolve, ms));
  };
}

// Get current force settings as formatted string
export function getForceSettings_() {
  const sim = window._psd3_simulation;
  if (!sim) return "No simulation found";

  const knownForces = [
    'centerStrong', 'center',
    'charge', 'charge2', 'chargePack', 'chargetree',
    'collision', 'collide2', 'collidePack',
    'link', 'links',
    'clusterX_M', 'clusterY_M',
    'packageOrbit', 'moduleOrbit'
  ];

  const paramsByType = {
    charge: ['strength', 'theta', 'distanceMin', 'distanceMax'],
    collide: ['radius', 'strength', 'iterations'],
    center: ['strength', 'x', 'y'],
    link: ['distance', 'iterations'],
    radial: ['strength', 'radius'],
    position: ['strength']
  };

  const getForceType = (name) => {
    if (name.includes('charge')) return 'charge';
    if (name.includes('collid')) return 'collide';
    if (name.includes('center')) return 'center';
    if (name.includes('link')) return 'link';
    if (name.includes('Orbit')) return 'radial';
    if (name.includes('cluster')) return 'position';
    return 'unknown';
  };

  let output = '-- Force Settings --\n';

  for (const name of knownForces) {
    const force = sim.force(name);
    if (force) {
      output += `\n${name}:\n`;
      const forceType = getForceType(name);
      const params = paramsByType[forceType] || [];

      for (const param of params) {
        if (typeof force[param] === 'function') {
          const val = force[param]();
          if (typeof val === 'number') {
            output += `  ${param}: ${val}\n`;
          } else if (typeof val === 'function') {
            // For function params, try to get slider value from DOM
            // Find the force item by name, then find the slider for this param
            const forceItems = document.querySelectorAll('.force-item');
            let sliderValue = null;

            for (const item of forceItems) {
              const nameEl = item.querySelector('.force-name');
              if (nameEl && nameEl.textContent === name) {
                // Found the force item, now find the slider for this param
                const sliders = item.querySelectorAll('.slider-row');
                for (const row of sliders) {
                  const label = row.querySelector('.slider-label');
                  if (label) {
                    // Match label to param (e.g., "Strength" -> "strength", "Dist Min" -> "distanceMin")
                    const labelText = label.textContent.toLowerCase().replace(/\s+/g, '');
                    const paramLower = param.toLowerCase();
                    if (labelText.includes(paramLower) || paramLower.includes(labelText)) {
                      const input = row.querySelector('input[type="range"]');
                      if (input) {
                        sliderValue = input.value;
                        break;
                      }
                    }
                  }
                }
                break;
              }
            }

            if (sliderValue !== null) {
              output += `  ${param}: <function> (slider: ${sliderValue})\n`;
            } else {
              output += `  ${param}: <function>\n`;
            }
          }
        }
      }
    }
  }

  return output;
}

// Copy text to clipboard
export function copyToClipboard_(text) {
  return function() {
    navigator.clipboard.writeText(text).then(() => {
      console.log('Settings copied to clipboard');
    }).catch(err => {
      console.error('Failed to copy:', err);
    });
  };
}
