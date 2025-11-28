// Force Control Panel FFI
// V2 only - works with SimulationManager's custom force loop

// Get list of active force names from V2 registry
export function getForceNames_() {
  const forces = window._psd3_forces_v2 || {};
  return Object.keys(forces);
}

// Update a force parameter
export function updateForceParam_(forceName) {
  return function(paramName) {
    return function(value) {
      return function() {
        const forces = window._psd3_forces_v2 || {};
        const force = forces[forceName];

        if (!force) {
          console.warn(`[V2] Force not found: ${forceName}`);
          return;
        }

        if (typeof force[paramName] === 'function') {
          force[paramName](value);
          console.log(`[V2] Set ${forceName}.${paramName} = ${value}`);

          // Bump alpha and restart
          const ref = window._psd3_simulation_v2;
          if (ref && ref.value) {
            ref.value.alpha = Math.max(ref.value.alpha, 0.3);
          }
          if (window._psd3_restart_v2) {
            window._psd3_restart_v2();
          }
        } else {
          console.warn(`[V2] Parameter not found: ${forceName}.${paramName}`);
        }
      };
    };
  };
}

// Toggle a force on/off
export function toggleForce_(forceName) {
  return function(enabled) {
    return function() {
      const forces = window._psd3_forces_v2 || {};
      window._psd3_disabled_forces_v2 = window._psd3_disabled_forces_v2 || {};

      if (enabled) {
        // Re-enable: restore from disabled storage
        if (window._psd3_disabled_forces_v2[forceName]) {
          forces[forceName] = window._psd3_disabled_forces_v2[forceName];
          delete window._psd3_disabled_forces_v2[forceName];
          console.log(`[V2] Enabled force: ${forceName}`);
        }
      } else {
        // Disable: move to disabled storage
        if (forces[forceName]) {
          window._psd3_disabled_forces_v2[forceName] = forces[forceName];
          delete forces[forceName];
          console.log(`[V2] Disabled force: ${forceName}`);
        }
      }

      // Bump alpha and restart
      const ref = window._psd3_simulation_v2;
      if (ref && ref.value) {
        ref.value.alpha = 0.5;
      }
      if (window._psd3_restart_v2) {
        window._psd3_restart_v2();
      }
    };
  };
}

// Start simulation
export function startSimulation_() {
  const ref = window._psd3_simulation_v2;
  if (!ref || !ref.value) {
    console.warn('[V2] No simulation found');
    return;
  }

  ref.value.alpha = 0.3;
  console.log('[V2] Start: alpha=0.3');

  if (window._psd3_restart_v2) {
    window._psd3_restart_v2();
  }
}

// Stop simulation
export function stopSimulation_() {
  const ref = window._psd3_simulation_v2;
  if (!ref || !ref.value) {
    console.warn('[V2] No simulation found');
    return;
  }

  ref.value.running = false;
  console.log('[V2] Stop: running=false');
}

// Reheat simulation
export function reheatSimulation_() {
  const ref = window._psd3_simulation_v2;
  if (!ref || !ref.value) {
    console.warn('[V2] No simulation found');
    return;
  }

  ref.value.alpha = 1.0;
  console.log('[V2] Reheat: alpha=1.0');

  if (window._psd3_restart_v2) {
    window._psd3_restart_v2();
  }
}

// Get current parameter value
export function getForceParam_(forceName) {
  return function(paramName) {
    return function() {
      const forces = window._psd3_forces_v2 || {};
      const force = forces[forceName];

      if (!force) return null;

      if (typeof force[paramName] === 'function') {
        const val = force[paramName]();
        if (typeof val === 'function') {
          // D3 wraps constant values in constant() functions
          // Call with dummy arg to extract the actual value
          // But real per-node accessors will fail, so catch errors
          try {
            const extracted = val({});
            if (typeof extracted === 'number') {
              return extracted;
            }
          } catch (e) {
            // Per-node accessor function, can't extract constant
          }
          return null;
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

// Get current force settings as formatted string
export function getForceSettings_() {
  const forces = window._psd3_forces_v2 || {};
  const forceNames = Object.keys(forces);

  if (forceNames.length === 0) {
    return "No forces registered";
  }

  const paramsByType = {
    charge: ['strength', 'theta', 'distanceMin', 'distanceMax'],
    collide: ['strength', 'iterations'],
    center: ['strength', 'x', 'y'],
    link: ['distance', 'strength', 'iterations'],
    radial: ['strength', 'radius'],
    position: ['strength']
  };

  const getForceType = (name) => {
    if (name.includes('charge') || name.includes('Charge')) return 'charge';
    if (name.includes('collid') || name.includes('Collid') || name.includes('collision')) return 'collide';
    if (name.includes('center')) return 'center';
    if (name.includes('link')) return 'link';
    if (name.includes('Orbit') || name.includes('radial')) return 'radial';
    if (name.includes('cluster') || name.includes('Cluster')) return 'position';
    return 'unknown';
  };

  let output = '-- Force Settings (V2) --\n';

  for (const name of forceNames) {
    const force = forces[name];
    output += `\n${name}:\n`;
    const forceType = getForceType(name);
    const params = paramsByType[forceType] || [];

    for (const param of params) {
      if (typeof force[param] === 'function') {
        const val = force[param]();
        if (typeof val === 'number') {
          output += `  ${param}: ${val}\n`;
        } else if (typeof val === 'function') {
          output += `  ${param}: <function>\n`;
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
      console.log('[V2] Settings copied to clipboard');
    }).catch(err => {
      console.error('[V2] Failed to copy:', err);
    });
  };
}
