// FFI for ForceConfigPOC

export function getSimulationFromWindow() {
  return () => {
    if (typeof window._psd3_simulation !== 'undefined') {
      return window._psd3_simulation;
    }
    return null;
  };
}

export function logMessage(msg) {
  return () => {
    console.log('[ForceConfigPOC]', msg);
  };
}
