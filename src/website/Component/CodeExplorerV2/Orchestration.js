// FFI for Orchestration.purs

// Get the simulation object stored in window by the PSD3 FFI
// The simulation is stored in window._psd3_simulation by initSimulation_ in PSD3/Internal/FFI.js
export function getSimulationFromWindow_() {
  if (window._psd3_simulation) {
    return window._psd3_simulation;
  }
  throw new Error("Simulation not found in window._psd3_simulation");
}
