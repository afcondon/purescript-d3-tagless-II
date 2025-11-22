// ============================================================================
// FFI for D3-style data binding
// ============================================================================
// All other DOM operations now use purescript-web-dom library functions
// These are the only custom FFI functions needed for D3-style __data__ binding

// Get data bound to an element (D3-style __data__ property)
// Returns null/undefined which PureScript Nullable handles properly
export function getElementData_(element) {
  return function() {
    return element.__data__;  // Returns undefined if not set - Nullable handles this
  };
}

// Set data on an element (D3-style __data__ property)
export function setElementData_(datum) {
  return function(element) {
    return function() {
      element.__data__ = datum;
    };
  };
}

// Set textContent property on an element
export function setTextContent_(text) {
  return function(element) {
    return function() {
      element.textContent = text;
    };
  };
}

// Extract D3 simulation handle from simulation state
// D3SimulationState_ is a newtype: SimState_ { handle_: ..., forceLibrary: ..., ... }
// Returns null if not initialized
export function getSimulationHandle_(simState) {
  // simState is SimState_ record, we need to unwrap and get handle_
  // In JavaScript, newtypes are transparent, so simState IS the record
  return simState.handle_ || null;
}

// ============================================================================
// MouseEvent accessors (candidates for purescript-web-uievents PR)
// ============================================================================

// Get offsetX from MouseEvent (position relative to target element)
export function offsetX(event) {
  return event.offsetX;
}

// Get offsetY from MouseEvent (position relative to target element)
export function offsetY(event) {
  return event.offsetY;
}

// ============================================================================
// Container clearing
// ============================================================================

// Clear all children from an element selected by CSS selector
export function clearElement_(selector) {
  return function() {
    const el = document.querySelector(selector);
    if (el) {
      el.innerHTML = '';
    }
  };
}
