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
