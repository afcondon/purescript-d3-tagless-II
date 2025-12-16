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

// ============================================================================
// JSON stringify for key-based joins
// ============================================================================

// Convert any value to JSON string for use as join key
// Used when types are erased and we can't rely on Eq instances
export function jsonStringify_(value) {
  return JSON.stringify(value);
}

// ============================================================================
// DOM-to-data synchronization
// ============================================================================

// Sync DOM transform positions back to __data__.x and __data__.y
// This reads the current transform attribute and updates the bound data
// Essential for transitioning from CSS animations to force simulation
export function syncDOMToData_(selector) {
  return function() {
    const elements = document.querySelectorAll(selector);
    elements.forEach(function(el) {
      const d = el.__data__;
      if (!d) return;

      const transform = el.getAttribute('transform');
      if (!transform) return;

      // Parse translate(x, y) from transform
      const match = transform.match(/translate\(\s*([^,\s]+)\s*,\s*([^)\s]+)\s*\)/);
      if (match) {
        d.x = parseFloat(match[1]);
        d.y = parseFloat(match[2]);
      }
    });
  };
}
