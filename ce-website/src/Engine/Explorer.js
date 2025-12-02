// FFI for Explorer module

// Clear all children from an element
export function clearElement(element) {
  return function() {
    while (element.firstChild) {
      element.removeChild(element.firstChild);
    }
  };
}
