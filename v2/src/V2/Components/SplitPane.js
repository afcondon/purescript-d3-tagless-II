// FFI for SplitPane component

export function highlightElement(element) {
  return function() {
    // Check if Prism is available and highlight the element
    if (typeof Prism !== 'undefined' && Prism.highlightElement) {
      try {
        Prism.highlightElement(element);
      } catch (e) {
        console.error('Prism highlighting error:', e);
      }
    }
  };
}
