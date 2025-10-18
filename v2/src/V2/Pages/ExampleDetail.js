// FFI for ExampleDetail component

export function highlightElement(element) {
  return function() {
    if (window.Prism) {
      window.Prism.highlightElement(element);
    }
  };
}
