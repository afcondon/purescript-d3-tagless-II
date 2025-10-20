// FFI for ExampleDetail component
// TODO wrap Prism.js elsewhere and lose this FFI file
export function highlightElement(element) {
  return function() {
    if (window.Prism) {
      window.Prism.highlightElement(element);
    }
  };
}
