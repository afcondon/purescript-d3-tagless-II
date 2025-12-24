// FFI for Rendering module

export function clearContainer(selector) {
  return function() {
    const element = document.querySelector(selector);
    if (element) {
      element.innerHTML = '';
    }
  };
}
