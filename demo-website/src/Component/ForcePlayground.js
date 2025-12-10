// FFI for ForcePlayground component

// Clear container by removing all children
export const clearContainer = selector => () => {
  const container = document.querySelector(selector);
  if (container) {
    container.innerHTML = '';
  }
};
