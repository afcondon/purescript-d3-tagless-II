// TreeBuilder2 FFI functions

export const clearContainer = (selector) => () => {
  const container = document.querySelector(selector);
  if (container) {
    container.innerHTML = '';
  }
};
