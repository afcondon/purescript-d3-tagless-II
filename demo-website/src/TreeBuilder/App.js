// TreeBuilder App FFI

export const clearPreviewContainer = (selector) => () => {
  const container = document.querySelector(selector);
  if (container) {
    container.innerHTML = '';
  }
};
