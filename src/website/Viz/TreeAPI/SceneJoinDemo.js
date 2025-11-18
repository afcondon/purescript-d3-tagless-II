// FFI for SceneJoinDemo

export function clearInnerHTML(selector) {
  return function() {
    const element = document.querySelector(selector);
    if (element) {
      element.innerHTML = '';
    }
  };
}
