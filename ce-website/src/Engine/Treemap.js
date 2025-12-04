// FFI for Engine.Treemap

export const unsafeFloor = x => Math.floor(x);

export const clearElement = element => () => {
  while (element.firstChild) {
    element.removeChild(element.firstChild);
  }
};
