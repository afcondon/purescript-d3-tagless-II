// FFI for exposing functions to window for console testing
export const exposeToWindow = name => value => () => {
  window[name] = value;
};
