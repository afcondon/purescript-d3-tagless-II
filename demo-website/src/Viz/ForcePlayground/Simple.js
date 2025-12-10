// FFI for Simple force playground

// Schedule a callback after the browser has had time to render
// Uses requestAnimationFrame + setTimeout to ensure DOM is ready
export function scheduleAfterRender(callback) {
  return function() {
    requestAnimationFrame(function() {
      setTimeout(function() {
        callback();
      }, 50);
    });
  };
}
