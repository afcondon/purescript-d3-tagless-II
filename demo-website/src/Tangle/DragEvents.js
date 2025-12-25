// FFI for document-level mouse event listeners (for drag tracking)
// Used by Tangle adjust controls for drag-to-adjust behavior

export const documentMouseMoveImpl = function(callback) {
  return function() {
    var handler = function(evt) {
      callback(evt.clientX)();
    };
    document.addEventListener('mousemove', handler);
    return function() {
      document.removeEventListener('mousemove', handler);
    };
  };
};

export const documentMouseUpImpl = function(callback) {
  return function() {
    var handler = function(_evt) {
      callback();
    };
    document.addEventListener('mouseup', handler);
    return function() {
      document.removeEventListener('mouseup', handler);
    };
  };
};
