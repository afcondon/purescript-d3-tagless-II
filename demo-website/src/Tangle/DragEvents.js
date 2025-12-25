// FFI for document-level mouse event listeners (for drag tracking)
// Used by Tangle adjust controls for drag-to-adjust behavior

export const documentMouseMoveImpl = function(callback) {
  return function() {
    var handler = function(evt) {
      callback(evt.clientX)();
    };
    // Use capture: true to catch event before D3 zoom/drag can intercept it
    document.addEventListener('mousemove', handler, { capture: true });
    return function() {
      document.removeEventListener('mousemove', handler, { capture: true });
    };
  };
};

export const documentMouseUpImpl = function(callback) {
  return function() {
    var handler = function(_evt) {
      callback();
    };
    // Use capture: true to catch event before D3 zoom/drag can intercept it
    document.addEventListener('mouseup', handler, { capture: true });
    return function() {
      document.removeEventListener('mouseup', handler, { capture: true });
    };
  };
};

// Click-to-end drag: listens for any click on the document to end drag mode
// This avoids conflicts with D3 zoom/pan which captures mouseup
export const documentClickImpl = function(callback) {
  return function() {
    var handler = function(_evt) {
      callback();
    };
    // Small delay to avoid the initial click that started the drag
    setTimeout(function() {
      document.addEventListener('click', handler, { capture: true });
    }, 100);
    return function() {
      document.removeEventListener('click', handler, { capture: true });
    };
  };
};
