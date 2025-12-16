// Generic element transformation based on bound data
//
// Uses native querySelectorAll for performance.
// Reads D3's __data__ property from each element.

// Transform circles: update cx/cy based on bound data
// transformer: (data) -> { cx, cy }
export function transformCircles_(containerSelector) {
  return function(transformer) {
    return function() {
      const selector = containerSelector + " circle";
      const elements = document.querySelectorAll(selector);

      elements.forEach(el => {
        const data = el.__data__;
        if (data !== undefined) {
          const pos = transformer(data);
          el.setAttribute("cx", pos.cx);
          el.setAttribute("cy", pos.cy);
        }
      });
    };
  };
}

// Transform lines: update x1/y1/x2/y2 based on bound data
// transformer: (data) -> { x1, y1, x2, y2 }
export function transformLines_(containerSelector) {
  return function(transformer) {
    return function() {
      const selector = containerSelector + " line";
      const elements = document.querySelectorAll(selector);

      elements.forEach(el => {
        const data = el.__data__;
        if (data !== undefined) {
          const pos = transformer(data);
          el.setAttribute("x1", pos.x1);
          el.setAttribute("y1", pos.y1);
          el.setAttribute("x2", pos.x2);
          el.setAttribute("y2", pos.y2);
        }
      });
    };
  };
}

// Transform paths: update d attribute based on bound data
// transformer: (data) -> pathString
export function transformPaths_(containerSelector) {
  return function(transformer) {
    return function() {
      const selector = containerSelector + " path";
      const elements = document.querySelectorAll(selector);

      elements.forEach(el => {
        const data = el.__data__;
        if (data !== undefined) {
          const d = transformer(data);
          el.setAttribute("d", d);
        }
      });
    };
  };
}

// Clear all child elements from a container
export function clearContainer_(selector) {
  return function() {
    const container = document.querySelector(selector);
    if (container) {
      container.innerHTML = "";
    }
  };
}

// Remove an element from the DOM entirely
export function removeElement_(selector) {
  return function() {
    const element = document.querySelector(selector);
    if (element) {
      element.remove();
    }
  };
}
