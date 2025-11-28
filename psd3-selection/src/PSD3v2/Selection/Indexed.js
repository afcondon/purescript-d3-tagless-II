// Query for a single element
export function querySelector_(selector) {
  return function(doc) {
    return function() {
      const element = doc.querySelector(selector);
      return element ? { constructor: {}, value0: element } : null;
    };
  };
}

// Query for all matching elements within an array of parent elements
export function querySelectorAll_(selector) {
  return function(parents) {
    return function() {
      const results = [];
      for (const parent of parents) {
        const elements = parent.querySelectorAll(selector);
        results.push(...elements);
      }
      return results;
    };
  };
}

// Create a new element
export function createElement_(tagName) {
  return function(doc) {
    return function() {
      return doc.createElement(tagName);
    };
  };
}

// Set an attribute on an element
export function setAttribute_(name) {
  return function(value) {
    return function(element) {
      return function() {
        element.setAttribute(name, value);
      };
    };
  };
}

// Append a child element to a parent
export function appendChild_(child) {
  return function(parent) {
    return function() {
      parent.appendChild(child);
    };
  };
}

// Remove an element from the DOM
export function removeElement_(element) {
  return function() {
    if (element.parentNode) {
      element.parentNode.removeChild(element);
    }
  };
}

// Get data bound to an element (D3-style __data__ property)
export function getElementData_(element) {
  return function() {
    const data = element.__data__;
    return data !== undefined ? { constructor: {}, value0: data } : null;
  };
}

// Set data on an element (D3-style __data__ property)
export function setElementData_(datum) {
  return function(element) {
    return function() {
      element.__data__ = datum;
    };
  };
}
