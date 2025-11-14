// FFI functions for PSD3v2.Interpreter.D3v2

/**
 * Set an attribute on an element synchronously (no Effect wrapper)
 * Used in D3 tick callbacks which must be synchronous
 * @param {string} name - The attribute name
 * @param {string} value - The attribute value
 * @param {Element} element - The DOM element
 * @returns {void}
 */
export const setAttributeSync_ = name => value => element => {
  if (name === "textContent") {
    element.textContent = value;
  } else {
    element.setAttribute(name, value);
  }
};
