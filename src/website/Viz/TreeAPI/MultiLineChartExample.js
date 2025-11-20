// FFI for MultiLineChartExample

/**
 * Set innerHTML of an element
 * @param {string} html - The HTML content to set
 * @param {Element} element - The DOM element
 * @returns {void}
 */
export function setInnerHTML_(html) {
  return element => () => {
    element.innerHTML = html;
  };
}

/**
 * setTimeout wrapper
 * @param {Function} effect - The PureScript Effect to run
 * @param {number} delay - Delay in milliseconds
 * @returns {number} The timeout ID
 */
export function setTimeout_(effect) {
  return delay => () => {
    return setTimeout(() => effect(), delay);
  };
}

/**
 * clearTimeout wrapper
 * @param {number} timeoutId - The timeout ID to clear
 */
export function clearTimeout_(timeoutId) {
  return () => {
    clearTimeout(timeoutId);
  };
}
