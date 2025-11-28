// FFI for MultiLineChartExample

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
