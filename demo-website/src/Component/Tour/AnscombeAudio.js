// Global state for managing the loop
let currentIntervalId = null;

export const setIntervalImpl = function(action) {
  return function(milliseconds) {
    return function() {
      // Clear any existing interval first
      if (currentIntervalId !== null) {
        clearInterval(currentIntervalId);
      }

      // Set up new interval
      currentIntervalId = setInterval(function() {
        action();
      }, milliseconds);
    };
  };
};

export const clearIntervalImpl = function(unit) {
  return function() {
    if (currentIntervalId !== null) {
      clearInterval(currentIntervalId);
      currentIntervalId = null;
    }
  };
};
