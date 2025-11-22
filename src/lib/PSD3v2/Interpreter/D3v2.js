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

// Debug: check if two objects are the same reference
export const debugSameRef_ = a => b => a === b;

// Debug: log object identity check - compare captured data with element.__data__
export const debugTickIdentity_ = label => datumArray => {
  if (datumArray.length === 0) return;

  // Only log once per label to avoid spam
  if (!window._tickDebugLogged) window._tickDebugLogged = {};
  if (window._tickDebugLogged[label]) return;
  window._tickDebugLogged[label] = true;

  console.log(`[${label}] Tick identity check:`);
  console.log(`  datumArray length: ${datumArray.length}`);

  // For links, check if source/target are objects with positions
  const first = datumArray[0];
  if (first && first.source) {
    console.log(`  First link source type: ${typeof first.source}`);
    console.log(`  First link source.x: ${first.source.x}`);
    console.log(`  First link source is object: ${typeof first.source === 'object'}`);
  } else if (first && first.x !== undefined) {
    console.log(`  First node x: ${first.x}, y: ${first.y}`);
  }
};

// Read __data__ from a DOM element (D3's bound data)
// Returns the datum or undefined if not bound
export const getElementData_ = element => element.__data__;
