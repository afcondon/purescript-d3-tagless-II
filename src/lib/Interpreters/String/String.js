// String Interpreter - generates D3 JavaScript code from PureScript DSL

// Helper to format element names for D3
function formatElement(element) {
  // Remove quotes if present and return as quoted string
  const el = element.toString().replace(/"/g, '');
  return `"${el.toLowerCase()}"`;
}

// Helper to format selector
function formatSelector(selector) {
  const s = selector.toString();
  // If it's already quoted, return as-is, otherwise add quotes
  return s.startsWith('"') || s.startsWith("'") ? s : `"${s}"`;
}

// Helper to format attribute values
function formatValue(value) {
  // If it looks like a function, return as-is
  if (typeof value === 'function' || value.toString().includes('=>') || value.toString().includes('function')) {
    return value.toString();
  }
  // If it's a number, return as-is
  if (!isNaN(value) && value !== '') {
    return value;
  }
  // If it's already quoted, return as-is
  const str = value.toString();
  if (str.startsWith('"') || str.startsWith("'")) {
    return str;
  }
  // Otherwise quote it
  return `"${str}"`;
}

export function showSelectAllInDOM_(selector) {
  return `d3.select(${formatSelector(selector)})`
}

export function showSelectAll_(selector) {
  return selection => {
    return `${selection}.selectAll(${formatSelector(selector)})`
  }
}

export function showEnterAndAppend_(element) {
  return selection => {
    return `${selection}.enter().append(${formatElement(element)})`
  }
}

export function showExit_(selection) {
  return `${selection}.exit()`
}

export function showAddTransition_(selection) {
  return transition => {
    let result = `${selection}.transition()`;
    if (transition.duration && transition.duration != 0) {
      result += `\n  .duration(${transition.duration})`;
    }
    if (transition.delay && transition.delay != 0) {
      result += `\n  .delay(${transition.delay})`;
    }
    if (transition.name && transition.name !== '') {
      result = `${selection}.transition("${transition.name}")`;
    }
    return result;
  }
}

export function showRemoveSelection_(selection) {
  return `${selection}.remove()`
}

export function showAppend_(element) {
  return selection => {
    return `${selection}.append(${formatElement(element)})`
  }
}

export function showKeyFunction_(data) {
  return keyFunction => selection => {
    const dataStr = Array.isArray(data) ? `[/* ${data.length} items */]` : data.toString();
    return `${selection}.data(${dataStr}, ${keyFunction})`
  }
}

export function showData_(data) {
  return selection => {
    const dataStr = Array.isArray(data) ? `[/* ${data.length} items */]` : data.toString();
    return `${selection}.data(${dataStr})`
  }
}

export function showSetAttr_(name) {
  return value => selection => {
    return `${selection}.attr("${name}", ${formatValue(value)})`
  }
}

export function showSetText_(value) {
  return selection => {
    return `${selection}.text(${formatValue(value)})`
  }
}

export function showSetHTML_(value) {
  return selection => {
    return `${selection}.html(${formatValue(value)})`
  }
}

export function showSetProperty_(value) {
  return selection => {
    return `${selection}.property(${formatValue(value)})`
  }
}

export function showSetOrdering_(ordering) {
  return selection => {
    return `${selection}.${ordering}()`
  }
}
