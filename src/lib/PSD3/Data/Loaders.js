/**
 * Data loading FFI - wraps D3's data loading functions
 */

// Load D3's data loading functions
// These are typically available as d3.csv, d3.json, etc.
// If using a modular D3 build, import them explicitly

export function loadCSVImpl(url) {
  return async function() {
    // Use native d3.csv if available, otherwise use fetch + d3.csvParse
    if (typeof d3 !== 'undefined' && d3.csv) {
      return await d3.csv(url);
    } else {
      // Fallback: manual CSV parsing (basic implementation)
      const response = await fetch(url);
      const text = await response.text();

      // Simple CSV parser
      const lines = text.trim().split('\n');
      const headers = lines[0].split(',').map(h => h.replace(/^"|"$/g, '').trim());

      return lines.slice(1).map(line => {
        const values = line.match(/(".*?"|[^,]+)(?=\s*,|\s*$)/g) || [];
        const obj = {};
        headers.forEach((header, i) => {
          obj[header] = values[i] ? values[i].replace(/^"|"$/g, '').trim() : '';
        });
        return obj;
      });
    }
  };
}

export function loadJSONImpl(url) {
  return async function() {
    if (typeof d3 !== 'undefined' && d3.json) {
      return await d3.json(url);
    } else {
      const response = await fetch(url);
      return await response.json();
    }
  };
}
