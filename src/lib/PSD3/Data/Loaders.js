/**
 * Data loading FFI - wraps D3's data loading functions
 */

// Load D3's data loading functions
// These are typically available as d3.csv, d3.json, etc.
// If using a modular D3 build, import them explicitly

export function loadCSVImpl(url) {
  return function(onError, onSuccess) {
    console.log('[PSD3.Data.Loaders] Loading CSV from:', url);

    // Create the promise
    const loadPromise = (async () => {
      // Use native d3.csv if available, otherwise use fetch + d3.csvParse
      if (typeof d3 !== 'undefined' && d3.csv) {
        console.log('[PSD3.Data.Loaders] Using d3.csv');
        const data = await d3.csv(url);
        console.log('[PSD3.Data.Loaders] Loaded', data.length, 'rows via d3.csv');
        return data;
      } else {
        console.log('[PSD3.Data.Loaders] Using fetch fallback');
        // Fallback: manual CSV parsing (basic implementation)
        const response = await fetch(url);
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const text = await response.text();

        // Simple CSV parser
        const lines = text.trim().split('\n');
        const headers = lines[0].split(',').map(h => h.replace(/^"|"$/g, '').trim());

        const data = lines.slice(1).map(line => {
          const values = line.match(/(".*?"|[^,]+)(?=\s*,|\s*$)/g) || [];
          const obj = {};
          headers.forEach((header, i) => {
            obj[header] = values[i] ? values[i].replace(/^"|"$/g, '').trim() : '';
          });
          return obj;
        });
        console.log('[PSD3.Data.Loaders] Loaded', data.length, 'rows via fetch');
        return data;
      }
    })();

    // Convert to callback style for EffectFnAff
    loadPromise
      .then(data => {
        console.log('[PSD3.Data.Loaders] Calling onSuccess with', data.length, 'rows');
        onSuccess(data);
      })
      .catch(error => {
        console.error('[PSD3.Data.Loaders] Error loading CSV:', error);
        onError(error);
      });

    // Return canceler (EffectFnAff expects this)
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
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
