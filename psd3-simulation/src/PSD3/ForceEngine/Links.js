// FFI for fast O(1) data structures in hot paths
// These replace O(n) array scans with JavaScript Set/Map

// =============================================================================
// IntSet - O(1) membership testing
// =============================================================================

// Build a Set from array of integers - O(n)
export const buildIntSet = (arr) => new Set(arr);

// Check membership - O(1)
export const intSetMember = (key) => (set) => set.has(key);

// =============================================================================
// IntMap - O(1) key-value lookup
// =============================================================================

// Build a Map from array of {key, value} pairs - O(n)
export const buildIntMap_ = (pairs) => {
  const map = new Map();
  for (let i = 0; i < pairs.length; i++) {
    map.set(pairs[i].key, pairs[i].value);
  }
  return map;
};

// Lookup by key - O(1), returns undefined if not found
export const intMapLookup_ = (key) => (map) => map.get(key);

// Check if value is null/undefined
export const isNull = (x) => x == null;

// =============================================================================
// StringSet - O(1) membership testing for string keys
// =============================================================================

// Build a Set from array of strings - O(n)
export const buildStringSet = (arr) => new Set(arr);

// Check membership - O(1)
export const stringSetMember = (key) => (set) => set.has(key);
