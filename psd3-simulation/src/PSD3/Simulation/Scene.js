// Scene FFI
//
// In-place mutation for rule application.
// Preserves object identity for D3 data binding.

// Apply rules in place with first-match-wins semantics (CSS-like cascade)
// For each node, find the first matching rule and apply it via Object.assign
//
// Note: Mutates in place, not wrapped in Effect thunk.
// TODO: Consider proper ST-based approach for principled mutation tracking.
export function applyRulesInPlace_(rules) {
  return function(nodesRef) {
    const nodes = nodesRef.value;
    for (let i = 0; i < nodes.length; i++) {
      const node = nodes[i];
      for (let j = 0; j < rules.length; j++) {
        const rule = rules[j];
        if (rule.select(node)) {
          const updated = rule.apply(node);
          Object.assign(node, updated);
          break; // First-match-wins: stop after first matching rule
        }
      }
    }
  };
}
