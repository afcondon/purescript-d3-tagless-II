export function highlightBlock_(selector) {
  return async => {
    var block = document.getElementById("prism")
    Prism.highlightElement(block);
  };
}

export function highlightString_(codetext) {
  const highlightedCode = Prism.highlight(codetext, Prism.languages.haskell, 'haskell');
  return highlightedCode
}

export function highlightAllLineNumbers_() {
  return () => {
    // Re-run Prism on all code blocks to trigger line-numbers plugin
    if (typeof Prism !== 'undefined' && Prism.highlightAll) {
      // Use setTimeout with a longer delay to ensure DOM is fully rendered by Halogen
      setTimeout(() => {
        // First, remove any existing line-numbers to avoid duplicates
        document.querySelectorAll('.line-numbers-rows').forEach(el => el.remove());

        // Then run Prism
        Prism.highlightAll();

        // Debug: log if line numbers were added
        const lineNumberElements = document.querySelectorAll('.line-numbers-rows');
        console.log('Line numbers elements found:', lineNumberElements.length);
      }, 100);
    }
  };
}