export function highlightBlock_(selector) {
  return async => {
    var block = document.getElementById("prism")
    Prism.highlightElement(block);
  };
}

export function highlightString_(codetext) {
  const highlightedCode = Prism.highlight(codetext, Prism.languages.purescript, 'purescript');
  return highlightedCode
}