exports.highlightBlock_ = selector => async => {
  var block = document.getElementById("prism")
  Prism.highlightElement(block);
}

exports.highlightString_ = codetext => {
  const highlightedCode = Prism.highlight(codetext, Prism.languages.purescript, 'purescript');
  return highlightedCode
}