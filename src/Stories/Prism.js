exports.highlightBlock_ = selector => async => {
  var block = document.getElementById("prism")
  Prism.highlightElement(block);
}