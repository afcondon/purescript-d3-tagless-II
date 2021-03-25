exports.nullSelectionJS = null

// d3SelectAll_ :: Selector -> SelectionJS
exports.d3SelectAll_ = function(selector) {
  console.log(`d3SelectAll: ${selector}`);
  return null;
} 
// d3Append_ =    :: String -> SelectionJS -> SelectionJS
exports.d3Append_ = element => selection => {
  console.log(`d3Append: ${element}`);
  return null;
}
// d3Join_ =      :: String -> SelectionJS -> SelectionJS
exports.d3Join_ = element => selection => {
  console.log(`d3Join: ${element}`);
  return null;
}

// d3SetAttr_      :: String -> D3Attr -> SelectionJS -> Unit 
exports.d3SetAttr_ = name => value => selection => {
  console.log(`selection.attr(${name}, ${value})`);
  // selection.attr(name, value);
}
