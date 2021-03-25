exports.nullSelectionJS = null

// d3SelectAll_ :: Selector -> SelectionJS
exports.d3SelectAll_ = function(selector) {
  console.log(`\td3SelectAll: ${selector}`);
  return "rootSelection";
} 
// d3Append_ =    :: String -> SelectionJS -> SelectionJS
exports.d3Append_ = element => selection => {
  console.log(`\td3Append: ${selection}.append(${element})`);
  return "appendedSelection";
}
// d3Join_ =      :: String -> SelectionJS -> SelectionJS
exports.d3Join_ = element => selection => {
  console.log(`\td3Join: ${element} to ${selection}`);
  return "joined";
}

// d3SetAttr_      :: String -> D3Attr -> SelectionJS -> Unit 
exports.d3SetAttr_ = name => value => selection => {
  console.log(`\t${selection}.attr(${name}, ${value})`);
  // selection.attr(name, value);
  return `attrAdded`
}
