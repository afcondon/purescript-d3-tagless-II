exports.nullD3Selection = null

// d3SelectAll_ :: Selector -> D3Selection
exports.d3SelectAllInDOM_ = selector => {
  console.log(`\td3SelectAllInDOM: ${selector}`);
  return "rootSelection";
  // return d3.selectAll(selector);
} 
// d3SelectAll_ :: Selector -> D3Selection
exports.d3SelectionSelectAll_ = selector => selection => {
  console.log(`\td3SelectionSelectAll: ${selection}.selectAll(${selector})`);
  return "JoinSelection";
  // return selection.selectAll(selector);
} 
// d3Enter_ :: D3Selection -> D3Selection
exports.d3EnterAndAppend_ = element => selection => {
  console.log(`\td3EnterAndAppend: ${selection}.enter().append(${element})`);
  return "EnterSelection"
  // return selection.enter().append(element);
}
// d3Exit_ :: D3Selection -> D3Selection
exports.d3Exit_ = selection => {
  console.log(`\td3Exit: ${selection}.exit()`);
  return "exit selection"
  // return selection.exit();
}
// d3RemoveSelection_ :: D3Selection -> D3Selection
exports.d3RemoveSelection_ = selection => {
  console.log(`\td3Remove: ${selection}.remove()`);
  return "removedSelection"
  // return selection.remove()
}
// d3Append_ :: String -> D3Selection -> D3Selection
exports.d3Append_ = element => selection => {
  console.log(`\td3Append: ${selection}.append(${element})`);
  return "appendedSelection";
  // return selection.append(selector);
}
// d3Data_ :: D3Data -> D3Selection -> D3Selection
exports.d3Data_ = data => selection => {
  console.log(`\td3Data: ${selection}.data(${data})`);
  return "dataBoundSelection"
}
// d3SetAttr_      :: String -> D3Attr -> D3Selection -> Unit 
exports.d3SetAttr_ = name => value => selection => {
  console.log(`\t${selection}.attr(${name}, ${value})`);
  // selection.attr(name, value);
  return `attrAdded`
}

