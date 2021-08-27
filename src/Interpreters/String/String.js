// TODO all these show statements are getting pruned out in DCE and causing crash at runtime if you have debug enabled
// so either have to backtrack on the extraction to functions or find a way to avoid them being pruned
exports.showSelectAllInDOM_ = selector => {
  return `\td3SelectAllInDOM: ${selector}`
}
exports.showSelectAll_ = selector => selection => {
  return `\td3SelectionSelectAll: ${selection}.selectAll(${selector})`
}
exports.showEnterAndAppend_ = element => selection => {
  return `\td3EnterAndAppend: ${selection}.enter().append(${element})`
}
exports.showExit_ = selection => {
  return `\td3Exit: ${selection}.exit()`
}
exports.showAddTransition_ = selection => transition => {
  if (transition.name == '') {
    const statement1 = `\td3addTransition: ${selection}.transition(${transition})`
    var statement2 = ''
    var statement3 = ''
    if (transition.duration != 0) {
      statement2 = `transition.duration(${transition.duration})`
    }
    if (transition.delay != 0) {
      statement3 = `\t\ttransition.delay(${transition.delay})`
    }
    return statement1 + statement2 + statement3
  } else {
    return `\td3addNamedTransition: ${selection}.transition(${transition})`
  }
}
exports.showRemoveSelection_ = selection => {
  return `\td3Remove: ${selection}.remove()`
}
exports.showAppend_ = element => selection => {
  return `\td3Append: ${selection}.append(${element})`
}
exports.showKeyFunction_ = data => keyFunction => selection => {
  return `\td3Data: ${selection}.data(${data}, ${keyFunction})`
}
exports.showData_ = data => selection => {
  return `\td3Data: ${selection}.data(${data})`
}
exports.showSetAttr_ = name => value => selection => {
  return `\t${selection}.attr(${name}, ${value})`
}
exports.showSetText_ = value => selection => {
  return `\t${selection}.text(${value})`
}
exports.showSetHTML_ = value => selection => {
  return `\t${selection}.html(${value})`
}
exports.showSetProperty_ = value => selection => {
  return `\t${selection}.property(${value})`
}
exports.showSetOrdering_ = ordering => selection => {
  return `\t${selection}.${ordering}()`
}
