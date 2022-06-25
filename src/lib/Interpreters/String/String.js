// TODO all these show statements are getting pruned out in DCE and causing crash at runtime if you have debug enabled
// so either have to backtrack on the extraction to functions or find a way to avoid them being pruned
export function showSelectAllInDOM_(selector) {
  return `\td3SelectAllInDOM: ${selector}`
}
export function showSelectAll_(selector) {
  return selection => {
    return `\td3SelectionSelectAll: ${selection}.selectAll(${selector})`
  }
}
export function showEnterAndAppend_(element) {
  return selection => {
    return `\td3EnterAndAppend: ${selection}.enter().append(${element})`
  }
}
export function showExit_(selection) {
  return `\td3Exit: ${selection}.exit()`
}
export function showAddTransition_(selection) {
  return transition => {
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
}
export function showRemoveSelection_(selection) {
  return `\td3Remove: ${selection}.remove()`
}
export function showAppend_(element) {
  return selection => {
    return `\td3Append: ${selection}.append(${element})`
  }
}
export function showKeyFunction_(data) {
  return keyFunction => selection => {
    return `\td3Data: ${selection}.data(${data}, ${keyFunction})`
  }
}
export function showData_(data) {
  return selection => {
    return `\td3Data: ${selection}.data(${data})`
  }
}
export function showSetAttr_(name) {
  return value => selection => {
    return `\t${selection}.attr(${name}, ${value})`
  }
}
export function showSetText_(value) {
  return selection => {
    return `\t${selection}.text(${value})`
  }
}
export function showSetHTML_(value) {
  return selection => {
    return `\t${selection}.html(${value})`
  }
}
export function showSetProperty_(value) {
  return selection => {
    return `\t${selection}.property(${value})`
  }
}
export function showSetOrdering_(ordering) {
  return selection => {
    return `\t${selection}.${ordering}()`
  }
}
