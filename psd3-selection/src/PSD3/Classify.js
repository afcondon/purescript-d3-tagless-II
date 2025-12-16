// Generic element classification based on bound data
//
// Uses native querySelectorAll and classList for performance.
// Reads D3's __data__ property from each element.

// Classify elements: apply a class based on bound data
// classifier: (data) -> className (empty string = skip)
export function classifyElements_(containerSelector) {
  return function(elementType) {
    return function(classifier) {
      return function() {
        const selector = containerSelector + " " + elementType;
        const elements = document.querySelectorAll(selector);

        elements.forEach(el => {
          const data = el.__data__;
          if (data !== undefined) {
            const className = classifier(data);
            if (className) {
              el.classList.add(className);
            }
          }
        });
      };
    };
  };
}

// Clear specified classes from all matching elements
export function clearClasses_(containerSelector) {
  return function(elementType) {
    return function(classNames) {
      return function() {
        const selector = containerSelector + " " + elementType;
        const elements = document.querySelectorAll(selector);

        elements.forEach(el => {
          classNames.forEach(cls => {
            el.classList.remove(cls);
          });
        });
      };
    };
  };
}
