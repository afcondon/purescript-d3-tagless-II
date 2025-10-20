// FFI for NavigationComponent
// TODO use a proper PureScript library to do setHash (there are several) and similarly find a library for openInNewTab functionality
export function setHash(hash) {
  return function() {
    window.location.hash = hash;
  };
}

export function openInNewTab(url) {
  return function() {
    window.open(url, '_blank');
  };
}
