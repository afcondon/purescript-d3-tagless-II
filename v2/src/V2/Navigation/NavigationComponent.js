// FFI for NavigationComponent

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
