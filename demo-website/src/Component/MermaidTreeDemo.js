// FFI for Mermaid rendering

export function runMermaid() {
  // Use setTimeout to ensure DOM has been updated
  setTimeout(function() {
    if (window.mermaid) {
      window.mermaid.run({
        querySelector: '.mermaid'
      });
    }
  }, 0);
}
