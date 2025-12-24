// FFI for ChartBuilder component

export function copyToClipboard(text) {
  return function() {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      // Modern clipboard API
      navigator.clipboard.writeText(text).then(
        function() {
          console.log("Copied to clipboard:", text);
        },
        function(err) {
          console.error("Failed to copy to clipboard:", err);
          fallbackCopy(text);
        }
      );
    } else {
      // Fallback for older browsers
      fallbackCopy(text);
    }
  };
}

function fallbackCopy(text) {
  const textarea = document.createElement("textarea");
  textarea.value = text;
  textarea.style.position = "fixed";
  textarea.style.opacity = "0";
  document.body.appendChild(textarea);
  textarea.select();
  try {
    document.execCommand("copy");
    console.log("Copied to clipboard (fallback):", text);
  } catch (err) {
    console.error("Fallback copy failed:", err);
  }
  document.body.removeChild(textarea);
}
