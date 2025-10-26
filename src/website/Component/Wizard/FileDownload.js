// JSZip is expected to be loaded via CDN in the HTML
// <script src="https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js"></script>

export const createZip = function() {
  return new JSZip();
};

export const addFile = function(zip) {
  return function(filename) {
    return function(content) {
      return function() {
        zip.file(filename, content);
      };
    };
  };
};

export const downloadZipImpl = function(zip) {
  return function(zipFilename) {
    return function(onError, onSuccess) {
      zip.generateAsync({ type: "blob" })
        .then(function(blob) {
          // Create a download link
          const url = URL.createObjectURL(blob);
          const link = document.createElement('a');
          link.href = url;
          link.download = zipFilename;

          // Trigger download
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);

          // Clean up the URL object
          setTimeout(function() {
            URL.revokeObjectURL(url);
          }, 100);

          onSuccess();
        })
        .catch(function(error) {
          onError(error);
        });

      // Required by Aff - cancel function
      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };
  };
};

export const copyToClipboard = function(text) {
  return function() {
    try {
      // Modern clipboard API
      if (navigator.clipboard && window.isSecureContext) {
        navigator.clipboard.writeText(text);
        return true;
      } else {
        // Fallback for older browsers
        const textArea = document.createElement('textarea');
        textArea.value = text;
        textArea.style.position = 'fixed';
        textArea.style.left = '-999999px';
        textArea.style.top = '-999999px';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();

        const success = document.execCommand('copy');
        document.body.removeChild(textArea);
        return success;
      }
    } catch (error) {
      console.error('Failed to copy to clipboard:', error);
      return false;
    }
  };
};
