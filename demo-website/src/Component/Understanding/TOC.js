/**
 * Initialize anchor link handling
 * Intercepts clicks on links with data-anchor-link attribute
 * and scrolls to the target element instead of changing routes
 */
(function() {
  // Use event delegation on document to handle dynamically added links
  document.addEventListener('click', function(event) {
    // Find the closest anchor element with data-anchor-link attribute
    const link = event.target.closest('a[data-anchor-link]');

    if (link) {
      // Prevent default link behavior (which would trigger routing)
      event.preventDefault();

      // Get the anchor ID from the data attribute
      const anchorId = link.getAttribute('data-anchor-link');

      // Find and scroll to the target element
      const element = document.getElementById(anchorId);
      if (element) {
        element.scrollIntoView({
          behavior: 'smooth',
          block: 'start'
        });
      } else {
        console.warn(`Anchor element with id "${anchorId}" not found`);
      }
    }
  });
})();

// Export a dummy value so PureScript can import this module
export const initAnchorLinks = undefined;
