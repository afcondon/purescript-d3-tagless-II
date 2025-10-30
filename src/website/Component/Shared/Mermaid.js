// For Effect String -> Effect Unit
export const logMessage = (msg) => () => {
  console.log(msg);
};

// For Effect Unit
export const renderMermaidDiagrams_ = () => {
  console.log('FFI: renderMermaidDiagrams_ effect executing');

  // Use requestAnimationFrame to ensure it runs after DOM is ready
  requestAnimationFrame(() => {
    console.log('FFI: requestAnimationFrame callback executing');

    try {
      // Check if mermaid is loaded
      if (typeof window.mermaid === 'undefined') {
        console.log('Mermaid not loaded yet, will retry...');
        setTimeout(() => renderMermaidDiagrams_(), 500);
        return;
      }

      console.log('Mermaid is loaded, looking for diagram elements...');

      // Find all mermaid containers that haven't been processed
      const elements = document.querySelectorAll('.mermaid:not([data-rendered])');

      console.log(`Found ${elements.length} Mermaid diagram elements`);

      if (elements.length === 0) {
        console.log('No unrendered Mermaid diagrams found');
        return;
      }

      console.log(`Processing ${elements.length} Mermaid diagrams...`);

      elements.forEach(async (element, index) => {
        try {
          console.log(`Processing diagram ${index + 1}...`);

          // Mark as rendered to avoid reprocessing
          element.setAttribute('data-rendered', 'true');

          // Get the diagram text (Halogen will have HTML-escaped it)
          let graphDefinition = element.textContent || element.innerText;

          console.log(`Diagram ${index + 1} text length: ${graphDefinition.length}`);

          // Create a unique ID for this diagram
          const id = 'mermaid-' + Math.random().toString(36).substr(2, 9);

          // Render the diagram
          const { svg } = await window.mermaid.render(id, graphDefinition);

          // Clear and insert the SVG
          element.innerHTML = svg;

          console.log(`✓ Rendered Mermaid diagram ${index + 1}: ${id}`);
        } catch (error) {
          console.error(`✗ Failed to render Mermaid diagram ${index + 1}:`, error);
          element.innerHTML = '<div style="color: red; padding: 1rem; border: 1px solid red;">Failed to render diagram: ' + error.message + '</div>';
        }
      });
    } catch (error) {
      console.error('Error in requestAnimationFrame callback:', error);
    }
  });

  console.log('FFI: requestAnimationFrame scheduled');
};