// FFI helpers for QueryDemo button controls

export function createGroupButtons(groups) {
  return function(colorFn) {
    return function(onToggle) {
      return function() {
        // Debug logging
        console.log('createGroupButtons called with:', { groups, colorFn, onToggle });

        // Create button container
        const container = document.createElement('div');
        container.className = 'query-demo-controls';
        container.style.cssText = 'position: fixed; top: 10px; right: 10px; background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.2); z-index: 1000; max-width: 200px;';

        // Add title
        const title = document.createElement('h3');
        title.textContent = 'Toggle Group Sizes';
        title.style.cssText = 'margin: 0 0 10px 0; font-size: 14px; font-weight: bold;';
        container.appendChild(title);

        // Add description
        const desc = document.createElement('p');
        desc.textContent = 'Click to toggle size for each group';
        desc.style.cssText = 'margin: 0 0 10px 0; font-size: 11px; color: #666;';
        container.appendChild(desc);

        // Track which groups are large
        const largeGroups = new Set();

        // Create button for each group
        groups.forEach(group => {
          const btn = document.createElement('button');
          btn.className = `group-btn group-btn-${group}`;
          btn.textContent = `Group ${group}`;
          btn.style.cssText = `display: block; margin: 5px 0; padding: 5px 10px; background: ${colorFn(group)}; color: white; border: none; border-radius: 3px; cursor: pointer; font-size: 12px; width: 100%; transition: all 0.2s;`;

          btn.addEventListener('click', () => {
            // Toggle group
            const isLarge = !largeGroups.has(group);
            if (isLarge) {
              largeGroups.add(group);
              btn.style.border = '2px solid #333';
              btn.style.fontWeight = 'bold';
            } else {
              largeGroups.delete(group);
              btn.style.border = 'none';
              btn.style.fontWeight = 'normal';
            }

            // Update circles directly using DOM API
            const selector = `.group-${group}`;
            console.log(`Looking for: ${selector}`);
            const circles = document.querySelectorAll(selector);
            console.log(`Found ${circles.length} circles`);
            const newRadius = isLarge ? 10.0 : 5.0;
            circles.forEach(circle => {
              circle.setAttribute('r', newRadius);
            });

            // Also call PureScript callback (for potential additional logic)
            // onToggle(group)(isLarge)();
          });

          container.appendChild(btn);
        });

        // Append to body
        document.body.appendChild(container);

        return container;
      };
    };
  };
}
