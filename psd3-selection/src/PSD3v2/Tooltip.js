// PSD3 Tooltip - Library support for tooltips

let tooltip = null;

const ensureTooltip = () => {
  if (!tooltip) {
    tooltip = document.createElement('div');
    tooltip.className = 'psd3-tooltip';
    tooltip.style.cssText = `
      position: fixed;
      display: none;
      background: rgba(20, 20, 30, 0.95);
      color: #f0f0f0;
      padding: 8px 12px;
      border-radius: 6px;
      font-size: 13px;
      max-width: 300px;
      pointer-events: none;
      z-index: 10000;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
    `;
    document.body.appendChild(tooltip);
  }
  return tooltip;
};

export const showTooltip_ = content => x => y => () => {
  const el = ensureTooltip();
  el.innerHTML = content;
  el.style.display = 'block';

  // Position with viewport bounds checking
  const rect = el.getBoundingClientRect();
  const viewW = window.innerWidth;
  const viewH = window.innerHeight;

  let posX = x + 15;
  let posY = y + 10;

  if (posX + rect.width > viewW) posX = x - rect.width - 15;
  if (posY + rect.height > viewH) posY = y - rect.height - 10;

  el.style.left = posX + 'px';
  el.style.top = posY + 'px';
};

export const hideTooltip_ = () => {
  ensureTooltip().style.display = 'none';
};

// Set custom CSS class for styling
export const setTooltipClass_ = className => () => {
  ensureTooltip().className = className;
};
