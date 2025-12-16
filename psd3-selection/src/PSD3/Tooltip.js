// PSD3 Tooltip - Library support for tooltips

let tooltip = null;
let tooltipConfig = null;

const ensureTooltip = () => {
  if (!tooltip) {
    tooltip = document.createElement('div');
    tooltip.className = 'psd3-tooltip';
    // Only set essential styles - positioning and display
    // All visual styling should come from CSS or configureTooltip
    tooltip.style.cssText = `
      position: fixed;
      display: none;
      pointer-events: none;
      z-index: 10000;
    `;
    document.body.appendChild(tooltip);

    // Apply config if it was set before tooltip was created
    if (tooltipConfig) {
      applyConfig(tooltipConfig);
    }
  }
  return tooltip;
};

// Apply configuration to tooltip element
const applyConfig = (config) => {
  const el = ensureTooltip();

  // Apply CSS properties from config
  if (config.background) el.style.background = config.background;
  if (config.color) el.style.color = config.color;
  if (config.padding) el.style.padding = config.padding;
  if (config.borderRadius) el.style.borderRadius = config.borderRadius;
  if (config.fontSize) el.style.fontSize = config.fontSize;
  if (config.fontFamily) el.style.fontFamily = config.fontFamily;
  if (config.border) el.style.border = config.border;
  if (config.boxShadow) el.style.boxShadow = config.boxShadow;
  if (config.maxWidth) el.style.maxWidth = config.maxWidth;
  if (config.lineHeight) el.style.lineHeight = config.lineHeight;
  if (config.backdropFilter) {
    el.style.backdropFilter = config.backdropFilter;
    el.style.webkitBackdropFilter = config.backdropFilter; // Safari
  }
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

// Configure tooltip styles programmatically
export const configureTooltip_ = config => () => {
  tooltipConfig = config;
  applyConfig(config);
};
