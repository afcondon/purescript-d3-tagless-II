// NarrativePanel - Contextual description panel that updates with view state
// "What Is Being Shown" - editorial style narrative that explains the current visualization

// Panel element references
let panel = null;
let heroText = null;
let hintText = null;
let colorKey = null;
let backButton = null;
let onBackCallback = null;

// Current color palette data
let currentPalette = [];

// Initialize the narrative panel
export const initNarrativePanel_ = (onBack) => () => {
  onBackCallback = onBack;

  // Remove existing panel if any
  const existing = document.getElementById("narrative-panel");
  if (existing) existing.remove();

  // Create panel container - now on left side
  panel = document.createElement("div");
  panel.id = "narrative-panel";
  panel.className = "floating-panel floating-panel--top-left narrative-panel";

  // Title (fixed)
  const title = document.createElement("div");
  title.className = "panel-title";
  title.textContent = "What Is Being Shown";
  panel.appendChild(title);

  // Hero text container
  heroText = document.createElement("div");
  heroText.className = "narrative-hero";
  heroText.innerHTML = "Loading...";
  panel.appendChild(heroText);

  // Hint text container
  hintText = document.createElement("div");
  hintText.className = "narrative-hint";
  hintText.innerHTML = "";
  panel.appendChild(hintText);

  // Color key container (hidden initially)
  colorKey = document.createElement("div");
  colorKey.className = "narrative-color-key";
  colorKey.style.display = "none";
  panel.appendChild(colorKey);

  // Back button (hidden initially)
  backButton = document.createElement("button");
  backButton.className = "btn-editorial narrative-back";
  backButton.innerHTML = "← Back to overview";
  backButton.style.display = "none";
  backButton.onclick = () => {
    if (onBackCallback) onBackCallback();
  };
  panel.appendChild(backButton);

  // Insert into DOM (will be positioned by CSS)
  document.body.appendChild(panel);
};

// Set the color palette for the color key
export const setColorPalette_ = (palette) => () => {
  currentPalette = palette;
  renderColorKey();
};

// Render the color key based on current palette
const renderColorKey = () => {
  if (!colorKey || currentPalette.length === 0) return;

  colorKey.innerHTML = `
    <div class="color-key-title">Packages</div>
    <div class="color-key-items">
      ${currentPalette.map(({ name, color }) => `
        <div class="color-key-item">
          <span class="color-key-swatch" style="background-color: ${color}"></span>
          <span class="color-key-label">${name}</span>
        </div>
      `).join("")}
    </div>
  `;
  colorKey.style.display = "block";
};

// Update panel for full view (packages + modules overview)
export const setFullViewNarrative_ = (projectName) => (moduleCount) => (packageCount) => () => {
  if (!heroText || !hintText || !backButton) return;

  heroText.innerHTML = `
    Viewing the dependency structure of <strong>${projectName}</strong>.<br/>
    <span class="narrative-stat">${moduleCount}</span> modules across
    <span class="narrative-stat">${packageCount}</span> packages.
  `;

  hintText.innerHTML = `
    Modules cluster around their package by color.<br/>
    <em>Click any module</em> to explore its neighborhood.
  `;

  // Show color key in full view
  if (colorKey && currentPalette.length > 0) {
    colorKey.style.display = "block";
  }

  backButton.style.display = "none";
};

// Update panel for neighborhood view (focused on a module)
export const setNeighborhoodNarrative_ = (moduleName) => (dependsOnCount) => (dependedByCount) => () => {
  if (!heroText || !hintText || !backButton) return;

  // Split module name for emphasis
  const parts = moduleName.split(".");
  const shortName = parts[parts.length - 1];
  const packagePath = parts.slice(0, -1).join(".");

  heroText.innerHTML = `
    Focused on <strong class="narrative-module">${shortName}</strong>
    ${packagePath ? `<span class="narrative-path">(${packagePath})</span>` : ""}<br/>
    Depends on <span class="narrative-stat narrative-callee">${dependsOnCount}</span> modules,
    depended on by <span class="narrative-stat narrative-caller">${dependedByCount}</span>.
  `;

  hintText.innerHTML = `
    Each bubble shows the module's declarations.<br/>
    <em>Hover a function</em> to see call relationships.
  `;

  // Hide color key in neighborhood view
  if (colorKey) {
    colorKey.style.display = "none";
  }

  backButton.style.display = "block";
};

// Update panel for declaration hover state
export const setDeclarationHoverNarrative_ = (moduleName) => (declName) => (callsCount) => (calledByCount) => () => {
  if (!hintText) return;

  if (callsCount === 0 && calledByCount === 0) {
    hintText.innerHTML = `
      <strong>${declName}</strong> has no cross-module calls.<br/>
      <em>Click</em> to see the full call graph.
    `;
  } else {
    hintText.innerHTML = `
      <strong>${declName}</strong> calls
      <span class="narrative-stat narrative-callee">${callsCount}</span> functions
      and is called by
      <span class="narrative-stat narrative-caller">${calledByCount}</span>.<br/>
      <em>Click</em> to see the detailed call graph.
    `;
  }
};

// Clear declaration hover state (restore neighborhood hint)
export const clearDeclarationHoverNarrative_ = () => {
  if (!hintText) return;

  hintText.innerHTML = `
    Each bubble shows the module's declarations.<br/>
    <em>Hover a function</em> to see call relationships.
  `;
};

// Remove the panel
export const removeNarrativePanel_ = () => {
  if (panel) {
    panel.remove();
    panel = null;
    heroText = null;
    hintText = null;
    colorKey = null;
    backButton = null;
    currentPalette = [];
  }
};
