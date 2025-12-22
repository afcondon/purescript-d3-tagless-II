// FFI for TreeBuilder3.App

// Render GUP UI elements (buttons and labels) using D3
export const renderGupUIFFI = (selected) => (wordA) => (wordB) => () => {
  const container = document.querySelector('#viz-output-content');
  if (!container) return;

  // Create SVG namespace element helper
  const svgNS = 'http://www.w3.org/2000/svg';
  const createSVG = (tag) => document.createElementNS(svgNS, tag);

  // Button A
  const btnAGroup = createSVG('g');
  btnAGroup.setAttribute('transform', 'translate(20, 10)');
  btnAGroup.style.cursor = 'pointer';
  btnAGroup.dataset.dataset = 'A';

  const btnARect = createSVG('rect');
  btnARect.setAttribute('x', '0');
  btnARect.setAttribute('y', '0');
  btnARect.setAttribute('width', '80');
  btnARect.setAttribute('height', '30');
  btnARect.setAttribute('rx', '4');
  btnARect.setAttribute('fill', selected === 'A' ? '#4A90E2' : '#E0E0E0');
  btnARect.setAttribute('stroke', '#333');
  btnARect.setAttribute('stroke-width', '1');
  btnAGroup.appendChild(btnARect);

  const btnAText = createSVG('text');
  btnAText.setAttribute('x', '40');
  btnAText.setAttribute('y', '20');
  btnAText.setAttribute('text-anchor', 'middle');
  btnAText.setAttribute('fill', selected === 'A' ? '#FFF' : '#333');
  btnAText.setAttribute('font-size', '14px');
  btnAText.setAttribute('font-weight', 'bold');
  btnAText.textContent = wordA;
  btnAGroup.appendChild(btnAText);

  // Button B
  const btnBGroup = createSVG('g');
  btnBGroup.setAttribute('transform', 'translate(110, 10)');
  btnBGroup.style.cursor = 'pointer';
  btnBGroup.dataset.dataset = 'B';

  const btnBRect = createSVG('rect');
  btnBRect.setAttribute('x', '0');
  btnBRect.setAttribute('y', '0');
  btnBRect.setAttribute('width', '80');
  btnBRect.setAttribute('height', '30');
  btnBRect.setAttribute('rx', '4');
  btnBRect.setAttribute('fill', selected === 'B' ? '#4A90E2' : '#E0E0E0');
  btnBRect.setAttribute('stroke', '#333');
  btnBRect.setAttribute('stroke-width', '1');
  btnBGroup.appendChild(btnBRect);

  const btnBText = createSVG('text');
  btnBText.setAttribute('x', '40');
  btnBText.setAttribute('y', '20');
  btnBText.setAttribute('text-anchor', 'middle');
  btnBText.setAttribute('fill', selected === 'B' ? '#FFF' : '#333');
  btnBText.setAttribute('font-size', '14px');
  btnBText.setAttribute('font-weight', 'bold');
  btnBText.textContent = wordB;
  btnBGroup.appendChild(btnBText);

  // Phase labels
  const labelEnter = createSVG('text');
  labelEnter.setAttribute('x', '210');
  labelEnter.setAttribute('y', '75');
  labelEnter.setAttribute('fill', '#4CAF50');
  labelEnter.setAttribute('font-size', '10px');
  labelEnter.textContent = 'Enter';

  const labelUpdate = createSVG('text');
  labelUpdate.setAttribute('x', '210');
  labelUpdate.setAttribute('y', '125');
  labelUpdate.setAttribute('fill', '#2196F3');
  labelUpdate.setAttribute('font-size', '10px');
  labelUpdate.textContent = 'Update';

  const labelExit = createSVG('text');
  labelExit.setAttribute('x', '210');
  labelExit.setAttribute('y', '175');
  labelExit.setAttribute('fill', '#F44336');
  labelExit.setAttribute('font-size', '10px');
  labelExit.textContent = 'Exit';

  // Append all elements
  container.appendChild(btnAGroup);
  container.appendChild(btnBGroup);
  container.appendChild(labelEnter);
  container.appendChild(labelUpdate);
  container.appendChild(labelExit);

  // Add click handlers
  btnAGroup.addEventListener('click', () => {
    // Call renderGupWithDataset("A") via a global callback
    if (window.__renderGupWithDataset) {
      window.__renderGupWithDataset('A');
    }
  });

  btnBGroup.addEventListener('click', () => {
    // Call renderGupWithDataset("B") via a global callback
    if (window.__renderGupWithDataset) {
      window.__renderGupWithDataset('B');
    }
  });

  // Store the render function globally for button callbacks
  window.__renderGupWithDataset = (dataset) => {
    // We need to call back into PureScript - for now, just re-render via DOM manipulation
    // This is a simplified approach; proper solution would use Halogen subscriptions
    import('./bundle.js').then(mod => {
      // This won't work directly - we need a different approach
    }).catch(() => {});

    // For now, manually update the DOM
    updateGupDisplay(dataset, wordA, wordB);
  };
};

// Update GUP display when dataset changes (simplified DOM manipulation)
function updateGupDisplay(selected, wordA, wordB) {
  const container = document.querySelector('#viz-output-content');
  if (!container) return;

  // Compute phases
  const fromWord = selected === 'A' ? wordB : wordA;
  const toWord = selected === 'A' ? wordA : wordB;

  const fromChars = fromWord.split('');
  const toChars = toWord.split('');

  const enterLetters = toChars.filter(c => !fromChars.includes(c));
  const updateLetters = toChars.filter(c => fromChars.includes(c));
  const exitLetters = fromChars.filter(c => !toChars.includes(c));

  // Clear existing content
  container.innerHTML = '';

  const svgNS = 'http://www.w3.org/2000/svg';
  const createSVG = (tag) => document.createElementNS(svgNS, tag);

  // Create letter group
  const letterGroup = createSVG('g');
  letterGroup.setAttribute('class', 'gup-letters');

  // Render enter letters (green, y=70)
  enterLetters.forEach((letter, i) => {
    const text = createSVG('text');
    text.setAttribute('x', 30 + i * 40);
    text.setAttribute('y', '70');
    text.setAttribute('text-anchor', 'middle');
    text.setAttribute('fill', '#4CAF50');
    text.setAttribute('font-size', '28px');
    text.setAttribute('font-weight', 'bold');
    text.textContent = letter;
    letterGroup.appendChild(text);
  });

  // Render update letters (blue, y=120)
  updateLetters.forEach((letter, i) => {
    const text = createSVG('text');
    text.setAttribute('x', 30 + i * 40);
    text.setAttribute('y', '120');
    text.setAttribute('text-anchor', 'middle');
    text.setAttribute('fill', '#2196F3');
    text.setAttribute('font-size', '28px');
    text.setAttribute('font-weight', 'bold');
    text.textContent = letter;
    letterGroup.appendChild(text);
  });

  // Render exit letters (red, y=170)
  exitLetters.forEach((letter, i) => {
    const text = createSVG('text');
    text.setAttribute('x', 30 + i * 40);
    text.setAttribute('y', '170');
    text.setAttribute('text-anchor', 'middle');
    text.setAttribute('fill', '#F44336');
    text.setAttribute('font-size', '28px');
    text.setAttribute('font-weight', 'bold');
    text.textContent = letter;
    letterGroup.appendChild(text);
  });

  container.appendChild(letterGroup);

  // Re-render UI (buttons and labels)
  // Button A
  const btnAGroup = createSVG('g');
  btnAGroup.setAttribute('transform', 'translate(20, 10)');
  btnAGroup.style.cursor = 'pointer';

  const btnARect = createSVG('rect');
  btnARect.setAttribute('x', '0');
  btnARect.setAttribute('y', '0');
  btnARect.setAttribute('width', '80');
  btnARect.setAttribute('height', '30');
  btnARect.setAttribute('rx', '4');
  btnARect.setAttribute('fill', selected === 'A' ? '#4A90E2' : '#E0E0E0');
  btnARect.setAttribute('stroke', '#333');
  btnARect.setAttribute('stroke-width', '1');
  btnAGroup.appendChild(btnARect);

  const btnAText = createSVG('text');
  btnAText.setAttribute('x', '40');
  btnAText.setAttribute('y', '20');
  btnAText.setAttribute('text-anchor', 'middle');
  btnAText.setAttribute('fill', selected === 'A' ? '#FFF' : '#333');
  btnAText.setAttribute('font-size', '14px');
  btnAText.setAttribute('font-weight', 'bold');
  btnAText.textContent = wordA;
  btnAGroup.appendChild(btnAText);

  btnAGroup.addEventListener('click', () => updateGupDisplay('A', wordA, wordB));
  container.appendChild(btnAGroup);

  // Button B
  const btnBGroup = createSVG('g');
  btnBGroup.setAttribute('transform', 'translate(110, 10)');
  btnBGroup.style.cursor = 'pointer';

  const btnBRect = createSVG('rect');
  btnBRect.setAttribute('x', '0');
  btnBRect.setAttribute('y', '0');
  btnBRect.setAttribute('width', '80');
  btnBRect.setAttribute('height', '30');
  btnBRect.setAttribute('rx', '4');
  btnBRect.setAttribute('fill', selected === 'B' ? '#4A90E2' : '#E0E0E0');
  btnBRect.setAttribute('stroke', '#333');
  btnBRect.setAttribute('stroke-width', '1');
  btnBGroup.appendChild(btnBRect);

  const btnBText = createSVG('text');
  btnBText.setAttribute('x', '40');
  btnBText.setAttribute('y', '20');
  btnBText.setAttribute('text-anchor', 'middle');
  btnBText.setAttribute('fill', selected === 'B' ? '#FFF' : '#333');
  btnBText.setAttribute('font-size', '14px');
  btnBText.setAttribute('font-weight', 'bold');
  btnBText.textContent = wordB;
  btnBGroup.appendChild(btnBText);

  btnBGroup.addEventListener('click', () => updateGupDisplay('B', wordA, wordB));
  container.appendChild(btnBGroup);

  // Phase labels
  const labels = [
    { y: 75, fill: '#4CAF50', text: 'Enter' },
    { y: 125, fill: '#2196F3', text: 'Update' },
    { y: 175, fill: '#F44336', text: 'Exit' }
  ];

  labels.forEach(({ y, fill, text }) => {
    const label = createSVG('text');
    label.setAttribute('x', '210');
    label.setAttribute('y', y);
    label.setAttribute('fill', fill);
    label.setAttribute('font-size', '10px');
    label.textContent = text;
    container.appendChild(label);
  });
}
