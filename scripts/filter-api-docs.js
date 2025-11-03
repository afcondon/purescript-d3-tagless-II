#!/usr/bin/env node

/**
 * Filter API documentation to only include library modules from src/lib
 * and replace external module links with a helpful explanation page.
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const GENERATED_DOCS_DIR = 'generated-docs/html';
const OUTPUT_DIR = 'docs/api';
const LIB_SOURCE_DIR = 'src/lib';

// Get all library module names from src/lib directory
function getLibraryModules() {
  try {
    const output = execSync(
      `find ${LIB_SOURCE_DIR} -type f -name "*.purs" | sed 's|${LIB_SOURCE_DIR}/||' | sed 's|/|.|g' | sed 's|.purs$||'`,
      { encoding: 'utf8' }
    );
    return output.trim().split('\n').filter(Boolean);
  } catch (error) {
    console.error('Error reading library modules:', error);
    return [];
  }
}

const LIBRARY_MODULES = new Set(getLibraryModules());

// Create the external link explanation page
const EXTERNAL_LINK_PAGE = `<!DOCTYPE HTML>
<html>
<head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>External Module - PSD3 API Docs</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      max-width: 800px;
      margin: 50px auto;
      padding: 20px;
      line-height: 1.6;
      color: #333;
    }
    h1 { color: #2c3e50; }
    .info-box {
      background-color: #f8f9fa;
      border-left: 4px solid #007acc;
      padding: 20px;
      margin: 20px 0;
    }
    a { color: #007acc; }
    code {
      background-color: #f4f4f4;
      padding: 2px 6px;
      border-radius: 3px;
      font-family: 'Courier New', monospace;
    }
  </style>
</head>
<body>
  <h1>External Module Documentation</h1>
  <div class="info-box">
    <p><strong>Note:</strong> To keep the documentation size manageable and avoid cluttering the repository,
    we only include API documentation for the library modules (those under <code>src/lib/</code>).</p>

    <p>For documentation on external dependencies from the PureScript ecosystem, please visit:</p>
    <ul>
      <li><a href="https://pursuit.purescript.org/" target="_blank">Pursuit</a> - The official PureScript package registry</li>
    </ul>

    <p>You can search for any module name on Pursuit to find its documentation, type signatures, and examples.</p>
  </div>

  <p><a href="index.html">← Back to PSD3 API Documentation</a></p>
</body>
</html>`;

function ensureDir(dir) {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

function isLibraryModule(filename) {
  // Convert filename (e.g., "PSD3.Attributes.html") to module name
  const moduleName = filename.replace(/\.html$/, '');
  return LIBRARY_MODULES.has(moduleName);
}

// Quadrant navigation HTML to inject
const QUADRANT_NAV_HTML = `
<style>
  .api-docs-header {
    background-color: #ffffff;
    border-bottom: 1px solid #e0ddd4;
    padding: 1rem 0;
    margin-bottom: 2rem;
  }
  .api-docs-header-content {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 2rem;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 2rem;
  }
  .api-docs-header-logo {
    height: 28px;
    width: auto;
  }
  .api-docs-header-middle {
    display: flex;
    align-items: center;
    gap: 1.5rem;
    flex: 1;
    justify-content: center;
  }
  .api-docs-header-label {
    font-family: 'Courier New', 'Courier', Monaco, monospace;
    font-size: 0.875rem;
    font-weight: bold;
    text-transform: uppercase;
    letter-spacing: 0.1em;
    color: #2c1810;
  }
  .api-docs-header-quadrant {
    display: grid;
    grid-template-columns: repeat(2, 16px);
    grid-template-rows: repeat(2, 16px);
    gap: 4px;
  }
  .api-docs-header-quadrant-box {
    width: 16px;
    height: 16px;
    border: 2px solid #8b7355;
    background-color: transparent;
    transition: all 0.2s ease;
    cursor: pointer;
  }
  .api-docs-header-quadrant-box:hover {
    background-color: #D4C9A8;
    border-color: #E63946;
  }
  .api-docs-header-quadrant-box--active {
    background-color: #E63946;
    border-color: #E63946;
  }
  .api-docs-header-nav {
    display: flex;
    align-items: center;
    gap: 2rem;
  }
  .api-docs-header-nav-link {
    font-size: 1rem;
    color: #666;
    text-decoration: none;
    font-weight: 500;
    transition: color 0.2s ease;
  }
  .api-docs-header-nav-link:hover {
    color: #333;
  }
</style>
<div class="api-docs-header">
  <div class="api-docs-header-content">
    <a href="../#/" style="display: flex; align-items: center;">
      <img src="../assets/psd3-logo-color.svg" alt="PSD3 Logo" class="api-docs-header-logo">
    </a>
    <div class="api-docs-header-middle">
      <span class="api-docs-header-label">Docs</span>
      <div class="api-docs-header-quadrant">
        <a href="../#/getting-started" class="api-docs-header-quadrant-box" title="Getting Started"></a>
        <a href="../#/howto" class="api-docs-header-quadrant-box" title="How-To Guides"></a>
        <a href="index.html" class="api-docs-header-quadrant-box api-docs-header-quadrant-box--active" title="API Reference"></a>
        <a href="../#/understanding/concepts" class="api-docs-header-quadrant-box" title="Understanding"></a>
      </div>
    </div>
    <nav class="api-docs-header-nav">
      <a href="../#/" class="api-docs-header-nav-link">← Back to Home</a>
    </nav>
  </div>
</div>
`;

function processHtmlFile(content, filename) {
  // Replace links to external modules with link to external.html
  let processed = content;

  // Inject quadrant navigation after opening body tag
  processed = processed.replace(/<body>/, `<body>${QUADRANT_NAV_HTML}`);

  // If this is the index page, filter out non-library modules from the list
  if (filename === 'index.html') {
    // Remove list items that link to non-library modules
    processed = processed.replace(/<li><a href="([^"]+\.html)">([^<]+)<\/a><\/li>/g, (match, href, text) => {
      // Keep library modules, remove everything else
      if (isLibraryModule(href)) {
        return match;
      }
      return ''; // Remove this list item
    });

    // Remove the search JavaScript since we have a small, filtered list
    // The search app expects a full index which we're not providing
    processed = processed.replace(/<script[^>]*src="\.\/docs-search-app\.js"[^>]*><\/script>\s*/g, '');
    processed = processed.replace(/<script[^>]*>window\.DocsSearchTypeIndex\s*=\s*\{\};?\s*window\.DocsSearchIndex\s*=\s*\{\};?\s*<\/script>/g, '');
    // Also remove the comment before the scripts
    processed = processed.replace(/<!--\s*Docs search index\.\s*-->/g, '');
  }

  // Find all links to .html files
  const linkRegex = /<a href="([^"#]+\.html)([^"]*)"/g;

  processed = processed.replace(linkRegex, (match, linkedFile, fragment) => {
    // If the linked file is not a library module, point to external.html
    if (!isLibraryModule(linkedFile) && linkedFile !== 'index.html' && linkedFile !== 'external.html') {
      return `<a href="external.html" title="External module - see Pursuit for docs"`;
    }
    // Keep the link as-is if it's a library module
    return match;
  });

  return processed;
}

function copyAndFilterDocs() {
  console.log('Filtering API documentation...');

  // Ensure output directory exists
  ensureDir(OUTPUT_DIR);

  // Copy and process index.html
  console.log('Processing index.html...');
  const indexPath = path.join(GENERATED_DOCS_DIR, 'index.html');
  if (fs.existsSync(indexPath)) {
    let indexContent = fs.readFileSync(indexPath, 'utf8');
    indexContent = processHtmlFile(indexContent, 'index.html');
    fs.writeFileSync(path.join(OUTPUT_DIR, 'index.html'), indexContent);
  }

  // Create external.html
  console.log('Creating external.html...');
  fs.writeFileSync(path.join(OUTPUT_DIR, 'external.html'), EXTERNAL_LINK_PAGE);

  // Copy support files (JS, CSS, etc.)
  console.log('Copying support files...');
  const supportFiles = ['docs-search-app.js'];
  supportFiles.forEach(file => {
    const sourcePath = path.join(GENERATED_DOCS_DIR, file);
    if (fs.existsSync(sourcePath)) {
      const targetPath = path.join(OUTPUT_DIR, file);
      fs.copyFileSync(sourcePath, targetPath);
    }
  });

  // Get all HTML files in generated-docs
  const allFiles = fs.readdirSync(GENERATED_DOCS_DIR);
  const htmlFiles = allFiles.filter(f => f.endsWith('.html') && f !== 'index.html');

  let copiedCount = 0;
  let skippedCount = 0;

  // Process each HTML file
  htmlFiles.forEach(file => {
    if (isLibraryModule(file)) {
      const sourcePath = path.join(GENERATED_DOCS_DIR, file);
      const targetPath = path.join(OUTPUT_DIR, file);

      let content = fs.readFileSync(sourcePath, 'utf8');
      content = processHtmlFile(content, file);
      fs.writeFileSync(targetPath, content);

      copiedCount++;
    } else {
      skippedCount++;
    }
  });

  console.log(`\nAPI documentation filtered successfully!`);
  console.log(`- Copied ${copiedCount} library module pages (from src/lib/)`);
  console.log(`- Skipped ${skippedCount} non-library module pages`);
  console.log(`- Filtered index.html to show only library modules`);
  console.log(`- Created external.html for external module links`);
  console.log(`\nDocumentation available at: ${OUTPUT_DIR}/index.html`);
}

// Run the script
try {
  copyAndFilterDocs();
} catch (error) {
  console.error('Error filtering docs:', error);
  process.exit(1);
}
