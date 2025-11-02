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

function processHtmlFile(content, filename) {
  // Replace links to external modules with link to external.html
  let processed = content;

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
