#!/usr/bin/env node

/**
 * Filter API documentation to only include our own modules (PSD3.* and D3.*)
 * and replace external module links with a helpful explanation page.
 */

const fs = require('fs');
const path = require('path');

const GENERATED_DOCS_DIR = 'generated-docs/html';
const OUTPUT_DIR = 'docs/api';
const OUR_MODULE_PREFIXES = ['PSD3.', 'D3.', 'CodeSnippet'];

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
    we only include API documentation for PSD3's own modules (those starting with <code>PSD3.*</code> or <code>D3.*</code>).</p>

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

function isOurModule(filename) {
  return OUR_MODULE_PREFIXES.some(prefix => filename.startsWith(prefix));
}

function processHtmlFile(content, filename) {
  // Replace links to external modules with link to external.html
  let processed = content;

  // Find all links to .html files
  const linkRegex = /<a href="([^"#]+\.html)([^"]*)"/g;

  processed = processed.replace(linkRegex, (match, linkedFile, fragment) => {
    // If the linked file is not one of our modules, point to external.html
    if (!isOurModule(linkedFile) && linkedFile !== 'index.html' && linkedFile !== 'external.html') {
      return `<a href="external.html" title="External module - see Pursuit for docs"`;
    }
    // Keep the link as-is if it's one of our modules
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

  // Get all HTML files in generated-docs
  const allFiles = fs.readdirSync(GENERATED_DOCS_DIR);
  const htmlFiles = allFiles.filter(f => f.endsWith('.html') && f !== 'index.html');

  let copiedCount = 0;
  let skippedCount = 0;

  // Process each HTML file
  htmlFiles.forEach(file => {
    if (isOurModule(file)) {
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
  console.log(`- Copied ${copiedCount} module pages (PSD3.*, D3.*)`);
  console.log(`- Skipped ${skippedCount} external module pages`);
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
