#!/usr/bin/env node

/**
 * Generate a PureScript module that embeds library source code as strings
 * This allows the Reference section to display module source without runtime file loading
 */

const fs = require('fs');
const path = require('path');

const LIB_DIR = path.join(__dirname, '../src/lib/PSD3');
const OUTPUT_FILE = path.join(__dirname, '../src/website/Component/Reference/ModuleSources.purs');

// Escape string for PureScript
function escapeString(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

// Read a file and return its content
function readSource(filePath) {
  try {
    return fs.readFileSync(filePath, 'utf8');
  } catch (err) {
    console.error(`Error reading ${filePath}:`, err.message);
    return `-- Error loading source for ${filePath}`;
  }
}

// Get all .purs files recursively
function getPursFiles(dir, basePath = '') {
  const files = [];
  const entries = fs.readdirSync(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    const relativePath = path.join(basePath, entry.name);

    if (entry.isDirectory()) {
      files.push(...getPursFiles(fullPath, relativePath));
    } else if (entry.name.endsWith('.purs')) {
      files.push({
        path: relativePath,
        fullPath: fullPath
      });
    }
  }

  return files;
}

// Generate the module
function generateModule() {
  const files = getPursFiles(LIB_DIR);

  let output = `module PSD3.Reference.ModuleSources where

import Foreign.Object as Object
import Foreign.Object (Object)

-- | Map of module paths to their source code
moduleSources :: Object String
moduleSources = Object.fromFoldable
  [ `;

  const entries = files.map(file => {
    const source = readSource(file.fullPath);
    const escapedSource = escapeString(source);
    return `Tuple "${file.path}" "${escapedSource}"`;
  });

  output += entries.join('\n  , ');
  output += '\n  ]\n';

  return output;
}

// Write the module
function main() {
  console.log('Generating ModuleSources.purs...');
  const moduleContent = generateModule();

  fs.writeFileSync(OUTPUT_FILE, moduleContent, 'utf8');
  console.log(`✓ Generated ${OUTPUT_FILE}`);
}

main();
