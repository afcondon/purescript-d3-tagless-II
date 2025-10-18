#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

// Recursively find all .purs files
function findPursFiles(dir, fileList = []) {
  const files = fs.readdirSync(dir);

  files.forEach(file => {
    const filePath = path.join(dir, file);
    const stat = fs.statSync(filePath);

    if (stat.isDirectory()) {
      findPursFiles(filePath, fileList);
    } else if (file.endsWith('.purs')) {
      fileList.push(filePath);
    }
  });

  return fileList;
}

// Extract snippets from a file
function extractSnippets(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const lines = content.split('\n');
  const snippets = [];

  let inSnippet = false;
  let snippetLines = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.includes('Snippet_Start')) {
      inSnippet = true;
      snippetLines = [];
      continue;
    }

    if (line.includes('Snippet_End')) {
      if (snippetLines.length > 0) {
        snippets.push(snippetLines.join('\n') + '\n');
      }
      inSnippet = false;
      snippetLines = [];
      continue;
    }

    if (inSnippet) {
      // Skip the Name: line if present, just collect the code
      if (!line.includes('Name:')) {
        snippetLines.push(line);
      }
    }
  }

  return snippets;
}

// Main execution
function main() {
  const srcDir = path.join(__dirname, '../../src/DemoApp');
  const outputDir = path.join(__dirname, '../../docs/code-examples');

  // Ensure output directory exists
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  // Find all .purs files
  const pursFiles = findPursFiles(srcDir);
  console.log(`Found ${pursFiles.length} .purs files`);

  // Extract and save snippets
  let totalSnippets = 0;
  pursFiles.forEach(file => {
    const snippets = extractSnippets(file);
    const snippetCount = snippets.length;

    if (snippetCount > 0) {
      // Get the base filename without extension
      const fileName = path.basename(file, '.purs');
      console.log(`${file}: ${snippetCount} snippet(s)`);

      snippets.forEach((content, index) => {
        // Generate numbered filename: ThreeLittleCircles-1.purs, ThreeLittleCircles-2.purs, etc.
        const snippetNumber = index + 1;
        const outputFileName = snippetCount === 1
          ? `${fileName}.purs`
          : `${fileName}-${snippetNumber}.purs`;
        const outputPath = path.join(outputDir, outputFileName);
        fs.writeFileSync(outputPath, content, 'utf8');
        console.log(`  - ${outputFileName}`);
        totalSnippets++;
      });
    }
  });

  console.log(`\nExtracted ${totalSnippets} snippet(s) to ${outputDir}`);
}

main();
