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
  let snippetName = null;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.includes('Snippet_Start')) {
      inSnippet = true;
      snippetLines = [];
      snippetName = null;
      continue;
    }

    if (line.includes('Snippet_End')) {
      if (snippetLines.length > 0) {
        snippets.push({
          name: snippetName,
          content: snippetLines.join('\n') + '\n'
        });
      }
      inSnippet = false;
      snippetLines = [];
      snippetName = null;
      continue;
    }

    if (inSnippet) {
      // Extract snippet name from Name: line
      if (line.includes('Name:')) {
        const nameMatch = line.match(/Name:\s*(.+)/);
        if (nameMatch) {
          snippetName = nameMatch[1].trim();
        }
      } else {
        snippetLines.push(line);
      }
    }
  }

  return snippets;
}

// Main execution
function main() {
  const srcDir = path.join(__dirname, '../../src/website/Viz');
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

      snippets.forEach((snippet, index) => {
        // Use snippet name if available, otherwise use numbered filename
        let outputFileName;
        if (snippet.name) {
          outputFileName = `${snippet.name}.purs`;
        } else {
          const snippetNumber = index + 1;
          outputFileName = snippetCount === 1
            ? `${fileName}.purs`
            : `${fileName}-${snippetNumber}.purs`;
        }

        const outputPath = path.join(outputDir, outputFileName);
        fs.writeFileSync(outputPath, snippet.content, 'utf8');
        console.log(`  - ${outputFileName}`);
        totalSnippets++;
      });
    }
  });

  console.log(`\nExtracted ${totalSnippets} snippet(s) to ${outputDir}`);
}

main();
