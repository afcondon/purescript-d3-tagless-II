#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

/**
 * Extract Code Snippets Script
 *
 * Scans PureScript files for snippet directives and extracts code from source files.
 * Generates a CodeSnippets.purs module with all extracted snippets.
 *
 * Directive format in comments:
 * -- SNIPPET: snippetName src/path/to/file.purs 10-25
 *
 * Or in triple-quote strings:
 * """
 * SNIPPET: snippetName src/path/to/file.purs 10-25
 * """
 */

const PROJECT_ROOT = path.resolve(__dirname, '..');
const SRC_DIR = path.join(PROJECT_ROOT, 'src');
const WEBSITE_DIR = path.join(SRC_DIR, 'website');
const OUTPUT_FILE = path.join(SRC_DIR, 'website', 'HTML', 'CodeSnippets.purs');
const HASHES_FILE = path.join(PROJECT_ROOT, '.snippet-hashes.json');

// Regex to match snippet directives (not in doc comments)
const SNIPPET_REGEX = /^(?!.*--\s*\|).*SNIPPET:\s*(\w+)\s+([\w\/.]+)\s+(\d+)-(\d+)/gm;

class SnippetExtractor {
  constructor() {
    this.snippets = new Map();
    this.hashes = this.loadHashes();
    this.warnings = [];
  }

  loadHashes() {
    try {
      if (fs.existsSync(HASHES_FILE)) {
        return JSON.parse(fs.readFileSync(HASHES_FILE, 'utf8'));
      }
    } catch (err) {
      console.warn('Could not load snippet hashes:', err.message);
    }
    return {};
  }

  saveHashes() {
    try {
      fs.writeFileSync(HASHES_FILE, JSON.stringify(this.hashes, null, 2));
    } catch (err) {
      console.warn('Could not save snippet hashes:', err.message);
    }
  }

  // Extract snippets from a single file
  extractFromFile(filePath) {
    const content = fs.readFileSync(filePath, 'utf8');
    const matches = Array.from(content.matchAll(SNIPPET_REGEX));

    matches.forEach(match => {
      const [, name, sourcePath, startLine, endLine] = match;
      const fullSourcePath = path.join(PROJECT_ROOT, sourcePath);

      if (!fs.existsSync(fullSourcePath)) {
        this.warnings.push(`Warning: Source file not found: ${sourcePath} (referenced in ${filePath})`);
        return;
      }

      const snippet = this.extractLines(fullSourcePath, parseInt(startLine), parseInt(endLine));
      const hash = this.hashContent(snippet);

      // Check if snippet has changed
      if (this.hashes[name] && this.hashes[name] !== hash) {
        this.warnings.push(`Warning: Snippet '${name}' source has changed since last extraction`);
      }

      this.snippets.set(name, {
        name,
        content: snippet,
        source: sourcePath,
        lines: `${startLine}-${endLine}`,
        hash
      });

      this.hashes[name] = hash;
    });
  }

  // Extract specific lines from a file and compact them
  extractLines(filePath, startLine, endLine) {
    const content = fs.readFileSync(filePath, 'utf8');
    const lines = content.split('\n');

    // Line numbers are 1-indexed, array is 0-indexed
    const extracted = lines.slice(startLine - 1, endLine);

    // Filter out documentation comments and excessive blank lines
    const compacted = [];
    let lastWasBlank = false;

    for (const line of extracted) {
      const trimmed = line.trim();

      // Skip PureScript doc comments (-- |)
      if (trimmed.startsWith('-- |')) {
        continue;
      }

      // Skip example code blocks in comments (-- | ```)
      if (trimmed === '-- | ```purescript' || trimmed === '-- | ```') {
        continue;
      }

      // Keep at most one consecutive blank line
      if (trimmed.length === 0) {
        if (!lastWasBlank) {
          compacted.push(line);
          lastWasBlank = true;
        }
        continue;
      }

      compacted.push(line);
      lastWasBlank = false;
    }

    // Find minimum indentation to normalize
    const minIndent = compacted
      .filter(line => line.trim().length > 0)
      .reduce((min, line) => {
        const match = line.match(/^(\s*)/);
        const indent = match ? match[1].length : 0;
        return Math.min(min, indent);
      }, Infinity);

    // Remove common indentation
    const normalized = compacted.map(line =>
      line.substring(Math.min(minIndent, line.length))
    );

    // Trim trailing blank lines
    while (normalized.length > 0 && normalized[normalized.length - 1].trim() === '') {
      normalized.pop();
    }

    return normalized.join('\n');
  }

  // Hash content for change detection
  hashContent(content) {
    return crypto.createHash('md5').update(content).digest('hex').substring(0, 8);
  }

  // Scan all website files for snippet directives
  scanFiles() {
    const scan = (dir) => {
      const entries = fs.readdirSync(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = path.join(dir, entry.name);

        if (entry.isDirectory()) {
          scan(fullPath);
        } else if (entry.isFile() && entry.name.endsWith('.purs')) {
          try {
            this.extractFromFile(fullPath);
          } catch (err) {
            console.error(`Error processing ${fullPath}:`, err.message);
          }
        }
      }
    };

    scan(WEBSITE_DIR);
  }

  // Generate the CodeSnippets.purs module
  generateModule() {
    const snippetArray = Array.from(this.snippets.values()).sort((a, b) =>
      a.name.localeCompare(b.name)
    );

    // Generate top-level string constants for each snippet to avoid parser issues
    const snippetConstants = snippetArray.map(s => {
      // Escape for PureScript string literal
      const escaped = s.content
        .replace(/\\/g, '\\\\')   // Escape backslashes first!
        .replace(/"/g, '\\"')      // Escape double quotes
        .replace(/\n/g, '\\n')     // Escape newlines
        .replace(/\r/g, '\\r')     // Escape carriage returns
        .replace(/\t/g, '\\t');    // Escape tabs

      return `snippet_${s.name}_content :: String\nsnippet_${s.name}_content = "${escaped}"`;
    }).join('\n\n');

    const moduleContent = `-- | Auto-generated module containing code snippets
-- | Generated by scripts/extract-snippets.js
-- | DO NOT EDIT THIS FILE MANUALLY
module CodeSnippets where

import Prelude

-- | Snippet metadata
type SnippetInfo =
  { name :: String
  , content :: String
  , source :: String
  , lines :: String
  }

-- ** Snippet Content Constants **

${snippetConstants}

-- | All available snippets
snippets :: Array SnippetInfo
snippets =
${snippetArray.length === 0 ? '  []' : '  [ ' + snippetArray.map(s => `{ name: "${s.name}"
    , content: snippet_${s.name}_content
    , source: "${s.source}"
    , lines: "${s.lines}"
    }`).join('\n  , ') + '\n  ]'}

-- | Look up a snippet by name
getSnippet :: String -> String
getSnippet name = case name of
${snippetArray.map(s => `  "${s.name}" -> snippet_${s.name}_content`).join('\n')}
  _ -> "Snippet not found: " <> name

-- | Get snippet info by name
getSnippetInfo :: String -> SnippetInfo
getSnippetInfo name = case name of
${snippetArray.map(s => `  "${s.name}" ->
    { name: "${s.name}"
    , content: snippet_${s.name}_content
    , source: "${s.source}"
    , lines: "${s.lines}"
    }`).join('\n')}
  _ ->
    { name: "not-found"
    , content: "Snippet not found: " <> name
    , source: ""
    , lines: ""
    }
`;

    return moduleContent;
  }

  // Main execution
  run() {
    console.log('🔍 Scanning for snippet directives...');
    this.scanFiles();

    console.log(`✅ Found ${this.snippets.size} snippet(s)`);

    if (this.warnings.length > 0) {
      console.log('\n⚠️  Warnings:');
      this.warnings.forEach(w => console.log(`   ${w}`));
    }

    console.log('\n📝 Generating CodeSnippets.purs...');
    const moduleContent = this.generateModule();
    fs.writeFileSync(OUTPUT_FILE, moduleContent);

    this.saveHashes();

    console.log(`✨ Done! Generated ${OUTPUT_FILE}`);

    if (this.snippets.size > 0) {
      console.log('\nExtracted snippets:');
      Array.from(this.snippets.values()).forEach(s => {
        console.log(`  - ${s.name} (${s.source} lines ${s.lines})`);
      });
    }
  }
}

// Run the extractor
const extractor = new SnippetExtractor();
extractor.run();
