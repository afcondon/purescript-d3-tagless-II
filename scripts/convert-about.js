#!/usr/bin/env node

/**
 * Convert notes/ABOUT.md to docs/about-content.html and generate TOC
 * This runs automatically during the bundle process
 */

const fs = require('fs');
const path = require('path');
const { marked } = require('marked');

const MARKDOWN_PATH = path.join(__dirname, '../notes/ABOUT.md');
const OUTPUT_PATH = path.join(__dirname, '../docs/about-content.html');
const TOC_PATH = path.join(__dirname, '../docs/about-toc.html');

// Read the markdown file
const markdown = fs.readFileSync(MARKDOWN_PATH, 'utf8');

// Extract TOC by parsing headings
const toc = [];
let headingId = 0;

// Custom renderer to add IDs to headings and build TOC
const renderer = {
  heading({ text, depth }) {
    const id = `heading-${headingId++}`;
    const textContent = typeof text === 'string' ? text : text.toString();

    // Add to TOC if it's h1, h2, or h3
    if (depth <= 3) {
      toc.push({
        id: id,
        text: textContent,
        level: depth
      });
    }

    return `<h${depth} id="${id}">${textContent}</h${depth}>\n`;
  }
};

marked.use({ renderer });

// Convert to HTML
const html = marked.parse(markdown);

// Write the HTML
fs.writeFileSync(OUTPUT_PATH, html, 'utf8');

// HTML escape function
function escapeHtml(text) {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}

// Generate TOC HTML - just the links (Halogen renders the structure)
const tocHtml = toc.map(item => {
  const indent = item.level === 1 ? '' : item.level === 2 ? 'toc-nav__item--level-2' : 'toc-nav__item--level-3';
  return `<a href="#${item.id}" class="toc-nav__item ${indent}">${escapeHtml(item.text)}</a>`;
}).join('\n');

fs.writeFileSync(TOC_PATH, tocHtml, 'utf8');

console.log(`✓ Converted ${MARKDOWN_PATH} to ${OUTPUT_PATH}`);
console.log(`✓ Generated TOC with ${toc.length} headings at ${TOC_PATH}`);
