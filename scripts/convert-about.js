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

// Generate TOC HTML with floating panel
const tocHtml = `
<div class="floating-panel toc-panel">
  <img src="bookmark.jpeg" alt="" class="toc-panel__bookmark-pin" />
  <div class="toc-panel__main">
    <div class="floating-panel__header">
      <h3 class="floating-panel__title">Contents</h3>
      <button class="floating-panel__toggle" type="button" onclick="this.closest('.floating-panel').classList.toggle('floating-panel--collapsed')">−</button>
    </div>
    <div class="floating-panel__content toc-panel__content">
      <nav class="toc-nav">
        ${toc.map(item => {
          const indent = item.level === 1 ? '' : item.level === 2 ? 'toc-nav__item--level-2' : 'toc-nav__item--level-3';
          return `<a href="#${item.id}" class="toc-nav__item ${indent}">${item.text}</a>`;
        }).join('\n        ')}
      </nav>
    </div>
  </div>
</div>
`;

fs.writeFileSync(TOC_PATH, tocHtml, 'utf8');

console.log(`✓ Converted ${MARKDOWN_PATH} to ${OUTPUT_PATH}`);
console.log(`✓ Generated TOC with ${toc.length} headings at ${TOC_PATH}`);
