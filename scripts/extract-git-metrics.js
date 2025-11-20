#!/usr/bin/env node

/**
 * Extract git history metrics for PureScript modules
 *
 * Generates metrics like:
 * - commitCount: total number of commits touching this file
 * - lastModified: days since last modification
 * - age: days since first commit
 * - authorCount: number of unique authors
 * - linesChanged: total lines added + deleted over lifetime
 * - recentCommits: commits in last 30 days
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

// Configuration
const SRC_DIR = 'src';
const OUTPUT_FILE = 'docs/data/module-metrics.json';

/**
 * Run a git command and return stdout
 */
function git(args) {
  try {
    return execSync(`git ${args}`, { encoding: 'utf8', maxBuffer: 10 * 1024 * 1024 });
  } catch (e) {
    return '';
  }
}

/**
 * Get all .purs files in the src directory
 */
function getPursFiles() {
  const result = execSync(`find ${SRC_DIR} -name "*.purs" -type f`, { encoding: 'utf8' });
  return result.trim().split('\n').filter(f => f);
}

/**
 * Extract metrics for a single file
 */
function getFileMetrics(filePath) {
  // Get commit count
  const logOutput = git(`log --oneline --follow -- "${filePath}"`);
  const commits = logOutput.trim().split('\n').filter(l => l);
  const commitCount = commits.length;

  if (commitCount === 0) {
    // File has no git history (untracked or new)
    return null;
  }

  // Get last modified date
  const lastModifiedStr = git(`log -1 --format="%ci" -- "${filePath}"`).trim();
  const lastModified = lastModifiedStr ? new Date(lastModifiedStr) : new Date();
  const daysSinceModified = Math.floor((Date.now() - lastModified.getTime()) / (1000 * 60 * 60 * 24));

  // Get first commit date (age)
  const firstCommitStr = git(`log --follow --format="%ci" -- "${filePath}" | tail -1`).trim();
  const firstCommit = firstCommitStr ? new Date(firstCommitStr) : new Date();
  const ageInDays = Math.floor((Date.now() - firstCommit.getTime()) / (1000 * 60 * 60 * 24));

  // Get unique authors
  const authorsOutput = git(`log --follow --format="%an" -- "${filePath}"`);
  const authors = [...new Set(authorsOutput.trim().split('\n').filter(a => a))];
  const authorCount = authors.length;

  // Get lines changed (total additions + deletions)
  const numstatOutput = git(`log --follow --numstat --format="" -- "${filePath}"`);
  let linesAdded = 0;
  let linesDeleted = 0;
  numstatOutput.trim().split('\n').forEach(line => {
    const match = line.match(/^(\d+)\s+(\d+)/);
    if (match) {
      linesAdded += parseInt(match[1], 10) || 0;
      linesDeleted += parseInt(match[2], 10) || 0;
    }
  });
  const linesChanged = linesAdded + linesDeleted;

  // Get recent commits (last 30 days)
  const thirtyDaysAgo = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
  const recentOutput = git(`log --oneline --since="${thirtyDaysAgo}" --follow -- "${filePath}"`);
  const recentCommits = recentOutput.trim().split('\n').filter(l => l).length;

  // Get current file size (lines of code)
  let lineCount = 0;
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    lineCount = content.split('\n').length;
  } catch (e) {
    // File might not exist on disk (deleted)
  }

  return {
    commitCount,
    daysSinceModified,
    ageInDays,
    authorCount,
    linesChanged,
    recentCommits,
    lineCount,
    authors
  };
}

/**
 * Convert file path to module name
 * e.g., "src/website/Component/Example.purs" -> "Component.Example"
 */
function pathToModuleName(filePath) {
  // Remove src/website/ or src/lib/ prefix and .purs suffix
  let modulePath = filePath
    .replace(/^src\/website\//, '')
    .replace(/^src\/lib\//, '')
    .replace(/\.purs$/, '');

  // Convert path separators to dots
  return modulePath.replace(/\//g, '.');
}

/**
 * Main function
 */
function main() {
  console.log('📊 Extracting git metrics for PureScript modules...\n');

  const files = getPursFiles();
  console.log(`Found ${files.length} .purs files\n`);

  const metrics = {};
  let processed = 0;
  let skipped = 0;

  for (const file of files) {
    const fileMetrics = getFileMetrics(file);

    if (fileMetrics) {
      const moduleName = pathToModuleName(file);
      metrics[moduleName] = {
        path: file,
        ...fileMetrics
      };
      processed++;
    } else {
      skipped++;
    }

    // Progress indicator
    if ((processed + skipped) % 50 === 0) {
      process.stdout.write(`  Processed ${processed + skipped}/${files.length} files\r`);
    }
  }

  console.log(`\n✅ Processed ${processed} files (${skipped} skipped - no git history)\n`);

  // Calculate normalized values (0-1 scale) for easy color mapping
  const allMetrics = Object.values(metrics);

  const maxCommits = Math.max(...allMetrics.map(m => m.commitCount));
  const maxAge = Math.max(...allMetrics.map(m => m.ageInDays));
  const maxRecency = Math.max(...allMetrics.map(m => m.daysSinceModified));
  const maxAuthors = Math.max(...allMetrics.map(m => m.authorCount));
  const maxChurn = Math.max(...allMetrics.map(m => m.linesChanged));
  const maxLines = Math.max(...allMetrics.map(m => m.lineCount));
  const maxRecent = Math.max(...allMetrics.map(m => m.recentCommits));

  // Add normalized values
  for (const moduleName of Object.keys(metrics)) {
    const m = metrics[moduleName];
    m.normalized = {
      commits: maxCommits > 0 ? m.commitCount / maxCommits : 0,
      age: maxAge > 0 ? m.ageInDays / maxAge : 0,
      recency: maxRecency > 0 ? 1 - (m.daysSinceModified / maxRecency) : 1, // Invert so recent = 1
      authors: maxAuthors > 0 ? m.authorCount / maxAuthors : 0,
      churn: maxChurn > 0 ? m.linesChanged / maxChurn : 0,
      size: maxLines > 0 ? m.lineCount / maxLines : 0,
      recentActivity: maxRecent > 0 ? m.recentCommits / maxRecent : 0
    };
  }

  // Summary statistics
  console.log('📈 Summary:');
  console.log(`  Max commits on single file: ${maxCommits}`);
  console.log(`  Oldest file: ${maxAge} days`);
  console.log(`  Most authors on single file: ${maxAuthors}`);
  console.log(`  Most churn (lines changed): ${maxChurn}`);
  console.log(`  Largest file: ${maxLines} lines`);
  console.log(`  Most recent commits (30 days): ${maxRecent}`);

  // Write output
  const output = {
    generated: new Date().toISOString(),
    fileCount: processed,
    maxValues: {
      commits: maxCommits,
      age: maxAge,
      recency: maxRecency,
      authors: maxAuthors,
      churn: maxChurn,
      size: maxLines,
      recentActivity: maxRecent
    },
    modules: metrics
  };

  fs.writeFileSync(OUTPUT_FILE, JSON.stringify(output, null, 2));
  console.log(`\n✨ Written to ${OUTPUT_FILE}`);
}

main();
