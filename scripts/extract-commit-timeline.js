#!/usr/bin/env node

/**
 * Extract commit timeline for git replay visualization
 *
 * Generates a chronological list of commits with affected modules,
 * distinguishing between newly created files and modifications.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

// Configuration
const OUTPUT_FILE = 'docs/data/commit-timeline.json';
const SRC_DIR = 'src';

/**
 * Run a git command and return stdout
 */
function git(args) {
  try {
    return execSync(`git ${args}`, { encoding: 'utf8', maxBuffer: 50 * 1024 * 1024 });
  } catch (e) {
    return '';
  }
}

/**
 * Extract actual module name from PureScript file content
 */
function getModuleNameFromFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    const match = content.match(/^module\s+([\w.]+)\s+where/m);
    if (match) {
      return match[1];
    }
  } catch (e) {
    // File might not exist (deleted in later commits)
  }

  // Fallback: derive from path
  let modulePath = filePath
    .replace(/^src\/website\//, '')
    .replace(/^src\/lib\//, '')
    .replace(/\.purs$/, '');
  return modulePath.replace(/\//g, '.');
}

/**
 * Get module name from file path, using current file content or git history
 */
function getModuleName(filePath, commitHash) {
  // First try current file
  const currentName = getModuleNameFromFile(filePath);
  if (currentName && !currentName.includes('/')) {
    return currentName;
  }

  // If file doesn't exist now, try to get content from the commit
  try {
    const content = execSync(`git show ${commitHash}:${filePath}`, { encoding: 'utf8' });
    const match = content.match(/^module\s+([\w.]+)\s+where/m);
    if (match) {
      return match[1];
    }
  } catch (e) {
    // File might not exist at that commit
  }

  // Fallback to path-based name
  let modulePath = filePath
    .replace(/^src\/website\//, '')
    .replace(/^src\/lib\//, '')
    .replace(/\.purs$/, '');
  return modulePath.replace(/\//g, '.');
}

/**
 * Main function
 */
function main() {
  console.log('🎬 Extracting commit timeline for git replay...\n');

  // Get all commits in chronological order (oldest first)
  // Format: hash|timestamp|author|subject
  const logOutput = git('log --reverse --format="%H|%at|%an|%s" -- "src/**/*.purs"');
  const commitLines = logOutput.trim().split('\n').filter(l => l);

  console.log(`Found ${commitLines.length} commits affecting .purs files\n`);

  // Track when each module was first seen (to detect creation vs modification)
  const moduleFirstSeen = new Map();

  // Build timeline
  const timeline = [];
  let processed = 0;

  for (const line of commitLines) {
    const [hash, timestamp, author, ...subjectParts] = line.split('|');
    const subject = subjectParts.join('|'); // In case subject contains |

    // Get files affected by this commit
    const diffOutput = git(`diff-tree --no-commit-id --name-status -r ${hash}`);
    const fileChanges = diffOutput.trim().split('\n').filter(l => l);

    const created = [];
    const modified = [];
    const deleted = [];

    for (const change of fileChanges) {
      const [status, ...pathParts] = change.split('\t');
      const filePath = pathParts.join('\t');

      // Only process .purs files in src/
      if (!filePath.endsWith('.purs') || !filePath.startsWith('src/')) {
        continue;
      }

      const moduleName = getModuleName(filePath, hash);

      if (status === 'A') {
        // File added
        created.push(moduleName);
        moduleFirstSeen.set(moduleName, hash);
      } else if (status === 'D') {
        // File deleted
        deleted.push(moduleName);
      } else if (status === 'M' || status.startsWith('R')) {
        // File modified or renamed
        if (!moduleFirstSeen.has(moduleName)) {
          // First time seeing this module (might be from before our tracking started)
          moduleFirstSeen.set(moduleName, hash);
          created.push(moduleName);
        } else {
          modified.push(moduleName);
        }
      }
    }

    // Only include commits that affect modules
    if (created.length > 0 || modified.length > 0 || deleted.length > 0) {
      timeline.push({
        hash: hash.substring(0, 7), // Short hash
        timestamp: parseInt(timestamp, 10),
        date: new Date(parseInt(timestamp, 10) * 1000).toISOString().split('T')[0],
        author,
        subject: subject.substring(0, 80), // Truncate long subjects
        created,
        modified,
        deleted
      });
    }

    processed++;
    if (processed % 100 === 0) {
      process.stdout.write(`  Processed ${processed}/${commitLines.length} commits\r`);
    }
  }

  console.log(`\n✅ Processed ${processed} commits, ${timeline.length} with module changes\n`);

  // Get list of all modules that ever existed
  const allModules = [...moduleFirstSeen.keys()].sort();

  // Summary
  console.log('📈 Summary:');
  console.log(`  Total commits with .purs changes: ${timeline.length}`);
  console.log(`  Total modules tracked: ${allModules.length}`);
  console.log(`  Date range: ${timeline[0]?.date || 'N/A'} to ${timeline[timeline.length - 1]?.date || 'N/A'}`);

  // Write output
  const output = {
    generated: new Date().toISOString(),
    commitCount: timeline.length,
    moduleCount: allModules.length,
    dateRange: {
      start: timeline[0]?.date || null,
      end: timeline[timeline.length - 1]?.date || null
    },
    modules: allModules,
    timeline
  };

  fs.writeFileSync(OUTPUT_FILE, JSON.stringify(output, null, 2));
  console.log(`\n✨ Written to ${OUTPUT_FILE}`);

  // Show sample
  console.log('\n📝 Sample commits:');
  for (let i = 0; i < Math.min(3, timeline.length); i++) {
    const c = timeline[i];
    console.log(`  ${c.date} ${c.hash}: ${c.subject.substring(0, 50)}...`);
    if (c.created.length) console.log(`    Created: ${c.created.slice(0, 3).join(', ')}${c.created.length > 3 ? '...' : ''}`);
    if (c.modified.length) console.log(`    Modified: ${c.modified.slice(0, 3).join(', ')}${c.modified.length > 3 ? '...' : ''}`);
  }
}

main();
