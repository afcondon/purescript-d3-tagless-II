#!/usr/bin/env node
/**
 * Populate source_code column in declarations table
 *
 * Reads source files and extracts code for each declaration based on source_span
 *
 * Usage:
 *   node populate-source-code.js [--project <name>] [--snapshot <id>]
 */

import duckdb from 'duckdb';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const DB_PATH = path.join(__dirname, '..', 'ce-data.duckdb');

// =============================================================================
// Database Helpers
// =============================================================================

function openDB() {
  return new Promise((resolve, reject) => {
    const db = new duckdb.Database(DB_PATH, (err) => {
      if (err) reject(err);
      else resolve(db);
    });
  });
}

function query(db, sql, ...params) {
  return new Promise((resolve, reject) => {
    if (params.length > 0) {
      db.all(sql, ...params, (err, rows) => {
        if (err) reject(err);
        else resolve(rows || []);
      });
    } else {
      db.all(sql, (err, rows) => {
        if (err) reject(err);
        else resolve(rows || []);
      });
    }
  });
}

function run(db, sql, ...params) {
  return new Promise((resolve, reject) => {
    if (params.length > 0) {
      db.run(sql, ...params, (err) => {
        if (err) reject(err);
        else resolve();
      });
    } else {
      db.exec(sql, (err) => {
        if (err) reject(err);
        else resolve();
      });
    }
  });
}

function closeDB(db) {
  return new Promise((resolve, reject) => {
    db.close((err) => {
      if (err) reject(err);
      else resolve();
    });
  });
}

// =============================================================================
// Source Code Extraction
// =============================================================================

function extractSourceCode(filePath, sourceSpan, repoPath) {
  try {
    if (!sourceSpan || !sourceSpan.name) {
      return null;
    }

    const fullPath = path.join(repoPath, sourceSpan.name);

    if (!fs.existsSync(fullPath)) {
      console.warn(`  ⚠️  File not found: ${fullPath}`);
      return null;
    }

    const fileContent = fs.readFileSync(fullPath, 'utf8');
    const lines = fileContent.split('\n');

    // source_span has {name, start: [line, col], end: [line, col]}
    const startLine = sourceSpan.start[0] - 1; // Convert to 0-indexed
    const endLine = sourceSpan.end[0] - 1;

    if (startLine < 0 || endLine >= lines.length) {
      console.warn(`  ⚠️  Invalid line range: ${startLine}-${endLine} in ${sourceSpan.name}`);
      return null;
    }

    // Extract the lines
    const codeLines = lines.slice(startLine, endLine + 1);
    return codeLines.join('\n');
  } catch (error) {
    console.error(`  ❌ Error reading ${filePath}:`, error.message);
    return null;
  }
}

// =============================================================================
// Main Logic
// =============================================================================

async function populateSourceCode(projectName = null, snapshotId = null) {
  const db = await openDB();

  try {
    // Apply migration if needed
    console.log('📝 Applying migration...');
    try {
      await run(db, 'ALTER TABLE declarations ADD COLUMN IF NOT EXISTS source_code TEXT');
      console.log('✅ Migration applied');
    } catch (err) {
      if (err.message.includes('already exists')) {
        console.log('✅ Column already exists');
      } else {
        throw err;
      }
    }

    // Get snapshots to process
    let snapshotQuery = `
      SELECT s.id, s.project_id, p.name as project_name, p.repo_path
      FROM snapshots s
      JOIN projects p ON s.project_id = p.id
    `;
    const queryParams = [];

    if (projectName) {
      snapshotQuery += ' WHERE p.name = ?';
      queryParams.push(projectName);
    }

    if (snapshotId) {
      snapshotQuery += (projectName ? ' AND' : ' WHERE') + ' s.id = ?';
      queryParams.push(snapshotId);
    }

    const snapshots = await query(db, snapshotQuery, ...queryParams);

    if (snapshots.length === 0) {
      console.log('❌ No snapshots found');
      return;
    }

    console.log(`\n📊 Processing ${snapshots.length} snapshot(s)...\n`);

    for (const snapshot of snapshots) {
      console.log(`📦 Snapshot ${snapshot.id}: ${snapshot.project_name}`);
      console.log(`   Repo: ${snapshot.repo_path}`);

      // Get declarations with source_span
      const declarations = await query(
        db,
        `SELECT id, module, title, source_span
         FROM declarations
         WHERE snapshot_id = ? AND source_span IS NOT NULL`,
        snapshot.id
      );

      console.log(`   Found ${declarations.length} declarations\n`);

      let updated = 0;
      let skipped = 0;

      for (const decl of declarations) {
        const sourceSpan = JSON.parse(decl.source_span);
        const sourceCode = extractSourceCode(decl.module, sourceSpan, snapshot.repo_path);

        if (sourceCode) {
          await run(
            db,
            'UPDATE declarations SET source_code = ? WHERE id = ?',
            sourceCode,
            decl.id
          );
          updated++;

          if (updated % 100 === 0) {
            console.log(`   ✓ Updated ${updated}/${declarations.length}...`);
          }
        } else {
          skipped++;
        }
      }

      console.log(`\n   ✅ Snapshot ${snapshot.id} complete:`);
      console.log(`      Updated: ${updated}`);
      console.log(`      Skipped: ${skipped}\n`);
    }

    console.log('🎉 All done!');
  } catch (error) {
    console.error('❌ Error:', error);
    throw error;
  } finally {
    await closeDB(db);
  }
}

// =============================================================================
// CLI
// =============================================================================

const args = process.argv.slice(2);
const options = {};

for (let i = 0; i < args.length; i++) {
  if (args[i].startsWith('--')) {
    const key = args[i].slice(2);
    const value = args[i + 1] && !args[i + 1].startsWith('--') ? args[++i] : true;
    options[key] = value;
  }
}

populateSourceCode(options.project, options.snapshot).catch((err) => {
  console.error('Fatal error:', err);
  process.exit(1);
});
