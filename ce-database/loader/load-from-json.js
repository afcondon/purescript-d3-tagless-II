#!/usr/bin/env node
/**
 * Load data from JSON files into DuckDB
 *
 * Reads from demo-website/public/data/spago-data/ and populates the database.
 */

import duckdb from 'duckdb';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const DB_PATH = path.join(__dirname, '..', 'ce-data.duckdb');
const DATA_DIR = path.join(__dirname, '..', '..', 'demo-website', 'public', 'data');
const SPAGO_DATA_DIR = path.join(DATA_DIR, 'spago-data');

console.log('Loading data into Code Explorer database...\n');
console.log(`Database: ${DB_PATH}`);
console.log(`Data directory: ${DATA_DIR}\n`);

// Helper to read JSON file
function readJson(filePath) {
  if (!fs.existsSync(filePath)) {
    console.warn(`  Warning: ${path.basename(filePath)} not found`);
    return null;
  }
  return JSON.parse(fs.readFileSync(filePath, 'utf8'));
}

// Open database
const db = new duckdb.Database(DB_PATH, async (err) => {
  if (err) {
    console.error('Failed to open database:', err);
    process.exit(1);
  }

  try {
    // =========================================================================
    // Load packages.json
    // =========================================================================
    console.log('Loading packages...');
    const packages = readJson(path.join(SPAGO_DATA_DIR, 'packages.json'));
    if (packages) {
      let count = 0;
      for (const [name, pkg] of Object.entries(packages)) {
        const depends = JSON.stringify(pkg.depends || []);
        db.run(
          'INSERT OR REPLACE INTO packages (name, depends) VALUES (?, ?)',
          name, depends
        );
        count++;
      }
      console.log(`  Loaded ${count} packages`);
    }

    // =========================================================================
    // Load modules.json + LOC.json
    // =========================================================================
    console.log('Loading modules...');
    const modules = readJson(path.join(SPAGO_DATA_DIR, 'modules.json'));
    const locData = readJson(path.join(SPAGO_DATA_DIR, 'LOC.json'));

    // Build LOC lookup map
    const locMap = new Map();
    if (locData && locData.loc) {
      for (const entry of locData.loc) {
        locMap.set(entry.path, entry.loc);
      }
    }

    if (modules) {
      let count = 0;
      for (const [name, mod] of Object.entries(modules)) {
        const depends = JSON.stringify(mod.depends || []);
        const loc = locMap.get(mod.path) || 0;
        db.run(
          'INSERT OR REPLACE INTO modules (name, package, path, depends, loc) VALUES (?, ?, ?, ?, ?)',
          name, mod.package, mod.path, depends, loc
        );
        count++;
      }
      console.log(`  Loaded ${count} modules`);
    }

    // =========================================================================
    // Load declarations.json
    // =========================================================================
    console.log('Loading declarations...');
    const declarations = readJson(path.join(SPAGO_DATA_DIR, 'declarations.json'));
    if (declarations && declarations.modules) {
      let count = 0;
      let id = 0;
      for (const [moduleName, moduleData] of Object.entries(declarations.modules)) {
        if (moduleData.declarations) {
          for (const decl of moduleData.declarations) {
            db.run(
              `INSERT OR REPLACE INTO declarations
               (id, module, title, kind, comments, source_span, type_signature)
               VALUES (?, ?, ?, ?, ?, ?, ?)`,
              id++,
              moduleName,
              decl.title,
              decl.kind,
              decl.comments || '',
              JSON.stringify(decl.sourceSpan || null),
              JSON.stringify(decl.typeSignature || null)
            );
            count++;
          }
        }
      }
      console.log(`  Loaded ${count} declarations`);
    }

    // =========================================================================
    // Load function-calls.json
    // =========================================================================
    console.log('Loading function calls...');
    const functionCalls = readJson(path.join(SPAGO_DATA_DIR, 'function-calls.json'));
    if (functionCalls && functionCalls.functions) {
      let count = 0;
      let id = 0;
      for (const [funcName, funcData] of Object.entries(functionCalls.functions)) {
        db.run(
          `INSERT OR REPLACE INTO function_calls
           (id, module, name, calls, called_by)
           VALUES (?, ?, ?, ?, ?)`,
          id++,
          funcData.module,
          funcData.name,
          JSON.stringify(funcData.calls || []),
          JSON.stringify(funcData.calledBy || [])
        );
        count++;
      }
      console.log(`  Loaded ${count} function call records`);
    }

    // =========================================================================
    // Load type-dependencies.json
    // =========================================================================
    console.log('Loading type dependencies...');
    const typeDeps = readJson(path.join(SPAGO_DATA_DIR, 'type-dependencies.json'));
    if (typeDeps && typeDeps.types) {
      let count = 0;
      let id = 0;
      for (const [typeName, typeData] of Object.entries(typeDeps.types)) {
        db.run(
          `INSERT OR REPLACE INTO type_dependencies
           (id, module, name, kind, used_by)
           VALUES (?, ?, ?, ?, ?)`,
          id++,
          typeData.module,
          typeData.name,
          typeData.kind,
          JSON.stringify(typeData.usedBy || [])
        );
        count++;
      }
      console.log(`  Loaded ${count} type dependency records`);
    }

    // =========================================================================
    // Load module-metrics.json
    // =========================================================================
    console.log('Loading module metrics...');
    const moduleMetrics = readJson(path.join(DATA_DIR, 'module-metrics.json'));
    if (moduleMetrics && moduleMetrics.modules) {
      let count = 0;
      for (const [moduleName, metrics] of Object.entries(moduleMetrics.modules)) {
        db.run(
          `INSERT OR REPLACE INTO module_metrics
           (module, path, commit_count, days_since_modified, age_in_days,
            author_count, lines_changed, recent_commits, line_count, authors, normalized)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
          moduleName,
          metrics.path,
          metrics.commitCount,
          metrics.daysSinceModified,
          metrics.ageInDays,
          metrics.authorCount,
          metrics.linesChanged,
          metrics.recentCommits,
          metrics.lineCount,
          JSON.stringify(metrics.authors || []),
          JSON.stringify(metrics.normalized || {})
        );
        count++;
      }
      console.log(`  Loaded ${count} module metric records`);
    }

    // =========================================================================
    // Load commit-timeline.json
    // =========================================================================
    console.log('Loading commit timeline...');
    const commitTimeline = readJson(path.join(DATA_DIR, 'commit-timeline.json'));
    if (commitTimeline && commitTimeline.timeline) {
      let count = 0;
      for (const commit of commitTimeline.timeline) {
        db.run(
          `INSERT OR REPLACE INTO commits
           (hash, timestamp, date, author, subject, created, modified, deleted)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
          commit.hash,
          commit.timestamp,
          commit.date,
          commit.author,
          commit.subject,
          JSON.stringify(commit.created || []),
          JSON.stringify(commit.modified || []),
          JSON.stringify(commit.deleted || [])
        );
        count++;
      }
      console.log(`  Loaded ${count} commits`);

      // Store timeline metadata
      db.run(
        `INSERT OR REPLACE INTO metadata (key, value) VALUES (?, ?)`,
        'commit_timeline_meta',
        JSON.stringify({
          generated: commitTimeline.generated,
          commitCount: commitTimeline.commitCount,
          moduleCount: commitTimeline.moduleCount,
          dateRange: commitTimeline.dateRange,
          modules: commitTimeline.modules
        })
      );
    }

    // =========================================================================
    // Update metadata
    // =========================================================================
    db.run(
      `INSERT OR REPLACE INTO metadata (key, value) VALUES (?, ?)`,
      'loaded_at',
      JSON.stringify(new Date().toISOString())
    );

    console.log('\nData loading complete!');

    db.close((err) => {
      if (err) {
        console.error('Failed to close database:', err);
        process.exit(1);
      }
      console.log('Database closed.');
    });

  } catch (e) {
    console.error('Error loading data:', e);
    process.exit(1);
  }
});
