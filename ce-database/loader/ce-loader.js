#!/usr/bin/env node
/**
 * Code Explorer Loader CLI
 *
 * Loads PureScript project data into the Code Explorer database.
 *
 * Usage:
 *   ce-loader add-project --name <name> --repo <path>
 *   ce-loader snapshot --project <name> [--label <label>] [--ref <git-ref>]
 *   ce-loader list-projects
 *   ce-loader list-snapshots --project <name>
 *
 * Examples:
 *   ce-loader add-project --name psd3 --repo /path/to/psd3
 *   ce-loader snapshot --project psd3 --label "v1.0.0"
 *   ce-loader snapshot --project psd3 --ref HEAD
 */

import duckdb from 'duckdb';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const DB_PATH = path.join(__dirname, '..', 'ce-data.duckdb');

// =============================================================================
// CLI Argument Parsing
// =============================================================================

function parseArgs(args) {
  const result = { command: null, options: {} };

  if (args.length === 0) {
    return result;
  }

  result.command = args[0];

  for (let i = 1; i < args.length; i++) {
    if (args[i].startsWith('--')) {
      const key = args[i].slice(2);
      const value = args[i + 1] && !args[i + 1].startsWith('--') ? args[++i] : true;
      result.options[key] = value;
    }
  }

  return result;
}

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
// Git Helpers
// =============================================================================

function gitCommand(repoPath, cmd) {
  try {
    return execSync(`git -C "${repoPath}" ${cmd}`, { encoding: 'utf8' }).trim();
  } catch (e) {
    return null;
  }
}

function getGitHash(repoPath, ref = 'HEAD') {
  return gitCommand(repoPath, `rev-parse ${ref}`);
}

function getGitTimestamp(repoPath, ref = 'HEAD') {
  const ts = gitCommand(repoPath, `log -1 --format=%ct ${ref}`);
  return ts ? parseInt(ts, 10) : null;
}

function getGitRef(repoPath) {
  // Try to get tag name first
  const tag = gitCommand(repoPath, 'describe --tags --exact-match HEAD 2>/dev/null');
  if (tag) return tag;

  // Fall back to branch name
  const branch = gitCommand(repoPath, 'rev-parse --abbrev-ref HEAD');
  return branch || 'HEAD';
}

// =============================================================================
// Data Extraction
// =============================================================================

function extractModulesFromSpago(repoPath) {
  // Run spago to get module graph
  const output = execSync(`cd "${repoPath}" && spago graph modules --json 2>/dev/null`, {
    encoding: 'utf8',
    maxBuffer: 50 * 1024 * 1024
  });
  return JSON.parse(output);
}

function extractPackagesFromSpago(repoPath) {
  // Run spago to get package list
  // Note: spago outputs info messages before JSON, so we need to find the JSON part
  const output = execSync(`cd "${repoPath}" && spago ls packages --json 2>/dev/null`, {
    encoding: 'utf8',
    maxBuffer: 10 * 1024 * 1024
  });

  // Find the start of JSON (first '{')
  const jsonStart = output.indexOf('{');
  if (jsonStart === -1) {
    throw new Error('No JSON found in spago output');
  }
  const jsonStr = output.slice(jsonStart);
  return JSON.parse(jsonStr);
}

function countLinesInFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    return content.split('\n').length;
  } catch {
    return 0;
  }
}

function extractDeclarationsFromDocs(repoPath) {
  // Look for docs.json files in output directory
  const outputDir = path.join(repoPath, 'output');
  const declarations = {};

  if (!fs.existsSync(outputDir)) {
    console.log('  Warning: No output directory found. Run `spago build` first.');
    return declarations;
  }

  const moduleDirs = fs.readdirSync(outputDir).filter(d => {
    const docsPath = path.join(outputDir, d, 'docs.json');
    return fs.existsSync(docsPath);
  });

  for (const moduleDir of moduleDirs) {
    const docsPath = path.join(outputDir, moduleDir, 'docs.json');
    try {
      const docs = JSON.parse(fs.readFileSync(docsPath, 'utf8'));
      if (docs.name && docs.declarations) {
        declarations[docs.name] = docs.declarations.map(d => ({
          title: d.title,
          kind: d.info?.declType || d.declType || 'unknown',
          comments: d.comments || '',
          sourceSpan: d.sourceSpan || null,
          typeSignature: d.info?.type || d.type || null
        }));
      }
    } catch (e) {
      // Skip malformed docs.json
    }
  }

  return declarations;
}

// =============================================================================
// Commands
// =============================================================================

async function addProject(options) {
  const { name, repo } = options;

  if (!name) {
    console.error('Error: --name is required');
    process.exit(1);
  }

  const repoPath = repo ? path.resolve(repo) : process.cwd();

  if (!fs.existsSync(repoPath)) {
    console.error(`Error: Repository path does not exist: ${repoPath}`);
    process.exit(1);
  }

  const db = await openDB();

  try {
    // Check if project already exists
    const existing = await query(db, 'SELECT id FROM projects WHERE name = ?', name);
    if (existing.length > 0) {
      console.log(`Project "${name}" already exists (id: ${existing[0].id})`);
      return;
    }

    // Get next ID
    const maxId = await query(db, 'SELECT COALESCE(MAX(id), 0) as max_id FROM projects');
    const newId = maxId[0].max_id + 1;

    // Insert project
    await run(db, 'INSERT INTO projects (id, name, repo_path) VALUES (?, ?, ?)',
      newId, name, repoPath);

    console.log(`Created project "${name}" (id: ${newId})`);
    console.log(`  Repository: ${repoPath}`);

  } finally {
    await closeDB(db);
  }
}

async function createSnapshot(options) {
  const { project, label, ref } = options;

  if (!project) {
    console.error('Error: --project is required');
    process.exit(1);
  }

  const db = await openDB();

  try {
    // Look up project
    const projects = await query(db, 'SELECT * FROM projects WHERE name = ?', project);
    if (projects.length === 0) {
      console.error(`Error: Project "${project}" not found`);
      process.exit(1);
    }
    const proj = projects[0];
    const repoPath = proj.repo_path;

    console.log(`Creating snapshot for project "${project}"...`);
    console.log(`  Repository: ${repoPath}`);

    // Get git info
    const gitRef = ref || getGitRef(repoPath);
    const gitHash = getGitHash(repoPath, gitRef);
    const gitTimestamp = getGitTimestamp(repoPath, gitRef);
    const snapshotAt = gitTimestamp ? new Date(gitTimestamp * 1000).toISOString() : new Date().toISOString();

    console.log(`  Git ref: ${gitRef}`);
    console.log(`  Git hash: ${gitHash}`);

    // Check for existing snapshot with same hash
    const existingSnapshot = await query(db,
      'SELECT id FROM snapshots WHERE project_id = ? AND git_hash = ?',
      proj.id, gitHash);
    if (existingSnapshot.length > 0) {
      console.log(`  Snapshot already exists for this commit (id: ${existingSnapshot[0].id})`);
      console.log('  Use a different --ref or commit new changes.');
      return;
    }

    // Create snapshot
    const maxSnapId = await query(db, 'SELECT COALESCE(MAX(id), 0) as max_id FROM snapshots');
    const snapshotId = maxSnapId[0].max_id + 1;

    await run(db,
      `INSERT INTO snapshots (id, project_id, git_hash, git_ref, label, snapshot_at)
       VALUES (?, ?, ?, ?, ?, ?)`,
      snapshotId, proj.id, gitHash, gitRef, label || null, snapshotAt);

    console.log(`  Created snapshot id: ${snapshotId}`);

    // Extract and load data
    console.log('\nExtracting data from codebase...');

    // Load modules
    console.log('  Loading modules...');
    try {
      const modulesData = extractModulesFromSpago(repoPath);
      let moduleCount = 0;
      let maxModId = (await query(db, 'SELECT COALESCE(MAX(id), 0) as max_id FROM modules'))[0].max_id;

      for (const [moduleName, moduleInfo] of Object.entries(modulesData)) {
        maxModId++;
        const modulePath = moduleInfo.path || '';
        const loc = modulePath ? countLinesInFile(path.join(repoPath, modulePath)) : 0;

        await run(db,
          `INSERT INTO modules (id, snapshot_id, name, package, path, depends, loc)
           VALUES (?, ?, ?, ?, ?, ?, ?)`,
          maxModId, snapshotId, moduleName,
          moduleInfo.package || 'unknown',
          modulePath,
          JSON.stringify(moduleInfo.depends || []),
          loc);
        moduleCount++;
      }
      console.log(`    Loaded ${moduleCount} modules`);
    } catch (e) {
      console.log(`    Warning: Could not extract modules: ${e.message}`);
    }

    // Load packages
    console.log('  Loading packages...');
    try {
      const packagesData = extractPackagesFromSpago(repoPath);
      let packageCount = 0;
      let maxPkgId = (await query(db, 'SELECT COALESCE(MAX(id), 0) as max_id FROM packages'))[0].max_id;

      // packagesData is an object: { "pkg-name": { type: "registry", value: { version: "x.y.z" } } }
      for (const [pkgName, pkgInfo] of Object.entries(packagesData)) {
        maxPkgId++;
        await run(db,
          `INSERT INTO packages (id, snapshot_id, name, depends)
           VALUES (?, ?, ?, ?)`,
          maxPkgId, snapshotId, pkgName,
          JSON.stringify([])); // spago ls packages doesn't include deps, would need spago.yaml parsing
        packageCount++;
      }
      console.log(`    Loaded ${packageCount} packages`);
    } catch (e) {
      console.log(`    Warning: Could not extract packages: ${e.message}`);
    }

    // Load declarations
    console.log('  Loading declarations...');
    try {
      const declarations = extractDeclarationsFromDocs(repoPath);
      let declCount = 0;
      let maxDeclId = (await query(db, 'SELECT COALESCE(MAX(id), 0) as max_id FROM declarations'))[0].max_id;

      for (const [moduleName, decls] of Object.entries(declarations)) {
        for (const decl of decls) {
          maxDeclId++;
          await run(db,
            `INSERT INTO declarations (id, snapshot_id, module, title, kind, comments, source_span, type_signature)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
            maxDeclId, snapshotId, moduleName,
            decl.title, decl.kind, decl.comments || '',
            JSON.stringify(decl.sourceSpan),
            JSON.stringify(decl.typeSignature));
          declCount++;
        }
      }
      console.log(`    Loaded ${declCount} declarations`);
    } catch (e) {
      console.log(`    Warning: Could not extract declarations: ${e.message}`);
    }

    console.log('\nSnapshot complete!');

  } finally {
    await closeDB(db);
  }
}

async function listProjects() {
  const db = await openDB();

  try {
    const projects = await query(db, `
      SELECT p.*, COUNT(s.id) as snapshot_count
      FROM projects p
      LEFT JOIN snapshots s ON p.id = s.project_id
      GROUP BY p.id
      ORDER BY p.name
    `);

    if (projects.length === 0) {
      console.log('No projects found. Use `ce-loader add-project` to add one.');
      return;
    }

    console.log('Projects:\n');
    for (const p of projects) {
      console.log(`  ${p.name} (id: ${p.id})`);
      console.log(`    Repository: ${p.repo_path}`);
      console.log(`    Snapshots: ${p.snapshot_count}`);
      console.log('');
    }

  } finally {
    await closeDB(db);
  }
}

async function listSnapshots(options) {
  const { project } = options;

  if (!project) {
    console.error('Error: --project is required');
    process.exit(1);
  }

  const db = await openDB();

  try {
    const projects = await query(db, 'SELECT * FROM projects WHERE name = ?', project);
    if (projects.length === 0) {
      console.error(`Error: Project "${project}" not found`);
      process.exit(1);
    }
    const proj = projects[0];

    const snapshots = await query(db, `
      SELECT s.*,
             (SELECT COUNT(*) FROM modules m WHERE m.snapshot_id = s.id) as module_count,
             (SELECT COUNT(*) FROM declarations d WHERE d.snapshot_id = s.id) as decl_count
      FROM snapshots s
      WHERE s.project_id = ?
      ORDER BY s.snapshot_at DESC
    `, proj.id);

    if (snapshots.length === 0) {
      console.log(`No snapshots found for project "${project}".`);
      console.log('Use `ce-loader snapshot --project ' + project + '` to create one.');
      return;
    }

    console.log(`Snapshots for "${project}":\n`);
    for (const s of snapshots) {
      console.log(`  [${s.id}] ${s.label || s.git_ref || 'unlabeled'}`);
      console.log(`    Git: ${s.git_hash?.slice(0, 8)} (${s.git_ref})`);
      console.log(`    Date: ${s.snapshot_at}`);
      console.log(`    Modules: ${s.module_count}, Declarations: ${s.decl_count}`);
      console.log('');
    }

  } finally {
    await closeDB(db);
  }
}

// =============================================================================
// Main
// =============================================================================

function printUsage() {
  console.log(`
Code Explorer Loader CLI

Usage:
  ce-loader add-project --name <name> [--repo <path>]
  ce-loader snapshot --project <name> [--label <label>] [--ref <git-ref>]
  ce-loader list-projects
  ce-loader list-snapshots --project <name>

Commands:
  add-project      Register a new project
  snapshot         Create a snapshot of a project's current state
  list-projects    List all registered projects
  list-snapshots   List snapshots for a project

Options:
  --name <name>    Project name (for add-project)
  --repo <path>    Path to repository (defaults to current directory)
  --project <name> Project name (for snapshot, list-snapshots)
  --label <label>  Human-readable label for snapshot (e.g., "v1.0.0")
  --ref <ref>      Git ref to snapshot (defaults to HEAD)
`);
}

async function main() {
  const { command, options } = parseArgs(process.argv.slice(2));

  if (!command || command === 'help' || command === '--help') {
    printUsage();
    process.exit(0);
  }

  // Check database exists
  if (!fs.existsSync(DB_PATH)) {
    console.error('Error: Database not found. Run `node init-schema.js --fresh` first.');
    process.exit(1);
  }

  switch (command) {
    case 'add-project':
      await addProject(options);
      break;
    case 'snapshot':
      await createSnapshot(options);
      break;
    case 'list-projects':
      await listProjects();
      break;
    case 'list-snapshots':
      await listSnapshots(options);
      break;
    default:
      console.error(`Unknown command: ${command}`);
      printUsage();
      process.exit(1);
  }
}

main().catch(err => {
  console.error('Error:', err.message);
  process.exit(1);
});
