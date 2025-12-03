-- Code Explorer Database Schema v2.0
-- Multi-project support with time-slice snapshots
-- DuckDB optimized for analytical queries on PureScript codebase data

-- =============================================================================
-- Multi-Project Infrastructure
-- =============================================================================

-- Projects: Independent codebases being analyzed
CREATE TABLE IF NOT EXISTS projects (
  id INTEGER PRIMARY KEY,
  name VARCHAR NOT NULL UNIQUE,      -- Display name: "halogen", "psd3"
  repo_path VARCHAR,                  -- Local path or URL to repo
  description TEXT,
  created_at TIMESTAMP DEFAULT current_timestamp
);

-- Snapshots: Point-in-time captures of a project
-- Each snapshot represents the codebase at a specific commit
CREATE TABLE IF NOT EXISTS snapshots (
  id INTEGER PRIMARY KEY,
  project_id INTEGER NOT NULL,
  git_hash VARCHAR,                   -- Full commit hash
  git_ref VARCHAR,                    -- Tag/branch name if applicable: "v7.0.0", "main"
  label VARCHAR,                      -- Human-friendly label: "Initial import", "After refactor"
  snapshot_at TIMESTAMP,              -- When the snapshot was taken (git commit time)
  created_at TIMESTAMP DEFAULT current_timestamp,
  FOREIGN KEY (project_id) REFERENCES projects(id)
);

CREATE INDEX IF NOT EXISTS idx_snapshots_project ON snapshots(project_id);
CREATE INDEX IF NOT EXISTS idx_snapshots_git_hash ON snapshots(git_hash);

-- =============================================================================
-- Core Entities (snapshot-scoped)
-- =============================================================================

-- Packages: Top-level dependency units
CREATE TABLE IF NOT EXISTS packages (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  name VARCHAR NOT NULL,
  depends JSON,                       -- Array of package names: ["dep1", "dep2"]
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id),
  UNIQUE (snapshot_id, name)
);

CREATE INDEX IF NOT EXISTS idx_packages_snapshot ON packages(snapshot_id);

-- Modules: PureScript modules within packages
CREATE TABLE IF NOT EXISTS modules (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  name VARCHAR NOT NULL,
  package VARCHAR,                    -- Package name (denormalized for query simplicity)
  path VARCHAR,
  depends JSON,                       -- Array of module names: ["Mod1", "Mod2"]
  loc INTEGER DEFAULT 0,
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id),
  UNIQUE (snapshot_id, name)
);

CREATE INDEX IF NOT EXISTS idx_modules_snapshot ON modules(snapshot_id);
CREATE INDEX IF NOT EXISTS idx_modules_package ON modules(snapshot_id, package);

-- =============================================================================
-- Code Analysis Data (snapshot-scoped)
-- =============================================================================

-- Declarations: Functions, types, typeclasses from docs.json
CREATE TABLE IF NOT EXISTS declarations (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  module VARCHAR NOT NULL,
  title VARCHAR,
  kind VARCHAR,                       -- typeSynonym, data, typeAlias, class, value, etc.
  comments TEXT,
  source_span JSON,                   -- {name, start: [line, col], end: [line, col]}
  type_signature JSON,                -- Nested type AST preserved as JSON
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id));

CREATE INDEX IF NOT EXISTS idx_declarations_snapshot ON declarations(snapshot_id);
CREATE INDEX IF NOT EXISTS idx_declarations_module ON declarations(snapshot_id, module);
CREATE INDEX IF NOT EXISTS idx_declarations_kind ON declarations(snapshot_id, kind);

-- Function calls: Cross-module function call graph from corefn.json
CREATE TABLE IF NOT EXISTS function_calls (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  module VARCHAR,
  name VARCHAR,
  calls JSON,                         -- [{target, targetModule, identifier, isCrossModule}]
  called_by JSON,                     -- [caller1, caller2]
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id));

CREATE INDEX IF NOT EXISTS idx_function_calls_snapshot ON function_calls(snapshot_id);
CREATE INDEX IF NOT EXISTS idx_function_calls_module ON function_calls(snapshot_id, module);

-- Type dependencies: Type usage graph
CREATE TABLE IF NOT EXISTS type_dependencies (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  module VARCHAR,
  name VARCHAR,
  kind VARCHAR,
  used_by JSON,                       -- [typeName1, typeName2]
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id));

CREATE INDEX IF NOT EXISTS idx_type_deps_snapshot ON type_dependencies(snapshot_id);
CREATE INDEX IF NOT EXISTS idx_type_deps_module ON type_dependencies(snapshot_id, module);

-- =============================================================================
-- Git History Data
-- =============================================================================

-- Module metrics: Git-derived metrics per module (snapshot-scoped)
-- These are computed relative to the snapshot's point in time
CREATE TABLE IF NOT EXISTS module_metrics (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  module VARCHAR NOT NULL,
  path VARCHAR,
  commit_count INTEGER,
  days_since_modified INTEGER,
  age_in_days INTEGER,
  author_count INTEGER,
  lines_changed INTEGER,
  recent_commits INTEGER,
  line_count INTEGER,
  authors JSON,                       -- ["Author1", "Author2"]
  normalized JSON,                    -- {commits, age, recency, authors, churn, size}
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id),
  UNIQUE (snapshot_id, module)
);

CREATE INDEX IF NOT EXISTS idx_module_metrics_snapshot ON module_metrics(snapshot_id);

-- Commits: Git commit timeline (project-scoped, not snapshot-scoped)
-- Full git history belongs to the project, viewable from any snapshot
CREATE TABLE IF NOT EXISTS commits (
  id INTEGER PRIMARY KEY,
  project_id INTEGER NOT NULL,
  hash VARCHAR NOT NULL,
  timestamp INTEGER,
  date VARCHAR,
  author VARCHAR,
  subject TEXT,
  created JSON,                       -- [module paths created]
  modified JSON,                      -- [module paths modified]
  deleted JSON,                       -- [module paths deleted]
  FOREIGN KEY (project_id) REFERENCES projects(id),
  UNIQUE (project_id, hash)
);

CREATE INDEX IF NOT EXISTS idx_commits_project ON commits(project_id);
CREATE INDEX IF NOT EXISTS idx_commits_timestamp ON commits(project_id, timestamp);
CREATE INDEX IF NOT EXISTS idx_commits_author ON commits(project_id, author);

-- =============================================================================
-- Metadata
-- =============================================================================

CREATE TABLE IF NOT EXISTS metadata (
  key VARCHAR PRIMARY KEY,
  value JSON
);

-- Insert schema version
INSERT OR REPLACE INTO metadata (key, value) VALUES
  ('schema_version', '"2.0.0"'),
  ('created_at', json_quote(current_timestamp));

-- =============================================================================
-- Useful Views
-- =============================================================================

-- Latest snapshot per project
CREATE VIEW IF NOT EXISTS latest_snapshots AS
SELECT s.*
FROM snapshots s
INNER JOIN (
  SELECT project_id, MAX(snapshot_at) as max_time
  FROM snapshots
  GROUP BY project_id
) latest ON s.project_id = latest.project_id AND s.snapshot_at = latest.max_time;

-- Project summary with counts
CREATE VIEW IF NOT EXISTS project_summary AS
SELECT
  p.id,
  p.name,
  p.repo_path,
  COUNT(DISTINCT s.id) as snapshot_count,
  MAX(s.snapshot_at) as latest_snapshot_at
FROM projects p
LEFT JOIN snapshots s ON p.id = s.project_id
GROUP BY p.id, p.name, p.repo_path;
