-- Code Explorer Database Schema
-- DuckDB optimized for analytical queries on PureScript codebase data

-- =============================================================================
-- Core Entities
-- =============================================================================

-- Packages: Top-level dependency units
CREATE TABLE IF NOT EXISTS packages (
  name VARCHAR PRIMARY KEY,
  depends JSON  -- Array of package names: ["dep1", "dep2"]
);

-- Modules: PureScript modules within packages
CREATE TABLE IF NOT EXISTS modules (
  name VARCHAR PRIMARY KEY,
  package VARCHAR,
  path VARCHAR,
  depends JSON,  -- Array of module names: ["Mod1", "Mod2"]
  loc INTEGER DEFAULT 0,
  FOREIGN KEY (package) REFERENCES packages(name)
);

-- Create index for package lookups
CREATE INDEX IF NOT EXISTS idx_modules_package ON modules(package);

-- =============================================================================
-- Code Analysis Data
-- =============================================================================

-- Declarations: Functions, types, typeclasses from docs.json
CREATE TABLE IF NOT EXISTS declarations (
  id INTEGER PRIMARY KEY,
  module VARCHAR,
  title VARCHAR,
  kind VARCHAR,  -- typeSynonym, data, typeAlias, class, value, etc.
  comments TEXT,
  source_span JSON,  -- {name, start: [line, col], end: [line, col]}
  type_signature JSON,  -- Nested type AST preserved as JSON
  FOREIGN KEY (module) REFERENCES modules(name)
);

CREATE INDEX IF NOT EXISTS idx_declarations_module ON declarations(module);
CREATE INDEX IF NOT EXISTS idx_declarations_kind ON declarations(kind);

-- Function calls: Cross-module function call graph from corefn.json
CREATE TABLE IF NOT EXISTS function_calls (
  id INTEGER PRIMARY KEY,
  module VARCHAR,
  name VARCHAR,
  calls JSON,      -- [{target, targetModule, identifier, isCrossModule}]
  called_by JSON   -- [caller1, caller2]
);

CREATE INDEX IF NOT EXISTS idx_function_calls_module ON function_calls(module);

-- Type dependencies: Type usage graph
CREATE TABLE IF NOT EXISTS type_dependencies (
  id INTEGER PRIMARY KEY,
  module VARCHAR,
  name VARCHAR,
  kind VARCHAR,
  used_by JSON  -- [typeName1, typeName2]
);

CREATE INDEX IF NOT EXISTS idx_type_dependencies_module ON type_dependencies(module);

-- =============================================================================
-- Git History Data
-- =============================================================================

-- Module metrics: Git-derived metrics per module
CREATE TABLE IF NOT EXISTS module_metrics (
  module VARCHAR PRIMARY KEY,
  path VARCHAR,
  commit_count INTEGER,
  days_since_modified INTEGER,
  age_in_days INTEGER,
  author_count INTEGER,
  lines_changed INTEGER,
  recent_commits INTEGER,
  line_count INTEGER,
  authors JSON,     -- ["Author1", "Author2"]
  normalized JSON   -- {commits, age, recency, authors, churn, size, recentActivity}
);

-- Commits: Git commit timeline
CREATE TABLE IF NOT EXISTS commits (
  hash VARCHAR PRIMARY KEY,
  timestamp INTEGER,
  date VARCHAR,
  author VARCHAR,
  subject TEXT,
  created JSON,   -- [module names]
  modified JSON,  -- [module names]
  deleted JSON    -- [module names]
);

CREATE INDEX IF NOT EXISTS idx_commits_timestamp ON commits(timestamp);
CREATE INDEX IF NOT EXISTS idx_commits_author ON commits(author);

-- =============================================================================
-- Metadata
-- =============================================================================

CREATE TABLE IF NOT EXISTS metadata (
  key VARCHAR PRIMARY KEY,
  value JSON
);

-- Insert schema version
INSERT OR REPLACE INTO metadata (key, value) VALUES
  ('schema_version', '"1.0.0"'),
  ('created_at', json_quote(current_timestamp));
