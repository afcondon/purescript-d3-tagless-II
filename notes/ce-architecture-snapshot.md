# Code Explorer Architecture Snapshot

**Date:** 2025-12-03
**Commit:** After merge of `refactor/purescript-rose-tree` into main

## Overview

The Code Explorer (CE) system consists of three packages that work together:

```
┌─────────────────────────────────────────────────────────────────────┐
│                          ce-website                                  │
│        (Browser - Halogen + D3 Force Simulation)                    │
│                              │                                       │
│                     Affjax HTTP requests                            │
│                              ▼                                       │
├─────────────────────────────────────────────────────────────────────┤
│                          ce-server                                   │
│             (Node.js - HTTPurple + RouteDuplex)                     │
│                              │                                       │
│                       DuckDB queries                                 │
│                              ▼                                       │
├─────────────────────────────────────────────────────────────────────┤
│                         ce-database                                  │
│               (DuckDB FFI bindings + Schema)                        │
│                              │                                       │
│                      ce-data.duckdb                                  │
└─────────────────────────────────────────────────────────────────────┘
```

## Package Details

### 1. ce-database

**Purpose:** DuckDB database bindings and schema definitions for Code Explorer data.

**Location:** `ce-database/`

**Dependencies (spago.yaml):**
- aff, aff-promise
- argonaut-core, argonaut-codecs
- arrays, effect, either, foreign, foreign-object
- maybe, nullable, prelude, transformers

**Key Files:**

| File | Description |
|------|-------------|
| `src/Database/DuckDB.purs` | Type-safe PureScript bindings for DuckDB |
| `src/Database/DuckDB.js` | FFI implementations using `duckdb` npm package |
| `schema/init.sql` | Database schema definition |
| `loader/init-schema.js` | Script to initialize database |
| `loader/load-from-json.js` | Script to load data from JSON files |
| `ce-data.duckdb` | The actual DuckDB database file |

**API Surface (Database.DuckDB module):**
```purescript
-- Types
Database :: Type         -- Opaque database connection
Statement :: Type        -- Prepared statement handle
Row = Foreign           -- Single row result
Rows = Array Foreign    -- Multiple rows

-- Connection
openDB :: String -> Aff Database
openMemoryDB :: Aff Database
closeDB :: Database -> Aff Unit

-- Queries
queryAll :: Database -> String -> Aff Rows
queryAllParams :: Database -> String -> Array Foreign -> Aff Rows
exec :: Database -> String -> Aff Unit
run :: Database -> String -> Array Foreign -> Aff Unit

-- Batch operations
execBatch :: Database -> Array String -> Aff Unit
prepare :: Database -> String -> Aff Statement
runPrepared :: Statement -> Array Foreign -> Aff Unit
finalize :: Statement -> Aff Unit

-- Helpers
toJsonString :: Foreign -> String
firstRow :: Rows -> Maybe Foreign
isEmpty :: Rows -> Boolean
```

**Database Schema:**

```sql
-- Core entities
packages (name PK, depends JSON)
modules (name PK, package FK, path, depends JSON, loc INTEGER)

-- Code analysis
declarations (id PK, module FK, title, kind, comments, source_span JSON, type_signature JSON)
function_calls (id PK, module, name, calls JSON, called_by JSON)
type_dependencies (id PK, module, name, kind, used_by JSON)

-- Git history
module_metrics (module PK, path, commit_count, days_since_modified, age_in_days,
                author_count, lines_changed, recent_commits, line_count, authors JSON, normalized JSON)
commits (hash PK, timestamp, date, author, subject, created JSON, modified JSON, deleted JSON)

-- Metadata
metadata (key PK, value JSON)
```

---

### 2. ce-server

**Purpose:** HTTP API server that queries DuckDB and returns JSON in the legacy format expected by ce-website.

**Location:** `ce-server/`

**Dependencies (spago.yaml):**
- aff, console, effect, prelude
- argonaut-core, argonaut-codecs
- arrays, either, foldable-traversable
- foreign, foreign-object
- **ce-database** (local package)
- httpurple
- maybe, node-buffer, node-fs
- strings, transformers, tuples

**Key Files:**

| File | Description |
|------|-------------|
| `src/Main.purs` | Server entry point (module `Server.Main`) |
| `src/API/Legacy.purs` | Endpoint handlers |
| `src/API/Legacy.js` | JSON builder FFI functions |
| `run.js` | Node.js runner script |
| `spago.yaml` | Package configuration |

**Server Configuration:**
- Port: 8080
- Database path: `./ce-database/ce-data.duckdb`

**API Endpoints:**

| Route | Handler | Description |
|-------|---------|-------------|
| `GET /data/spago-data/modules.json` | `modulesJson` | Module dependency graph |
| `GET /data/spago-data/packages.json` | `packagesJson` | Package dependency graph |
| `GET /data/spago-data/LOC.json` | `locJson` | Lines of code per module |
| `GET /data/spago-data/declarations-summary.json` | `declarationsSummaryJson` | Declarations per module |
| `GET /data/module-metrics.json` | `moduleMetricsJson` | Git metrics per module |
| `GET /data/commit-timeline.json` | `commitTimelineJson` | Git commit history |
| `GET /health` | `ok "OK"` | Health check |

**Routing (RouteDuplex):**
```purescript
data Route
  = ModulesJson
  | PackagesJson
  | LocJson
  | DeclarationsSummaryJson
  | ModuleMetricsJson
  | CommitTimelineJson
  | Health

route :: RouteDuplex' Route
route = root $ sum
  { "ModulesJson": path "data/spago-data/modules.json" noArgs
  , "PackagesJson": path "data/spago-data/packages.json" noArgs
  , "LocJson": path "data/spago-data/LOC.json" noArgs
  , "DeclarationsSummaryJson": path "data/spago-data/declarations-summary.json" noArgs
  , "ModuleMetricsJson": path "data/module-metrics.json" noArgs
  , "CommitTimelineJson": path "data/commit-timeline.json" noArgs
  , "Health": path "health" noArgs
  }
```

**Response Format:**
- All responses include headers: `Content-Type: application/json`, `Access-Control-Allow-Origin: *`
- JSON is built via FFI functions that transform DB rows to legacy format

**Running the Server:**
```bash
cd ce-server
node run.js
# Or from workspace root:
node ce-server/run.js
```

---

### 3. ce-website

**Purpose:** Browser-based interactive visualization of the codebase using D3 force simulation and Halogen.

**Location:** `ce-website/`

**Dependencies (spago.yaml):**
- aff, affjax, affjax-web
- argonaut-core, argonaut-codecs
- arrays, console, effect, either
- foldable-traversable, foreign, foreign-object
- halogen
- integers, maybe, newtype, nullable, numbers
- ordered-collections, prelude, random
- **psd3-layout**, **psd3-selection**, **psd3-simulation** (local D3 packages)
- refs, strings, tuples
- web-dom, web-events, web-html

**Key Files:**

| File | Description |
|------|-------------|
| `src/Main.purs` | Application entry point |
| `src/Data/Loader.purs` | API client and data transformation |
| `src/Types.purs` | Core type definitions |
| `src/Component/SpagoGridApp.purs` | Main Halogen application |
| `src/Engine/Scene.purs` | Scene management |
| `src/Engine/Scenes.purs` | Scene definitions |
| `src/Engine/BubblePack.purs` | Bubble pack visualization |
| `src/Engine/Explorer.purs` | Explorer component |
| `src/Viz/SpagoGridTest/*.purs` | Layout implementations |
| `public/index.html` | HTML entry point |
| `public/bundle.js` | Compiled JS bundle |
| `public/styles.css` | Stylesheet |

**API Configuration:**
```purescript
-- src/Data/Loader.purs
apiBaseUrl :: String
apiBaseUrl = "http://localhost:8080"
```

**Core Types:**
```purescript
-- Module representation
type Module =
  { name :: String, package :: String, depends :: Array String, path :: String, loc :: Maybe Int }

-- Package representation
type Package =
  { name :: String, depends :: Array String, modules :: Array String }

-- Simulation node (extends SimulationNode from psd3-simulation)
type SimNode = SimulationNode
  ( name :: String
  , nodeType :: NodeType      -- ModuleNode | PackageNode
  , package :: String
  , r :: Number               -- Radius (LOC-based)
  , cluster :: Int            -- Package ID for coloring
  , targets :: Array Int      -- Dependency IDs
  , sources :: Array Int      -- Dependent IDs
  , gridX :: Number           -- Grid scene position
  , gridY :: Number
  , orbitAngle :: Number      -- Orbit scene angle
  , treeX :: Number           -- Tree scene position
  , treeY :: Number
  , isInTree :: Boolean       -- Is in spanning tree
  )

-- Link types
data LinkType = M2M_Tree | M2M_Graph | P2P | M2P

type SimLink = Link Int (linkType :: LinkType)

-- Loaded model
type LoadedModel =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  , declarations :: DeclarationsMap
  , moduleCount :: Int
  , packageCount :: Int
  }
```

**Data Loading Flow:**
1. `loadModel` fetches from all API endpoints in parallel
2. JSON is decoded to typed records
3. Packages get IDs 0 to (packageCount-1)
4. Modules get IDs packageCount to (packageCount+moduleCount-1)
5. Grid positions computed per package cluster
6. Tree positions computed via spanning tree from dependency graph
7. Nodes marked with `isInTree` based on reachability from root

**Building:**
```bash
# From workspace root
spago bundle --package ce-website --module Main --outfile ce-website/public/bundle.js
```

**Serving:**
```bash
npx http-server ce-website/public -p 1235 -c-1
```

---

## Data Flow

```
1. JSON files in demo-website/public/data/
   ├── spago-data/modules.json
   ├── spago-data/packages.json
   ├── spago-data/LOC.json
   ├── spago-data/declarations.json
   ├── module-metrics.json
   └── commit-timeline.json
           │
           ▼
2. node ce-database/loader/init-schema.js  (creates schema)
   node ce-database/loader/load-from-json.js  (loads data)
           │
           ▼
3. ce-database/ce-data.duckdb  (DuckDB file)
           │
           ▼
4. ce-server (HTTPurple on port 8080)
   - Queries DuckDB
   - Returns JSON via Legacy.* handlers
           │
           ▼
5. ce-website (Browser)
   - Fetches from http://localhost:8080
   - Transforms to SimNode/SimLink
   - Renders with D3 force simulation
```

---

## Running the System

**Prerequisites:**
- Node.js with npm
- Spago/PureScript toolchain

**Steps:**

```bash
# 1. Build all packages
spago build

# 2. Initialize and load database (if needed)
cd ce-database
node loader/init-schema.js
node loader/load-from-json.js
cd ..

# 3. Start the API server
node ce-server/run.js
# → Server running on http://localhost:8080

# 4. Bundle the website
spago bundle --package ce-website --module Main --outfile ce-website/public/bundle.js

# 5. Serve the website
npx http-server ce-website/public -p 1235 -c-1
# → Open http://localhost:1235
```

---

## Notes

- **DuckDB Locking:** DuckDB uses file-level locking. Only one process can have the database open at a time. The ce-server holds the connection while running.

- **Module Naming:** ce-server uses `module Server.Main` to avoid collision with ce-website's `module Main`. Spago workspace handles this via separate package configurations.

- **CORS:** ce-server includes `Access-Control-Allow-Origin: *` header for browser access.

- **Bundle Output:** The spago bundle command creates a nested path issue (`ce-website/ce-website/public/bundle.js`). Currently handled by manually moving the file.
