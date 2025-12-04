module Server.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Database.DuckDB as DB
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (serve, ok, notFound)
import Routing.Duplex (RouteDuplex', root, path, int, segment)
import Routing.Duplex.Generic (noArgs, sum)
import API.Legacy as Legacy
import API.Projects as Projects

-- =============================================================================
-- Routes
-- =============================================================================

-- | Application routes
-- | Using flat URL structure for simplicity with RouteDuplex
data Route
  -- Legacy endpoints (default to latest snapshot)
  = ModulesJson
  | PackagesJson
  | LocJson
  | DeclarationsSummaryJson
  | ModuleMetricsJson
  | CommitTimelineJson
  | FunctionCallsJson
  -- Project-specific data endpoints
  | ProjectModulesJson Int
  | ProjectPackagesJson Int
  | ProjectLocJson Int
  | ProjectDeclarationsSummaryJson Int
  -- Granular module endpoints (on-demand loading)
  | GetModuleDeclarations String
  | GetModuleFunctionCalls String
  -- Batch endpoints (comma-separated module names)
  | GetBatchFunctionCalls String
  | GetBatchDeclarations String
  -- Project/Snapshot API
  | ListProjects
  | GetProject Int
  | GetSnapshot Int
  -- Health
  | Health

derive instance Generic Route _

-- | Route mapping
route :: RouteDuplex' Route
route = root $ sum
  -- Legacy endpoints (backward compatible)
  { "ModulesJson": path "data/spago-data/modules.json" noArgs
  , "PackagesJson": path "data/spago-data/packages.json" noArgs
  , "LocJson": path "data/spago-data/LOC.json" noArgs
  , "DeclarationsSummaryJson": path "data/spago-data/declarations-summary.json" noArgs
  , "ModuleMetricsJson": path "data/module-metrics.json" noArgs
  , "CommitTimelineJson": path "data/commit-timeline.json" noArgs
  , "FunctionCallsJson": path "data/function-calls.json" noArgs
  -- Project-specific data endpoints (flat URLs: /api/project-modules/:id)
  , "ProjectModulesJson": path "api/project-modules" (int segment)
  , "ProjectPackagesJson": path "api/project-packages" (int segment)
  , "ProjectLocJson": path "api/project-loc" (int segment)
  , "ProjectDeclarationsSummaryJson": path "api/project-declarations-summary" (int segment)
  -- Granular module endpoints (flat URLs for routing-duplex compatibility)
  , "GetModuleDeclarations": path "api/module-declarations" segment
  , "GetModuleFunctionCalls": path "api/module-function-calls" segment
  -- Batch endpoints (comma-separated module names in segment)
  , "GetBatchFunctionCalls": path "api/batch-function-calls" segment
  , "GetBatchDeclarations": path "api/batch-declarations" segment
  -- Project/Snapshot API
  , "ListProjects": path "api/projects" noArgs
  , "GetProject": path "api/projects" (int segment)
  , "GetSnapshot": path "api/snapshots" (int segment)
  -- Health
  , "Health": path "health" noArgs
  }

-- =============================================================================
-- Server Configuration
-- =============================================================================

dbPath :: String
dbPath = "./ce-database/ce-data.duckdb"

-- =============================================================================
-- Main
-- =============================================================================

-- | Entry point - opens DB then starts server
main :: Effect Unit
main = launchAff_ do
  -- Open database connection
  db <- DB.openDB dbPath
  liftEffect $ log $ "Connected to database: " <> dbPath

  -- Start server (this returns a close callback, which we ignore)
  liftEffect do
    _ <- serve { port: 8080 } { route, router: mkRouter db }
    log "Server running on http://localhost:8080"
    log ""
    log "Project API:"
    log "  GET /api/projects           - List all projects"
    log "  GET /api/projects/:id       - Get project with snapshots"
    log "  GET /api/snapshots/:id      - Get snapshot details"
    log ""
    log "Granular module endpoints:"
    log "  GET /api/module-declarations/:name    - Declarations for a module"
    log "  GET /api/module-function-calls/:name  - Function calls for a module"
    log ""
    log "Legacy endpoints (uses latest snapshot):"
    log "  GET /data/spago-data/modules.json"
    log "  GET /data/spago-data/packages.json"
    log "  GET /data/spago-data/LOC.json"
    log "  GET /data/spago-data/declarations-summary.json"
    log "  GET /data/module-metrics.json"
    log "  GET /data/commit-timeline.json"
  where
  mkRouter db { route: r } = case r of
    -- Legacy endpoints - use latest snapshot
    ModulesJson -> withLatestSnapshot db Nothing \sid -> Legacy.modulesJson db sid
    PackagesJson -> withLatestSnapshot db Nothing \sid -> Legacy.packagesJson db sid
    LocJson -> withLatestSnapshot db Nothing \sid -> Legacy.locJson db sid
    DeclarationsSummaryJson -> withLatestSnapshot db Nothing \sid -> Legacy.declarationsSummaryJson db sid
    ModuleMetricsJson -> withLatestSnapshot db Nothing \sid -> Legacy.moduleMetricsJson db sid
    CommitTimelineJson -> withLatestSnapshot db Nothing \sid -> Legacy.commitTimelineJson db sid
    FunctionCallsJson -> withLatestSnapshot db Nothing \sid -> Legacy.functionCallsJson db sid
    -- Project-specific data endpoints
    ProjectModulesJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.modulesJson db sid
    ProjectPackagesJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.packagesJson db sid
    ProjectLocJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.locJson db sid
    ProjectDeclarationsSummaryJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.declarationsSummaryJson db sid
    -- Granular module endpoints
    GetModuleDeclarations modName -> withLatestSnapshot db Nothing \sid -> Legacy.moduleDeclarationsJson db sid modName
    GetModuleFunctionCalls modName -> withLatestSnapshot db Nothing \sid -> Legacy.moduleFunctionCallsJson db sid modName
    -- Batch endpoints (comma-separated module names)
    GetBatchFunctionCalls modules -> withLatestSnapshot db Nothing \sid -> Legacy.batchFunctionCallsJson db sid modules
    GetBatchDeclarations modules -> withLatestSnapshot db Nothing \sid -> Legacy.batchDeclarationsJson db sid modules
    -- Project/Snapshot API
    ListProjects -> Projects.listProjects db
    GetProject pid -> Projects.getProject db pid
    GetSnapshot sid -> Projects.getSnapshot db sid
    -- Health
    Health -> ok "OK"

-- | Helper to get latest snapshot for a project and run handler, or return 404
withLatestSnapshot :: DB.Database -> Maybe Int -> (Int -> Aff _) -> Aff _
withLatestSnapshot db mProjectId handler = do
  mSnapshotId <- Projects.getLatestSnapshotId db mProjectId
  case mSnapshotId of
    Nothing -> notFound
    Just sid -> handler sid
