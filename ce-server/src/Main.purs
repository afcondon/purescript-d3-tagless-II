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
    ModulesJson -> withLatestSnapshot db \sid -> Legacy.modulesJson db sid
    PackagesJson -> withLatestSnapshot db \sid -> Legacy.packagesJson db sid
    LocJson -> withLatestSnapshot db \sid -> Legacy.locJson db sid
    DeclarationsSummaryJson -> withLatestSnapshot db \sid -> Legacy.declarationsSummaryJson db sid
    ModuleMetricsJson -> withLatestSnapshot db \sid -> Legacy.moduleMetricsJson db sid
    CommitTimelineJson -> withLatestSnapshot db \sid -> Legacy.commitTimelineJson db sid
    -- Project/Snapshot API
    ListProjects -> Projects.listProjects db
    GetProject pid -> Projects.getProject db pid
    GetSnapshot sid -> Projects.getSnapshot db sid
    -- Health
    Health -> ok "OK"

-- | Helper to get latest snapshot and run handler, or return 404
withLatestSnapshot :: DB.Database -> (Int -> Aff _) -> Aff _
withLatestSnapshot db handler = do
  mSnapshotId <- Projects.getLatestSnapshotId db Nothing
  case mSnapshotId of
    Nothing -> notFound
    Just sid -> handler sid
