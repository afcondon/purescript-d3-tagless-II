module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Database.DuckDB as DB
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (serve, ok)
import Routing.Duplex (RouteDuplex', root, path)
import Routing.Duplex.Generic (noArgs, sum)
import API.Legacy as Legacy

-- =============================================================================
-- Routes
-- =============================================================================

-- | Application routes
data Route
  = ModulesJson
  | PackagesJson
  | LocJson
  | ModuleMetricsJson
  | CommitTimelineJson
  | Health

derive instance Generic Route _

-- | Route mapping
route :: RouteDuplex' Route
route = root $ sum
  { "ModulesJson": path "data/spago-data/modules.json" noArgs
  , "PackagesJson": path "data/spago-data/packages.json" noArgs
  , "LocJson": path "data/spago-data/LOC.json" noArgs
  , "ModuleMetricsJson": path "data/module-metrics.json" noArgs
  , "CommitTimelineJson": path "data/commit-timeline.json" noArgs
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
    log "Legacy endpoints:"
    log "  GET /data/spago-data/modules.json"
    log "  GET /data/spago-data/packages.json"
    log "  GET /data/spago-data/LOC.json"
    log "  GET /data/module-metrics.json"
    log "  GET /data/commit-timeline.json"
  where
  mkRouter db { route: r } = case r of
    ModulesJson -> Legacy.modulesJson db
    PackagesJson -> Legacy.packagesJson db
    LocJson -> Legacy.locJson db
    ModuleMetricsJson -> Legacy.moduleMetricsJson db
    CommitTimelineJson -> Legacy.commitTimelineJson db
    Health -> ok "OK"
