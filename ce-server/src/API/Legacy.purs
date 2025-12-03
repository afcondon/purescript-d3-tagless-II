-- | Legacy API Endpoints
-- |
-- | Drop-in replacements for static JSON files.
-- | Returns data in exact same format as the original files.
-- | Now with snapshot_id support for multi-project schema.
module API.Legacy
  ( modulesJson
  , packagesJson
  , locJson
  , declarationsSummaryJson
  , moduleMetricsJson
  , commitTimelineJson
  ) where

import Prelude

import Database.DuckDB (Database, queryAll, queryAllParams)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import HTTPurple (Response, ok')
import HTTPurple.Headers (ResponseHeaders, headers)

-- | JSON content type header
jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json", "Access-Control-Allow-Origin": "*" }

-- =============================================================================
-- /data/spago-data/modules.json
-- =============================================================================

-- | Returns modules in format: { "ModuleName": { depends: [], package: "", path: "" } }
modulesJson :: Database -> Int -> Aff Response
modulesJson db snapshotId = do
  rows <- queryAllParams db """
    SELECT
      name,
      depends,
      package,
      path
    FROM modules
    WHERE snapshot_id = ?
    ORDER BY name
  """ [unsafeToForeign snapshotId]
  -- Convert rows to JSON object format
  let json = buildModulesJson rows
  ok' jsonHeaders json

-- Build modules JSON from query results
foreign import buildModulesJson :: Array Foreign -> String

-- =============================================================================
-- /data/spago-data/packages.json
-- =============================================================================

-- | Returns packages in format: { "package-name": { depends: [] } }
packagesJson :: Database -> Int -> Aff Response
packagesJson db snapshotId = do
  rows <- queryAllParams db """
    SELECT
      name,
      depends
    FROM packages
    WHERE snapshot_id = ?
    ORDER BY name
  """ [unsafeToForeign snapshotId]
  let json = buildPackagesJson rows
  ok' jsonHeaders json

foreign import buildPackagesJson :: Array Foreign -> String

-- =============================================================================
-- /data/spago-data/LOC.json
-- =============================================================================

-- | Returns LOC in format: { loc: [{ loc: N, path: "" }] }
locJson :: Database -> Int -> Aff Response
locJson db snapshotId = do
  rows <- queryAllParams db """
    SELECT
      loc,
      path
    FROM modules
    WHERE snapshot_id = ? AND loc > 0
    ORDER BY path
  """ [unsafeToForeign snapshotId]
  let json = buildLocJson rows
  ok' jsonHeaders json

foreign import buildLocJson :: Array Foreign -> String

-- =============================================================================
-- /data/spago-data/declarations-summary.json
-- =============================================================================

-- | Returns declarations summary in format: { "ModuleName": [{ kind: "", title: "" }] }
declarationsSummaryJson :: Database -> Int -> Aff Response
declarationsSummaryJson db snapshotId = do
  rows <- queryAllParams db """
    SELECT
      module,
      kind,
      title
    FROM declarations
    WHERE snapshot_id = ?
    ORDER BY module, title
  """ [unsafeToForeign snapshotId]
  let json = buildDeclarationsSummaryJson rows
  ok' jsonHeaders json

foreign import buildDeclarationsSummaryJson :: Array Foreign -> String

-- =============================================================================
-- /data/module-metrics.json
-- =============================================================================

-- | Returns module metrics in original format
moduleMetricsJson :: Database -> Int -> Aff Response
moduleMetricsJson db snapshotId = do
  -- Get metadata (still in metadata table, may need to be snapshot-scoped later)
  metaRows <- queryAll db """
    SELECT value FROM metadata WHERE key = 'loaded_at'
  """

  -- Get max values for normalization display
  maxRows <- queryAllParams db """
    SELECT
      MAX(commit_count) as max_commits,
      MAX(age_in_days) as max_age,
      MAX(days_since_modified) as max_recency,
      MAX(author_count) as max_authors,
      MAX(lines_changed) as max_churn,
      MAX(line_count) as max_size
    FROM module_metrics
    WHERE snapshot_id = ?
  """ [unsafeToForeign snapshotId]

  -- Get all module metrics
  rows <- queryAllParams db """
    SELECT
      module,
      path,
      commit_count,
      days_since_modified,
      age_in_days,
      author_count,
      lines_changed,
      recent_commits,
      line_count,
      authors,
      normalized
    FROM module_metrics
    WHERE snapshot_id = ?
    ORDER BY module
  """ [unsafeToForeign snapshotId]

  let json = buildModuleMetricsJson metaRows maxRows rows
  ok' jsonHeaders json

foreign import buildModuleMetricsJson :: Array Foreign -> Array Foreign -> Array Foreign -> String

-- =============================================================================
-- /data/commit-timeline.json
-- =============================================================================

-- | Returns commit timeline in original format
-- | Note: commits are project-scoped, so we need to get project_id from snapshot
commitTimelineJson :: Database -> Int -> Aff Response
commitTimelineJson db snapshotId = do
  -- Get project_id from snapshot
  projectRows <- queryAllParams db """
    SELECT project_id FROM snapshots WHERE id = ?
  """ [unsafeToForeign snapshotId]

  -- Get timeline metadata
  metaRows <- queryAll db """
    SELECT value FROM metadata WHERE key = 'commit_timeline_meta'
  """

  -- Get all commits for this project
  rows <- queryAllParams db """
    SELECT
      c.hash,
      c.timestamp,
      c.date,
      c.author,
      c.subject,
      c.created,
      c.modified,
      c.deleted
    FROM commits c
    JOIN snapshots s ON c.project_id = s.project_id
    WHERE s.id = ?
    ORDER BY c.timestamp ASC
  """ [unsafeToForeign snapshotId]

  let json = buildCommitTimelineJson metaRows rows
  ok' jsonHeaders json

foreign import buildCommitTimelineJson :: Array Foreign -> Array Foreign -> String
