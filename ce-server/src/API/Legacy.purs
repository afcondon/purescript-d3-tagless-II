-- | Legacy API Endpoints
-- |
-- | Drop-in replacements for static JSON files.
-- | Returns data in exact same format as the original files.
module API.Legacy
  ( modulesJson
  , packagesJson
  , locJson
  , moduleMetricsJson
  , commitTimelineJson
  ) where

import Prelude

import Database.DuckDB (Database, queryAll)
import Effect.Aff (Aff)
import Foreign (Foreign)
import HTTPurple (Response, ok')
import HTTPurple.Headers (ResponseHeaders, headers)

-- | JSON content type header
jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json", "Access-Control-Allow-Origin": "*" }

-- =============================================================================
-- /data/spago-data/modules.json
-- =============================================================================

-- | Returns modules in format: { "ModuleName": { depends: [], package: "", path: "" } }
modulesJson :: Database -> Aff Response
modulesJson db = do
  rows <- queryAll db """
    SELECT
      name,
      depends,
      package,
      path
    FROM modules
    ORDER BY name
  """
  -- Convert rows to JSON object format
  let json = buildModulesJson rows
  ok' jsonHeaders json

-- Build modules JSON from query results
foreign import buildModulesJson :: Array Foreign -> String

-- =============================================================================
-- /data/spago-data/packages.json
-- =============================================================================

-- | Returns packages in format: { "package-name": { depends: [] } }
packagesJson :: Database -> Aff Response
packagesJson db = do
  rows <- queryAll db """
    SELECT
      name,
      depends
    FROM packages
    ORDER BY name
  """
  let json = buildPackagesJson rows
  ok' jsonHeaders json

foreign import buildPackagesJson :: Array Foreign -> String

-- =============================================================================
-- /data/spago-data/LOC.json
-- =============================================================================

-- | Returns LOC in format: { loc: [{ loc: N, path: "" }] }
locJson :: Database -> Aff Response
locJson db = do
  rows <- queryAll db """
    SELECT
      loc,
      path
    FROM modules
    WHERE loc > 0
    ORDER BY path
  """
  let json = buildLocJson rows
  ok' jsonHeaders json

foreign import buildLocJson :: Array Foreign -> String

-- =============================================================================
-- /data/module-metrics.json
-- =============================================================================

-- | Returns module metrics in original format
moduleMetricsJson :: Database -> Aff Response
moduleMetricsJson db = do
  -- Get metadata
  metaRows <- queryAll db """
    SELECT value FROM metadata WHERE key = 'loaded_at'
  """

  -- Get max values for normalization display
  maxRows <- queryAll db """
    SELECT
      MAX(commit_count) as max_commits,
      MAX(age_in_days) as max_age,
      MAX(days_since_modified) as max_recency,
      MAX(author_count) as max_authors,
      MAX(lines_changed) as max_churn,
      MAX(line_count) as max_size
    FROM module_metrics
  """

  -- Get all module metrics
  rows <- queryAll db """
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
    ORDER BY module
  """

  let json = buildModuleMetricsJson metaRows maxRows rows
  ok' jsonHeaders json

foreign import buildModuleMetricsJson :: Array Foreign -> Array Foreign -> Array Foreign -> String

-- =============================================================================
-- /data/commit-timeline.json
-- =============================================================================

-- | Returns commit timeline in original format
commitTimelineJson :: Database -> Aff Response
commitTimelineJson db = do
  -- Get timeline metadata
  metaRows <- queryAll db """
    SELECT value FROM metadata WHERE key = 'commit_timeline_meta'
  """

  -- Get all commits
  rows <- queryAll db """
    SELECT
      hash,
      timestamp,
      date,
      author,
      subject,
      created,
      modified,
      deleted
    FROM commits
    ORDER BY timestamp ASC
  """

  let json = buildCommitTimelineJson metaRows rows
  ok' jsonHeaders json

foreign import buildCommitTimelineJson :: Array Foreign -> Array Foreign -> String
