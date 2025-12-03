-- | Projects API Endpoints
-- |
-- | Multi-project and snapshot management endpoints.
module API.Projects
  ( listProjects
  , getProject
  , listSnapshots
  , getSnapshot
  , getLatestSnapshotId
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Database.DuckDB (Database, queryAll, queryAllParams, firstRow)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import HTTPurple (Response, ok', notFound)
import HTTPurple.Headers (ResponseHeaders, headers)

-- | JSON content type header with CORS
jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json", "Access-Control-Allow-Origin": "*" }

-- =============================================================================
-- GET /api/projects
-- =============================================================================

-- | List all projects with snapshot counts
listProjects :: Database -> Aff Response
listProjects db = do
  rows <- queryAll db """
    SELECT
      p.id,
      p.name,
      p.repo_path,
      p.description,
      p.created_at,
      COUNT(s.id) as snapshot_count,
      MAX(s.snapshot_at) as latest_snapshot_at
    FROM projects p
    LEFT JOIN snapshots s ON p.id = s.project_id
    GROUP BY p.id, p.name, p.repo_path, p.description, p.created_at
    ORDER BY p.name
  """
  let json = buildProjectsListJson rows
  ok' jsonHeaders json

foreign import buildProjectsListJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/projects/:id
-- =============================================================================

-- | Get a single project with its snapshots
getProject :: Database -> Int -> Aff Response
getProject db projectId = do
  -- Get project details
  projectRows <- queryAllParams db """
    SELECT id, name, repo_path, description, created_at
    FROM projects
    WHERE id = ?
  """ [unsafeToForeign projectId]

  case firstRow projectRows of
    Nothing -> notFound
    Just project -> do
      -- Get snapshots for this project
      snapshots <- queryAllParams db """
        SELECT
          s.id,
          s.git_hash,
          s.git_ref,
          s.label,
          s.snapshot_at,
          s.created_at,
          (SELECT COUNT(*) FROM modules m WHERE m.snapshot_id = s.id) as module_count,
          (SELECT COUNT(*) FROM packages p WHERE p.snapshot_id = s.id) as package_count,
          (SELECT COUNT(*) FROM declarations d WHERE d.snapshot_id = s.id) as declaration_count
        FROM snapshots s
        WHERE s.project_id = ?
        ORDER BY s.snapshot_at DESC
      """ [unsafeToForeign projectId]

      let json = buildProjectWithSnapshotsJson project snapshots
      ok' jsonHeaders json

foreign import buildProjectWithSnapshotsJson :: Foreign -> Array Foreign -> String

-- =============================================================================
-- GET /api/projects/:id/snapshots
-- =============================================================================

-- | List snapshots for a project
listSnapshots :: Database -> Int -> Aff Response
listSnapshots db projectId = do
  rows <- queryAllParams db """
    SELECT
      s.id,
      s.project_id,
      s.git_hash,
      s.git_ref,
      s.label,
      s.snapshot_at,
      s.created_at,
      (SELECT COUNT(*) FROM modules m WHERE m.snapshot_id = s.id) as module_count,
      (SELECT COUNT(*) FROM packages p WHERE p.snapshot_id = s.id) as package_count,
      (SELECT COUNT(*) FROM declarations d WHERE d.snapshot_id = s.id) as declaration_count
    FROM snapshots s
    WHERE s.project_id = ?
    ORDER BY s.snapshot_at DESC
  """ [unsafeToForeign projectId]
  let json = buildSnapshotsListJson rows
  ok' jsonHeaders json

foreign import buildSnapshotsListJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/snapshots/:id
-- =============================================================================

-- | Get a single snapshot with summary
getSnapshot :: Database -> Int -> Aff Response
getSnapshot db snapshotId = do
  rows <- queryAllParams db """
    SELECT
      s.id,
      s.project_id,
      s.git_hash,
      s.git_ref,
      s.label,
      s.snapshot_at,
      s.created_at,
      p.name as project_name,
      (SELECT COUNT(*) FROM modules m WHERE m.snapshot_id = s.id) as module_count,
      (SELECT COUNT(*) FROM packages pkg WHERE pkg.snapshot_id = s.id) as package_count,
      (SELECT COUNT(*) FROM declarations d WHERE d.snapshot_id = s.id) as declaration_count
    FROM snapshots s
    JOIN projects p ON s.project_id = p.id
    WHERE s.id = ?
  """ [unsafeToForeign snapshotId]

  case firstRow rows of
    Nothing -> notFound
    Just snapshot -> do
      let json = buildSnapshotJson snapshot
      ok' jsonHeaders json

foreign import buildSnapshotJson :: Foreign -> String

-- =============================================================================
-- Helper: Get latest snapshot ID for a project
-- =============================================================================

-- | Get the ID of the latest snapshot for a project (or any project if none specified)
getLatestSnapshotId :: Database -> Maybe Int -> Aff (Maybe Int)
getLatestSnapshotId db mProjectId = do
  rows <- case mProjectId of
    Just pid -> queryAllParams db """
      SELECT id FROM snapshots
      WHERE project_id = ?
      ORDER BY snapshot_at DESC
      LIMIT 1
    """ [unsafeToForeign pid]
    Nothing -> queryAll db """
      SELECT id FROM snapshots
      ORDER BY snapshot_at DESC
      LIMIT 1
    """
  pure $ toMaybe <<< getIdFromRow_ =<< firstRow rows

foreign import getIdFromRow_ :: Foreign -> Nullable Int
