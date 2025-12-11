-- | Git History Visualization using DAG Tree
-- |
-- | Visualizes git commit history as a DAG tree where:
-- | - The main branch forms the tree spine
-- | - Merge commits create extra links to their second parents
-- |
-- | This demonstrates the DAGTree library feature with a real-world use case.
-- |
-- | == Current Status
-- |
-- | Phase 1: Uses sample/mock data for visualization testing.
-- | Phase 2 (TODO): Connect to ce-server API for real git history.
module CodeExplorer.GitHistory
  ( -- * Types
    GitCommit
  , GitHistory
  , GitDAG
  -- * Sample Data
  , sampleGitHistory
  -- * DAG Construction
  , buildGitDAG
  -- * Rendering
  , renderGitHistory
  ) where

import Prelude

import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tree (Tree, mkTree)
import Effect (Effect)
import PSD3.Data.DAGTree (DAGTree, DAGLink, dagTree, addLinks, layoutDAGTree)
import PSD3.Data.Tree (TreeLayout(..))
import PSD3v2.Attribute.Types (cx, cy, radius, fill, stroke, strokeWidth, textContent, x, y, x1, x2, y1, y2, class_, fontSize, width, height) as Attr
import PSD3v2.Capabilities.Selection (select, appendChild, appendData)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | A git commit with relevant metadata
type GitCommit =
  { sha :: String -- ^ Short SHA (7 chars)
  , message :: String -- ^ Commit message (first line)
  , author :: String -- ^ Author name
  , date :: String -- ^ ISO date string
  , parentShas :: Array String -- ^ Parent commit SHAs (1 for normal, 2+ for merges)
  , isMerge :: Boolean -- ^ True if this is a merge commit
  , branch :: Maybe String -- ^ Branch name if this is a branch head
  }

-- | Git history as an array of commits (newest first)
type GitHistory = Array GitCommit

-- | Git history as a DAG tree
type GitDAG = DAGTree String GitCommit

-- | A positioned commit node (after layout)
type PositionedCommit = { datum :: GitCommit, x :: Number, y :: Number, depth :: Int }

-- | Tree link between positioned commits
type TreeLink = { source :: PositionedCommit, target :: PositionedCommit }

-- | Extra link (merge) between positioned commits
type ExtraLink = { source :: PositionedCommit, target :: PositionedCommit, linkType :: String }

-- | Helper: commit node radius (larger for merges)
commitRadius :: PositionedCommit -> Number
commitRadius n = if n.datum.isMerge then 10.0 else 7.0

-- | Helper: commit node fill color
commitFill :: PositionedCommit -> String
commitFill n = if n.datum.isMerge then "#7B68EE" else "#4A90A4"

-- | Helper: commit label text
commitLabel :: PositionedCommit -> String
commitLabel n = n.datum.sha <> " " <> truncateMessage 30 n.datum.message

-- | Truncate message to max length with ellipsis
truncateMessage :: Int -> String -> String
truncateMessage maxLen msg =
  if String.length msg > maxLen then String.take (maxLen - 3) msg <> "..."
  else msg

-- =============================================================================
-- Sample Data
-- =============================================================================

-- | Sample git history for testing
-- |
-- | Represents a simple feature branch workflow:
-- | ```
-- |   main:     A---B---C---F---G  (merges from feature)
-- |              \     /   /
-- |   feature:    D---E---/
-- | ```
sampleGitHistory :: GitHistory
sampleGitHistory =
  [ { sha: "g7h8i9j"
    , message: "Update docs for release"
    , author: "Alice"
    , date: "2024-01-15"
    , parentShas: [ "f4e5f6g" ]
    , isMerge: false
    , branch: Just "main"
    }
  , { sha: "f4e5f6g"
    , message: "Merge feature into main"
    , author: "Bob"
    , date: "2024-01-14"
    , parentShas: [ "c1d2e3f", "e3d4e5f" ] -- merge commit
    , isMerge: true
    , branch: Nothing
    }
  , { sha: "e3d4e5f"
    , message: "Complete feature implementation"
    , author: "Charlie"
    , date: "2024-01-13"
    , parentShas: [ "d2c3d4e" ]
    , isMerge: false
    , branch: Just "feature"
    }
  , { sha: "d2c3d4e"
    , message: "Start feature work"
    , author: "Charlie"
    , date: "2024-01-12"
    , parentShas: [ "a1b2c3d" ]
    , isMerge: false
    , branch: Nothing
    }
  , { sha: "c1d2e3f"
    , message: "Fix bug in core module"
    , author: "Alice"
    , date: "2024-01-11"
    , parentShas: [ "b0a1b2c" ]
    , isMerge: false
    , branch: Nothing
    }
  , { sha: "b0a1b2c"
    , message: "Add new API endpoint"
    , author: "Bob"
    , date: "2024-01-10"
    , parentShas: [ "a1b2c3d" ]
    , isMerge: false
    , branch: Nothing
    }
  , { sha: "a1b2c3d"
    , message: "Initial commit"
    , author: "Alice"
    , date: "2024-01-01"
    , parentShas: []
    , isMerge: false
    , branch: Nothing
    }
  ]

-- =============================================================================
-- DAG Construction
-- =============================================================================

-- | Build a DAG tree from git history
-- |
-- | The tree structure follows the first parent (main branch).
-- | Merge commits create extra links to their second parents.
buildGitDAG :: GitHistory -> GitDAG
buildGitDAG history =
  let
    -- Find root commit (no parents)
    rootCommit = Array.find (\c -> Array.null c.parentShas) history

    -- Build tree following first parent only
    buildTree :: GitCommit -> Tree GitCommit
    buildTree commit =
      let
        -- Find commits whose first parent is this commit
        children = Array.filter
          ( \c -> case Array.head c.parentShas of
              Just parentSha -> parentSha == commit.sha
              Nothing -> false
          )
          history
        childTrees = map buildTree children
      in
        mkTree commit (List.fromFoldable childTrees)

    -- Extract merge links (second+ parents)
    mergeLinks :: Array (DAGLink String)
    mergeLinks = Array.concatMap extractMergeLinks history

    extractMergeLinks :: GitCommit -> Array (DAGLink String)
    extractMergeLinks commit =
      if commit.isMerge then case Array.tail commit.parentShas of
        Just secondaryParents ->
          map
            ( \parentSha ->
                { source: commit.sha
                , target: parentSha
                , linkType: "merge"
                }
            )
            secondaryParents
        Nothing -> []
      else []

    tree = case rootCommit of
      Just root -> buildTree root
      Nothing -> mkTree
        { sha: "empty"
        , message: "Empty history"
        , author: ""
        , date: ""
        , parentShas: []
        , isMerge: false
        , branch: Nothing
        }
        List.Nil
  in
    dagTree tree _.sha # addLinks mergeLinks

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render git history visualization
-- |
-- | Creates an SVG with:
-- | - Circles for commits (sized by merge status)
-- | - Lines for parent-child relationships
-- | - Dashed lines for merge links
-- | - Labels with SHA and message
renderGitHistory
  :: String -- ^ Selector for container element
  -> { width :: Number, height :: Number }
  -> GitHistory
  -> Effect Unit
renderGitHistory selector size history = void $ runD3v2M do
  container <- select selector

  -- Build and layout the DAG
  let
    dag = buildGitDAG history
    positioned = layoutDAGTree Vertical size dag

  -- Create SVG
  svg <- appendChild SVG
    [ Attr.width size.width
    , Attr.height size.height
    , Attr.class_ "git-history"
    ]
    container

  -- Render tree links (parent -> child)
  linksGroup <- appendChild Group [ Attr.class_ "tree-links" ] svg
  _ <- appendData Line positioned.treeLinks
    [ Attr.x1 (_.source.x :: TreeLink -> Number)
    , Attr.y1 (_.source.y :: TreeLink -> Number)
    , Attr.x2 (_.target.x :: TreeLink -> Number)
    , Attr.y2 (_.target.y :: TreeLink -> Number)
    , Attr.stroke "#708090"
    , Attr.strokeWidth 2.0
    ]
    linksGroup

  -- Render extra links (merge commits)
  mergeGroup <- appendChild Group [ Attr.class_ "merge-links" ] svg
  _ <- appendData Line positioned.extraLinks
    [ Attr.x1 (_.source.x :: ExtraLink -> Number)
    , Attr.y1 (_.source.y :: ExtraLink -> Number)
    , Attr.x2 (_.target.x :: ExtraLink -> Number)
    , Attr.y2 (_.target.y :: ExtraLink -> Number)
    , Attr.stroke "#7B68EE"
    , Attr.strokeWidth 2.0
    , Attr.class_ "merge-link"
    -- Note: stroke-dasharray would need to be added as a custom attribute
    ]
    mergeGroup

  -- Render commit nodes
  nodesGroup <- appendChild Group [ Attr.class_ "commits" ] svg
  _ <- appendData Circle positioned.nodes
    [ Attr.cx (_.x :: PositionedCommit -> Number)
    , Attr.cy (_.y :: PositionedCommit -> Number)
    , Attr.radius (commitRadius :: PositionedCommit -> Number)
    , Attr.fill (commitFill :: PositionedCommit -> String)
    , Attr.stroke "#fff"
    , Attr.strokeWidth 2.0
    ]
    nodesGroup

  -- Render commit labels
  labelsGroup <- appendChild Group [ Attr.class_ "labels" ] svg
  _ <- appendData Text positioned.nodes
    [ Attr.x ((\n -> n.x + 15.0) :: PositionedCommit -> Number)
    , Attr.y ((\n -> n.y + 4.0) :: PositionedCommit -> Number)
    , Attr.textContent (commitLabel :: PositionedCommit -> String)
    , Attr.fontSize 11.0
    , Attr.fill "#2F4F4F"
    ]
    labelsGroup

  pure unit
