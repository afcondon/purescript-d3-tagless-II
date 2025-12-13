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
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)
import Effect.Class (liftEffect)

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
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Build and layout the DAG
  let
    dag = buildGitDAG history
    positioned = layoutDAGTree Vertical size dag

  -- Build the structure tree (empty groups to be filled later)
  let structureTree :: T.Tree Unit
      structureTree =
        T.named SVG "svg"
          [ v3Attr "width" (lit size.width)
          , v3Attr "height" (lit size.height)
          , v3AttrStr "class" (str "git-history")
          ]
          `T.withChildren`
            [ T.named Group "tree-links" [ v3AttrStr "class" (str "tree-links") ]
            , T.named Group "merge-links" [ v3AttrStr "class" (str "merge-links") ]
            , T.named Group "commits" [ v3AttrStr "class" (str "commits") ]
            , T.named Group "labels" [ v3AttrStr "class" (str "labels") ]
            ]

  -- Render structure
  selections <- renderTree container structureTree

  -- Reselect groups for data rendering
  treeLinksGroup <- liftEffect $ reselectD3v2 "tree-links" selections
  mergeLinksGroup <- liftEffect $ reselectD3v2 "merge-links" selections
  commitsGroup <- liftEffect $ reselectD3v2 "commits" selections
  labelsGroup <- liftEffect $ reselectD3v2 "labels" selections

  -- Render tree links
  let treeLinksTree :: T.Tree TreeLink
      treeLinksTree =
        T.joinData "tree-links-data" "line" positioned.treeLinks $ \link ->
          T.elem Line
            [ v3Attr "x1" (lit link.source.x)
            , v3Attr "y1" (lit link.source.y)
            , v3Attr "x2" (lit link.target.x)
            , v3Attr "y2" (lit link.target.y)
            , v3AttrStr "stroke" (str "#708090")
            , v3Attr "stroke-width" (lit 2.0)
            ]
  _ <- renderTree treeLinksGroup treeLinksTree

  -- Render merge links
  let mergeLinksTree :: T.Tree ExtraLink
      mergeLinksTree =
        T.joinData "merge-links-data" "line" positioned.extraLinks $ \link ->
          T.elem Line
            [ v3Attr "x1" (lit link.source.x)
            , v3Attr "y1" (lit link.source.y)
            , v3Attr "x2" (lit link.target.x)
            , v3Attr "y2" (lit link.target.y)
            , v3AttrStr "stroke" (str "#7B68EE")
            , v3Attr "stroke-width" (lit 2.0)
            , v3AttrStr "class" (str "merge-link")
            ]
  _ <- renderTree mergeLinksGroup mergeLinksTree

  -- Render commit nodes
  let nodesTree :: T.Tree PositionedCommit
      nodesTree =
        T.joinData "commits-data" "circle" positioned.nodes $ \node ->
          T.elem Circle
            [ v3Attr "cx" (lit node.x)
            , v3Attr "cy" (lit node.y)
            , v3Attr "r" (lit (commitRadius node))
            , v3AttrStr "fill" (str (commitFill node))
            , v3AttrStr "stroke" (str "#fff")
            , v3Attr "stroke-width" (lit 2.0)
            ]
  _ <- renderTree commitsGroup nodesTree

  -- Render commit labels
  let labelsTree :: T.Tree PositionedCommit
      labelsTree =
        T.joinData "labels-data" "text" positioned.nodes $ \node ->
          T.elem Text
            [ v3Attr "x" (lit (node.x + 15.0))
            , v3Attr "y" (lit (node.y + 4.0))
            , v3AttrStr "textContent" (str (commitLabel node))
            , v3Attr "font-size" (lit 11.0)
            , v3AttrStr "fill" (str "#2F4F4F")
            ]
  _ <- renderTree labelsGroup labelsTree

  pure unit
