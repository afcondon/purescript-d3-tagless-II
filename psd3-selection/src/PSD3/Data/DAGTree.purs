-- | DAG Tree: Tree Layout with Extra Links
-- |
-- | A DAG (Directed Acyclic Graph) Tree is a tree structure with additional
-- | cross-links that don't fit the tree hierarchy. This is useful for visualizing
-- | structures that are "mostly hierarchical" with some extra connections.
-- |
-- | Examples:
-- | - Git commit graphs (tree + merge commits)
-- | - State machines (tree of states + back-edges)
-- | - The GUP flow diagram (tree + merge arrows)
-- | - Dependency graphs with highlighted cycles
-- |
-- | == Design
-- |
-- | The tree is laid out using standard algorithms (Tidy/Dendrogram), which
-- | assigns x,y positions to each node. The extra links are then rendered
-- | as paths between nodes using their computed positions.
-- |
-- | This keeps tree layout clean while allowing graph-like visualizations.
-- |
-- | == Usage
-- |
-- | 1. Build your tree structure with unique node IDs
-- | 2. Define extra links between node IDs
-- | 3. Apply tree layout to get positions
-- | 4. Render tree edges + extra links
-- |
-- | ```purescript
-- | let dagTree =
-- |   { tree: mkTree root children
-- |   , extraLinks:
-- |       [ { source: "enter", target: "merge", linkType: "flow" }
-- |       , { source: "update", target: "merge", linkType: "flow" }
-- |       ]
-- |   }
-- |
-- | -- Layout tree
-- | let positioned = layoutDAGTree Vertical dagTree
-- |
-- | -- Render with Tree API
-- | renderDAGTree positioned linkGenerator
-- | ```
module PSD3.Data.DAGTree
  ( -- * Types
    DAGTree
  , DAGLink
  , PositionedDAGTree
  , PositionedNode
    -- * Construction
  , dagTree
  , addLink
  , addLinks
    -- * Layout
  , layoutDAGTree
    -- * Rendering Helpers
  , getNodePosition
  , getExtraLinkPositions
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tree (Tree, mkTree)
import Data.Int as Data.Int
import PSD3.Data.Tree (TreeLayout(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | A link between two nodes that isn't part of the tree hierarchy.
-- |
-- | These links are rendered after the tree layout is computed,
-- | using the positions assigned by the tree algorithm.
type DAGLink nodeId =
  { source :: nodeId      -- ^ Source node ID
  , target :: nodeId      -- ^ Target node ID
  , linkType :: String    -- ^ Type/category for styling (e.g., "merge", "back-edge")
  }

-- | A tree structure with extra non-hierarchical links.
-- |
-- | The tree defines the layout (determines node positions).
-- | The extra links are overlaid after layout.
type DAGTree nodeId datum =
  { tree :: Tree datum
  , extraLinks :: Array (DAGLink nodeId)
  , getId :: datum -> nodeId    -- ^ Extract ID from datum for link resolution
  }

-- | A node with its computed position from tree layout.
type PositionedNode datum =
  { datum :: datum
  , x :: Number
  , y :: Number
  , depth :: Int          -- ^ Depth in tree (root = 0)
  }

-- | A DAG tree after layout has been applied.
-- |
-- | Contains positioned nodes and resolved link coordinates.
type PositionedDAGTree nodeId datum =
  { nodes :: Array (PositionedNode datum)
  , treeLinks :: Array { source :: PositionedNode datum, target :: PositionedNode datum }
  , extraLinks :: Array
      { source :: PositionedNode datum
      , target :: PositionedNode datum
      , linkType :: String
      }
  , nodeMap :: Map nodeId (PositionedNode datum)  -- ^ For quick lookup
  }

-- =============================================================================
-- Construction
-- =============================================================================

-- | Create a DAG tree from a tree and ID extractor.
-- |
-- | ```purescript
-- | let dag = dagTree myTree _.id
-- | ```
dagTree :: forall nodeId datum. Ord nodeId => Tree datum -> (datum -> nodeId) -> DAGTree nodeId datum
dagTree tree getId = { tree, extraLinks: [], getId }

-- | Add a single extra link to a DAG tree.
-- |
-- | ```purescript
-- | dag # addLink { source: "a", target: "b", linkType: "merge" }
-- | ```
addLink :: forall nodeId datum. DAGLink nodeId -> DAGTree nodeId datum -> DAGTree nodeId datum
addLink link dag = dag { extraLinks = Array.snoc dag.extraLinks link }

-- | Add multiple extra links to a DAG tree.
-- |
-- | ```purescript
-- | dag # addLinks
-- |   [ { source: "a", target: "c", linkType: "merge" }
-- |   , { source: "b", target: "c", linkType: "merge" }
-- |   ]
-- | ```
addLinks :: forall nodeId datum f. Foldable f => f (DAGLink nodeId) -> DAGTree nodeId datum -> DAGTree nodeId datum
addLinks links dag = foldl (flip addLink) dag links

-- =============================================================================
-- Layout
-- =============================================================================

-- | Apply tree layout and resolve link positions.
-- |
-- | This computes positions for all nodes using a tree layout algorithm,
-- | then resolves the extra links to their source/target coordinates.
-- |
-- | The layout parameter determines orientation:
-- | - Vertical: root at top, children below
-- | - Horizontal: root at left, children right
-- | - Radial: root at center, children radiating out
-- |
-- | Note: This is a simplified layout that doesn't use D3's tree algorithms.
-- | For production use with large trees, integrate with d3.tree() or d3.cluster().
layoutDAGTree
  :: forall nodeId datum
   . Ord nodeId
  => TreeLayout
  -> { width :: Number, height :: Number }
  -> DAGTree nodeId datum
  -> PositionedDAGTree nodeId datum
layoutDAGTree layout size dag =
  let
    -- Compute positions using simple recursive layout
    positioned = layoutTree layout size dag.tree

    -- Build lookup map
    nodeMap = buildNodeMap dag.getId positioned

    -- Build tree links (parent -> child)
    treeLinks = buildTreeLinks positioned

    -- Resolve extra links
    extraLinks = resolveExtraLinks dag.extraLinks nodeMap
  in
    { nodes: flattenPositioned positioned
    , treeLinks
    , extraLinks
    , nodeMap
    }

-- | Simple recursive tree layout.
-- |
-- | For a proper implementation, this should call D3's tree layout.
-- | This is a placeholder that does basic positioning.
layoutTree
  :: forall datum
   . TreeLayout
  -> { width :: Number, height :: Number }
  -> Tree datum
  -> Tree (PositionedNode datum)
layoutTree layout size tree = go 0 0.0 1.0 tree
  where
    go :: Int -> Number -> Number -> Tree datum -> Tree (PositionedNode datum)
    go depth xMin xMax t =
      let
        datum = head t
        children = tail t
        childCount = List.length children

        -- Position this node at center of its allocated space
        x = (xMin + xMax) / 2.0
        y = case layout of
          Vertical -> (toNumber depth) * (size.height / toNumber (maxDepth tree + 1))
          Horizontal -> (toNumber depth) * (size.width / toNumber (maxDepth tree + 1))
          Radial -> (toNumber depth) * 50.0  -- Simplified radial

        positioned = { datum, x, y, depth }

        -- Layout children with evenly divided horizontal space
        childWidth = (xMax - xMin) / toNumber (max 1 childCount)
        positionedChildren = layoutChildren 0 children

        layoutChildren :: Int -> List (Tree datum) -> List (Tree (PositionedNode datum))
        layoutChildren _ List.Nil = List.Nil
        layoutChildren i (child : rest) =
          let
            childXMin = xMin + (toNumber i) * childWidth
            childXMax = childXMin + childWidth
            positioned' = go (depth + 1) childXMin childXMax child
          in positioned' : layoutChildren (i + 1) rest
      in
        mkTree positioned positionedChildren

    toNumber :: Int -> Number
    toNumber = Data.Int.toNumber

-- | Calculate maximum depth of a tree
maxDepth :: forall a. Tree a -> Int
maxDepth tree = go 0 tree
  where
    go d t =
      let children = tail t
      in case List.uncons children of
        Nothing -> d
        Just _ -> foldl max d (map (go (d + 1)) (List.toUnfoldable children :: Array _))

-- | Build a map from node ID to positioned node
buildNodeMap
  :: forall nodeId datum
   . Ord nodeId
  => (datum -> nodeId)
  -> Tree (PositionedNode datum)
  -> Map nodeId (PositionedNode datum)
buildNodeMap getId tree = go Map.empty tree
  where
    go acc t =
      let
        node = head t
        nodeId = getId node.datum
        acc' = Map.insert nodeId node acc
        children = tail t
      in
        foldl go acc' (List.toUnfoldable children :: Array _)

-- | Build tree links from parent-child relationships
buildTreeLinks
  :: forall datum
   . Tree (PositionedNode datum)
  -> Array { source :: PositionedNode datum, target :: PositionedNode datum }
buildTreeLinks tree = go [] tree
  where
    go acc t =
      let
        parent = head t
        children = tail t
        childArray = List.toUnfoldable children :: Array _
        -- Add links from this parent to each child
        newLinks = map (\child -> { source: parent, target: head child }) childArray
        -- Recurse into children
        childLinks = Array.concatMap (go []) childArray
      in
        acc <> newLinks <> childLinks

-- | Resolve extra links using the node position map
resolveExtraLinks
  :: forall nodeId datum
   . Ord nodeId
  => Array (DAGLink nodeId)
  -> Map nodeId (PositionedNode datum)
  -> Array { source :: PositionedNode datum, target :: PositionedNode datum, linkType :: String }
resolveExtraLinks links nodeMap = Array.mapMaybe resolve links
  where
    resolve link = do
      source <- Map.lookup link.source nodeMap
      target <- Map.lookup link.target nodeMap
      pure { source, target, linkType: link.linkType }

-- | Flatten a positioned tree to an array of nodes
flattenPositioned :: forall datum. Tree (PositionedNode datum) -> Array (PositionedNode datum)
flattenPositioned tree = go [] tree
  where
    go acc t =
      let
        node = head t
        children = tail t
        childArray = List.toUnfoldable children :: Array _
      in
        acc <> [node] <> Array.concatMap (go []) childArray

-- =============================================================================
-- Rendering Helpers
-- =============================================================================

-- | Get position of a node by ID.
getNodePosition
  :: forall nodeId datum
   . Ord nodeId
  => nodeId
  -> PositionedDAGTree nodeId datum
  -> Maybe { x :: Number, y :: Number }
getNodePosition nodeId dag = do
  node <- Map.lookup nodeId dag.nodeMap
  pure { x: node.x, y: node.y }

-- | Get source and target positions for all extra links.
-- |
-- | Useful for generating path data for link rendering.
getExtraLinkPositions
  :: forall nodeId datum
   . PositionedDAGTree nodeId datum
  -> Array { sourceX :: Number, sourceY :: Number, targetX :: Number, targetY :: Number, linkType :: String }
getExtraLinkPositions dag = map toPositions dag.extraLinks
  where
    toPositions link =
      { sourceX: link.source.x
      , sourceY: link.source.y
      , targetX: link.target.x
      , targetY: link.target.y
      , linkType: link.linkType
      }

