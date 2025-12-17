-- | Tree Layout Computation
-- |
-- | Computes tree positions for a dependency graph given a root node.
-- | This is visualization logic, not data loading - it belongs in the Viz layer.
-- |
-- | Key functions:
-- | - `computeTreeLayout`: Given root ID, nodes, links -> positioned tree data
-- | - `findRootModule`: Heuristic to find the project's entry point
-- |
-- | Architecture:
-- | - Loader provides raw graph data (nodes with targets/sources, links)
-- | - TreeView calls this module to compute layout on demand
-- | - Supports any root node, enabling subtree visualization
module Data.TreeLayout
  ( computeTreeLayout
  , findRootModule
  , TreeLayoutResult
  , TreeNode
  , module TreeStyle
  ) where

import DataViz.Layout.Hierarchy.TreeStyle (TreeStyle, TreeOrientation(..), linkPath, orientation) as TreeStyle

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tree (Tree, mkTree)
import Control.Comonad.Cofree (head, tail)
import Data.Tuple (Tuple(..))
import Data.Number (cos, pi, sin)
import DataViz.Layout.Hierarchy.Tree as Tree
import PSD3.Data.Graph as Graph
import PSD3.Data.Graph.Algorithms as Algorithms
import Types (SimNode, SimLink, NodeType(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | A node with computed tree position
type TreeNode =
  { id :: Int
  , name :: String
  , treeX :: Number         -- Cartesian X (for vertical tree)
  , treeY :: Number         -- Cartesian Y (for vertical tree)
  , radialX :: Number       -- Radial X (for radial tree)
  , radialY :: Number       -- Radial Y (for radial tree)
  , isInTree :: Boolean     -- True if reachable from root
  , simNode :: SimNode      -- Original node data
  }

-- | Result of tree layout computation
type TreeLayoutResult =
  { treeNodes :: Array TreeNode        -- Nodes with positions
  , treeEdges :: Set (Tuple Int Int)   -- Edges in the spanning tree
  , rootId :: Int                      -- The root node ID
  , rootX :: Number                    -- Root's final X position
  , rootY :: Number                    -- Root's final Y position
  }

-- =============================================================================
-- Main API
-- =============================================================================

-- | Compute tree layout for a dependency graph
-- |
-- | Given a root node ID, computes:
-- | - Spanning tree from root following dependency edges
-- | - Tree positions for all reachable nodes
-- | - Both cartesian (vertical/horizontal tree) and radial projections
-- |
-- | Parameters:
-- | - style: TreeStyle (bundles orientation with matching link path generator)
-- | - rootId: The node to use as tree root
-- | - nodes: All SimNodes (will filter to modules)
-- | - links: All SimLinks (will filter to module-to-module)
computeTreeLayout :: TreeStyle.TreeStyle -> Int -> Array SimNode -> Array SimLink -> TreeLayoutResult
computeTreeLayout style rootId nodes links =
  let
    -- Filter to modules only (packages don't participate in tree)
    moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes

    -- Build node type map ONCE for O(1) lookups (not per-link!)
    nodeTypeMap = Map.fromFoldable $ nodes <#> \n -> Tuple n.id n.nodeType
    moduleLinks = Array.filter (\l -> isModuleLinkFast nodeTypeMap l) links

    -- Build graph model for algorithms
    graphConfig :: Graph.GraphConfig SimNode SimLink
    graphConfig =
      { getNodeId: _.id
      , getLinkSource: _.source
      , getLinkTarget: _.target
      }
    graphModel = Graph.buildGraphModel graphConfig moduleNodes moduleLinks

    -- Run reachability analysis to get spanning tree
    reachability = Algorithms.getReachableNodes graphConfig rootId graphModel

    -- Get set of reachable node IDs
    reachableNodeIds = Set.insert rootId (Set.fromFoldable reachability.nodes)

    -- Spanning tree edges as Set for O(1) lookup
    spanningTreeEdges = Set.fromFoldable reachability.spanningTree

    -- Build tree structure from spanning tree edges
    idTree = buildTreeFromEdges rootId reachability.spanningTree

    -- Convert to tree with position fields
    dataTree = mapTree
      (\nodeId -> { id: nodeId, x: 0.0, y: 0.0, depth: 0, height: 0 })
      idTree

    -- Run tree layout algorithm
    treeLayoutSize = 1000.0
    treeConfig =
      { size: { width: treeLayoutSize, height: treeLayoutSize }
      , minSeparation: 1.0
      , separation: Nothing
      , layerScale: Nothing
      , layerSeparation: Nothing
      }
    laidOutTree = Tree.tree treeConfig dataTree

    -- Flatten to position map
    treePositionMap = flattenTreeToPositionMap laidOutTree

    -- Create TreeNodes with positions
    treeNodes = moduleNodes <#> \node ->
      let
        inTree = Set.member node.id reachableNodeIds
        pos = projectToCartesian (TreeStyle.orientation style) treeLayoutSize (Map.lookup node.id treePositionMap)
      in
        { id: node.id
        , name: node.name
        , treeX: pos.cartX
        , treeY: pos.cartY
        , radialX: pos.radialX
        , radialY: pos.radialY
        , isInTree: inTree
        , simNode: node
        }

    -- Find root position
    rootPos = case Array.find (\n -> n.id == rootId) treeNodes of
      Just rn -> { x: rn.treeX, y: rn.treeY }
      Nothing -> { x: 0.0, y: 0.0 }
  in
    { treeNodes
    , treeEdges: spanningTreeEdges
    , rootId
    , rootX: rootPos.x
    , rootY: rootPos.y
    }

-- | Find the root module (the project's main entry point)
-- |
-- | Strategy:
-- | 1. Look for module named "*.Main" (common PureScript convention)
-- | 2. Fall back to module with most dependents (sources)
findRootModule :: Array SimNode -> Int
findRootModule modules =
  let moduleNodes = Array.filter (\m -> m.nodeType == ModuleNode) modules
  in
  -- First, look for *.Main by name suffix
  case Array.find (\m -> isMainModule m.name) moduleNodes of
    Just m -> m.id
    Nothing ->
      -- Fallback: module with most sources (most dependents = likely root)
      case Array.last $ Array.sortWith (\m -> Array.length m.sources) moduleNodes of
        Just m -> m.id
        Nothing -> 0

-- =============================================================================
-- Internal Helpers
-- =============================================================================

-- | Check if a module name ends with .Main
isMainModule :: String -> Boolean
isMainModule name =
  -- Simple suffix check - name ends with ".Main" or is exactly "Main"
  let len = String.length name
  in len >= 5 && String.drop (len - 5) name == ".Main"
     || name == "Main"

-- | Check if a link is between two modules (not packages)
-- | Fast version: takes pre-built map for O(1) lookups
isModuleLinkFast :: Map Int NodeType -> SimLink -> Boolean
isModuleLinkFast nodeTypeMap link =
  let
    srcType = Map.lookup link.source nodeTypeMap
    tgtType = Map.lookup link.target nodeTypeMap
  in
    srcType == Just ModuleNode && tgtType == Just ModuleNode

-- | Build tree from spanning tree edges
buildTreeFromEdges :: Int -> Array (Tuple Int Int) -> Tree Int
buildTreeFromEdges rootId edges = go rootId
  where
  getChildren :: Int -> Array Int
  getChildren nodeId =
    Array.mapMaybe (\(Tuple src tgt) -> if src == nodeId then Just tgt else Nothing) edges

  go :: Int -> Tree Int
  go nodeId =
    let
      children = getChildren nodeId
      childTrees = go <$> children
    in
      mkTree nodeId (List.fromFoldable childTrees)

-- | Map function over tree values
mapTree :: forall a b. (a -> b) -> Tree a -> Tree b
mapTree f t = mkTree (f (head t)) (map (mapTree f) (tail t))

-- | Flatten tree to map of positions
flattenTreeToPositionMap :: Tree { id :: Int, x :: Number, y :: Number, depth :: Int, height :: Int } -> Map Int { x :: Number, y :: Number }
flattenTreeToPositionMap tree = go tree Map.empty
  where
  go t acc =
    let node = head t
        children = tail t
        acc' = Map.insert node.id { x: node.x, y: node.y } acc
    in
      foldl (\a child -> go child a) acc' children

-- | Project layout coordinates to cartesian and radial
-- |
-- | Vertical tree: center horizontally, root at top
-- | Horizontal tree: center vertically, root at left
-- | Radial tree: polar projection, root at center
-- |
-- | IMPORTANT: Must match treemap viewBox dimensions!
-- | Treemap viewBox: -950 -570 1900 1140 (centered at origin)
projectToCartesian :: TreeStyle.TreeOrientation -> Number -> Maybe { x :: Number, y :: Number } -> { cartX :: Number, cartY :: Number, radialX :: Number, radialY :: Number }
projectToCartesian orient layoutSize mPos =
  case mPos of
    Nothing -> { cartX: 0.0, cartY: 0.0, radialX: 0.0, radialY: 0.0 }
    Just { x: treeX, y: treeY } ->
      let
        -- Tree parameters - MUST match treemap viewBox!
        -- viewBox is -950 -570 1900 1140, so:
        -- width = 1900 (from -950 to +950)
        -- height = 1140 (from -570 to +570)
        -- Use slightly smaller to leave margin
        maxTreeWidth = 1800.0   -- fit within -900 to +900
        maxTreeHeight = 1000.0  -- fit within -500 to +500

        -- Radial tree parameters
        maxRadius = 500.0  -- fit within viewBox

        -- Cartesian projection depends on orientation
        -- treeX/layoutSize goes 0->1, treeY/layoutSize goes 0->1
        { cartX, cartY } = case orient of
          TreeStyle.Vertical ->
            -- Root at top, tree grows downward
            { cartX: (treeX / layoutSize - 0.5) * maxTreeWidth
            , cartY: (treeY / layoutSize - 0.5) * maxTreeHeight
            }
          TreeStyle.Horizontal ->
            -- Root at left, tree grows rightward
            -- Swap X and Y: treeY becomes cartX, treeX becomes cartY
            { cartX: (treeY / layoutSize - 0.5) * maxTreeWidth
            , cartY: (treeX / layoutSize - 0.5) * maxTreeHeight
            }
          TreeStyle.Radial ->
            -- Radial projection (polar, root at center)
            let angle = (treeX / layoutSize) * 2.0 * pi - (pi / 2.0)
                rad = (treeY / layoutSize) * maxRadius
            in { cartX: rad * cos angle, cartY: rad * sin angle }
          TreeStyle.Custom ->
            -- Default to vertical for custom
            { cartX: (treeX / layoutSize - 0.5) * maxTreeWidth
            , cartY: (treeY / layoutSize - 0.5) * maxTreeHeight
            }

        -- Radial projection (polar, root at center) - kept for backward compat
        angle = (treeX / layoutSize) * 2.0 * pi - (pi / 2.0)
        rad = (treeY / layoutSize) * maxRadius
        radialX = rad * cos angle
        radialY = rad * sin angle
      in
        { cartX, cartY, radialX, radialY }
