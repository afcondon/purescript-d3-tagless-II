module D3.Viz.Spago.Tree where

import Prelude

import D3.Viz.Spago.Files (LinkType(..), SpagoLink, isP2P_Link)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, TreeFields, spagoGraphConfig)
import PSD3.Data.Graph.Algorithms (getReachableNodes)
import PSD3.Data.Node (NodeID)
import DataViz.Layout.Hierarchy.Tree as Tree
import Data.Array (elem, filter, partition)
import Data.Number (pi)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import Control.Comonad.Cofree (head, tail)
import Data.Tree (Tree, mkTree)
import Data.Tuple (Tuple(..))

-- | Helper to create links from tuples
tupleToLink :: LinkType -> Tuple NodeID NodeID -> SpagoLink
tupleToLink linktype (Tuple source target) =
  { source, target, linktype, inSim: true }

-- | Change the linktype of an existing link
changeLinkType :: LinkType -> SpagoLink -> SpagoLink
changeLinkType newLinktype link = link { linktype = newLinktype }

-- | Build tree structure from rootID and dependency links
-- | Returns: Tree of NodeIDs representing the dependency hierarchy
buildIDTree :: NodeID -> Array SpagoLink -> Tree NodeID
buildIDTree rootID links = go rootID
  where
  -- Extract children for a given node from links
  getChildren :: NodeID -> Array NodeID
  getChildren nodeId =
    let
      matchingLinks = filter (\link -> link.source == nodeId) links
    in
      _.target <$> matchingLinks

  -- Recursively build tree
  go :: NodeID -> Tree NodeID
  go nodeId =
    let
      children = getChildren nodeId
      childTrees = go <$> children
    in
      mkTree nodeId (List.fromFoldable childTrees)

-- | Flatten tree structure to map of node positions
-- | Recursively walks tree and builds Map NodeID TreeFields
flattenTreeToMap :: forall r. Tree { id :: NodeID, x :: Number, y :: Number, depth :: Int | r } -> Map NodeID TreeFields
flattenTreeToMap tree = go tree M.empty
  where
  go :: Tree { id :: NodeID, x :: Number, y :: Number, depth :: Int | r } -> Map NodeID TreeFields -> Map NodeID TreeFields
  go t acc =
    let
      node = head t
      children = tail t
      isLeaf = case children of
        Nil -> true
        _ -> false
      childIDs = Array.fromFoldable $ (\child -> (head child).id) <$> children
      treeFields = { x: node.x, y: node.y, isTreeLeaf: isLeaf, depth: node.depth, childIDs }
      accWithNode = M.insert node.id treeFields acc
    in
      foldl (\m child -> go child m) accWithNode children

-- | Main tree reduction function
-- | Builds dependency tree, runs layout, updates node positions
treeReduction :: NodeID -> SpagoModel -> SpagoModel
treeReduction rootID model = do
  let
    reachable = getReachableNodes spagoGraphConfig rootID model.graphModel
    reachableNodes = Array.fromFoldable reachable.nodes

    -- Separate package links from module links
    onlyPackageLinks = filter isP2P_Link model.links

    -- Use spanning tree edges directly (these are the tree links)
    treelinks = (tupleToLink M2M_Tree) <$> reachable.spanningTree

    -- Mark redundant edges as graph links (not tree links)
    prunedTreeLinks = (tupleToLink M2M_Graph) <$> reachable.redundantEdges

    -- Partition nodes: reachable (in tree) vs unreachable
    treenodes = partition
      (\n -> (n.id `elem` reachableNodes) || n.id == rootID)
      model.nodes

    -- Build tree structure from dependency links
    idTree = buildIDTree rootID treelinks

    -- Convert NodeID tree to tree with position fields
    -- We need to map each NodeID to its simulation node data
    nodeMap = M.fromFoldable $ (\n -> Tuple n.id n) <$> model.nodes

    -- Create tree with node data (x, y, depth fields initialized to 0)
    dataTree = mapTree
      ( \nodeId ->
          case M.lookup nodeId nodeMap of
            Just node -> { id: nodeId, x: node.x, y: node.y, depth: 0 }
            Nothing -> { id: nodeId, x: 0.0, y: 0.0, depth: 0 }
      )
      idTree

    -- Run tree layout
    -- For radial tree: width = 2π (full circle in radians), height = max radius
    -- The height is divided by treeDepthMultiplier (2.0) in rendering, so use 800 for 400px radius
    config = { size: { width: 2.0 * pi, height: 800.0 }, minSeparation: 0.1, separation: Nothing, layerScale: Nothing }
    laidOutTree = Tree.tree config dataTree

    -- Flatten tree to map of positions
    treeDerivedDataMap = flattenTreeToMap laidOutTree

    -- Update nodes with tree positions
    positionedNodes = updateNodeTreeXY treenodes.yes treeDerivedDataMap
    unpositionedNodes = treenodes.no

    -- Combine all links with correct types
    links = treelinks <> prunedTreeLinks <> onlyPackageLinks

  model
    { links = links
    , nodes = positionedNodes <> unpositionedNodes
    , tree = model.tree -- Keep existing tree (or Nothing) - positions are in id2TreeData
    , maps = model.maps { id2TreeData = treeDerivedDataMap }
    }

-- | Map function over tree values
mapTree :: forall a b. (a -> b) -> Tree a -> Tree b
mapTree f t = mkTree (f (head t)) (mapTree f <$> tail t)

-- | Update simulation nodes with tree XY positions
-- | IMPORTANT: Also sets connected = true for nodes in the tree
updateNodeTreeXY :: Array SpagoSimNode -> Map NodeID TreeFields -> Array SpagoSimNode
updateNodeTreeXY nodes treeDerivedDataMap = updateNode <$> nodes
  where
  updateNode :: SpagoSimNode -> SpagoSimNode
  updateNode node =
    case M.lookup node.id treeDerivedDataMap of
      Nothing -> node
      Just treeFields ->
        node
          { treeXY = notNull { x: treeFields.x, y: treeFields.y }
          , treeDepth = notNull treeFields.depth
          , connected = true -- Mark as part of tree so isUsedModule returns true
          }

-- | Build tree is the main entry point (alias for treeReduction)
buildTree :: NodeID -> SpagoModel -> SpagoModel
buildTree = treeReduction
