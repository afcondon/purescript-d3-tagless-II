module PSD3.Layout.Hierarchy.Cluster
  ( ClusterNode(..)
  , ClusterConfig
  , SeparationFn
  , defaultClusterConfig
  , cluster
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Foldable (foldl)
import PSD3.Layout.Hierarchy.Types (HierarchyNode(..))

-- | Cluster layout node with x, y positions
-- The cluster layout creates dendrograms - node-link diagrams that place leaf nodes
-- at the same depth. Parents are centered over their children.
newtype ClusterNode a = ClusterNode
  { data_ :: a
  , depth :: Int
  , x :: Number
  , y :: Number
  , children :: Array (ClusterNode a)
  }

derive instance Functor ClusterNode

-- | Configuration for cluster layout
type ClusterConfig =
  { size :: { width :: Number, height :: Number }
  , separation :: SeparationFn
  , nodeSize :: Maybe { width :: Number, height :: Number }
  }

type SeparationFn = forall a. ClusterNode a -> ClusterNode a -> Number

-- | Default separation: 1 for siblings, 2 for non-siblings
-- Requires parent reference which we approximate by checking if both are leaves
defaultSeparation :: forall a. ClusterNode a -> ClusterNode a -> Number
defaultSeparation (ClusterNode a) (ClusterNode b) =
  -- In D3, this checks if a.parent === b.parent
  -- Since we don't have parent refs here, we use a simple heuristic
  -- This will be correct when called from the algorithm
  if a.depth == b.depth then 1.0 else 2.0

-- | Default cluster configuration
defaultClusterConfig :: ClusterConfig
defaultClusterConfig =
  { size: { width: 1.0, height: 1.0 }
  , separation: defaultSeparation
  , nodeSize: Nothing
  }

-- | Main cluster layout function
-- Implements D3's cluster layout algorithm for dendrograms
--
-- Algorithm:
-- 1. First walk (post-order):
--    - Leaves get sequential x positions based on separation
--    - Parents get x = mean of children's x values
--    - Parents get y = 1 + max of children's y values
-- 2. Find leftmost and rightmost leaves
-- 3. Second walk (post-order):
--    - Normalize x to fit size
--    - Normalize y to fit size (inverted: root at top)
cluster :: forall a. ClusterConfig -> HierarchyNode a -> ClusterNode a
cluster config root =
  let
    -- Step 1: First walk - compute initial x and y positions
    { tree: initialTree, lastLeaf: _ } = firstWalk config root

    -- Step 2: Find leftmost and rightmost leaves for scaling
    left = leafLeft initialTree
    right = leafRight initialTree
    ClusterNode leftNode = left
    ClusterNode rightNode = right

    -- Calculate x extent
    x0 = leftNode.x - (config.separation left right) / 2.0
    x1 = rightNode.x + (config.separation right left) / 2.0

    -- Step 3: Second walk - normalize to desired size
    ClusterNode rootNode = initialTree
    normalizedTree = case config.nodeSize of
      Just nodeSize ->
        -- nodeSize mode: center around root, scale by node size
        secondWalkNodeSize initialTree rootNode.x rootNode.y nodeSize.width nodeSize.height
      Nothing ->
        -- size mode: normalize to [0, 1] then scale
        secondWalkSize initialTree x0 x1 rootNode.y config.size.width config.size.height

  in normalizedTree

-- | Convert HierarchyNode to ClusterNode (no layout yet)
hierarchyToClusterNode :: forall a. HierarchyNode a -> ClusterNode a
hierarchyToClusterNode (HNode h) =
  ClusterNode
    { data_: h.data_
    , depth: h.depth
    , x: 0.0
    , y: 0.0
    , children: map hierarchyToClusterNode h.children
    }

-- Internal representation for tracking parent information during first walk
type InternalCluster a =
  { node :: ClusterNode a
  , parent :: Maybe Int
  , nodeId :: Int
  }

-- | First walk: compute initial positions (post-order traversal)
-- Uses internal representation to track parents for correct separation
firstWalk :: forall a. ClusterConfig -> HierarchyNode a -> { tree :: ClusterNode a, lastLeaf :: Maybe (ClusterNode a) }
firstWalk config root =
  let
    -- Build tree with internal tracking, starting with nodeId 0, no parent
    result = firstWalkInternal config root Nothing 0 Nothing
  in
    { tree: result.tree, lastLeaf: map _.node result.lastLeaf }

firstWalkInternal :: forall a. ClusterConfig -> HierarchyNode a -> Maybe Int -> Int -> Maybe (InternalCluster a) -> { tree :: ClusterNode a, lastLeaf :: Maybe (InternalCluster a), nextId :: Int }
firstWalkInternal config (HNode node) parentId myId previousLeaf =
  if Array.length node.children == 0 then
    -- Leaf node: assign sequential x position
    let
      x = case previousLeaf of
        Nothing -> 0.0
        Just prevLeaf ->
          let
            -- Check if same parent for separation
            sameParent = case prevLeaf.parent, parentId of
              Just p1, Just p2 -> p1 == p2
              _, _ -> false

            sep = if sameParent then 1.0 else 2.0
            ClusterNode prev = prevLeaf.node
          in prev.x + sep

      leafNode = ClusterNode
        { data_: node.data_
        , depth: node.depth
        , x: x
        , y: 0.0
        , children: []
        }

      internal = { node: leafNode, parent: parentId, nodeId: myId }
    in
      { tree: leafNode, lastLeaf: Just internal, nextId: myId + 1 }
  else
    -- Internal node: process children then compute mean x and max y
    let
      -- Process all children, threading through lastLeaf
      processChildren = foldl
        (\acc child ->
          let result = firstWalkInternal config child (Just myId) acc.nextId acc.lastLeaf
          in { children: Array.snoc acc.children result.tree
             , lastLeaf: result.lastLeaf
             , nextId: result.nextId
             }
        )
        { children: [], lastLeaf: previousLeaf, nextId: myId + 1 }
        node.children

      childrenNodes = processChildren.children

      -- Compute mean of children's x values
      meanX = foldl (\sum (ClusterNode c) -> sum + c.x) 0.0 childrenNodes / toNumber (Array.length childrenNodes)

      -- Compute max of children's y values, then add 1
      maxY = 1.0 + foldl (\m (ClusterNode c) -> max m c.y) 0.0 childrenNodes

      internalNode = ClusterNode
        { data_: node.data_
        , depth: node.depth
        , x: meanX
        , y: maxY
        , children: childrenNodes
        }
    in
      { tree: internalNode, lastLeaf: processChildren.lastLeaf, nextId: processChildren.nextId }

-- | Find the leftmost leaf (follow first child repeatedly)
leafLeft :: forall a. ClusterNode a -> ClusterNode a
leafLeft node@(ClusterNode n) =
  case Array.head n.children of
    Nothing -> node
    Just child -> leafLeft child

-- | Find the rightmost leaf (follow last child repeatedly)
leafRight :: forall a. ClusterNode a -> ClusterNode a
leafRight node@(ClusterNode n) =
  case Array.last n.children of
    Nothing -> node
    Just child -> leafRight child

-- | Second walk: normalize x and y to size (post-order)
secondWalkSize :: forall a. ClusterNode a -> Number -> Number -> Number -> Number -> Number -> ClusterNode a
secondWalkSize (ClusterNode node) x0 x1 rootY dx dy =
  let
    -- Normalize x to [0, 1] range, then scale by dx
    normalizedX = (node.x - x0) / (x1 - x0) * dx

    -- Normalize y: invert (root at top) and scale by dy
    normalizedY = (1.0 - (if rootY == 0.0 then 1.0 else node.y / rootY)) * dy

    -- Recursively normalize children
    normalizedChildren = map (\child -> secondWalkSize child x0 x1 rootY dx dy) node.children
  in
    ClusterNode
      { data_: node.data_
      , depth: node.depth
      , x: normalizedX
      , y: normalizedY
      , children: normalizedChildren
      }

-- | Second walk: normalize for nodeSize mode (post-order)
secondWalkNodeSize :: forall a. ClusterNode a -> Number -> Number -> Number -> Number -> ClusterNode a
secondWalkNodeSize (ClusterNode node) rootX rootY dx dy =
  let
    -- Center around root, scale by node size
    normalizedX = (node.x - rootX) * dx
    normalizedY = (rootY - node.y) * dy

    -- Recursively normalize children
    normalizedChildren = map (\child -> secondWalkNodeSize child rootX rootY dx dy) node.children
  in
    ClusterNode
      { data_: node.data_
      , depth: node.depth
      , x: normalizedX
      , y: normalizedY
      , children: normalizedChildren
      }
