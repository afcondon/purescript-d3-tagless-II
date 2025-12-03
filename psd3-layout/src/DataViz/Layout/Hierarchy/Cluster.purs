-- | DataViz.Layout.Hierarchy.Cluster
-- |
-- | Cluster (dendrogram) layout using Data.Tree
-- | Uses Reingold-Tilford algorithm for x-positioning (like Tree)
-- | But positions nodes by height (distance from leaves) instead of depth
-- | Result: All leaf nodes appear at the same level (y = 0)
module DataViz.Layout.Hierarchy.Cluster
  ( ClusterConfig
  , defaultClusterConfig
  , cluster
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Tree (Tree, mkTree)
import Data.List (List(..), fromFoldable)
import Data.Foldable (maximum, minimum)
import Data.Foldable as Data.Foldable
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Int (toNumber)

-- | Configuration for cluster layout
type ClusterConfig =
  { size :: { width :: Number, height :: Number }
  , minSeparation :: Number -- Minimum horizontal separation between siblings
  }

-- | Default configuration
defaultClusterConfig :: ClusterConfig
defaultClusterConfig =
  { size: { width: 800.0, height: 600.0 }
  , minSeparation: 1.0
  }

-- | Cluster layout in 4 steps:
-- | 1. addHeight: Bottom-up pass computing height (distance from deepest leaf)
-- | 2. sortByHeight: Sort children by height to minimize crossovers (D3 pattern)
-- | 3. render: Bottom-up pass assigning sequential x to leaves, mean x to parents
-- | 4. addY: Add y coordinates based on height (leaves at 0)
-- | 5. scale: Scale abstract coordinates to pixel coordinates
-- |
-- | Key difference from Tree:
-- | - All leaves get sequential x positions (not contour-based)
-- | - y is based on height (distance from leaves) not depth
-- | - All leaves appear at y = 0 (dendrogram)
-- | - Children sorted by height (descending) to minimize crossovers
-- |
-- | Input must have x, y, height fields (initial values don't matter, they'll be overwritten)
cluster
  :: forall r
   . ClusterConfig
  -> Tree { x :: Number, y :: Number, height :: Int | r }
  -> Tree { x :: Number, y :: Number, height :: Int | r }
cluster config inputTree =
  let
    -- Step 1: Compute height field (distance from deepest leaf)
    withHeight = addHeight inputTree

    -- Step 2: Sort children by height (descending) to minimize crossovers
    sorted = sortByHeight withHeight

    -- Step 3: Assign sequential x positions to leaves
    rendered = render config.minSeparation sorted

    -- Step 4: Center root on the entire tree (all leaves)
    centered = centerRoot rendered.tree

    -- Step 5: Add y coordinates based on height
    withCoords = addYCoordinates centered

    -- Step 6: Scale to final pixel coordinates
    scaled = scaleToPixels config withCoords
  in
    scaled

-- | Center the root node on the full tree
-- | After rendering, the root is centered on its immediate children
-- | This adjusts all x positions so the root aligns with the midpoint of all LEAVES
centerRoot :: forall r. Tree { x :: Number, y :: Number, height :: Int | r } -> Tree { x :: Number, y :: Number, height :: Int | r }
centerRoot tree =
  let
    -- Find leftmost and rightmost leaves (leaves are positioned sequentially)
    allNodes = Array.fromFoldable tree
    leaves = Array.filter (\n -> n.height == 0) allNodes

    -- First and last leaves define the extent
    firstLeaf = Array.head leaves
    lastLeaf = Array.last leaves

    minX = case firstLeaf of
      Just leaf -> leaf.x
      Nothing -> 0.0

    maxX = case lastLeaf of
      Just leaf -> leaf.x
      Nothing -> 0.0

    -- Find root's current x
    root = head tree
    rootX = root.x

    -- Calculate where root should be (center of LEAF spread)
    targetX = (minX + maxX) / 2.0

    -- Calculate offset to shift entire tree
    offset = targetX - rootX

    -- Shift all nodes by offset
    shiftTree :: Tree { x :: Number, y :: Number, height :: Int | r } -> Tree { x :: Number, y :: Number, height :: Int | r }
    shiftTree t =
      let val = head t
          children = tail t
      in mkTree (val { x = val.x + offset }) (map shiftTree children)
  in
    shiftTree tree

-- | Sort children by height (descending) to minimize crossovers
-- | This is the D3 recommended pattern for dendrograms
-- | Ensures deeper subtrees are positioned before shallower ones
sortByHeight :: forall r. Tree { x :: Number, y :: Number, height :: Int | r } -> Tree { x :: Number, y :: Number, height :: Int | r }
sortByHeight t =
  let
    val = head t
    children = tail t
    -- Recursively sort grandchildren first
    sortedGrandchildren = map sortByHeight children

    -- Convert to Array, sort, then back to List
    childArray = Array.fromFoldable sortedGrandchildren
    sortedArray = Array.sortBy compareByHeight childArray
    sortedChildrenList = fromFoldable sortedArray
  in
    mkTree val sortedChildrenList
  where
  compareByHeight :: forall s. Tree { height :: Int | s } -> Tree { height :: Int | s } -> Ordering
  compareByHeight t1 t2 = compare (head t2).height (head t1).height -- Descending

-- | Compute height field (distance from deepest leaf)
-- | Bottom-up traversal: leaves get 0, parents get 1 + max(children's height)
addHeight :: forall r. Tree { x :: Number, y :: Number, height :: Int | r } -> Tree { x :: Number, y :: Number, height :: Int | r }
addHeight t =
  let
    val = head t
    children = tail t
    -- Recursively compute children's heights
    childrenWithHeight = map addHeight children

    -- Leaf: height = 0
    -- Internal: height = 1 + max(children's height)
    nodeHeight = case Array.fromFoldable childrenWithHeight of
      [] -> 0
      childArray ->
        let
          childHeights = map (\c -> (head c).height) childArray
          maxChildHeight = fromMaybe 0 $ maximum childHeights
        in
          1 + maxChildHeight
  in
    mkTree (val { height = nodeHeight }) childrenWithHeight

-- | Bottom-up pass: assign sequential x positions to ALL leaves
-- | Then set parent x = midpoint between leftmost and rightmost descendants
-- | This centers each parent on its subtree's full extent, reducing crossovers
-- | Returns tree with absolute x positions (not offsets like Tree)
render
  :: forall r
   . Number
  -> Tree { x :: Number, y :: Number, height :: Int | r }
  -> { tree :: Tree { x :: Number, y :: Number, height :: Int | r }, lastLeafX :: Number }
render minSep inputTree =
  renderInternal 0.0 inputTree
  where
  -- Find extent (min and max x) of all leaves in a subtree
  findExtent :: Tree { x :: Number, y :: Number, height :: Int | r } -> { minX :: Number, maxX :: Number }
  findExtent t =
    let val = head t
        children = tail t
    in case Array.fromFoldable children of
      [] -> { minX: val.x, maxX: val.x } -- Leaf node
      childArray ->
        let
          childExtents = map findExtent childArray
          minX = fromMaybe val.x $ minimum $ map (\e -> e.minX) childExtents
          maxX = fromMaybe val.x $ maximum $ map (\e -> e.maxX) childExtents
        in
          { minX, maxX }

  -- Thread through lastLeafX to assign sequential positions
  renderInternal :: Number -> Tree { x :: Number, y :: Number, height :: Int | r } -> { tree :: Tree { x :: Number, y :: Number, height :: Int | r }, lastLeafX :: Number }
  renderInternal currentLeafX t =
    let val = head t
        children = tail t
    in case Array.fromFoldable children of
      -- Leaf node: assign sequential x position
      [] ->
        let
          leafX = currentLeafX
          leafNode = mkTree (val { x = leafX }) Nil
        in
          { tree: leafNode, lastLeafX: leafX + minSep }

      -- Internal node: process children, then center on subtree extent
      childArray ->
        let
          -- Process all children, threading through lastLeafX
          processChildren = foldl
            ( \acc child ->
                let
                  result = renderInternal acc.lastLeafX child
                in
                  { trees: Array.snoc acc.trees result.tree
                  , lastLeafX: result.lastLeafX
                  }
            )
            { trees: [], lastLeafX: currentLeafX }
            childArray

          childTrees = processChildren.trees
          childrenList = fromFoldable childTrees

          -- Center parent on full subtree extent (leftmost to rightmost leaf)
          -- Fold over all children to find the min and max x positions
          allChildExtents = map findExtent childTrees
          minX = fromMaybe 0.0 $ minimum $ map (\e -> e.minX) allChildExtents
          maxX = fromMaybe 0.0 $ maximum $ map (\e -> e.maxX) allChildExtents
          centerX = (minX + maxX) / 2.0

          internalNode = mkTree (val { x = centerX }) childrenList
        in
          { tree: internalNode, lastLeafX: processChildren.lastLeafX }

  foldl :: forall a b. (a -> b -> a) -> a -> Array b -> a
  foldl f init arr = Data.Foldable.foldl f init arr

-- | Add y coordinates based on height
-- | For cluster: y = height (leaves have height=0, so y=0, all at same level)
addYCoordinates :: forall r. Tree { x :: Number, y :: Number, height :: Int | r } -> Tree { x :: Number, y :: Number, height :: Int | r }
addYCoordinates t =
  let
    val = head t
    children = tail t
    -- y is simply the height (distance from deepest leaf)
    nodeY = toNumber val.height
    -- Recursively process children
    childrenWithY = map addYCoordinates children
  in
    mkTree (val { y = nodeY }) childrenWithY

-- | Scale abstract coordinates to pixel coordinates
scaleToPixels
  :: forall r
   . ClusterConfig
  -> Tree { height :: Int, x :: Number, y :: Number | r }
  -> Tree { height :: Int, x :: Number, y :: Number | r }
scaleToPixels config inputTree =
  let
    -- Find x range using Foldable instance
    allNodes = Array.fromFoldable inputTree
    allX = map (\n -> n.x) allNodes
    minX = fromMaybe 0.0 $ minimum allX
    maxX = fromMaybe 1.0 $ maximum allX
    xRange = if maxX - minX == 0.0 then 1.0 else maxX - minX

    -- Find max height (all leaves are at 0, root is at max height)
    allHeights = map (\n -> n.height) allNodes
    maxHeight = fromMaybe 1 $ maximum allHeights

    -- Scale functions
    scaleX x = ((x - minX) / xRange) * config.size.width
    -- For cluster: invert y so leaves are at bottom (or right/outside for projections)
    -- y = 0 (leaves) should map to size.height
    -- y = maxHeight (root) should map to 0
    scaleY height = (1.0 - (toNumber height / toNumber maxHeight)) * config.size.height

    -- Apply scaling via map
    go t =
      let val = head t
          children = tail t
      in mkTree (val { x = scaleX val.x, y = scaleY val.height }) (map go children)
  in
    go inputTree
