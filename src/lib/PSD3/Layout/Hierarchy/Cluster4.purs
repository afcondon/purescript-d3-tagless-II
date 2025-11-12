-- | PSD3.Layout.Hierarchy.Cluster4
-- |
-- | Cluster (dendrogram) layout using Data.Tree
-- | Uses Reingold-Tilford algorithm for x-positioning (like Tree4)
-- | But positions nodes by height (distance from leaves) instead of depth
-- | Result: All leaf nodes appear at the same level (y = 0)
module PSD3.Layout.Hierarchy.Cluster4 where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable)
import Data.Foldable (maximum, minimum)
import Data.Foldable as Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Array as Array
import Data.Int (toNumber)

-- | Configuration for cluster layout
type ClusterConfig =
  { size :: { width :: Number, height :: Number }
  , minSeparation :: Number  -- Minimum horizontal separation between siblings
  }

-- | Default configuration
defaultClusterConfig :: ClusterConfig
defaultClusterConfig =
  { size: { width: 800.0, height: 600.0 }
  , minSeparation: 1.0
  }


-- | Cluster layout in 3 steps:
-- | 1. addHeight: Bottom-up pass computing height (distance from deepest leaf)
-- | 2. render: Bottom-up pass assigning sequential x to leaves, mean x to parents
-- | 3. addY: Add y coordinates based on height (leaves at 0)
-- | 4. scale: Scale abstract coordinates to pixel coordinates
-- |
-- | Key difference from Tree4:
-- | - All leaves get sequential x positions (not contour-based)
-- | - y is based on height (distance from leaves) not depth
-- | - All leaves appear at y = 0 (dendrogram)
-- |
-- | Input must have x, y, height fields (initial values don't matter, they'll be overwritten)
cluster :: forall r.
  ClusterConfig ->
  Tree { x :: Number, y :: Number, height :: Int | r } ->
  Tree { x :: Number, y :: Number, height :: Int | r }
cluster config inputTree =
  let
    -- Step 1: Compute height field (distance from deepest leaf)
    withHeight = addHeight inputTree

    -- Step 2: Assign sequential x positions to leaves
    rendered = render config.minSeparation withHeight

    -- Step 3: Add y coordinates based on height
    withCoords = addYCoordinates rendered.tree

    -- Step 4: Scale to final pixel coordinates
    scaled = scaleToPixels config withCoords
  in
    scaled

-- | Compute height field (distance from deepest leaf)
-- | Bottom-up traversal: leaves get 0, parents get 1 + max(children's height)
addHeight :: forall r. Tree { x :: Number, y :: Number, height :: Int | r } -> Tree { x :: Number, y :: Number, height :: Int | r }
addHeight (Node val children) =
  let
    -- Recursively compute children's heights
    childrenWithHeight = map addHeight children

    -- Leaf: height = 0
    -- Internal: height = 1 + max(children's height)
    nodeHeight = case Array.fromFoldable childrenWithHeight of
      [] -> 0
      childArray ->
        let
          childHeights = map (\(Node v _) -> v.height) childArray
          maxChildHeight = fromMaybe 0 $ maximum childHeights
        in
          1 + maxChildHeight
  in
    Node (val { height = nodeHeight }) childrenWithHeight

-- | Bottom-up pass: assign sequential x positions to ALL leaves
-- | Then set parent x = mean of children x
-- | This is simpler than Tree4 - no contour scanning needed for dendrograms
-- | Returns tree with absolute x positions (not offsets like Tree4)
render :: forall r.
  Number ->
  Tree { x :: Number, y :: Number, height :: Int | r } ->
  { tree :: Tree { x :: Number, y :: Number, height :: Int | r }, lastLeafX :: Number }
render minSep inputTree =
  renderInternal 0.0 inputTree
  where
    -- Thread through lastLeafX to assign sequential positions
    renderInternal :: Number -> Tree { x :: Number, y :: Number, height :: Int | r } -> { tree :: Tree { x :: Number, y :: Number, height :: Int | r }, lastLeafX :: Number }
    renderInternal currentLeafX (Node val children) =
      case Array.fromFoldable children of
        -- Leaf node: assign sequential x position
        [] ->
          let leafX = currentLeafX
              leafNode = Node (val { x = leafX }) Nil
          in { tree: leafNode, lastLeafX: leafX + minSep }

        -- Internal node: process children, then set x = mean of children x
        childArray ->
          let
            -- Process all children, threading through lastLeafX
            processChildren = foldl
              (\acc child ->
                let result = renderInternal acc.lastLeafX child
                in { trees: Array.snoc acc.trees result.tree
                   , lastLeafX: result.lastLeafX
                   }
              )
              { trees: [], lastLeafX: currentLeafX }
              childArray

            childTrees = processChildren.trees

            -- Compute mean of children's x values
            childrenList = fromFoldable childTrees
            meanX = foldl (\sum (Node v _) -> sum + v.x) 0.0 childTrees / toNumber (Array.length childTrees)

            internalNode = Node (val { x = meanX }) childrenList
          in
            { tree: internalNode, lastLeafX: processChildren.lastLeafX }

    foldl :: forall a b. (a -> b -> a) -> a -> Array b -> a
    foldl f init arr = Data.Foldable.foldl f init arr

-- | Add y coordinates based on height
-- | For cluster: y = height (leaves have height=0, so y=0, all at same level)
addYCoordinates :: forall r. Tree { x :: Number, y :: Number, height :: Int | r } -> Tree { x :: Number, y :: Number, height :: Int | r }
addYCoordinates (Node val children) =
  let
    -- y is simply the height (distance from deepest leaf)
    nodeY = toNumber val.height
    -- Recursively process children
    childrenWithY = map addYCoordinates children
  in
    Node (val { y = nodeY }) childrenWithY

-- | Scale abstract coordinates to pixel coordinates
scaleToPixels :: forall r.
  ClusterConfig ->
  Tree { height :: Int, x :: Number, y :: Number | r } ->
  Tree { height :: Int, x :: Number, y :: Number | r }
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
    go (Node val children) =
      Node
        (val { x = scaleX val.x, y = scaleY val.height })
        (map go children)
  in
    go inputTree
