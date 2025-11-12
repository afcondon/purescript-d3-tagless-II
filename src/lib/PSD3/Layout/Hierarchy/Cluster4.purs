-- | PSD3.Layout.Hierarchy.Cluster4
-- |
-- | Cluster (dendrogram) layout using Data.Tree
-- | Uses Reingold-Tilford algorithm for x-positioning (like Tree4)
-- | But positions nodes by height (distance from leaves) instead of depth
-- | Result: All leaf nodes appear at the same level (y = 0)
module PSD3.Layout.Hierarchy.Cluster4 where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable, length, zipWith, scanl)
import Data.Foldable (foldl, maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
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

-- | A contour is a list of offsets at each depth level
-- | Represents the left or right edge of a subtree
type Contour = List Number

-- | Contours for a subtree: left edge and right edge at each depth
data Contours = Contours
  { left :: Contour   -- Leftmost x offset at each depth
  , right :: Contour  -- Rightmost x offset at each depth
  }

-- | Empty contours for a leaf
emptyContours :: Contours
emptyContours = Contours { left: Nil, right: Nil }

-- | Single node contours (just the root at position 0)
singletonContours :: Contours
singletonContours = Contours { left: Cons 0.0 Nil, right: Cons 0.0 Nil }

-- | Cluster layout in 4 steps:
-- | 1. addHeight: Bottom-up pass computing height (distance from deepest leaf)
-- | 2. render: Bottom-up pass computing relative x positions using contours
-- | 3. petrify: Top-down pass converting distances to absolute (x,y) coordinates
-- | 4. scale: Scale abstract coordinates to pixel coordinates
-- |
-- | Key difference from Tree4: y is based on height (distance from leaves) not depth
-- | This makes all leaves appear at y = 0 (dendrogram)
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

    -- Step 2: Compute relative x positions (bottom-up, same as Tree4)
    rendered = render config.minSeparation withHeight

    -- Step 3: Convert offsets to absolute coordinates (top-down)
    -- For cluster: y = height (0 for leaves, increases toward root)
    withCoords = petrify 0.0 rendered

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

-- | Bottom-up pass: compute relative x positions using contour scanning
-- | This is identical to Tree4's render function
-- | Annotates tree with Tuple containing offset and original data
render :: forall r.
  Number ->
  Tree { x :: Number, y :: Number, height :: Int | r } ->
  Tree (Tuple { offset :: Number } { x :: Number, y :: Number, height :: Int | r })
render minSep inputTree =
  case renderWithContours inputTree of
    Tuple t _ -> t
  where
    -- Internal function that also returns contours (captures minSep from outer scope)
    renderWithContours :: Tree { x :: Number, y :: Number, height :: Int | r } -> Tuple (Tree (Tuple { offset :: Number } { x :: Number, y :: Number, height :: Int | r })) Contours
    renderWithContours (Node val children) =
      let childCount = length children
      in case childCount of
        -- Leaf node: offset = 0, contours = singleton
        0 -> Tuple (Node (Tuple { offset: 0.0 } val) Nil) singletonContours

        -- Internal node: process children, position them, compute contours
        _ ->
          let
            -- Recursively process all children to get their trees and contours
            childResults = map renderWithContours children
            childTrees = map (\(Tuple t _) -> t) childResults
            childContours = map (\(Tuple _ c) -> c) childResults

            -- Scan adjacent pairs to find needed separations
            separations = computeSeparations minSep childContours

            -- Position children: first at 0, rest offset by cumulative separations
            -- PureScript's scanl doesn't include the initial value
            childAbsoluteOffsets = Cons 0.0 (scanl (+) 0.0 separations)

            -- Parent is centered at midpoint of leftmost and rightmost child
            leftmostOffset = fromMaybe 0.0 $ Array.head $ Array.fromFoldable childAbsoluteOffsets
            rightmostOffset = fromMaybe 0.0 $ Array.last $ Array.fromFoldable childAbsoluteOffsets
            parentOffset = (leftmostOffset + rightmostOffset) / 2.0

            -- Convert child offsets to be relative to parent's centered position
            childRelativeOffsets = map (\absOffset -> absOffset - parentOffset) childAbsoluteOffsets

            -- Update each child tree with its relative offset
            childrenWithOffsets = zipWith updateOffset childTrees childRelativeOffsets

            -- Combine child contours into parent contours
            parentContours = spliceContours childRelativeOffsets childContours

            resultTree = Node (Tuple { offset: parentOffset } val) childrenWithOffsets
          in
            Tuple resultTree parentContours

    -- Update a child tree's offset in its Tuple annotation
    updateOffset :: Tree (Tuple { offset :: Number } { x :: Number, y :: Number, height :: Int | r }) -> Number -> Tree (Tuple { offset :: Number } { x :: Number, y :: Number, height :: Int | r })
    updateOffset (Node (Tuple offsetRec original) children) newOffset =
      Node (Tuple (offsetRec { offset = newOffset }) original) children

-- | Compute separations needed between adjacent child subtrees
-- | For n children, returns (n-1) separation values
computeSeparations :: Number -> List Contours -> List Number
computeSeparations minSep contours =
  case contours of
    Nil -> Nil
    Cons _ Nil -> Nil
    _ -> zipWith (scanContours minSep) contours (tailSafe contours)
  where
    tailSafe :: forall a. List a -> List a
    tailSafe Nil = Nil
    tailSafe (Cons _ xs) = xs

-- | Scan two contours to find minimum separation needed
-- | Walks down both contours level by level, ensuring minSep at each level
scanContours :: Number -> Contours -> Contours -> Number
scanContours minSep (Contours left) (Contours right) =
  go minSep left.right right.left
  where
    -- Current separation starts at minSep
    -- Walk down right contour of left subtree and left contour of right subtree
    go :: Number -> Contour -> Contour -> Number
    go currentSep Nil _ = currentSep
    go currentSep _ Nil = currentSep
    go currentSep (Cons lr rest1) (Cons rl rest2) =
      let
        -- Distance between the two nodes at this level
        actualSep = currentSep + rl - lr

        -- If too close, increase separation
        neededSep = if actualSep < minSep
                    then currentSep + (minSep - actualSep)
                    else currentSep
      in
        go neededSep rest1 rest2

-- | Combine child contours into parent contours
-- | Given n children at positions offsets[0..n-1], build the parent's contours
spliceContours :: List Number -> List Contours -> Contours
spliceContours offsets contours =
  case Array.fromFoldable $ zipWith Tuple offsets contours of
    [] -> singletonContours  -- No children (shouldn't happen, but safe)
    [Tuple offset (Contours c)] ->
      -- Single child: shift its contours by offset
      Contours
        { left: Cons 0.0 (map (\x -> x + offset) c.left)
        , right: Cons 0.0 (map (\x -> x + offset) c.right)
        }
    pairs ->
      -- Multiple children: leftmost left contour + rightmost right contour
      let
        Tuple firstOffset (Contours firstContours) = fromMaybe (Tuple 0.0 emptyContours) $ Array.head pairs
        Tuple lastOffset (Contours lastContours) = fromMaybe (Tuple 0.0 emptyContours) $ Array.last pairs

        leftContour = Cons 0.0 (map (\x -> x + firstOffset) firstContours.left)
        rightContour = Cons 0.0 (map (\x -> x + lastOffset) lastContours.right)
      in
        Contours { left: leftContour, right: rightContour }

-- | Top-down pass: convert offsets to absolute (x, y) coordinates
-- | Strips Tuple annotation and updates x, y fields in the original record
-- | Each node's x = parent.x + node.offset
-- | Each node's y = height (KEY DIFFERENCE from Tree4: uses height not depth)
petrify :: forall r.
  Number ->  -- Parent's x coordinate
  Tree (Tuple { offset :: Number } { x :: Number, y :: Number, height :: Int | r }) ->
  Tree { x :: Number, y :: Number, height :: Int | r }
petrify parentX (Node (Tuple offsetRec original) children) =
  let
    -- Node's x is parent's x plus its offset
    nodeX = parentX + offsetRec.offset
    -- For cluster: y is height (distance from leaves)
    -- Leaves have height = 0, so y = 0 (all at same level)
    nodeY = toNumber original.height

    -- Recursively process children with this node's x as parent
    childrenWithCoords = map (petrify nodeX) children
  in
    Node (original { x = nodeX, y = nodeY }) childrenWithCoords

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
