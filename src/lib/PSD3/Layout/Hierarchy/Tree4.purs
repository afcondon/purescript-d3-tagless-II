-- | PSD3.Layout.Hierarchy.Tree4
-- |
-- | Reingold-Tilford tree layout adapted from the elegant Haskell implementation
-- | Translates binary tree algorithm to n-ary trees (Data.Tree)
module PSD3.Layout.Hierarchy.Tree4 where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable, length, zipWith, scanl, (..))
import Data.Foldable (foldl, maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Int (toNumber)

-- | Configuration for tree layout
type TreeConfig =
  { size :: { width :: Number, height :: Number }
  , minSeparation :: Number  -- Minimum horizontal separation between siblings
  }

-- | Default configuration
defaultTreeConfig :: TreeConfig
defaultTreeConfig =
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

-- | Tree layout in 3 steps:
-- | 1. render: Bottom-up pass computing relative positions (distances from parent)
-- | 2. petrify: Top-down pass converting distances to absolute (x,y) coordinates
-- | 3. scale: Scale abstract coordinates to pixel coordinates
-- |
-- | Uses Tuple annotation to temporarily hold offset values during layout computation
-- | Input must have x, y, depth fields (initial values don't matter, they'll be overwritten)
tree :: forall r.
  TreeConfig ->
  Tree { x :: Number, y :: Number, depth :: Int | r } ->
  Tree { x :: Number, y :: Number, depth :: Int | r }
tree config inputTree =
  let
    -- Step 1: Add depth field
    withDepth = addDepth 0 inputTree

    -- Step 2: Compute relative positions (bottom-up)
    -- Annotates tree with Tuple { offset :: Number } original
    rendered = render config.minSeparation withDepth

    -- Step 3: Convert offsets to absolute coordinates (top-down)
    -- Strips Tuple annotation, adds x and y fields
    withCoords = petrify 0.0 rendered

    -- Step 4: Scale to final pixel coordinates
    scaled = scaleToPixels config withCoords
  in
    scaled

-- | Update depth field (top-down traversal)
-- | Input must already have depth field, which gets overwritten
addDepth :: forall r. Int -> Tree { x :: Number, y :: Number, depth :: Int | r } -> Tree { x :: Number, y :: Number, depth :: Int | r }
addDepth currentDepth (Node val children) =
  Node
    (val { depth = currentDepth })
    (map (addDepth (currentDepth + 1)) children)

-- | Bottom-up pass: compute relative positions using contour scanning
-- | Annotates tree with Tuple containing offset and original data
render :: forall r.
  Number ->
  Tree { x :: Number, y :: Number, depth :: Int | r } ->
  Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r })
render minSep inputTree =
  case renderWithContours inputTree of
    Tuple t _ -> t
  where
    -- Internal function that also returns contours (captures minSep from outer scope)
    renderWithContours :: Tree { x :: Number, y :: Number, depth :: Int | r } -> Tuple (Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r })) Contours
    renderWithContours (Node val children) =
      case length children of
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
            childOffsets = scanl (+) 0.0 separations

            -- Update each child tree with its offset
            childrenWithOffsets = zipWith updateOffset childTrees childOffsets

            -- Parent is centered at midpoint of leftmost and rightmost child
            leftmostOffset = fromMaybe 0.0 $ Array.head $ Array.fromFoldable childOffsets
            rightmostOffset = fromMaybe 0.0 $ Array.last $ Array.fromFoldable childOffsets
            parentOffset = (leftmostOffset + rightmostOffset) / 2.0

            -- Combine child contours into parent contours
            parentContours = spliceContours childOffsets childContours

            resultTree = Node (Tuple { offset: parentOffset } val) childrenWithOffsets
          in
            Tuple resultTree parentContours

    -- Update a child tree's offset in its Tuple annotation
    updateOffset :: Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r }) -> Number -> Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r })
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
-- | Each node's y = depth
petrify :: forall r.
  Number ->  -- Parent's x coordinate
  Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r }) ->
  Tree { x :: Number, y :: Number, depth :: Int | r }
petrify parentX (Node (Tuple offsetRec original) children) =
  let
    -- Node's x is parent's x plus its offset
    nodeX = parentX + offsetRec.offset
    nodeY = toNumber original.depth  -- y is just depth (will be scaled later)

    -- Recursively process children with this node's x as parent
    childrenWithCoords = map (petrify nodeX) children
  in
    Node (original { x = nodeX, y = nodeY }) childrenWithCoords

-- | Scale abstract coordinates to pixel coordinates
scaleToPixels :: forall r.
  TreeConfig ->
  Tree { depth :: Int, x :: Number, y :: Number | r } ->
  Tree { depth :: Int, x :: Number, y :: Number | r }
scaleToPixels config inputTree =
  let
    -- Find x range using Foldable instance
    allNodes = Array.fromFoldable inputTree
    allX = map (\n -> n.x) allNodes
    minX = fromMaybe 0.0 $ minimum allX
    maxX = fromMaybe 1.0 $ maximum allX
    xRange = if maxX - minX == 0.0 then 1.0 else maxX - minX

    -- Find max depth
    allDepths = map (\n -> n.depth) allNodes
    maxDepth = fromMaybe 1 $ maximum allDepths

    -- Scale functions
    scaleX x = ((x - minX) / xRange) * config.size.width
    scaleY depth = (toNumber depth / toNumber maxDepth) * config.size.height

    -- Apply scaling via map
    go (Node val children) =
      Node
        (val { x = scaleX val.x, y = scaleY val.depth })
        (map go children)
  in
    go inputTree
