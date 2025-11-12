-- | PSD3.Layout.Hierarchy.Tree3
-- |
-- | Pure functional tree layout using Data.Tree and Reingold-Tilford algorithm
-- | Implements D3's tree layout in idiomatic PureScript
module PSD3.Layout.Hierarchy.Tree3 where

import Prelude

import Data.Tree (Tree(..), treeMapDeep)
import Data.List (List(..), length, fromFoldable, foldl, zipWith, (..))
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber, floor)
import Unsafe.Coerce (unsafeCoerce)

-- | Configuration for tree layout
type TreeConfig =
  { size :: { width :: Number, height :: Number }
  , separation :: Int -> Int -> Number  -- depth -> depth -> separation
  }

-- | Default configuration matching D3
defaultTreeConfig :: TreeConfig
defaultTreeConfig =
  { size: { width: 800.0, height: 600.0 }
  , separation: \a b -> if a == b then 1.0 else 2.0
  }

-- | Tree layout algorithm using Reingold-Tilford
-- | Adds x, y, depth, height to existing record fields
-- |
-- | This is the "functional pearl" implementation:
-- | 1. Add depth (top-down traversal)
-- | 2. Add height (bottom-up using treeMapDeep)
-- | 3. Apply Reingold-Tilford (bottom-up, then top-down)
-- | 4. Scale to final coordinates
tree :: forall r.
  TreeConfig ->
  Tree { | r } ->
  Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r }
tree config inputTree =
  let
    -- Step 1: Add depth (distance from root)
    withDepth = addDepth 0 inputTree

    -- Step 2: Add height (max distance to leaf) using treeMapDeep
    withHeight = addHeight withDepth

    -- Step 3: Compute relative positions (x is relative, y is depth)
    withRelativeX = computeRelativePositions withHeight

    -- Step 4: Scale to actual coordinates
    withPositions = scalePositions config withRelativeX
  in
    withPositions

-- | Add depth field (top-down traversal)
addDepth :: forall r. Int -> Tree { | r } -> Tree { depth :: Int | r }
addDepth currentDepth (Node val children) =
  Node
    (unsafeCoerce $ { depth: currentDepth } `unsafeCoerce` val)
    (map (addDepth (currentDepth + 1)) children)

-- | Add height field (bottom-up using treeMapDeep)
-- | height = max distance to any leaf
addHeight :: forall r. Tree { depth :: Int | r } -> Tree { depth :: Int, height :: Int | r }
addHeight = treeMapDeep \nodeVal childHeights ->
  let maxChildHeight = fromMaybe 0 $ maximum $ map (\c -> (unsafeCoerce c :: { height :: Int }).height) childHeights
      height = if length childHeights == 0 then 0 else maxChildHeight + 1
  in unsafeCoerce $ { height: height } `unsafeCoerce` nodeVal

-- | Compute relative X positions using Reingold-Tilford
-- | This is a simplified version - assigns each node position relative to siblings
computeRelativePositions :: forall r.
  Tree { depth :: Int, height :: Int | r } ->
  Tree { depth :: Int, height :: Int, relativeX :: Number | r }
computeRelativePositions tree =
  let
    -- Bottom-up: compute subtree extents
    -- Top-down: finalize positions
    -- For now: simple breadth-first spacing
    go :: Number -> Tree { depth :: Int, height :: Int | r } -> Tree { depth :: Int, height :: Int, relativeX :: Number | r }
    go x (Node val children) =
      let
        numChildren = length children
        -- Space children evenly
        childrenWithX = zipWith go (spacedPositions x (toNumber numChildren)) children
      in
        Node (unsafeCoerce $ { relativeX: x } `unsafeCoerce` val) childrenWithX
  in
    go 0.0 tree

-- | Helper: create evenly spaced positions centered around a point
spacedPositions :: Number -> Number -> List Number
spacedPositions center n =
  if n == 0.0
  then Nil
  else
    let start = center - (n - 1.0) / 2.0
        positions = map (\i -> start + toNumber i) (0 .. (floor n - 1))
    in fromFoldable positions

-- | Scale relative positions to actual coordinates
scalePositions :: forall r.
  TreeConfig ->
  Tree { depth :: Int, height :: Int, relativeX :: Number | r } ->
  Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r }
scalePositions config inputTree =
  let
    -- Extract all relativeX values using Foldable instance
    allNodes = unsafeCoerce inputTree :: Tree { relativeX :: Number, depth :: Int }
    nodesArray = Array.fromFoldable allNodes
    allXValues = map (\n -> (unsafeCoerce n :: { relativeX :: Number }).relativeX) nodesArray

    minX = fromMaybe 0.0 $ minimum allXValues
    maxX = fromMaybe 1.0 $ maximum allXValues
    xRange = if maxX - minX == 0.0 then 1.0 else maxX - minX

    -- Scale function
    scaleX x = ((x - minX) / xRange) * config.size.width
    scaleY depth = (toNumber depth / 10.0) * config.size.height  -- Simplified

    go (Node val children) =
      let
        nodeVal = unsafeCoerce val :: { relativeX :: Number, depth :: Int }
        x = scaleX nodeVal.relativeX
        y = scaleY nodeVal.depth
      in
        Node (unsafeCoerce $ { x: x, y: y } `unsafeCoerce` val) (map go children)
  in
    go inputTree
