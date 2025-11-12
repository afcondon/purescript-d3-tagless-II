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
  Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r } ->
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
addDepth :: forall r. Int -> Tree {  depth :: Int | r } -> Tree { depth :: Int | r }
addDepth currentDepth (Node val children) =
  Node
    val { depth = currentDepth }
    (map (addDepth (currentDepth + 1)) children)

-- | Add height field (bottom-up using treeMapDeep)
-- | height = max distance to any leaf
addHeight :: forall r. Tree { depth :: Int, height :: Int | r } -> Tree { depth :: Int, height :: Int | r }
addHeight = treeMapDeep \nodeVal childHeights ->
  let maxChildHeight = fromMaybe 0 $ maximum $ map _.height childHeights
  in nodeVal { height = if length childHeights == 0 then 0 else maxChildHeight + 1}

-- | Compute relative X positions using the Reingold-Tilford algorithm
-- |
-- | The Reingold-Tilford algorithm produces aesthetically pleasing tree layouts by:
-- | 1. Positioning each node as close as possible to its children
-- | 2. Ensuring no two nodes at the same depth overlap
-- | 3. Making the tree as narrow as possible while satisfying constraints
-- |
-- | ALGORITHM (implemented in two passes):
-- |
-- | FIRST PASS (bottom-up, post-order):
-- | - For each LEAF node:
-- |   - Set x = 0 (relative to parent)
-- |   - Set modifier = 0
-- | - For each INTERNAL node with children:
-- |   - Recursively process all children first
-- |   - Position children with initial spacing (index-based)
-- |   - For each pair of adjacent subtrees:
-- |     - Compute contours (leftmost and rightmost x at each depth)
-- |     - Find minimum separation needed to prevent overlap
-- |     - Shift right subtree if necessary (update modifiers)
-- |   - Set node's x = midpoint of leftmost and rightmost child
-- |   - Compute node's modifier = accumulated shifts needed for this subtree
-- |
-- | SECOND PASS (top-down, pre-order):
-- | - Start at root with accumulated modifier = 0
-- | - For each node:
-- |   - Add accumulated modifier to node's x (converts relative to absolute)
-- |   - Pass (accumulated + node's modifier) to children
-- |
-- | INPUTS:
-- | - Tree must have x, depth, height fields (x is initially garbage, will be overwritten)
-- | - separation function determines minimum space between siblings
-- |
-- | OUTPUTS:
-- | - Tree with x values representing horizontal positions (in abstract coordinate space)
-- | - These are NOT yet scaled to final pixel coordinates
-- |
-- | NOTES:
-- | - x coordinates are in an abstract space where unit = 1
-- | - Nodes at same depth will have different x values to prevent overlap
-- | - The leftmost node will have the minimum x value (possibly negative)
-- | - Use Tuple annotation if you need to track both temporary and final values
-- |   (e.g., Tree (Tuple { modifier :: Number | r } { x :: Number | r }))
computeRelativePositions :: forall r.
  Tree { depth :: Int, height :: Int, x :: Number | r } ->
  Tree { depth :: Int, height :: Int, x :: Number | r }
computeRelativePositions tree = tree

-- | Scale relative positions to actual pixel coordinates
-- |
-- | After Reingold-Tilford computes abstract positions, we need to:
-- | 1. Map x values from abstract space to pixel space (0 to width)
-- | 2. Map depth to y pixel coordinates (0 to height)
-- | 3. Handle the configured size and optional nodeSize
-- |
-- | ALGORITHM:
-- |
-- | FOR X COORDINATES:
-- | - Find minimum x value in tree (use Foldable instance to get all nodes)
-- | - Find maximum x value in tree
-- | - Compute xRange = max - min
-- | - For each node: finalX = ((x - min) / xRange) * config.size.width
-- | - This maps [min, max] to [0, width]
-- |
-- | FOR Y COORDINATES:
-- | - Find maximum depth in tree
-- | - For each node: finalY = (depth / maxDepth) * config.size.height
-- | - This maps depth levels uniformly across the height
-- |
-- | ALTERNATIVE (if config.nodeSize is used instead of size):
-- | - Don't scale x, keep abstract coordinates
-- | - Set y = depth * config.nodeSize.height
-- | - This uses "node size" mode where layout determines total size
-- |
-- | INPUTS:
-- | - Tree with x coordinates from Reingold-Tilford (abstract space)
-- | - config with size specification
-- |
-- | OUTPUTS:
-- | - Tree with x, y in final pixel coordinates
-- | - x values in [0, config.size.width]
-- | - y values in [0, config.size.height]
-- |
-- | NOTES:
-- | - Must traverse entire tree to find min/max (use Foldable instance)
-- | - D3 has two modes: size (scales to fit) vs nodeSize (fixed node spacing)
-- | - For now, implement size mode only
scalePositions :: forall r.
  TreeConfig ->
  Tree { depth :: Int, height :: Int, x :: Number, y :: Number | r } ->
  Tree { depth :: Int, height :: Int, x :: Number, y :: Number | r }
scalePositions config inputTree = inputTree
