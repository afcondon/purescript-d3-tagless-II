-- | PSD3.Layout.Hierarchy.Tree4
-- |
-- | Reingold-Tilford tree layout adapted from the elegant Haskell implementation
-- | Translates binary tree algorithm to n-ary trees (Data.Tree)
module PSD3.Layout.Hierarchy.Tree4 where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable, length, scanl, zipWith)
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Int (toNumber)

-- | Configuration for tree layout
-- |
-- | The `separation` function controls horizontal spacing between adjacent nodes.
-- | It receives the node data for two adjacent nodes and returns the desired separation.
-- | Default behavior uses `minSeparation` for all nodes.
-- |
-- | The `layerScale` function controls vertical spacing between depth levels.
-- | It receives the depth (0 = root) and returns a scale factor for that level.
-- | Default is identity (linear scaling).
-- |
-- | Example for "flatter at top" effect:
-- | ```purescript
-- | config { layerScale = \depth -> toNumber depth ** 1.5 }
-- | ```
type TreeConfig a =
  { size :: { width :: Number, height :: Number }
  , minSeparation :: Number  -- Minimum horizontal separation between siblings
  , separation :: Maybe (a -> a -> Number)  -- Custom separation function based on node data
  , layerScale :: Maybe (Int -> Number)  -- Custom vertical spacing by depth
  }

-- | Default configuration
defaultTreeConfig :: forall a. TreeConfig a
defaultTreeConfig =
  { size: { width: 800.0, height: 600.0 }
  , minSeparation: 1.0
  , separation: Nothing
  , layerScale: Nothing
  }

-- | Configuration with custom layer scaling
-- |
-- | Common patterns:
-- | - `\d -> toNumber d ** 1.5` - compressed at top, expanded at bottom
-- | - `\d -> log (toNumber d + 1.0)` - very flat at top
-- | - `\d -> sqrt (toNumber d)` - moderate compression at top
withLayerScale :: forall a. (Int -> Number) -> TreeConfig a -> TreeConfig a
withLayerScale scale config = config { layerScale = Just scale }

-- | Configuration with custom separation function
-- |
-- | Example for radius-based separation:
-- | ```purescript
-- | withSeparation (\a b -> (a.radius + b.radius) / 2.0 + 1.0) config
-- | ```
withSeparation :: forall a. (a -> a -> Number) -> TreeConfig a -> TreeConfig a
withSeparation sep config = config { separation = Just sep }

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
  TreeConfig { x :: Number, y :: Number, depth :: Int | r } ->
  Tree { x :: Number, y :: Number, depth :: Int | r } ->
  Tree { x :: Number, y :: Number, depth :: Int | r }
tree config inputTree =
  let
    -- Step 1: Add depth field
    withDepth = addDepth 0 inputTree

    -- Step 2: Compute relative positions (bottom-up)
    -- Annotates tree with Tuple { offset :: Number } original
    rendered = render config.minSeparation config.separation withDepth

    -- Step 3: Convert offsets to absolute coordinates (top-down)
    -- Strips Tuple annotation, adds x and y fields
    withCoords = petrify 0.0 rendered

    -- Step 4: Scale to final pixel coordinates
    scaled = scaleToPixels config withCoords
  in
    scaled

-- | Tree layout with height-based sorting for consistent ordering with Cluster4
-- | Use this variant when your tree has height computed and you want sorted children
treeWithSorting :: forall r.
  TreeConfig { x :: Number, y :: Number, depth :: Int, height :: Int | r } ->
  Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r } ->
  Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r }
treeWithSorting config inputTree =
  let
    -- Step 1: Add depth field
    withDepth = addDepth 0 inputTree

    -- Step 2: Compute height field (distance from deepest leaf)
    withHeight = addHeightField withDepth

    -- Step 3: Sort children by height (descending) for consistent ordering with Cluster4
    sorted = sortByHeight withHeight

    -- Step 4: Compute relative positions (bottom-up)
    -- Annotates tree with Tuple { offset :: Number } original
    rendered = render config.minSeparation config.separation sorted

    -- Step 5: Convert offsets to absolute coordinates (top-down)
    -- Strips Tuple annotation, adds x and y fields
    withCoords = petrify 0.0 rendered

    -- Step 6: Scale to final pixel coordinates
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
  Maybe ({ x :: Number, y :: Number, depth :: Int | r } -> { x :: Number, y :: Number, depth :: Int | r } -> Number) ->
  Tree { x :: Number, y :: Number, depth :: Int | r } ->
  Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r })
render minSep separationFn inputTree =
  case renderWithContours inputTree of
    Tuple t _ -> t
  where
    -- Internal function that also returns contours (captures minSep and separationFn from outer scope)
    renderWithContours :: Tree { x :: Number, y :: Number, depth :: Int | r } -> Tuple (Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r })) Contours
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

            -- Extract root node data from each child tree for separation calculation
            childRootData = map getRootData childTrees

            -- Compute base separations for adjacent pairs using separation function
            baseSeparations = computeBaseSeparations minSep separationFn childRootData

            -- Scan adjacent pairs to find needed separations (using base separations)
            separations = computeSeparationsWithBase baseSeparations childContours

            -- Position children: first at 0, rest offset by cumulative separations
            -- These are absolute positions within the sibling group
            -- NOTE: PureScript's scanl doesn't include the initial value (unlike Haskell)
            -- So we must explicitly prepend 0.0
            childAbsoluteOffsets = Cons 0.0 (scanl (+) 0.0 separations)

            -- Parent is centered at midpoint of leftmost and rightmost child
            leftmostOffset = fromMaybe 0.0 $ Array.head $ Array.fromFoldable childAbsoluteOffsets
            rightmostOffset = fromMaybe 0.0 $ Array.last $ Array.fromFoldable childAbsoluteOffsets
            parentOffset = (leftmostOffset + rightmostOffset) / 2.0

            -- Convert child offsets to be relative to parent's centered position
            -- Each child's offset = absoluteOffset - parentOffset
            childRelativeOffsets = map (\absOffset -> absOffset - parentOffset) childAbsoluteOffsets

            -- Update each child tree with its relative offset
            childrenWithOffsets = zipWith updateOffset childTrees childRelativeOffsets

            -- Combine child contours into parent contours
            parentContours = spliceContours childRelativeOffsets childContours

            resultTree = Node (Tuple { offset: parentOffset } val) childrenWithOffsets
          in
            Tuple resultTree parentContours

    -- Update a child tree's offset in its Tuple annotation
    updateOffset :: Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r }) -> Number -> Tree (Tuple { offset :: Number } { x :: Number, y :: Number, depth :: Int | r })
    updateOffset (Node (Tuple offsetRec original) children) newOffset =
      Node (Tuple (offsetRec { offset = newOffset }) original) children

-- | Extract root node data from a tree with Tuple annotation
getRootData :: forall r. Tree (Tuple { offset :: Number } r) -> r
getRootData (Node (Tuple _ nodeData) _) = nodeData

-- | Compute base separations for adjacent pairs using separation function
-- | For n nodes, returns (n-1) separation values
computeBaseSeparations :: forall r.
  Number ->  -- default minSep
  Maybe (r -> r -> Number) ->  -- optional separation function
  List r ->  -- node data for each child
  List Number  -- base separation for each adjacent pair
computeBaseSeparations minSep separationFn nodes =
  case nodes of
    Nil -> Nil
    Cons _ Nil -> Nil
    _ -> zipWith (getSep separationFn minSep) nodes (tailSafe nodes)
  where
    -- When separation function exists, ADD minSep to it (minSep acts as base spacing)
    getSep :: Maybe (r -> r -> Number) -> Number -> r -> r -> Number
    getSep Nothing defSep _ _ = defSep
    getSep (Just sepFn) baseMinSep a b = sepFn a b + baseMinSep

    tailSafe :: forall a. List a -> List a
    tailSafe Nil = Nil
    tailSafe (Cons _ xs) = xs

-- | Compute separations with per-pair base separations
-- | Uses base separation for each pair instead of uniform minSep
computeSeparationsWithBase :: List Number -> List Contours -> List Number
computeSeparationsWithBase baseSeps contours =
  case Tuple baseSeps contours of
    Tuple Nil _ -> Nil
    Tuple _ Nil -> Nil
    Tuple _ (Cons _ Nil) -> Nil
    _ ->
      -- Zip baseSeps with pairs of contours
      let contourPairs = zipWith Tuple contours (tailSafe contours)
      in zipWith applyBase baseSeps contourPairs
  where
    applyBase :: Number -> Tuple Contours Contours -> Number
    applyBase base (Tuple c1 c2) = scanContoursWithBase base c1 c2

    tailSafe :: forall a. List a -> List a
    tailSafe Nil = Nil
    tailSafe (Cons _ xs) = xs

-- | Scan two contours with a specific base separation
scanContoursWithBase :: Number -> Contours -> Contours -> Number
scanContoursWithBase baseSep (Contours left) (Contours right) =
  go baseSep left.right right.left
  where
    go :: Number -> Contour -> Contour -> Number
    go currentSep Nil _ = currentSep
    go currentSep _ Nil = currentSep
    go currentSep (Cons lr rest1) (Cons rl rest2) =
      let
        actualSep = currentSep + rl - lr
        neededSep = if actualSep < baseSep
                    then currentSep + (baseSep - actualSep)
                    else currentSep
      in
        go neededSep rest1 rest2

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
-- | At each depth level, take the minimum of all left contours and maximum of all right contours
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
      -- Multiple children: merge all contours level by level
      -- At each level, take the leftmost of all left contours and rightmost of all right contours
      let
        -- Shift all contours by their offsets
        shiftedContours = map (\(Tuple offset (Contours c)) ->
          Contours { left: map (\x -> x + offset) c.left
                   , right: map (\x -> x + offset) c.right
                   }) pairs

        -- Merge contours level by level
        mergedLeft = mergeContoursLeft (map (\(Contours c) -> c.left) shiftedContours)
        mergedRight = mergeContoursRight (map (\(Contours c) -> c.right) shiftedContours)
      in
        Contours { left: Cons 0.0 mergedLeft, right: Cons 0.0 mergedRight }

-- | Merge left contours: at each level, take the minimum
mergeContoursLeft :: Array Contour -> Contour
mergeContoursLeft contours =
  case contours of
    [] -> Nil
    _ -> mergeLevel contours
  where
    mergeLevel :: Array Contour -> Contour
    mergeLevel cs =
      let
        -- Get all values at current level (filter out Nil)
        currentLevels = Array.mapMaybe
          (\c -> case c of
            Cons x _ -> Just x
            Nil -> Nothing)
          cs
      in
        case Array.head currentLevels of
          Nothing -> Nil  -- All contours ended
          Just _ ->
            let
              minVal = fromMaybe 0.0 $ minimum currentLevels
              -- Get rest of each contour
              restContours = Array.mapMaybe
                (\c -> case c of
                  Cons _ rest -> Just rest
                  Nil -> Nothing)
                cs
            in
              Cons minVal (mergeLevel restContours)

-- | Merge right contours: at each level, take the maximum
mergeContoursRight :: Array Contour -> Contour
mergeContoursRight contours =
  case contours of
    [] -> Nil
    _ -> mergeLevel contours
  where
    mergeLevel :: Array Contour -> Contour
    mergeLevel cs =
      let
        -- Get all values at current level (filter out Nil)
        currentLevels = Array.mapMaybe
          (\c -> case c of
            Cons x _ -> Just x
            Nil -> Nothing)
          cs
      in
        case Array.head currentLevels of
          Nothing -> Nil  -- All contours ended
          Just _ ->
            let
              maxVal = fromMaybe 0.0 $ maximum currentLevels
              -- Get rest of each contour
              restContours = Array.mapMaybe
                (\c -> case c of
                  Cons _ rest -> Just rest
                  Nil -> Nothing)
                cs
            in
              Cons maxVal (mergeLevel restContours)

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
  TreeConfig { depth :: Int, x :: Number, y :: Number | r } ->
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

    -- Apply layer scale function if provided
    layerScaleFn = fromMaybe toNumber config.layerScale

    -- Compute scaled depths for normalization
    scaledDepths = map (\n -> layerScaleFn n.depth) allNodes
    maxScaledDepth = fromMaybe 1.0 $ maximum scaledDepths

    -- Scale functions
    scaleX x = ((x - minX) / xRange) * config.size.width
    scaleY depth =
      let scaledDepth = layerScaleFn depth
      in (scaledDepth / maxScaledDepth) * config.size.height

    -- Apply scaling via map
    go (Node val children) =
      Node
        (val { x = scaleX val.x, y = scaleY val.depth })
        (map go children)
  in
    go inputTree

-- | Compute height field (distance from deepest leaf)
-- | Bottom-up traversal: leaves get 0, parents get 1 + max(children's height)
addHeightField :: forall r. Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r } -> Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r }
addHeightField (Node val children) =
  let
    -- Recursively compute children's heights
    childrenWithHeight = map addHeightField children

    -- Leaf: height = 0
    -- Internal: height = 1 + max(children's height)
    nodeHeight = case Array.fromFoldable childrenWithHeight of
      [] -> 0
      childArray ->
        let childHeights = map (\(Node v _) -> v.height) childArray
            maxChildHeight = fromMaybe 0 (maximum childHeights)
        in maxChildHeight + 1
  in
    Node (val { height = nodeHeight }) childrenWithHeight

-- | Sort children by height (descending) to minimize crossovers
-- | This matches Cluster4's behavior for consistent child ordering during animations
sortByHeight :: forall r. Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r } -> Tree { x :: Number, y :: Number, depth :: Int, height :: Int | r }
sortByHeight (Node val children) =
  let
    -- Recursively sort grandchildren first
    sortedGrandchildren = map sortByHeight children

    -- Convert to Array, sort, then back to List
    childArray = Array.fromFoldable sortedGrandchildren
    sortedArray = Array.sortBy compareByHeight childArray
    sortedChildrenList = fromFoldable sortedArray
  in
    Node val sortedChildrenList
  where
    compareByHeight :: forall s. Tree { height :: Int | s } -> Tree { height :: Int | s } -> Ordering
    compareByHeight (Node a _) (Node b _) = compare b.height a.height  -- Descending
