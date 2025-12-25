-- | D3 Partition Layout
-- |
-- | Implements the D3 partition layout algorithm for hierarchical data.
-- | Creates rectangular partitions that can be rendered as:
-- | - Icicle charts (vertical stacked rectangles)
-- | - Sunburst charts (radial partitions)
-- |
-- | Based on: https://github.com/d3/d3-hierarchy/blob/main/src/partition.js
module DataViz.Layout.Hierarchy.Partition
  ( PartitionNode(..)
  , PartitionConfig
  , defaultPartitionConfig
  , partition
  , HierarchyData(..)
  , hierarchy
  -- Sunburst utilities
  , sunburstArcPath
  , flattenPartition
  , fixParallelLayout
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (ceil, floor, pi, cos, sin)

-- | Partition node with rectangular coordinates
data PartitionNode a = PartNode
  { data_ :: a -- Original data
  , depth :: Int -- Distance from root
  , height :: Int -- Distance to deepest leaf
  , value :: Number -- Aggregated value
  , children :: Array (PartitionNode a)
  , x0 :: Number -- Left/inner edge
  , y0 :: Number -- Top/start edge
  , x1 :: Number -- Right/outer edge
  , y1 :: Number -- Bottom/end edge
  }

derive instance Eq a => Eq (PartitionNode a)
derive instance Ord a => Ord (PartitionNode a)
derive instance Functor PartitionNode

instance Show a => Show (PartitionNode a) where
  show (PartNode n) =
    "PartNode { data_: " <> show n.data_
      <> ", depth: "
      <> show n.depth
      <> ", height: "
      <> show n.height
      <> ", value: "
      <> show n.value
      <> ", children: ["
      <> show (Array.length n.children)
      <> " items]"
      <> ", x0: "
      <> show n.x0
      <> ", y0: "
      <> show n.y0
      <> ", x1: "
      <> show n.x1
      <> ", y1: "
      <> show n.y1
      <>
        " }"

-- | Configuration for partition layout
type PartitionConfig :: forall k. k -> Type
type PartitionConfig a =
  { size :: { width :: Number, height :: Number } -- Canvas size (dx, dy)
  , padding :: Number -- Padding between rectangles
  , round :: Boolean -- Round to integers for pixels
  }

-- | Default configuration
defaultPartitionConfig :: forall a. PartitionConfig a
defaultPartitionConfig =
  { size: { width: 1.0, height: 1.0 }
  , padding: 0.0
  , round: false
  }

-- | Hierarchical data structure (can have children)
newtype HierarchyData a = HierarchyData
  { data_ :: a
  , value :: Maybe Number
  , children :: Maybe (Array (HierarchyData a))
  }

-- | Convert hierarchical data to PartitionNode (before layout)
hierarchy :: forall a. HierarchyData a -> PartitionNode a
hierarchy = go 0
  where
  go :: Int -> HierarchyData a -> PartitionNode a
  go depth (HierarchyData node) =
    let
      kids = fromMaybe [] $ map (map (go (depth + 1))) node.children

      -- Calculate height (distance to deepest leaf)
      childHeights = map (\(PartNode n) -> n.height) kids
      maxChildHeight = Array.foldl max 0 childHeights
      nodeHeight = if Array.null kids then 0 else maxChildHeight + 1

      -- Calculate value (sum of children or leaf value)
      childValues = map (\(PartNode n) -> n.value) kids
      sumChildValues = Array.foldl (+) 0.0 childValues
      nodeValue = case node.value of
        Just v -> v
        Nothing -> if Array.null kids then 0.0 else sumChildValues
    in
      PartNode
        { data_: node.data_
        , depth: depth
        , height: nodeHeight
        , value: nodeValue
        , children: kids
        , x0: 0.0 -- Will be set by partition
        , y0: 0.0
        , x1: 0.0
        , y1: 0.0
        }

-- | Apply partition layout to hierarchical data
-- |
-- | Algorithm:
-- | 1. Calculate number of layers (n = height + 1)
-- | 2. Initialize root coordinates
-- | 3. Position all nodes (pre-order traversal)
-- | 4. Optional: round coordinates to integers
partition :: forall a. PartitionConfig a -> PartitionNode a -> PartitionNode a
partition config root =
  let
    dx = config.size.width
    dy = config.size.height
    padding = config.padding

    PartNode rootData = root
    n = rootData.height + 1 -- Total number of layers

    -- Initialize root coordinates
    initialRoot = PartNode $ rootData
      { x0 = padding
      , y0 = padding
      , x1 = dx
      , y1 = dy / (toNumber n)
      }

    -- Position all nodes
    positioned = positionNode config dy n initialRoot

    -- Optional: round to integers
    rounded = if config.round then roundNode positioned else positioned
  in
    rounded

-- | Position a node and its children
positionNode :: forall a. PartitionConfig a -> Number -> Int -> PartitionNode a -> PartitionNode a
positionNode config dy n (PartNode node) =
  let
    -- If node has children, partition them horizontally
    positionedChildren =
      if Array.null node.children then node.children
      else
        let
          -- Children occupy the next layer down
          childY0 = dy * (toNumber (node.depth + 1)) / (toNumber n)
          childY1 = dy * (toNumber (node.depth + 2)) / (toNumber n)

          -- Partition children horizontally by value (dice)
          dicedChildren = dice node.children node.x0 childY0 node.x1 childY1

          -- Recursively position grandchildren
          recursed = map (positionNode config dy n) dicedChildren
        in
          recursed

    -- Apply padding
    x0 = node.x0
    y0 = node.y0
    x1' = node.x1 - config.padding
    y1' = node.y1 - config.padding

    -- Handle edge case: padding too large
    { x0: finalX0, x1: finalX1 } =
      if x1' < x0 then { x0: (x0 + x1') / 2.0, x1: (x0 + x1') / 2.0 }
      else { x0: x0, x1: x1' }

    { y0: finalY0, y1: finalY1 } =
      if y1' < y0 then { y0: (y0 + y1') / 2.0, y1: (y0 + y1') / 2.0 }
      else { y0: y0, y1: y1' }
  in
    PartNode $ node
      { children = positionedChildren
      , x0 = finalX0
      , y0 = finalY0
      , x1 = finalX1
      , y1 = finalY1
      }

-- | Horizontal partitioning (dice) - distribute children by value
-- |
-- | Algorithm:
-- | 1. Calculate scaling factor: k = (x1 - x0) / parent.value
-- | 2. For each child, assign horizontal slice proportional to its value
dice :: forall a. Array (PartitionNode a) -> Number -> Number -> Number -> Number -> Array (PartitionNode a)
dice children x0 y0 x1 y1 =
  let
    -- Calculate total value
    totalValue = Array.foldl (\acc (PartNode n) -> acc + n.value) 0.0 children

    -- Scaling factor
    k =
      if totalValue > 0.0 then (x1 - x0) / totalValue
      else 0.0

    -- Position each child
    result = Array.foldl (positionChild k y0 y1) { positioned: [], currentX: x0 } children
  in
    result.positioned

-- | Helper for dice: position a single child
positionChild
  :: forall a
   . Number
  -> Number
  -> Number
  -> { positioned :: Array (PartitionNode a), currentX :: Number }
  -> PartitionNode a
  -> { positioned :: Array (PartitionNode a), currentX :: Number }
positionChild k y0 y1 acc (PartNode node) =
  let
    x0' = acc.currentX
    x1' = acc.currentX + node.value * k

    positioned = PartNode $ node
      { x0 = x0'
      , y0 = y0
      , x1 = x1'
      , y1 = y1
      }
  in
    { positioned: Array.snoc acc.positioned positioned
    , currentX: x1'
    }

-- | Round all coordinates to integers (for pixel-perfect rendering)
roundNode :: forall a. PartitionNode a -> PartitionNode a
roundNode (PartNode node) =
  PartNode $ node
    { x0 = floor node.x0
    , y0 = floor node.y0
    , x1 = ceil node.x1
    , y1 = ceil node.y1
    , children = map roundNode node.children
    }

-- =============================================================================
-- SUNBURST UTILITIES
-- =============================================================================

-- | Convert partition coordinates to sunburst arc path (SVG path string)
-- |
-- | Parameters:
-- | - x0, x1: Normalized angles [0,1] representing angular extent around the circle
-- | - y0, y1: Normalized radii [0,1] representing distance from center
-- | - radius: Total radius of the sunburst in pixels
-- |
-- | The function handles the special case where the arc spans nearly a full circle
-- | (>99%), which SVG cannot render as a single arc, by splitting it into two
-- | semicircular arcs.
-- |
-- | Based on D3's arc generator: https://github.com/d3/d3-shape#arc
sunburstArcPath :: Number -> Number -> Number -> Number -> Number -> String
sunburstArcPath x0_ y0_ x1_ y1_ radius =
  let
    -- Convert normalized y to radius
    innerRadius = y0_ * radius
    outerRadius = y1_ * radius

    -- Check if arc spans nearly full circle (>99%) - SVG can't handle this with single arc
    arcSpan = x1_ - x0_
  in
    if arcSpan > 0.99 then
      -- Full ring: use two semicircular arcs
      let
        -- Top point (angle = -π/2)
        topOuterX = 0.0
        topOuterY = -outerRadius
        topInnerX = 0.0
        topInnerY = -innerRadius
        -- Bottom point (angle = π/2)
        bottomOuterX = 0.0
        bottomOuterY = outerRadius
        bottomInnerX = 0.0
        bottomInnerY = innerRadius
      in
        -- Draw full ring as two semicircles
        "M" <> show topOuterX <> "," <> show topOuterY
          -- Outer arc: top to bottom (right side)
          <> "A" <> show outerRadius <> "," <> show outerRadius <> " 0 0 1 "
          <> show bottomOuterX <> "," <> show bottomOuterY
          -- Outer arc: bottom to top (left side)
          <> "A" <> show outerRadius <> "," <> show outerRadius <> " 0 0 1 "
          <> show topOuterX <> "," <> show topOuterY
          -- Line to inner top
          <> "M" <> show topInnerX <> "," <> show topInnerY
          -- Inner arc: top to bottom (right side, counterclockwise)
          <> "A" <> show innerRadius <> "," <> show innerRadius <> " 0 0 0 "
          <> show bottomInnerX <> "," <> show bottomInnerY
          -- Inner arc: bottom to top (left side)
          <> "A" <> show innerRadius <> "," <> show innerRadius <> " 0 0 0 "
          <> show topInnerX <> "," <> show topInnerY
          <> "Z"
    else
      -- Normal arc path
      let
        -- Convert normalized x to angles (0 to 2π), starting at top
        startAngle = x0_ * 2.0 * pi - (pi / 2.0)
        endAngle = x1_ * 2.0 * pi - (pi / 2.0)

        -- Calculate arc points
        x00 = cos startAngle * innerRadius
        y00 = sin startAngle * innerRadius
        x01 = cos endAngle * innerRadius
        y01 = sin endAngle * innerRadius
        x10 = cos startAngle * outerRadius
        y10 = sin startAngle * outerRadius
        x11 = cos endAngle * outerRadius
        y11 = sin endAngle * outerRadius

        -- Large arc flag: 1 if angle > π, 0 otherwise
        largeArc = if (endAngle - startAngle) > pi then 1 else 0
      in
        -- SVG path for arc segment
        "M" <> show x10 <> "," <> show y10
          <> "A" <> show outerRadius <> "," <> show outerRadius
          <> " 0 " <> show largeArc <> " 1 "
          <> show x11 <> "," <> show y11
          <> "L" <> show x01 <> "," <> show y01
          <> "A" <> show innerRadius <> "," <> show innerRadius
          <> " 0 " <> show largeArc <> " 0 "
          <> show x00 <> "," <> show y00
          <> "Z"

-- | Flatten PartitionNode tree to array (pre-order traversal)
-- |
-- | Returns all nodes in the tree as a flat array, with the root first followed
-- | by all descendants in pre-order traversal order.
flattenPartition :: forall a. PartitionNode a -> Array (PartitionNode a)
flattenPartition node@(PartNode n) =
  if Array.null n.children then [ node ]
  else [ node ] <> (n.children >>= flattenPartition)

-- | Fix parallel layout for sunburst diagrams
-- |
-- | Makes children of "parallel" nodes share the parent's angular extent and stack
-- | radially (same angle, different radius) instead of dividing angular space.
-- | This is useful for visualizing simultaneous/overlapping elements.
-- |
-- | The predicate function determines which nodes should be treated as "parallel".
-- | For example: `\nodeData -> nodeData.nodeType == "parallel"`
fixParallelLayout :: forall a. (a -> Boolean) -> PartitionNode a -> PartitionNode a
fixParallelLayout isParallel (PartNode node) =
  let
    -- Recursively fix children first
    fixedChildren = map (fixParallelLayout isParallel) node.children

    -- If this node is a parallel node, adjust children to stack radially
    adjustedChildren =
      if isParallel node.data_ then
        stackRadially node.x0 node.x1 node.y1 fixedChildren
      else
        fixedChildren
  in
    PartNode (node { children = adjustedChildren })
  where
  -- Stack children radially: same angular extent, divided radial space
  stackRadially :: Number -> Number -> Number -> Array (PartitionNode a) -> Array (PartitionNode a)
  stackRadially parentX0 parentX1 _parentY1 children =
    let
      numChildren = Array.length children
      -- Get the y0 and y1 from first child (they all have same y range from partition)
      -- Children extend OUTWARD from parent, so use their actual y range
      baseY0 = case Array.head children of
        Just (PartNode c) -> c.y0
        Nothing -> 0.0
      maxY1 = case Array.head children of
        Just (PartNode c) -> c.y1
        Nothing -> 1.0
      -- Total radial space available for children (their outer ring)
      totalRadialSpace = maxY1 - baseY0
      -- Divide radial space equally among children
      radialSlice = if numChildren > 0 then totalRadialSpace / Int.toNumber numChildren else 0.0
    in
      Array.mapWithIndex (stackChild parentX0 parentX1 baseY0 radialSlice) children

  -- Stack a single child at its radial position
  stackChild :: Number -> Number -> Number -> Number -> Int -> PartitionNode a -> PartitionNode a
  stackChild parentX0 parentX1 baseY0 radialSlice idx (PartNode child) =
    let
      -- Calculate this child's radial band
      newY0 = baseY0 + Int.toNumber idx * radialSlice
      newY1 = newY0 + radialSlice
      -- Recursively adjust all descendants
      adjustedDescendants = map (adjustDescendant parentX0 parentX1 newY0 newY1 child.x0 child.x1 child.y0 child.y1) child.children
    in
      -- Set angular extent to parent's and radial extent to this slice
      PartNode (child { x0 = parentX0, x1 = parentX1, y0 = newY0, y1 = newY1, children = adjustedDescendants })

  -- Adjust descendants: remap their coordinates relative to the new parent extent
  adjustDescendant :: Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> PartitionNode a -> PartitionNode a
  adjustDescendant newParentX0 newParentX1 newParentY0 newParentY1 oldParentX0 oldParentX1 oldParentY0 oldParentY1 (PartNode desc) =
    let
      -- Remap angular coordinates proportionally
      oldXWidth = oldParentX1 - oldParentX0
      newXWidth = newParentX1 - newParentX0
      relativeX0 = if oldXWidth > 0.0 then (desc.x0 - oldParentX0) / oldXWidth else 0.0
      relativeX1 = if oldXWidth > 0.0 then (desc.x1 - oldParentX0) / oldXWidth else 1.0
      newX0 = newParentX0 + relativeX0 * newXWidth
      newX1 = newParentX0 + relativeX1 * newXWidth

      -- Remap radial coordinates proportionally
      oldYHeight = oldParentY1 - oldParentY0
      newYHeight = newParentY1 - newParentY0
      relativeY0 = if oldYHeight > 0.0 then (desc.y0 - oldParentY0) / oldYHeight else 0.0
      relativeY1 = if oldYHeight > 0.0 then (desc.y1 - oldParentY0) / oldYHeight else 1.0
      newY0 = newParentY0 + relativeY0 * newYHeight
      newY1 = newParentY0 + relativeY1 * newYHeight

      -- Recursively adjust this descendant's children
      adjustedChildren = map (adjustDescendant newX0 newX1 newY0 newY1 desc.x0 desc.x1 desc.y0 desc.y1) desc.children
    in
      PartNode (desc { x0 = newX0, x1 = newX1, y0 = newY0, y1 = newY1, children = adjustedChildren })
