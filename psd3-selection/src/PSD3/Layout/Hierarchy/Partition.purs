-- | D3 Partition Layout
-- |
-- | Implements the D3 partition layout algorithm for hierarchical data.
-- | Creates rectangular partitions that can be rendered as:
-- | - Icicle charts (vertical stacked rectangles)
-- | - Sunburst charts (radial partitions)
-- |
-- | Based on: https://github.com/d3/d3-hierarchy/blob/main/src/partition.js
module PSD3.Layout.Hierarchy.Partition
  ( PartitionNode(..)
  , PartitionConfig
  , defaultPartitionConfig
  , partition
  , HierarchyData(..)
  , hierarchy
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (ceil, floor)

-- | Partition node with rectangular coordinates
data PartitionNode a = PartNode
  { data_ :: a          -- Original data
  , depth :: Int        -- Distance from root
  , height :: Int       -- Distance to deepest leaf
  , value :: Number     -- Aggregated value
  , children :: Array (PartitionNode a)
  , x0 :: Number        -- Left/inner edge
  , y0 :: Number        -- Top/start edge
  , x1 :: Number        -- Right/outer edge
  , y1 :: Number        -- Bottom/end edge
  }

derive instance Eq a => Eq (PartitionNode a)
derive instance Ord a => Ord (PartitionNode a)
derive instance Functor PartitionNode

instance Show a => Show (PartitionNode a) where
  show (PartNode n) =
    "PartNode { data_: " <> show n.data_ <>
    ", depth: " <> show n.depth <>
    ", height: " <> show n.height <>
    ", value: " <> show n.value <>
    ", children: [" <> show (Array.length n.children) <> " items]" <>
    ", x0: " <> show n.x0 <>
    ", y0: " <> show n.y0 <>
    ", x1: " <> show n.x1 <>
    ", y1: " <> show n.y1 <>
    " }"

-- | Configuration for partition layout
type PartitionConfig a =
  { size :: { width :: Number, height :: Number }  -- Canvas size (dx, dy)
  , padding :: Number                              -- Padding between rectangles
  , round :: Boolean                               -- Round to integers for pixels
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
          , x0: 0.0  -- Will be set by partition
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
    n = rootData.height + 1  -- Total number of layers

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
    positionedChildren = if Array.null node.children
      then node.children
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
    { x0: finalX0, x1: finalX1 } = if x1' < x0
      then { x0: (x0 + x1') / 2.0, x1: (x0 + x1') / 2.0 }
      else { x0: x0, x1: x1' }

    { y0: finalY0, y1: finalY1 } = if y1' < y0
      then { y0: (y0 + y1') / 2.0, y1: (y0 + y1') / 2.0 }
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
    k = if totalValue > 0.0
        then (x1 - x0) / totalValue
        else 0.0

    -- Position each child
    result = Array.foldl (positionChild k y0 y1) { positioned: [], currentX: x0 } children
  in
    result.positioned

-- | Helper for dice: position a single child
positionChild :: forall a. Number -> Number -> Number ->
                 { positioned :: Array (PartitionNode a), currentX :: Number } ->
                 PartitionNode a ->
                 { positioned :: Array (PartitionNode a), currentX :: Number }
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
