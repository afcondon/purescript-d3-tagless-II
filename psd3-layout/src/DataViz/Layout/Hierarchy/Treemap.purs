-- | DataViz.Layout.Hierarchy.Treemap
-- |
-- | Pure PureScript implementation of D3's treemap layout.
-- | Partitions rectangles to represent hierarchical data with area proportional to value.
module DataViz.Layout.Hierarchy.Treemap
  ( TreemapNode(..)
  , TileFunction
  , TreemapConfig
  , phi
  , defaultTreemapConfig
  , toTreemapNode
  , treemap
  , dice
  , slice
  , sliceDice
  , binary
  , squarify
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))

-- | Node with treemap layout coordinates
-- | Extends ValuedNode with x0, y0, x1, y1 bounds
data TreemapNode a = TNode
  { data_ :: a
  , depth :: Int
  , height :: Int
  , value :: Number
  , children :: Array (TreemapNode a)
  , x0 :: Number -- Left edge
  , y0 :: Number -- Top edge
  , x1 :: Number -- Right edge
  , y1 :: Number -- Bottom edge
  }

derive instance eqTreemapNode :: Eq a => Eq (TreemapNode a)
derive instance ordTreemapNode :: Ord a => Ord (TreemapNode a)

instance showTreemapNode :: Show a => Show (TreemapNode a) where
  show (TNode n) = "TNode { data_: " <> show n.data_ <> ", bounds: [" <> show n.x0 <> "," <> show n.y0 <> "," <> show n.x1 <> "," <> show n.y1 <> "] }"

-- | Tiling function type
-- | Takes parent node and bounds, positions children
type TileFunction a = TreemapNode a -> Number -> Number -> Number -> Number -> TreemapNode a

-- | Treemap configuration
type TreemapConfig a =
  { size :: { width :: Number, height :: Number }
  , tile :: TileFunction a
  , round :: Boolean
  , paddingInner :: Number
  , paddingOuter :: Number
  , paddingTop :: Number
  , paddingRight :: Number
  , paddingBottom :: Number
  , paddingLeft :: Number
  }

-- | Golden ratio for squarify
phi :: Number
phi = (1.0 + sqrt 5.0) / 2.0 -- ≈ 1.618034

-- | Default treemap configuration
defaultTreemapConfig :: forall a. TreemapConfig a
defaultTreemapConfig =
  { size: { width: 1.0, height: 1.0 }
  , tile: squarify phi
  , round: false
  , paddingInner: 0.0
  , paddingOuter: 0.0
  , paddingTop: 0.0
  , paddingRight: 0.0
  , paddingBottom: 0.0
  , paddingLeft: 0.0
  }

-- | Convert ValuedNode to TreemapNode (without positioning)
toTreemapNode :: forall a. ValuedNode a -> TreemapNode a
toTreemapNode (VNode n) =
  TNode
    { data_: n.data_
    , depth: n.depth
    , height: n.height
    , value: n.value
    , children: map toTreemapNode n.children
    , x0: 0.0
    , y0: 0.0
    , x1: 0.0
    , y1: 0.0
    }

-- | Main treemap layout function
treemap :: forall a. TreemapConfig a -> ValuedNode a -> TreemapNode a
treemap config valuedRoot =
  let
    -- Convert to TreemapNode
    root = toTreemapNode valuedRoot

    -- Set initial bounds
    rootWithBounds = TNode case root of
      TNode n -> n
        { x0 = 0.0
        , y0 = 0.0
        , x1 = config.size.width
        , y1 = config.size.height
        }

    -- Position all nodes
    positioned = positionTree config rootWithBounds []

  in
    positioned

-- | Position tree recursively (pre-order traversal)
positionTree :: forall a. TreemapConfig a -> TreemapNode a -> Array Number -> TreemapNode a
positionTree config (TNode n) paddingStack =
  let
    -- Get padding for this depth
    p = case Array.index paddingStack n.depth of
      Just pad -> pad
      Nothing -> 0.0

    -- Apply outer padding
    x0 = n.x0 + p
    y0 = n.y0 + p
    x1 = n.x1 - p
    y1 = n.y1 - p

    -- Collapse to center if padding exceeds space
    x0' = if x1 < x0 then (x0 + x1) / 2.0 else x0
    x1' = if x1 < x0 then (x0 + x1) / 2.0 else x1
    y0' = if y1 < y0 then (y0 + y1) / 2.0 else y0
    y1' = if y1 < y0 then (y0 + y1) / 2.0 else y1

    -- Update node with padded bounds
    nodeWithPadding = TNode (n { x0 = x0', y0 = y0', x1 = x1', y1 = y1' })

  in
    if Array.null n.children then nodeWithPadding -- Leaf node
    else
      let
        -- Calculate inner padding for children
        innerPad = config.paddingInner / 2.0
        newPaddingStack = paddingStack <> [ innerPad ]

        -- Apply directional padding
        childX0 = x0' + config.paddingLeft - innerPad
        childY0 = y0' + config.paddingTop - innerPad
        childX1 = x1' - config.paddingRight + innerPad
        childY1 = y1' - config.paddingBottom + innerPad

        -- Collapse if needed
        childX0' = if childX1 < childX0 then (childX0 + childX1) / 2.0 else childX0
        childX1' = if childX1 < childX0 then (childX0 + childX1) / 2.0 else childX1
        childY0' = if childY1 < childY0 then (childY0 + childY1) / 2.0 else childY0
        childY1' = if childY1 < childY0 then (childY0 + childY1) / 2.0 else childY1

        -- Call tiling function to position children
        nodeWithTiledChildren = config.tile nodeWithPadding childX0' childY0' childX1' childY1'

        -- Recursively position children's descendants
        TNode tiledNode = nodeWithTiledChildren
        finalChildren = map (\child -> positionTree config child newPaddingStack) tiledNode.children

      in
        TNode (tiledNode { children = finalChildren })

-- | Dice tiling: horizontal partitioning (varies x, constant y)
dice :: forall a. TreemapNode a -> Number -> Number -> Number -> Number -> TreemapNode a
dice (TNode parent) x0 y0 x1 y1 =
  let
    -- Scaling factor
    k =
      if parent.value /= 0.0 then (x1 - x0) / parent.value
      else 0.0

    -- Position each child
    positioned = foldl
      ( \acc child ->
          let
            TNode c = child
            currentX = acc.x
            width = c.value * k

            positionedChild = TNode
              ( c
                  { x0 = currentX
                  , x1 = currentX + width
                  , y0 = y0
                  , y1 = y1
                  }
              )
          in
            { children: Array.snoc acc.children positionedChild
            , x: currentX + width
            }
      )
      { children: [], x: x0 }
      parent.children

  in
    TNode (parent { children = positioned.children })

-- | Slice tiling: vertical partitioning (constant x, varies y)
slice :: forall a. TreemapNode a -> Number -> Number -> Number -> Number -> TreemapNode a
slice (TNode parent) x0 y0 x1 y1 =
  let
    -- Scaling factor
    k =
      if parent.value /= 0.0 then (y1 - y0) / parent.value
      else 0.0

    -- Position each child
    positioned = foldl
      ( \acc child ->
          let
            TNode c = child
            currentY = acc.y
            height = c.value * k

            positionedChild = TNode
              ( c
                  { x0 = x0
                  , x1 = x1
                  , y0 = currentY
                  , y1 = currentY + height
                  }
              )
          in
            { children: Array.snoc acc.children positionedChild
            , y: currentY + height
            }
      )
      { children: [], y: y0 }
      parent.children

  in
    TNode (parent { children = positioned.children })

-- | Slice-dice tiling: alternates between slice and dice at each depth level
-- | Provides a simple alternating pattern
sliceDice :: forall a. TileFunction a
sliceDice parent x0 y0 x1 y1 =
  let
    TNode p = parent
  in
    if (p.depth `mod` 2) == 0 then slice parent x0 y0 x1 y1
    else dice parent x0 y0 x1 y1

-- | Binary tiling: divides space based on splitting the value sum
-- | Simpler approach: split children into two groups, position each group with slice/dice
binary :: forall a. TileFunction a
binary (TNode parent) x0 y0 x1 y1 =
  let
    children = parent.children
    n = Array.length children
  in
    if n == 0 then TNode parent
    else if n == 1 then
      -- Single child gets full space
      case Array.index children 0 of
        Just (TNode child) ->
          TNode (parent { children = [ TNode (child { x0 = x0, y0 = y0, x1 = x1, y1 = y1 }) ] })
        Nothing -> TNode parent
    else
      -- Multiple children: split into two groups
      let
        totalValue = parent.value
        dx = x1 - x0
        dy = y1 - y0

        -- Split vertically if wider than tall
        splitVertically = dx > dy

        -- Find midpoint (half the total value)
        halfValue = totalValue / 2.0

        -- Find split index
        findSplitIndex :: Number -> Int -> Int
        findSplitIndex accValue idx =
          if idx >= n - 1 then idx
          else
            case Array.index children idx of
              Just (TNode child) ->
                let
                  newAccValue = accValue + child.value
                in
                  if newAccValue >= halfValue then idx
                  else findSplitIndex newAccValue (idx + 1)
              Nothing -> findSplitIndex accValue (idx + 1)

        splitIdx = findSplitIndex 0.0 0
        leftChildren = Array.slice 0 (splitIdx + 1) children
        rightChildren = Array.slice (splitIdx + 1) n children

        leftValue = foldl (\sum (TNode c) -> sum + c.value) 0.0 leftChildren

        -- Calculate split position
        splitPos =
          if totalValue > 0.0 then leftValue / totalValue
          else 0.5

        -- Position left and right groups
        leftPositioned =
          if splitVertically then dice (TNode (parent { children = leftChildren, value = leftValue })) x0 y0 (x0 + dx * splitPos) y1
          else slice (TNode (parent { children = leftChildren, value = leftValue })) x0 y0 x1 (y0 + dy * splitPos)

        rightPositioned =
          if splitVertically then dice (TNode (parent { children = rightChildren, value = totalValue - leftValue })) (x0 + dx * splitPos) y0 x1 y1
          else slice (TNode (parent { children = rightChildren, value = totalValue - leftValue })) x0 (y0 + dy * splitPos) x1 y1

        TNode leftNode = leftPositioned
        TNode rightNode = rightPositioned
      in
        TNode (parent { children = leftNode.children <> rightNode.children })

-- | Squarify tiling: minimizes aspect ratios (creates more square-like rectangles)
squarify :: forall a. Number -> TileFunction a
squarify ratio (TNode parent) x0 y0 x1 y1 =
  let
    result = squarifyRatio ratio (TNode parent) x0 y0 x1 y1
  in
    TNode (parent { children = result.children })

-- | Core squarify algorithm
squarifyRatio
  :: forall a
   . Number
  -> TreemapNode a
  -> Number
  -> Number
  -> Number
  -> Number
  -> { children :: Array (TreemapNode a), x0 :: Number, y0 :: Number }
squarifyRatio ratio (TNode parent) initX0 initY0 initX1 initY1 =
  let
    nodes = parent.children
    n = Array.length nodes

    go :: Int -> Int -> Number -> Number -> Number -> Number -> Number -> Array (TreemapNode a) -> Array (TreemapNode a)
    go i0 i1 x0 y0 x1 y1 remainingValue acc =
      if i0 >= n then acc
      else
        let
          dx = x1 - x0
          dy = y1 - y0

          -- Find first non-empty node
          findNonEmpty :: Int -> Number -> { i1 :: Int, sumValue :: Number }
          findNonEmpty idx sv =
            if idx >= n then { i1: idx, sumValue: sv }
            else
              let
                TNode child = case Array.index nodes idx of
                  Just c -> c
                  Nothing -> TNode parent -- Shouldn't happen
                val = child.value
              in
                if val /= 0.0 then { i1: idx + 1, sumValue: val }
                else findNonEmpty (idx + 1) sv

          firstNode = findNonEmpty i1 0.0

          -- Calculate alpha and initial beta
          alpha = max (dy / dx) (dx / dy) / (remainingValue * ratio)
          beta0 = firstNode.sumValue * firstNode.sumValue * alpha

          -- Try adding more nodes while ratio improves
          tryAdd
            :: Int
            -> Number
            -> Number
            -> Number
            -> Number
            -> { i1 :: Int, sumValue :: Number, minRatio :: Number }
          tryAdd idx sumVal minVal maxVal minRat =
            if idx >= n then { i1: idx, sumValue: sumVal, minRatio: minRat }
            else
              let
                TNode child = case Array.index nodes idx of
                  Just c -> c
                  Nothing -> TNode parent
                nodeValue = child.value
                newSum = sumVal + nodeValue
                newMin = min minVal nodeValue
                newMax = max maxVal nodeValue
                newBeta = newSum * newSum * alpha
                newRatio = max (newMax / newBeta) (newBeta / newMin)
              in
                if newRatio > minRat then { i1: idx, sumValue: sumVal, minRatio: minRat } -- Reject
                else tryAdd (idx + 1) newSum newMin newMax newRatio -- Accept

          TNode firstChild = case Array.index nodes (firstNode.i1 - 1) of
            Just c -> c
            Nothing -> TNode parent
          initial = tryAdd firstNode.i1 firstNode.sumValue firstChild.value firstChild.value
            (max (firstChild.value / beta0) (beta0 / firstChild.value))

          -- Extract row of children
          rowChildren = Array.slice i0 initial.i1 nodes

          -- Determine orientation
          isDice = dx < dy

          -- Position row
          -- Create row node with correct value (row's sum, not parent's value)
          rowNode = TNode (parent { children = rowChildren, value = initial.sumValue })

          positioned =
            if isDice then
              let
                newY0 =
                  if remainingValue /= 0.0 then y0 + dy * initial.sumValue / remainingValue
                  else y1
                TNode diced = dice rowNode x0 y0 x1 newY0
              in
                { children: diced.children, nextX0: x0, nextY0: newY0, nextX1: x1, nextY1: y1 }
            else
              let
                newX0 =
                  if remainingValue /= 0.0 then x0 + dx * initial.sumValue / remainingValue
                  else x1
                TNode sliced = slice rowNode x0 y0 newX0 y1
              in
                { children: sliced.children, nextX0: newX0, nextY0: y0, nextX1: x1, nextY1: y1 }

        in
          go initial.i1 initial.i1 positioned.nextX0 positioned.nextY0 positioned.nextX1 positioned.nextY1
            (remainingValue - initial.sumValue)
            (acc <> positioned.children)

  in
    { children: go 0 0 initX0 initY0 initX1 initY1 parent.value []
    , x0: initX0
    , y0: initY0
    }
