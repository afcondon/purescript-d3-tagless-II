module PSD3.Shared.HierarchyHelpers where

import Prelude

import PSD3.Data.Node (D3_TreeNode)
import PSD3.Internal.FFI (hNodeDepth_, hNodeHeight_, hNodeX_, hNodeY_, hNodeR_, hNodeX0_, hNodeY0_, hNodeX1_, hNodeY1_)
import Data.Int (round)
import Unsafe.Coerce (unsafeCoerce)

-- | Extract the name from a hierarchy node's data
-- | D3 hierarchy nodes have a .data property containing the original data
getName :: forall r. D3_TreeNode r -> String
getName node = case unsafeCoerce (unsafeCoerce node).data.name of
  n -> n

-- | Standard depth-based color scheme for hierarchy visualizations
-- | Provides consistent colors across treemap, icicle, bubble chart, etc.
-- | Uses a vibrant, harmonious color palette
depthColor :: Int -> String
depthColor depth = case depth of
  0 -> "#0ea5e9"  -- Sky blue (root level)
  1 -> "#8b5cf6"  -- Purple
  2 -> "#ec4899"  -- Pink
  3 -> "#f59e0b"  -- Amber
  4 -> "#10b981"  -- Emerald
  5 -> "#f97316"  -- Orange
  6 -> "#06b6d4"  -- Cyan
  _ -> "#64748b"  -- Slate gray (very deep levels)

-- | Type-safe accessor record for D3 hierarchy nodes
-- | Provides consistent, computed access to node properties across all hierarchy layouts
-- | Similar to treeDatum_ pattern used in tree visualizations
hierarchyNode_ :: forall r.
  { name :: D3_TreeNode r -> String
  , depth :: D3_TreeNode r -> Number
  , depthInt :: D3_TreeNode r -> Int
  , height :: D3_TreeNode r -> Number

  -- Rectangular layout properties (partition/treemap)
  , x0 :: D3_TreeNode r -> Number
  , y0 :: D3_TreeNode r -> Number
  , x1 :: D3_TreeNode r -> Number
  , y1 :: D3_TreeNode r -> Number
  , rectWidth :: D3_TreeNode r -> Number
  , rectHeight :: D3_TreeNode r -> Number

  -- Circular layout properties (pack/bubble)
  , x :: D3_TreeNode r -> Number
  , y :: D3_TreeNode r -> Number
  , r :: D3_TreeNode r -> Number

  -- Computed properties
  , color :: D3_TreeNode r -> String
  , hasArea :: D3_TreeNode r -> Boolean
  , hasCircleArea :: D3_TreeNode r -> Boolean
  }
hierarchyNode_ =
  { name: getName
  , depth: hNodeDepth_
  , depthInt: \node -> round (hNodeDepth_ node)
  , height: hNodeHeight_

  -- Rectangular properties
  , x0: hNodeX0_
  , y0: hNodeY0_
  , x1: hNodeX1_
  , y1: hNodeY1_
  , rectWidth: \node -> hNodeX1_ node - hNodeX0_ node
  , rectHeight: \node -> hNodeY1_ node - hNodeY0_ node

  -- Circular properties
  , x: hNodeX_
  , y: hNodeY_
  , r: hNodeR_

  -- Computed
  , color: \node -> depthColor (round $ hNodeDepth_ node)
  , hasArea: \node ->
      let w = hNodeX1_ node - hNodeX0_ node
          h = hNodeY1_ node - hNodeY0_ node
      in w > 0.0 && h > 0.0
  , hasCircleArea: \node -> hNodeR_ node > 0.0
  }

-- | Check if a rectangular node is large enough to show a label
-- | Common pattern across treemap and icicle visualizations
canShowLabel :: forall r.
  { minWidth :: Number, minHeight :: Number } ->
  D3_TreeNode r ->
  Boolean
canShowLabel { minWidth, minHeight } node =
  hierarchyNode_.rectWidth node > minWidth &&
  hierarchyNode_.rectHeight node > minHeight

-- | Check if a circular node is large enough to show a label
canShowCircleLabel :: forall r.
  { minRadius :: Number } ->
  D3_TreeNode r ->
  Boolean
canShowCircleLabel { minRadius } node =
  hierarchyNode_.r node > minRadius
