-- | TreeBuilder3 Theme
-- |
-- | Centralized color palette and visual styling for the tree editor.
-- | Uses purescript-colors for type-safe color manipulation.
-- |
-- | Example usage with Friendly DSL:
-- | ```purescript
-- | F.fill (F.color Theme.nodeStroke)
-- | F.stroke (F.color $ darken 0.1 Theme.linkColor)
-- | ```
module TreeBuilder3.Theme
  ( -- * Node Colors by Type
    nodeTypeColor
    -- * Semantic Colors
  , linkColor
  , nodeStroke
  , nameLabelColor
  , keyHintsColor
  , nodeLabelLight
  , nodeLabelDark
    -- * Stroke Widths
  , strokeWidthNormal
  , strokeWidthSelected
  , strokeWidthLink
    -- * Font Sizes (in pixels)
  , fontSizeLabel
  , fontSizeBadge
  , fontSizeName
  , fontSizeHints
    -- * Node Dimensions
  , nodeWidth
  , nodeHeight
  , nodeCornerRadius
  , badgeWidth
  , badgeHeight
  , badgeCornerRadius
    -- * Re-export color utilities
  , module Color
  ) where

import Color (Color, fromHexString, black, white)
import Color (Color, darken, desaturate, fromHexString, lighten, saturate, toHexString, black, white) as Color
import Data.Maybe (fromMaybe)

import TreeBuilder3.Types (DslNodeType(..))

-- =============================================================================
-- Helper
-- =============================================================================

-- | Parse hex to Color with fallback to black
hex :: String -> Color
hex s = fromMaybe Color.black (fromHexString s)

-- =============================================================================
-- Node Colors by AST Type
-- =============================================================================

-- | Get color for a DSL node type
nodeTypeColor :: DslNodeType -> Color
nodeTypeColor = case _ of
  -- Element nodes
  NodeElem _ -> hex "#6B7280" -- Gray

  -- Join types
  NodeJoin -> hex "#E2D24A" -- Yellow
  NodeNestedJoin -> hex "#D4A017" -- Gold
  NodeUpdateJoin -> hex "#4A90E2" -- Blue
  NodeUpdateNestedJoin -> hex "#9B4AE2" -- Purple

  -- Attrs and behaviors
  NodeAttr _ -> hex "#68D391" -- Pastel green
  NodeBehavior _ -> hex "#E27A4A" -- Orange

  -- GUP selection phases (classic D3 GUP demo colors)
  NodeEnter -> hex "#2CA02C" -- Green (enter = new)
  NodeUpdate -> hex "#7F7F7F" -- Gray (update = existing)
  NodeExit -> hex "#8C564B" -- Brown (exit = removed)

  -- Pending types - lighter/desaturated versions
  PendingElement -> hex "#9CA3AF" -- Light gray
  PendingAttr -> hex "#86EFAC" -- Light green
  PendingAttrValue _ -> hex "#86EFAC" -- Light green
  PendingBehavior -> hex "#FDBA74" -- Light orange

-- =============================================================================
-- Semantic Colors
-- =============================================================================

-- | Color for tree links/edges
linkColor :: Color
linkColor = hex "#888888"

-- | Stroke color for node rectangles
nodeStroke :: Color
nodeStroke = hex "#333333"

-- | Color for name labels (shown to left of nodes)
nameLabelColor :: Color
nameLabelColor = hex "#4A90E2"

-- | Color for key hint text (shown when selected)
keyHintsColor :: Color
keyHintsColor = hex "#666666"

-- | Light text color (for dark node backgrounds)
nodeLabelLight :: Color
nodeLabelLight = Color.white

-- | Dark text color (for light node backgrounds)
nodeLabelDark :: Color
nodeLabelDark = Color.black

-- =============================================================================
-- Stroke Widths
-- =============================================================================

strokeWidthNormal :: Number
strokeWidthNormal = 2.0

strokeWidthSelected :: Number
strokeWidthSelected = 3.0

strokeWidthLink :: Number
strokeWidthLink = 2.0

-- =============================================================================
-- Font Sizes (in pixels)
-- =============================================================================

fontSizeLabel :: Number
fontSizeLabel = 11.0

fontSizeBadge :: Number
fontSizeBadge = 9.0

fontSizeName :: Number
fontSizeName = 10.0

fontSizeHints :: Number
fontSizeHints = 10.0

-- =============================================================================
-- Node Dimensions
-- =============================================================================

nodeWidth :: Number
nodeWidth = 80.0

nodeHeight :: Number
nodeHeight = 24.0

nodeCornerRadius :: Number
nodeCornerRadius = 4.0

badgeWidth :: Number
badgeWidth = 56.0

badgeHeight :: Number
badgeHeight = 20.0

badgeCornerRadius :: Number
badgeCornerRadius = 10.0
