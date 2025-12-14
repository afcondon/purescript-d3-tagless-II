-- | Type-Safe Tree Styles
-- |
-- | Bundles tree layout orientation with matching link path generators.
-- | This prevents mismatches like using horizontal beziers with vertical trees.
-- |
-- | Usage:
-- | ```purescript
-- | -- Get a bundled style with matching link generator
-- | let style = verticalTree { width: 800.0, height: 600.0 }
-- |
-- | -- Use the bundled link generator
-- | let pathD = style.linkPath x1 y1 x2 y2
-- |
-- | -- Or create a custom style
-- | let custom = customTree myConfig myLinkFn
-- | ```
module DataViz.Layout.Hierarchy.TreeStyle
  ( TreeStyle
  , LinkPathFn
  -- Smart constructors (ensure matching pairs)
  , verticalTree
  , horizontalTree
  , radialTree
  -- Accessors
  , linkPath
  , orientation
  -- For custom/advanced usage
  , customTree
  , TreeOrientation(..)
  ) where

import Prelude

import DataViz.Layout.Hierarchy.Link (linkBezierVertical, linkBezierHorizontal, linkBezierRadialCartesian)

-- | Tree orientation (used as phantom type marker)
data TreeOrientation
  = Vertical
  | Horizontal
  | Radial
  | Custom

derive instance eqTreeOrientation :: Eq TreeOrientation

instance showTreeOrientation :: Show TreeOrientation where
  show Vertical = "Vertical"
  show Horizontal = "Horizontal"
  show Radial = "Radial"
  show Custom = "Custom"

-- | Type alias for link path generator functions
-- | Takes source (x1, y1) and target (x2, y2), returns SVG path string
type LinkPathFn = Number -> Number -> Number -> Number -> String

-- | A tree style bundles orientation with its matching link path generator.
-- | The orientation is a phantom type that enables type-safe composition.
type TreeStyle =
  { orientation :: TreeOrientation
  , linkPath :: LinkPathFn
  }

-- =============================================================================
-- Smart Constructors
-- =============================================================================

-- | Vertical tree: root at top, children below
-- | Uses vertical bezier curves (control points at midpoint Y)
verticalTree :: TreeStyle
verticalTree =
  { orientation: Vertical
  , linkPath: linkBezierVertical
  }

-- | Horizontal tree: root at left, children to the right
-- | Uses horizontal bezier curves (control points at midpoint X)
horizontalTree :: TreeStyle
horizontalTree =
  { orientation: Horizontal
  , linkPath: linkBezierHorizontal
  }

-- | Radial tree: root at center, children radiate outward
-- | Uses radial bezier curves (control points follow arc)
radialTree :: TreeStyle
radialTree =
  { orientation: Radial
  , linkPath: linkBezierRadialCartesian
  }

-- | Custom tree style with user-provided link generator
-- | Use this when you need a non-standard pairing
customTree :: LinkPathFn -> TreeStyle
customTree fn =
  { orientation: Custom
  , linkPath: fn
  }

-- =============================================================================
-- Accessors
-- =============================================================================

-- | Get the link path generator from a tree style
linkPath :: TreeStyle -> LinkPathFn
linkPath = _.linkPath

-- | Get the orientation from a tree style
orientation :: TreeStyle -> TreeOrientation
orientation = _.orientation
