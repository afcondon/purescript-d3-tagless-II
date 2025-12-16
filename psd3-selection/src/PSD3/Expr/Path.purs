-- | PSD3v3 Path DSL
-- |
-- | Pre-canned path generators for common visualization patterns.
-- | These cover 90%+ of data visualization path needs:
-- | - Links (trees, Sankey, network graphs)
-- | - Ribbons (chord diagrams)
-- | - Arcs (pie/donut charts)
-- | - Simple shapes (lines, areas)
-- |
-- | More expressive than Mermaid/Vegalite, but without requiring
-- | arbitrary imperative path string building.
module PSD3.Expr.Path
  ( class PathExpr
  -- Simple paths
  , linePath
  , polylinePath
  -- Link paths (for hierarchies and flows)
  , linkHorizontal
  , linkVertical
  , linkRadial
  -- Sankey-style
  , sankeyLink
  -- Chord/ribbon paths
  , ribbon
  , arc
  -- Utility
  , closePath
  ) where

import Prelude

-- | Path expressions - generate SVG path strings
-- |
-- | Each interpreter handles these differently:
-- | - Eval: computes the actual path string
-- | - CodeGen: generates `(linkHorizontal x1 y1 x2 y2)`
-- | - SVG: computes the path string (same as Eval for paths)
class PathExpr repr where

  -- =========================================================================
  -- Simple Paths
  -- =========================================================================

  -- | Straight line from (x1,y1) to (x2,y2)
  -- | Generates: "M x1,y1 L x2,y2"
  linePath
    :: repr Number  -- x1
    -> repr Number  -- y1
    -> repr Number  -- x2
    -> repr Number  -- y2
    -> repr String

  -- | Polyline through multiple points
  -- | Takes array of {x, y} points
  -- | Generates: "M x0,y0 L x1,y1 L x2,y2 ..."
  polylinePath
    :: repr (Array { x :: Number, y :: Number })
    -> repr String

  -- =========================================================================
  -- Link Paths (Trees, DAGs, Networks)
  -- =========================================================================

  -- | Horizontal link with cubic bezier (left-to-right trees)
  -- | Control points at horizontal midpoint
  -- |
  -- | ```
  -- |  source ----╮
  -- |             ╰---- target
  -- | ```
  -- |
  -- | Generates: "M x1,y1 C mx,y1 mx,y2 x2,y2" where mx = (x1+x2)/2
  linkHorizontal
    :: repr Number  -- source x
    -> repr Number  -- source y
    -> repr Number  -- target x
    -> repr Number  -- target y
    -> repr String

  -- | Vertical link with cubic bezier (top-down trees)
  -- | Control points at vertical midpoint
  -- |
  -- | ```
  -- |     source
  -- |        |
  -- |        ╰---╮
  -- |            target
  -- | ```
  -- |
  -- | Generates: "M x1,y1 C x1,my x2,my x2,y2" where my = (y1+y2)/2
  linkVertical
    :: repr Number  -- source x
    -> repr Number  -- source y
    -> repr Number  -- target x
    -> repr Number  -- target y
    -> repr String

  -- | Radial link for radial/circular tree layouts
  -- | Takes angles (radians) and radii instead of x,y
  -- |
  -- | Generates curved path from (angle1, radius1) to (angle2, radius2)
  linkRadial
    :: repr Number  -- source angle (radians)
    -> repr Number  -- source radius
    -> repr Number  -- target angle (radians)
    -> repr Number  -- target radius
    -> repr String

  -- =========================================================================
  -- Sankey Links
  -- =========================================================================

  -- | Sankey-style horizontal flow link
  -- | Like linkHorizontal but with variable width (y0/y1 at each end)
  -- |
  -- | ```
  -- |  ╭──────────╮
  -- |  │  flow    │
  -- |  ╰──────────╯
  -- | ```
  -- |
  -- | Generates a filled shape (not just a stroke path)
  sankeyLink
    :: repr Number  -- source x
    -> repr Number  -- source y0 (top)
    -> repr Number  -- source y1 (bottom)
    -> repr Number  -- target x
    -> repr Number  -- target y0 (top)
    -> repr Number  -- target y1 (bottom)
    -> repr String

  -- =========================================================================
  -- Chord/Ribbon Paths (Circular layouts)
  -- =========================================================================

  -- | Ribbon for chord diagrams
  -- | Connects two arcs on a circle with curved bands
  -- |
  -- | Takes: source arc (startAngle, endAngle), target arc (startAngle, endAngle), radius
  -- |
  -- | The ribbon is a closed shape suitable for fill
  ribbon
    :: repr Number  -- source start angle
    -> repr Number  -- source end angle
    -> repr Number  -- target start angle
    -> repr Number  -- target end angle
    -> repr Number  -- inner radius
    -> repr Number  -- outer radius
    -> repr String

  -- | Arc segment (for pie/donut charts, chord diagram outer ring)
  -- |
  -- | Generates arc from startAngle to endAngle at given radii
  -- | Closed shape suitable for fill
  arc
    :: repr Number  -- start angle (radians)
    -> repr Number  -- end angle (radians)
    -> repr Number  -- inner radius (0 for pie, >0 for donut)
    -> repr Number  -- outer radius
    -> repr String

  -- =========================================================================
  -- Utility
  -- =========================================================================

  -- | Append "Z" to close a path
  closePath :: repr String -> repr String
