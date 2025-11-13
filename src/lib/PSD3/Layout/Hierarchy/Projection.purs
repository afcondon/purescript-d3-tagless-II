-- | PSD3.Layout.Hierarchy.Projection
-- |
-- | Coordinate projection utilities for transforming hierarchy layout coordinates
-- | into screen space. Provides building blocks for common projections (vertical,
-- | horizontal, radial, isometric) that users can compose or extend.
-- |
-- | The tree layout algorithm computes positions in abstract (x, y) space where:
-- |   - x represents the breadth/spread dimension
-- |   - y represents the depth dimension
-- |
-- | These projections transform those abstract coordinates into screen coordinates
-- | for rendering in different orientations and coordinate systems.

module PSD3.Layout.Hierarchy.Projection
  ( -- * Coordinate Accessors for TreeNode
    verticalX
  , verticalY
  , horizontalX
  , horizontalY
  , radialX
  , radialY
  , isometricX
  , isometricY
  , isometricScale
  -- * Coordinate Accessors for ClusterNode
  , clusterVerticalX
  , clusterVerticalY
  , clusterHorizontalX
  , clusterHorizontalY
  , clusterRadialX
  , clusterRadialY
  , clusterIsometricX
  , clusterIsometricY
  -- * Link Path Generators
  , verticalLinkPath
  , horizontalLinkPath
  , radialLinkPath
  , isometricLinkPath
  , isometricTreeLinkPath
  , isometricClusterLinkPath
  ) where

import Prelude
import PSD3.Layout.Hierarchy.Tree (TreeNode(..))
import PSD3.Layout.Hierarchy.Cluster (ClusterNode(..))
import Data.Number (cos, sin, pi, sqrt, abs)

-- ============================================================================
-- Coordinate Accessors for TreeNode
-- ============================================================================

-- | Vertical projection (identity): x → x, y → y
-- | Standard top-to-bottom tree with root at top
verticalX :: forall a. TreeNode a -> Number
verticalX (TreeNode n) = n.x

verticalY :: forall a. TreeNode a -> Number
verticalY (TreeNode n) = n.y

-- | Horizontal projection (swapped): x → y, y → x
-- | Left-to-right tree with root on left
horizontalX :: forall a. TreeNode a -> Number
horizontalX (TreeNode n) = n.y

horizontalY :: forall a. TreeNode a -> Number
horizontalY (TreeNode n) = n.x

-- | Radial projection: (angle, radius) → (x, y) in polar coordinates
-- | Tree emanates from center in circular pattern
-- | Assumes x is angle (in radians) and y is radius
radialX :: forall a. TreeNode a -> Number
radialX (TreeNode n) = n.y * cos(n.x)

radialY :: forall a. TreeNode a -> Number
radialY (TreeNode n) = n.y * sin(n.x)

-- | Isometric projection: transforms x,y into isometric 2.5D view
-- | Uses standard isometric angle (30 degrees) for pleasant depth perception
-- | This creates a "cabinet" or "dimetric" view common in technical drawings
-- |
-- | The transformation places root at top-right, spreading NW to SE:
-- |   iso_x = (y - x) * cos(30°)
-- |   iso_y = (x + y) * sin(30°)
-- |
-- | The swap (y - x instead of x - y) creates the NW-SE diagonal orientation
isometricX :: forall a. TreeNode a -> Number
isometricX (TreeNode n) = (n.y - n.x) * cos(pi / 6.0)  -- 30 degrees, swapped for NW-SE

isometricY :: forall a. TreeNode a -> Number
isometricY (TreeNode n) = (n.x + n.y) * sin(pi / 6.0)  -- 30 degrees

-- | Scale factor for isometric projections to prevent excessive compression
-- | Typical isometric projections compress the apparent size, so we scale up
isometricScale :: Number
isometricScale = sqrt 2.0

-- ============================================================================
-- Coordinate Accessors for ClusterNode (Dendrogram)
-- ============================================================================

-- | Vertical projection for cluster/dendrogram
clusterVerticalX :: forall a. ClusterNode a -> Number
clusterVerticalX (ClusterNode n) = n.x

clusterVerticalY :: forall a. ClusterNode a -> Number
clusterVerticalY (ClusterNode n) = n.y

-- | Horizontal projection for cluster/dendrogram
clusterHorizontalX :: forall a. ClusterNode a -> Number
clusterHorizontalX (ClusterNode n) = n.y

clusterHorizontalY :: forall a. ClusterNode a -> Number
clusterHorizontalY (ClusterNode n) = n.x

-- | Radial projection for cluster/dendrogram
clusterRadialX :: forall a. ClusterNode a -> Number
clusterRadialX (ClusterNode n) = n.y * cos(n.x)

clusterRadialY :: forall a. ClusterNode a -> Number
clusterRadialY (ClusterNode n) = n.y * sin(n.x)

-- | Isometric projection for cluster/dendrogram
-- | Same NW-SE orientation as tree isometric
clusterIsometricX :: forall a. ClusterNode a -> Number
clusterIsometricX (ClusterNode n) = (n.y - n.x) * cos(pi / 6.0)  -- swapped for NW-SE

clusterIsometricY :: forall a. ClusterNode a -> Number
clusterIsometricY (ClusterNode n) = (n.x + n.y) * sin(pi / 6.0)

-- ============================================================================
-- Link Path Generators
-- ============================================================================

-- | Generate SVG path for vertical tree links
-- | Creates a cubic Bezier curve with control points at midpoint Y
-- | This produces the classic "elbow" connector look
verticalLinkPath :: Number -> Number -> Number -> Number -> String
verticalLinkPath x1 y1 x2 y2 =
  let midY = (y1 + y2) / 2.0
  in "M" <> show x1 <> "," <> show y1
     <> " C" <> show x1 <> "," <> show midY
     <> " " <> show x2 <> "," <> show midY
     <> " " <> show x2 <> "," <> show y2

-- | Generate SVG path for horizontal tree links
-- | Creates a cubic Bezier curve with control points at midpoint X
horizontalLinkPath :: Number -> Number -> Number -> Number -> String
horizontalLinkPath x1 y1 x2 y2 =
  let midX = (x1 + x2) / 2.0
  in "M" <> show x1 <> "," <> show y1
     <> " C" <> show midX <> "," <> show y1
     <> " " <> show midX <> "," <> show y2
     <> " " <> show x2 <> "," <> show y2

-- | Generate SVG path for radial tree links
-- | Draws a smooth curve in polar coordinates
-- | Takes parent and child positions in polar coords (angle, radius)
-- | and converts to Cartesian for SVG path
radialLinkPath :: Number -> Number -> Number -> Number -> String
radialLinkPath angle1 radius1 angle2 radius2 =
  let
    -- Parent position in Cartesian
    x1 = radius1 * cos(angle1)
    y1 = radius1 * sin(angle1)
    -- Child position in Cartesian
    x2 = radius2 * cos(angle2)
    y2 = radius2 * sin(angle2)
    -- Control points at midpoint radius
    midRadius = (radius1 + radius2) / 2.0
    cx1 = midRadius * cos(angle1)
    cy1 = midRadius * sin(angle1)
    cx2 = midRadius * cos(angle2)
    cy2 = midRadius * sin(angle2)
  in "M" <> show x1 <> "," <> show y1
     <> " C" <> show cx1 <> "," <> show cy1
     <> " " <> show cx2 <> "," <> show cy2
     <> " " <> show x2 <> "," <> show y2

-- | Generate SVG path for isometric tree links (simple version)
-- | Uses cubic Bezier curves that follow the isometric perspective
-- |
-- | In isometric view, the "vertical" direction appears as a NW-SE diagonal.
-- | To make curves appear to enter/exit nodes from isometric top/bottom,
-- | we keep control points at the same Y coordinate as their node, but move
-- | them partway along X. This creates a curve that follows the isometric
-- | depth direction.
-- |
-- | NOTE: This simple version doesn't account for left vs right children.
-- | Use isometricTreeLinkPath or isometricClusterLinkPath for better curves.
isometricLinkPath :: Number -> Number -> Number -> Number -> String
isometricLinkPath x1 y1 x2 y2 =
  -- Control points: keep Y from respective nodes, use midpoint X
  -- This makes the curve follow the isometric depth (perpendicular to NW-SE diagonal)
  let midX = (x1 + x2) / 2.0
  in "M" <> show x1 <> "," <> show y1
     <> " C" <> show midX <> "," <> show y1
     <> " " <> show midX <> "," <> show y2
     <> " " <> show x2 <> "," <> show y2

-- | Generate SVG path for isometric tree links (node-aware version for TreeNode)
-- | Creates smooth bezier curves that follow the isometric projection aesthetic
-- | Uses asymmetric offsets: minimal parent offset, larger child offset for best appearance
isometricTreeLinkPath :: forall a. TreeNode a -> TreeNode a -> String
isometricTreeLinkPath parent child =
  let
    -- Get projected coordinates
    x1 = isometricX parent
    y1 = isometricY parent
    x2 = isometricX child
    y2 = isometricY child

    -- Distance-based offset calculation
    distance = abs (x2 - x1) + abs (y2 - y1)
    parentOffset = distance * 0.1    -- Small offset at parent
    childOffset = distance * 0.25    -- Larger offset at child

    -- Calculate control points
    -- Both control points move along NW-SE diagonal (+x, +y for parent; -x, -y for child)
    -- This creates curves that flow naturally with the isometric perspective
    cx1 = x1 + parentOffset
    cy1 = y1 + parentOffset
    cx2 = x2 - childOffset
    cy2 = y2 - childOffset
  in
    "M" <> show x1 <> "," <> show y1
    <> " C" <> show cx1 <> "," <> show cy1
    <> " " <> show cx2 <> "," <> show cy2
    <> " " <> show x2 <> "," <> show y2

-- | Generate SVG path for isometric cluster links (node-aware version for ClusterNode)
-- | Same as isometricTreeLinkPath but for ClusterNode
isometricClusterLinkPath :: forall a. ClusterNode a -> ClusterNode a -> String
isometricClusterLinkPath parent child =
  let
    -- Get projected coordinates
    x1 = clusterIsometricX parent
    y1 = clusterIsometricY parent
    x2 = clusterIsometricX child
    y2 = clusterIsometricY child

    -- Distance-based offset calculation
    distance = abs (x2 - x1) + abs (y2 - y1)
    parentOffset = distance * 0.1    -- Small offset at parent
    childOffset = distance * 0.25    -- Larger offset at child

    -- Calculate control points
    -- Both control points move along NW-SE diagonal (+x, +y for parent; -x, -y for child)
    cx1 = x1 + parentOffset
    cy1 = y1 + parentOffset
    cx2 = x2 - childOffset
    cy2 = y2 - childOffset
  in
    "M" <> show x1 <> "," <> show y1
    <> " C" <> show cx1 <> "," <> show cy1
    <> " " <> show cx2 <> "," <> show cy2
    <> " " <> show x2 <> "," <> show y2
