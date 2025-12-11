-- | PSD3v3 Path Generators
-- |
-- | Pure functions that generate SVG path strings.
-- | Used by interpreters to compute actual path data.
module PSD3v3.Path.Generators
  ( -- Simple
    genLinePath
  , genPolylinePath
  -- Links
  , genLinkHorizontal
  , genLinkVertical
  , genLinkRadial
  -- Sankey
  , genSankeyLink
  -- Chord
  , genRibbon
  , genArc
  ) where

import Prelude

import Data.Array (uncons)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin, pi)

-- =============================================================================
-- Simple Paths
-- =============================================================================

-- | Straight line path
genLinePath :: Number -> Number -> Number -> Number -> String
genLinePath x1 y1 x2 y2 =
  "M " <> show x1 <> "," <> show y1 <>
  " L " <> show x2 <> "," <> show y2

-- | Polyline through points
genPolylinePath :: Array { x :: Number, y :: Number } -> String
genPolylinePath points =
  case uncons points of
    Nothing -> ""
    Just { head, tail } ->
      "M " <> show head.x <> "," <> show head.y <>
      foldl (\acc p -> acc <> " L " <> show p.x <> "," <> show p.y) "" tail

-- =============================================================================
-- Link Paths
-- =============================================================================

-- | Horizontal cubic bezier link (for left-to-right trees)
-- | Control points at horizontal midpoint
genLinkHorizontal :: Number -> Number -> Number -> Number -> String
genLinkHorizontal x1 y1 x2 y2 =
  let mx = (x1 + x2) / 2.0
  in "M " <> show x1 <> "," <> show y1 <>
     " C " <> show mx <> "," <> show y1 <>
     " " <> show mx <> "," <> show y2 <>
     " " <> show x2 <> "," <> show y2

-- | Vertical cubic bezier link (for top-down trees)
-- | Control points at vertical midpoint
genLinkVertical :: Number -> Number -> Number -> Number -> String
genLinkVertical x1 y1 x2 y2 =
  let my = (y1 + y2) / 2.0
  in "M " <> show x1 <> "," <> show y1 <>
     " C " <> show x1 <> "," <> show my <>
     " " <> show x2 <> "," <> show my <>
     " " <> show x2 <> "," <> show y2

-- | Radial link for circular layouts
-- | Converts polar (angle, radius) to cartesian and creates curved path
genLinkRadial :: Number -> Number -> Number -> Number -> String
genLinkRadial angle1 radius1 angle2 radius2 =
  let
    -- Convert polar to cartesian (angle=0 is up, clockwise)
    x1 = radius1 * sin angle1
    y1 = -radius1 * cos angle1  -- negative because SVG y is down
    x2 = radius2 * sin angle2
    y2 = -radius2 * cos angle2
    -- Midpoint radius for control points
    mr = (radius1 + radius2) / 2.0
    ma = (angle1 + angle2) / 2.0
    mx = mr * sin ma
    my = -mr * cos ma
  in "M " <> show x1 <> "," <> show y1 <>
     " Q " <> show mx <> "," <> show my <>
     " " <> show x2 <> "," <> show y2

-- =============================================================================
-- Sankey Links
-- =============================================================================

-- | Sankey flow link - a filled ribbon shape
-- | Goes from source (x, y0-y1) to target (x, y0-y1)
genSankeyLink :: Number -> Number -> Number -> Number -> Number -> Number -> String
genSankeyLink sx sy0 sy1 tx ty0 ty1 =
  let
    -- Horizontal midpoint for bezier control
    mx = (sx + tx) / 2.0
  in
    -- Top edge: source top to target top (bezier)
    "M " <> show sx <> "," <> show sy0 <>
    " C " <> show mx <> "," <> show sy0 <>
    " " <> show mx <> "," <> show ty0 <>
    " " <> show tx <> "," <> show ty0 <>
    -- Right edge: target top to target bottom
    " L " <> show tx <> "," <> show ty1 <>
    -- Bottom edge: target bottom to source bottom (bezier, reverse direction)
    " C " <> show mx <> "," <> show ty1 <>
    " " <> show mx <> "," <> show sy1 <>
    " " <> show sx <> "," <> show sy1 <>
    -- Close path
    " Z"

-- =============================================================================
-- Chord/Ribbon Paths
-- =============================================================================

-- | Ribbon for chord diagrams
-- | Connects two arcs with quadratic bezier curves through center
genRibbon :: Number -> Number -> Number -> Number -> Number -> Number -> String
genRibbon sa0 sa1 ta0 ta1 innerR outerR =
  let
    -- Source arc endpoints (on outer radius)
    sx0 = outerR * sin sa0
    sy0 = -outerR * cos sa0
    sx1 = outerR * sin sa1
    sy1 = -outerR * cos sa1

    -- Target arc endpoints (on outer radius)
    tx0 = outerR * sin ta0
    ty0 = -outerR * cos ta0
    tx1 = outerR * sin ta1
    ty1 = -outerR * cos ta1

    -- For the bezier curves connecting source to target,
    -- we use the center (0,0) as control point for smooth ribbons

    -- Large arc flag for the arcs (1 if angle > pi)
    sourceArcFlag = if (sa1 - sa0) > pi then "1" else "0"
    targetArcFlag = if (ta1 - ta0) > pi then "1" else "0"
  in
    -- Start at source arc start
    "M " <> show sx0 <> "," <> show sy0 <>
    -- Arc along source
    " A " <> show outerR <> "," <> show outerR <> " 0 " <> sourceArcFlag <> " 1 " <> show sx1 <> "," <> show sy1 <>
    -- Quadratic bezier to target arc start (through center)
    " Q 0,0 " <> show tx0 <> "," <> show ty0 <>
    -- Arc along target
    " A " <> show outerR <> "," <> show outerR <> " 0 " <> targetArcFlag <> " 1 " <> show tx1 <> "," <> show ty1 <>
    -- Quadratic bezier back to source arc start (through center)
    " Q 0,0 " <> show sx0 <> "," <> show sy0 <>
    " Z"

-- | Arc segment for pie/donut charts
-- | Creates a "slice" shape between two angles
genArc :: Number -> Number -> Number -> Number -> String
genArc startAngle endAngle innerRadius outerRadius =
  let
    -- Outer arc endpoints
    ox0 = outerRadius * sin startAngle
    oy0 = -outerRadius * cos startAngle
    ox1 = outerRadius * sin endAngle
    oy1 = -outerRadius * cos endAngle

    -- Inner arc endpoints (for donut hole)
    ix0 = innerRadius * sin startAngle
    iy0 = -innerRadius * cos startAngle
    ix1 = innerRadius * sin endAngle
    iy1 = -innerRadius * cos endAngle

    -- Large arc flag
    largeArc = if (endAngle - startAngle) > pi then "1" else "0"
  in
    if innerRadius <= 0.0 then
      -- Pie slice (no hole)
      "M 0,0" <>
      " L " <> show ox0 <> "," <> show oy0 <>
      " A " <> show outerRadius <> "," <> show outerRadius <> " 0 " <> largeArc <> " 1 " <> show ox1 <> "," <> show oy1 <>
      " Z"
    else
      -- Donut slice (with hole)
      "M " <> show ox0 <> "," <> show oy0 <>
      " A " <> show outerRadius <> "," <> show outerRadius <> " 0 " <> largeArc <> " 1 " <> show ox1 <> "," <> show oy1 <>
      " L " <> show ix1 <> "," <> show iy1 <>
      " A " <> show innerRadius <> "," <> show innerRadius <> " 0 " <> largeArc <> " 0 " <> show ix0 <> "," <> show iy0 <>
      " Z"
