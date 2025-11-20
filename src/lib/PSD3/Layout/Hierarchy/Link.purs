module PSD3.Layout.Hierarchy.Link where

import Prelude

import Data.Number (cos, sin, pi)

-- | Link style for hierarchical layouts
data LinkStyle
  = StepHorizontal  -- H-V-H steps: horizontal, then vertical, then horizontal
  | StepVertical    -- V-H-V steps: vertical, then horizontal, then vertical
  | BezierVertical  -- Bezier curve for top-down trees
  | BezierHorizontal -- Bezier curve for left-to-right trees
  | BezierRadial    -- Bezier curve for radial layouts
  | Diagonal        -- Straight diagonal line

derive instance eqLinkStyle :: Eq LinkStyle

instance showLinkStyle :: Show LinkStyle where
  show StepHorizontal = "Step H"
  show StepVertical = "Step V"
  show BezierVertical = "Bezier V"
  show BezierHorizontal = "Bezier H"
  show BezierRadial = "Radial"
  show Diagonal = "Diagonal"

-- | Generate SVG path for horizontal step link (H-V-H)
-- | Good for left-to-right or right-to-left trees
linkStepHorizontal :: Number -> Number -> Number -> Number -> String
linkStepHorizontal x1 y1 x2 y2 =
  let midX = (x1 + x2) / 2.0
  in "M" <> show x1 <> "," <> show y1 <>
     "H" <> show midX <>
     "V" <> show y2 <>
     "H" <> show x2

-- | Generate SVG path for vertical step link (V-H-V)
-- | Good for top-to-bottom trees (dendrograms)
linkStepVertical :: Number -> Number -> Number -> Number -> String
linkStepVertical x1 y1 x2 y2 =
  let midY = (y1 + y2) / 2.0
  in "M" <> show x1 <> "," <> show y1 <>
     "V" <> show midY <>
     "H" <> show x2 <>
     "V" <> show y2

-- | Generate SVG path for vertical bezier link
-- | Good for aesthetic top-down tree diagrams
linkBezierVertical :: Number -> Number -> Number -> Number -> String
linkBezierVertical x1 y1 x2 y2 =
  let midY = (y1 + y2) / 2.0
  in "M" <> show x1 <> "," <> show y1 <>
     "C" <> show x1 <> "," <> show midY <>
     " " <> show x2 <> "," <> show midY <>
     " " <> show x2 <> "," <> show y2

-- | Generate SVG path for horizontal bezier link
-- | Good for aesthetic left-to-right tree diagrams
linkBezierHorizontal :: Number -> Number -> Number -> Number -> String
linkBezierHorizontal x1 y1 x2 y2 =
  let midX = (x1 + x2) / 2.0
  in "M" <> show x1 <> "," <> show y1 <>
     "C" <> show midX <> "," <> show y1 <>
     " " <> show midX <> "," <> show y2 <>
     " " <> show x2 <> "," <> show y2

-- | Generate SVG path for radial bezier link
-- | Converts Cartesian to polar coordinates for radial layouts
-- | Input: source (x1, y1) and target (x2, y2) in tree coordinates
-- | where x represents angle and y represents radius
linkBezierRadial :: Number -> Number -> Number -> Number -> String
linkBezierRadial x1 y1 x2 y2 =
  let
    -- Convert tree coordinates to polar (x = angle in degrees, y = radius)
    angle1 = (x1 - 90.0) * pi / 180.0
    angle2 = (x2 - 90.0) * pi / 180.0
    r1 = y1
    r2 = y2

    -- Convert to Cartesian
    sx = r1 * cos angle1
    sy = r1 * sin angle1
    tx = r2 * cos angle2
    ty = r2 * sin angle2

    -- Control points for smooth curve
    cr1 = (r1 + r2) / 2.0
    cx1 = cr1 * cos angle1
    cy1 = cr1 * sin angle1
    cx2 = cr1 * cos angle2
    cy2 = cr1 * sin angle2
  in
    "M" <> show sx <> "," <> show sy <>
    "C" <> show cx1 <> "," <> show cy1 <>
    " " <> show cx2 <> "," <> show cy2 <>
    " " <> show tx <> "," <> show ty

-- | Generate SVG path for diagonal link (straight line)
linkDiagonal :: Number -> Number -> Number -> Number -> String
linkDiagonal x1 y1 x2 y2 =
  "M" <> show x1 <> "," <> show y1 <>
  "L" <> show x2 <> "," <> show y2

-- | Get link generator function for a given style
linkGenerator :: LinkStyle -> (Number -> Number -> Number -> Number -> String)
linkGenerator StepHorizontal = linkStepHorizontal
linkGenerator StepVertical = linkStepVertical
linkGenerator BezierVertical = linkBezierVertical
linkGenerator BezierHorizontal = linkBezierHorizontal
linkGenerator BezierRadial = linkBezierRadial
linkGenerator Diagonal = linkDiagonal
