-- | DataViz.Layout.StateMachine.Path
-- |
-- | SVG path generation for state machine diagrams.
-- | Converts layout positions into SVG path strings.
module DataViz.Layout.StateMachine.Path
  ( transitionPathD
  , selfLoopPathD
  , arrowheadPathD
  , initialArrowPathD
  , stateEllipse
  , stateFinalRing
  ) where

import Prelude

import Data.Number (cos, sin, pi, sqrt)
import DataViz.Layout.StateMachine.Types (TransitionPath, StatePosition)

-- | Generate SVG path d attribute for a transition arrow
-- | Uses quadratic bezier curve
transitionPathD :: TransitionPath -> String
transitionPathD path =
  if path.isSelfLoop
    then selfLoopPathD path
    else
      "M " <> show path.startX <> " " <> show path.startY <>
      " Q " <> show path.controlX <> " " <> show path.controlY <>
      " " <> show path.endX <> " " <> show path.endY

-- | Generate SVG path for a self-loop
-- | Uses an SVG arc for a clean circular loop
-- | The arc radius is computed from the distance between start and control points
selfLoopPathD :: TransitionPath -> String
selfLoopPathD path =
  let
    -- Compute radius from the geometry
    -- Control point is at the apex of the loop, start/end are where it meets the state
    dx = path.controlX - path.startX
    dy = path.controlY - path.startY
    radius = sqrt (dx * dx + dy * dy) * 0.8  -- Approximate radius

    -- SVG arc: A rx ry x-axis-rotation large-arc-flag sweep-flag x y
    -- large-arc-flag = 1 for arc > 180 degrees
    -- sweep-flag = 1 for clockwise
  in
    "M " <> show path.startX <> " " <> show path.startY <>
    " A " <> show radius <> " " <> show radius <>  -- rx, ry (circular)
    " 0" <>                                         -- x-axis-rotation
    " 1" <>                                         -- large-arc-flag (draw the big arc)
    " 1" <>                                         -- sweep-flag (clockwise)
    " " <> show path.endX <> " " <> show path.endY

-- | Generate SVG path for an arrowhead at the end of a transition
-- | Creates a simple triangle pointing in the direction of the arrow
arrowheadPathD :: Number -> Number -> Number -> Number -> String
arrowheadPathD tipX tipY angle size =
  let
    -- Two points behind the tip, spread apart
    backAngle1 = angle + pi - 0.4  -- ~23 degrees spread
    backAngle2 = angle + pi + 0.4
    back1x = tipX + size * cos backAngle1
    back1y = tipY + size * sin backAngle1
    back2x = tipX + size * cos backAngle2
    back2y = tipY + size * sin backAngle2
  in
    "M " <> show tipX <> " " <> show tipY <>
    " L " <> show back1x <> " " <> show back1y <>
    " L " <> show back2x <> " " <> show back2y <>
    " Z"

-- | Generate SVG path for the initial state arrow
-- | A simple horizontal arrow pointing right
initialArrowPathD :: { x :: Number, y :: Number, angle :: Number } -> Number -> String
initialArrowPathD pos len =
  let
    endX = pos.x + len * cos pos.angle
    endY = pos.y + len * sin pos.angle
  in
    "M " <> show pos.x <> " " <> show pos.y <>
    " L " <> show endX <> " " <> show endY

-- | Get ellipse attributes for a state
-- | Returns { cx, cy, rx, ry } ready for SVG ellipse element
stateEllipse :: StatePosition -> { cx :: Number, cy :: Number, rx :: Number, ry :: Number }
stateEllipse pos =
  { cx: pos.cx
  , cy: pos.cy
  , rx: pos.rx
  , ry: pos.ry
  }

-- | Get the inner ring for a final state (double circle effect)
-- | Returns slightly smaller ellipse attributes
stateFinalRing :: StatePosition -> Number -> { cx :: Number, cy :: Number, rx :: Number, ry :: Number }
stateFinalRing pos gap =
  { cx: pos.cx
  , cy: pos.cy
  , rx: max 0.0 (pos.rx - gap)
  , ry: max 0.0 (pos.ry - gap)
  }
