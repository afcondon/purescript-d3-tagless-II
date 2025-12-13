-- | DataViz.Layout.StateMachine.Layout
-- |
-- | Pure layout algorithms for state machine visualization.
-- | Arranges states in a circular layout and computes curved
-- | arrow paths for transitions.
module DataViz.Layout.StateMachine.Layout
  ( layout
  , layoutWithConfig
  , LayoutConfig
  , defaultConfig
  , circularLayout
  , gridLayout
  ) where

import Prelude

import Data.Array (filter, length, mapWithIndex)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pi, cos, sin, sqrt, atan2)
import DataViz.Layout.StateMachine.Types (State, Transition, StateMachine, StatePosition, TransitionPath, LayoutState, LayoutTransition, StateMachineLayout)

-- | Configuration for state machine layout
type LayoutConfig =
  { stateRadiusX :: Number      -- Horizontal radius for state ellipse
  , stateRadiusY :: Number      -- Vertical radius for state ellipse
  , layoutRadius :: Number      -- Radius of circular arrangement
  , margin :: Number            -- Margin around the diagram
  , selfLoopRadius :: Number    -- Radius for self-loop curves
  , arrowOffset :: Number       -- Gap between arrow and state edge
  , initialArrowLength :: Number -- Length of arrow pointing to initial state
  }

-- | Default layout configuration
defaultConfig :: LayoutConfig
defaultConfig =
  { stateRadiusX: 40.0
  , stateRadiusY: 25.0
  , layoutRadius: 150.0
  , margin: 80.0
  , selfLoopRadius: 50.0
  , arrowOffset: 3.0
  , initialArrowLength: 40.0
  }

-- | Layout a state machine with default configuration using circular layout
layout :: forall extra. StateMachine extra -> StateMachineLayout extra
layout = layoutWithConfig defaultConfig circularLayout

-- | Layout with custom configuration and layout strategy
layoutWithConfig :: forall extra.
  LayoutConfig ->
  (LayoutConfig -> Array (State extra) -> Array (LayoutState extra)) ->
  StateMachine extra ->
  StateMachineLayout extra
layoutWithConfig config layoutFn machine =
  let
    -- Position states
    layoutStates = layoutFn config machine.states

    -- Compute transitions
    layoutTransitions = map (layoutTransition config layoutStates) machine.transitions

    -- Find initial state for the entry arrow
    initialArrow = computeInitialArrow config layoutStates

    -- Compute bounding box
    { width, height } = computeBounds config layoutStates
  in
    { states: layoutStates
    , transitions: layoutTransitions
    , width
    , height
    , initialArrow
    }

-- | Circular layout: arrange states in a circle
circularLayout :: forall extra. LayoutConfig -> Array (State extra) -> Array (LayoutState extra)
circularLayout config states =
  let
    n = length states
    angleStep = if n > 0 then 2.0 * pi / toNumber n else 0.0
    centerX = config.layoutRadius + config.margin
    centerY = config.layoutRadius + config.margin
  in
    mapWithIndex (positionState centerX centerY angleStep) states
  where
  positionState :: Number -> Number -> Number -> Int -> State extra -> LayoutState extra
  positionState cx cy step idx state =
    let
      -- Start from top (-pi/2) and go clockwise
      angle = -pi / 2.0 + toNumber idx * step
      position =
        { cx: cx + config.layoutRadius * cos angle
        , cy: cy + config.layoutRadius * sin angle
        , rx: config.stateRadiusX
        , ry: config.stateRadiusY
        }
    in
      { state, position }

-- | Grid layout: arrange states in rows
gridLayout :: forall extra. LayoutConfig -> Array (State extra) -> Array (LayoutState extra)
gridLayout config states =
  let
    n = length states
    cols = max 1 (ceil (sqrt (toNumber n)))
    cellWidth = 2.0 * config.stateRadiusX + 40.0
    cellHeight = 2.0 * config.stateRadiusY + 60.0
  in
    mapWithIndex (positionState cols cellWidth cellHeight) states
  where
  positionState :: Int -> Number -> Number -> Int -> State extra -> LayoutState extra
  positionState cols cellW cellH idx state =
    let
      col = idx `mod` cols
      row = idx / cols
      position =
        { cx: config.margin + toNumber col * cellW + cellW / 2.0
        , cy: config.margin + toNumber row * cellH + cellH / 2.0
        , rx: config.stateRadiusX
        , ry: config.stateRadiusY
        }
    in
      { state, position }

-- | Compute a transition path between two states
layoutTransition :: forall extra.
  LayoutConfig ->
  Array (LayoutState extra) ->
  Transition ->
  LayoutTransition
layoutTransition config states transition =
  let
    fromPos = findStatePosition states transition.from
    toPos = findStatePosition states transition.to
    -- Compute layout center as centroid of all states
    layoutCenter = computeLayoutCenter states
    path = case fromPos, toPos of
      Just from, Just to ->
        if transition.from == transition.to
          then selfLoopPath config layoutCenter from
          else arcPath config from to (countParallelTransitions states transition)
      _, _ -> defaultPath
  in
    { transition, path }

-- | Compute the centroid of all states (layout center)
computeLayoutCenter :: forall extra. Array (LayoutState extra) -> { cx :: Number, cy :: Number }
computeLayoutCenter states =
  let
    n = toNumber (length states)
    sumX = foldl (\acc s -> acc + s.position.cx) 0.0 states
    sumY = foldl (\acc s -> acc + s.position.cy) 0.0 states
  in
    if n > 0.0
      then { cx: sumX / n, cy: sumY / n }
      else { cx: 0.0, cy: 0.0 }

-- | Find the position of a state by id
findStatePosition :: forall extra. Array (LayoutState extra) -> String -> Maybe StatePosition
findStatePosition states id =
  case filter (\s -> s.state.id == id) states of
    [s] -> Just s.position
    _ -> Nothing

-- | Count parallel transitions (for offsetting multiple arrows between same states)
countParallelTransitions :: forall extra. Array (LayoutState extra) -> Transition -> Int
countParallelTransitions _ _ = 0  -- Simplified for now

-- | Compute arc path between two different states
arcPath :: LayoutConfig -> StatePosition -> StatePosition -> Int -> TransitionPath
arcPath config from to _offset =
  let
    -- Vector from source to target
    dx = to.cx - from.cx
    dy = to.cy - from.cy
    dist = sqrt (dx * dx + dy * dy)

    -- Normalized direction
    nx = if dist > 0.0 then dx / dist else 1.0
    ny = if dist > 0.0 then dy / dist else 0.0

    -- Start point: edge of source ellipse
    startX = from.cx + nx * (from.rx + config.arrowOffset)
    startY = from.cy + ny * (from.ry + config.arrowOffset)

    -- End point: edge of target ellipse
    endX = to.cx - nx * (to.rx + config.arrowOffset)
    endY = to.cy - ny * (to.ry + config.arrowOffset)

    -- Control point: perpendicular offset for curve
    -- More curvature for longer distances
    -- Positive perpendicular = curve bows to the right (clockwise)
    curveAmount = min 30.0 (dist * 0.15)
    perpX = ny * curveAmount   -- Flipped sign for clockwise curve
    perpY = -nx * curveAmount  -- Flipped sign for clockwise curve
    midX = (startX + endX) / 2.0
    midY = (startY + endY) / 2.0
    controlX = midX + perpX
    controlY = midY + perpY

    -- Label position: near the control point
    labelX = controlX
    labelY = controlY - 8.0

    -- Angle at endpoint for arrowhead
    -- Tangent of quadratic bezier at t=1 is (end - control)
    tangentX = endX - controlX
    tangentY = endY - controlY
    angle = atan2 tangentY tangentX
  in
    { startX, startY, controlX, controlY, endX, endY, labelX, labelY, angle, isSelfLoop: false }

-- | Compute self-loop path (arrow from state back to itself)
-- | Start and end points are on the state's edge, arc bulges outward
selfLoopPath :: LayoutConfig -> { cx :: Number, cy :: Number } -> StatePosition -> TransitionPath
selfLoopPath config layoutCenter pos =
  let
    loopRadius = config.selfLoopRadius * 0.5  -- Radius of the arc

    -- Direction from layout center to this state (outward direction)
    dx = pos.cx - layoutCenter.cx
    dy = pos.cy - layoutCenter.cy
    dist = sqrt (dx * dx + dy * dy)

    -- Outward angle (direction away from center)
    outwardAngle = if dist > 0.0 then atan2 dy dx else -pi / 2.0

    -- Start and end points on the STATE's edge, spread around the outward direction
    spread = 0.6  -- radians of spread on the state's edge
    startAngle = outwardAngle - spread
    endAngle = outwardAngle + spread

    -- Points where the loop meets the state's edge
    startX = pos.cx + pos.rx * cos startAngle
    startY = pos.cy + pos.ry * sin startAngle
    endX = pos.cx + pos.rx * cos endAngle
    endY = pos.cy + pos.ry * sin endAngle

    -- Control point: used for label positioning (apex of the arc)
    -- The arc bulges outward from the midpoint between start and end
    controlX = pos.cx + (pos.rx + loopRadius * 1.5) * cos outwardAngle
    controlY = pos.cy + (pos.ry + loopRadius * 1.5) * sin outwardAngle

    -- Label at the apex of the loop
    labelX = pos.cx + (pos.rx + loopRadius * 2.0 + 10.0) * cos outwardAngle
    labelY = pos.cy + (pos.ry + loopRadius * 2.0 + 10.0) * sin outwardAngle

    -- Arrowhead angle: tangent to arc at end point, pointing into the state
    -- For clockwise arc, tangent points perpendicular to the radius
    angle = endAngle - pi / 2.0
  in
    { startX, startY, controlX, controlY, endX, endY, labelX, labelY, angle, isSelfLoop: true }

-- | Default path for missing states
defaultPath :: TransitionPath
defaultPath =
  { startX: 0.0, startY: 0.0
  , controlX: 0.0, controlY: 0.0
  , endX: 0.0, endY: 0.0
  , labelX: 0.0, labelY: 0.0
  , angle: 0.0
  , isSelfLoop: false
  }

-- | Compute the initial state entry arrow
computeInitialArrow :: forall extra. LayoutConfig -> Array (LayoutState extra) -> { x :: Number, y :: Number, angle :: Number }
computeInitialArrow config states =
  case filter (\s -> s.state.isInitial) states of
    [initial] ->
      let
        -- Arrow comes from the left
        x = initial.position.cx - initial.position.rx - config.initialArrowLength
        y = initial.position.cy
        angle = 0.0  -- Points right
      in
        { x, y, angle }
    _ -> { x: 0.0, y: 0.0, angle: 0.0 }

-- | Compute bounding box for the diagram
computeBounds :: forall extra. LayoutConfig -> Array (LayoutState extra) -> { width :: Number, height :: Number }
computeBounds config states =
  let
    initial = { minX: 0.0, minY: 0.0, maxX: 0.0, maxY: 0.0 }
    bounds = foldl updateBounds initial states
    updateBounds acc s =
      { minX: min acc.minX (s.position.cx - s.position.rx)
      , minY: min acc.minY (s.position.cy - s.position.ry)
      , maxX: max acc.maxX (s.position.cx + s.position.rx)
      , maxY: max acc.maxY (s.position.cy + s.position.ry)
      }
  in
    { width: bounds.maxX + config.margin
    , height: bounds.maxY + config.margin
    }

-- | Ceiling function for integers
ceil :: Number -> Int
ceil n =
  let i = floor n
  in if toNumber i < n then i + 1 else i

-- | Floor function
floor :: Number -> Int
floor n =
  let i = truncate n
  in if toNumber i > n then i - 1 else i

-- | Truncate to integer
truncate :: Number -> Int
truncate n = if n >= 0.0 then truncatePos n else -(truncatePos (-n))
  where
  truncatePos :: Number -> Int
  truncatePos x = go 0 x
    where
    go acc remaining
      | remaining < 1.0 = acc
      | otherwise = go (acc + 1) (remaining - 1.0)
