-- | Tangle.Halogen - Halogen rendering for Tangle documents
-- |
-- | Provides a reusable component that renders TangleDoc with interactive controls.
-- | Supports:
-- | - Toggle: click to flip boolean
-- | - Cycle: click to advance through options
-- | - Adjust: click to step, wheel to adjust smoothly
-- | - Display: non-interactive highlighted value
module Tangle.Halogen
  ( -- * Rendering functions
    renderDoc
  , renderDocWithAction

  -- * Types for customization
  , ControlAction(..)
  , ControlStyle
  , defaultStyle
  ) where

import Prelude

import Data.Int as Int
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tangle.Core (TangleDoc(..), TangleSegment(..), Control(..), cycleNext)
import Web.UIEvent.MouseEvent (MouseEvent, shiftKey, clientX)
import Web.UIEvent.WheelEvent (WheelEvent, deltaY)

-- =============================================================================
-- Types
-- =============================================================================

-- | Action types that the renderer can emit
-- |
-- | - ToggleClicked: Boolean toggled (id, new value)
-- | - CycleClicked: Advance to next option (id, new value)
-- | - AdjustChanged: Numeric value changed (id, new value)
-- | - AdjustDragStart: Drag started (id, current value, start X position, step, min, max)
-- | - ActionClicked: One-way action triggered (id, action value)
data ControlAction
  = ToggleClicked String Boolean      -- id, new value
  | CycleClicked String String        -- id, new value
  | AdjustChanged String Number       -- id, new value
  | AdjustDragStart                   -- id, startValue, startX, step, min, max
      { id :: String
      , startValue :: Number
      , startX :: Number
      , step :: Number
      , min :: Number
      , max :: Number
      }
  | ActionClicked String String       -- id, action value

-- | Style configuration for controls
type ControlStyle =
  { controlClass :: String          -- Class for interactive controls
  , displayClass :: String          -- Class for display-only values
  , toggleActiveClass :: String     -- Additional class when toggle is true
  , toggleInactiveClass :: String   -- Additional class when toggle is false
  , adjustClass :: String           -- Additional class for adjustable values
  }

-- | Default styling (matches TangleJS conventions)
defaultStyle :: ControlStyle
defaultStyle =
  { controlClass: "tangle-control"
  , displayClass: "tangle-value"
  , toggleActiveClass: "tangle-active"
  , toggleInactiveClass: "tangle-inactive"
  , adjustClass: "tangle-adjust"
  }

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render a TangleDoc with an action handler
-- |
-- | The action handler receives ControlActions and should update state accordingly.
renderDocWithAction
  :: forall w action
   . ControlStyle
  -> (ControlAction -> action)  -- How to wrap control actions
  -> TangleDoc
  -> HH.HTML w action
renderDocWithAction style toAction (TangleDoc segments) =
  HH.span_ (map (renderSegment style toAction) segments)

-- | Render with default styling
renderDoc
  :: forall w action
   . (ControlAction -> action)
  -> TangleDoc
  -> HH.HTML w action
renderDoc = renderDocWithAction defaultStyle

-- | Render a single segment
renderSegment
  :: forall w action
   . ControlStyle
  -> (ControlAction -> action)
  -> TangleSegment
  -> HH.HTML w action
renderSegment _style _toAction (TextSegment s) =
  HH.text s
renderSegment style toAction (ControlSegment ctrl) =
  renderControl style toAction ctrl

-- | Render a control based on its type
renderControl
  :: forall w action
   . ControlStyle
  -> (ControlAction -> action)
  -> Control
  -> HH.HTML w action

-- Toggle: click to flip
renderControl style toAction (Toggle { id, current, trueLabel, falseLabel }) =
  let
    displayText = if current then trueLabel else falseLabel
    newValue = not current
    stateClass = if current then style.toggleActiveClass else style.toggleInactiveClass
    classes = [ HH.ClassName style.controlClass, HH.ClassName stateClass ]
  in
    HH.span
      [ HP.classes classes
      , HE.onClick \_ -> toAction (ToggleClicked id newValue)
      ]
      [ HH.text displayText ]

-- Cycle: click to advance to next option
renderControl style toAction (Cycle { id, current, options }) =
  let
    nextValue = cycleNext current options
  in
    HH.span
      [ HP.class_ (HH.ClassName style.controlClass)
      , HE.onClick \_ -> toAction (CycleClicked id nextValue)
      ]
      [ HH.text current ]

-- Adjust: drag to change, click to step, wheel to adjust
-- Drag left/right = continuous adjust (primary interaction)
-- Click = step up, Shift+Click = step down, Wheel = continuous adjust
renderControl style toAction (Adjust { id, current, min: minVal, max: maxVal, step, format }) =
  let
    clamp v = max minVal (min maxVal v)
    stepUp = clamp (current + step)
    stepDown = clamp (current - step)

    handleClick :: MouseEvent -> action
    handleClick evt =
      let newVal = if shiftKey evt then stepDown else stepUp
      in toAction (AdjustChanged id newVal)

    handleWheel :: WheelEvent -> action
    handleWheel evt =
      let delta = deltaY evt
          -- Wheel down (positive delta) = decrease, wheel up = increase
          newVal = clamp (current - delta * step * 0.01)
      in toAction (AdjustChanged id newVal)

    handleMouseDown :: MouseEvent -> action
    handleMouseDown evt =
      let startX = Int.toNumber (clientX evt)
      in toAction (AdjustDragStart { id, startValue: current, startX, step, min: minVal, max: maxVal })
  in
    HH.span
      [ HP.classes [ HH.ClassName style.controlClass, HH.ClassName style.adjustClass ]
      , HP.attr (HH.AttrName "title") "Drag to adjust, click to step, or scroll"
      , HE.onMouseDown handleMouseDown
      , HE.onClick handleClick
      , HE.onWheel handleWheel
      ]
      [ HH.text (format current) ]

-- Display: non-interactive
renderControl style _toAction (Display { value }) =
  HH.span
    [ HP.class_ (HH.ClassName style.displayClass) ]
    [ HH.text value ]

-- Action: one-way trigger (like a link)
renderControl style toAction (Action { id, label, actionValue }) =
  HH.span
    [ HP.class_ (HH.ClassName style.controlClass)
    , HE.onClick \_ -> toAction (ActionClicked id actionValue)
    ]
    [ HH.text label ]
