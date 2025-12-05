-- | Tangle.Halogen - Halogen rendering for Tangle documents
-- |
-- | Provides a reusable component that renders TangleDoc with interactive controls.
-- | Supports:
-- | - Toggle: click to flip boolean
-- | - Cycle: click to advance through options
-- | - Adjust: drag left/right to change numeric value (future)
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

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tangle.Core (TangleDoc(..), TangleSegment(..), Control(..), cycleNext)

-- =============================================================================
-- Types
-- =============================================================================

-- | Action types that the renderer can emit
-- |
-- | - ToggleClicked: Boolean toggled (id, new value)
-- | - CycleClicked: Advance to next option (id, new value)
-- | - AdjustChanged: Numeric value changed via drag (id, new value)
data ControlAction
  = ToggleClicked String Boolean      -- id, new value
  | CycleClicked String String        -- id, new value
  | AdjustChanged String Number       -- id, new value

-- | Style configuration for controls
type ControlStyle =
  { controlClass :: String          -- Class for interactive controls
  , displayClass :: String          -- Class for display-only values
  , toggleActiveClass :: String     -- Additional class when toggle is true
  , hoverClass :: String            -- Class for hover state (via CSS)
  }

-- | Default styling (matches TangleJS conventions)
defaultStyle :: ControlStyle
defaultStyle =
  { controlClass: "tangle-control"
  , displayClass: "tangle-value"
  , toggleActiveClass: "tangle-active"
  , hoverClass: "tangle-hover"
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
    classes = [ HH.ClassName style.controlClass ]
           <> if current then [ HH.ClassName style.toggleActiveClass ] else []
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

-- Adjust: drag to change (TODO: implement drag behavior)
-- For now, just display the value with special styling
renderControl style _toAction (Adjust { id: _, current, format }) =
  HH.span
    [ HP.class_ (HH.ClassName style.controlClass)
    , HP.attr (HH.AttrName "data-adjustable") "true"
    -- TODO: Add drag event handlers
    ]
    [ HH.text (format current) ]

-- Display: non-interactive
renderControl style _toAction (Display { value }) =
  HH.span
    [ HP.class_ (HH.ClassName style.displayClass) ]
    [ HH.text value ]
