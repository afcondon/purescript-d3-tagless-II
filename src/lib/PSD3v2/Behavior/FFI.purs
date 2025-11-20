module PSD3v2.Behavior.FFI
  ( attachZoom_
  , attachSimpleDrag_
  , attachSimulationDrag_
  , attachClick_
  , attachClickWithDatum_
  , attachMouseEnter_
  , attachMouseLeave_
  , attachHighlight_
  , attachMouseMoveWithInfo_
  , attachMouseEnterWithInfo_
  , attachMouseLeaveWithInfo_
  , MouseEventInfoJS
  ) where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import PSD3.Internal.Types (D3Simulation_)
import Web.DOM.Element (Element)

-- | Attach zoom behavior to a DOM element
-- |
-- | Parameters:
-- | - element: The DOM element to attach zoom to (typically SVG)
-- | - scaleMin: Minimum zoom scale (e.g., 0.5 for 50%)
-- | - scaleMax: Maximum zoom scale (e.g., 4.0 for 400%)
-- | - targetSelector: CSS selector for the element to transform (e.g., ".zoom-group")
-- |
-- | Returns the element for chaining.
foreign import attachZoom_
  :: Element
  -> Number
  -> Number
  -> String
  -> Effect Element

-- | Attach simple drag behavior to a DOM element
-- |
-- | Enables dragging with transform translation.
-- | Returns the element for chaining.
foreign import attachSimpleDrag_
  :: Element
  -> Unit
  -> Effect Element

-- | Attach simulation-aware drag behavior to a DOM element
-- |
-- | Enables dragging with force simulation reheat.
-- | When dragging:
-- | - Dragstart: Sets fx/fy fixed positions, reheats simulation
-- | - Drag: Updates fx/fy to follow mouse
-- | - Dragend: Clears fx/fy, cools simulation
-- |
-- | Returns the element for chaining.
foreign import attachSimulationDrag_
  :: Element
  -> Nullable D3Simulation_
  -> String  -- Label for drag event namespace
  -> Effect Element

-- | Attach click handler without datum access
-- |
-- | Attaches a simple click handler that doesn't access the bound datum.
-- | Also sets cursor to pointer for clickable elements.
-- |
-- | Returns the element for chaining.
foreign import attachClick_
  :: Element
  -> Effect Unit
  -> Effect Element

-- | Attach click handler with datum access
-- |
-- | Attaches a click handler that receives the datum bound to the element.
-- | The datum is recovered from D3's __data__ property.
-- | Type safety is preserved through the Selection's phantom type.
-- | Also sets cursor to pointer for clickable elements.
-- |
-- | Returns the element for chaining.
foreign import attachClickWithDatum_
  :: forall datum
   . Element
  -> (datum -> Effect Unit)
  -> Effect Element

-- | Attach mouseenter handler with datum access
-- |
-- | Attaches a handler that fires when mouse enters the element.
-- | Does not bubble (unlike mouseover).
-- | The datum is recovered from D3's __data__ property.
-- |
-- | Returns the element for chaining.
foreign import attachMouseEnter_
  :: forall datum
   . Element
  -> (datum -> Effect Unit)
  -> Effect Element

-- | Attach mouseleave handler with datum access
-- |
-- | Attaches a handler that fires when mouse leaves the element.
-- | Does not bubble (unlike mouseout).
-- | The datum is recovered from D3's __data__ property.
-- |
-- | Returns the element for chaining.
foreign import attachMouseLeave_
  :: forall datum
   . Element
  -> (datum -> Effect Unit)
  -> Effect Element

-- | Attach hover highlight behavior
-- |
-- | Applies styles on mouseenter and resets on mouseleave.
-- | Also raises the element to front on hover.
-- |
-- | Parameters:
-- | - element: DOM element to attach to
-- | - enterStyles: Array of {attr, value} to apply on enter
-- | - leaveStyles: Array of {attr, value} to apply on leave
-- |
-- | Returns the element for chaining.
foreign import attachHighlight_
  :: Element
  -> Array { attr :: String, value :: String }
  -> Array { attr :: String, value :: String }
  -> Effect Element

-- | Mouse event info type (matches PureScript MouseEventInfo)
type MouseEventInfoJS =
  { clientX :: Number
  , clientY :: Number
  , pageX :: Number
  , pageY :: Number
  , offsetX :: Number
  , offsetY :: Number
  }

-- | Attach mousemove handler with event info
-- |
-- | Passes datum and mouse coordinates to the handler.
foreign import attachMouseMoveWithInfo_
  :: forall datum
   . Element
  -> (datum -> MouseEventInfoJS -> Effect Unit)
  -> Effect Element

-- | Attach mouseenter handler with event info
foreign import attachMouseEnterWithInfo_
  :: forall datum
   . Element
  -> (datum -> MouseEventInfoJS -> Effect Unit)
  -> Effect Element

-- | Attach mouseleave handler with event info
foreign import attachMouseLeaveWithInfo_
  :: forall datum
   . Element
  -> (datum -> MouseEventInfoJS -> Effect Unit)
  -> Effect Element
