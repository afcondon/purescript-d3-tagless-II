module PSD3v2.Behavior.FFI
  ( attachZoom_
  , attachDrag_
  ) where

import Prelude

import Effect (Effect)
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

-- | Attach drag behavior to a DOM element
-- |
-- | Parameters:
-- | - element: The DOM element to attach drag to
-- | - unit: Unit placeholder for curried application
-- |
-- | Returns the element for chaining.
foreign import attachDrag_
  :: Element
  -> Unit
  -> Effect Element
