module PSD3v2.Behavior.FFI
  ( attachZoom_
  , attachSimpleDrag_
  , attachSimulationDrag_
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
