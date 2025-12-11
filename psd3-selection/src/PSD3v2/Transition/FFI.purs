module PSD3v2.Transition.FFI
  ( D3Transition
  , createTransition_
  , transitionSetAttribute_
  , transitionRemove_
  , maybeMillisecondsToNullable
  , maybeEasingToNullable
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import PSD3v2.Transition.Types (Easing)
import Web.DOM.Element (Element)

-- | Opaque type representing a D3 transition
-- | This is returned by D3's .transition() method
foreign import data D3Transition :: Type

-- | Create a D3 transition from an element
-- |
-- | Calls D3's selection.transition() and configures duration, delay, and easing.
-- | Returns a D3 transition object that can be used to set animated attributes.
foreign import createTransition_
  :: Number                -- duration in milliseconds
  -> Nullable Number       -- optional delay in milliseconds
  -> Nullable String       -- optional easing function name (from Easing's Show instance)
  -> Element               -- element to transition
  -> Effect D3Transition   -- resulting transition

-- | Set an attribute on a D3 transition
-- |
-- | This is similar to Element.setAttribute but works on transitions,
-- | causing the attribute to animate to the target value.
foreign import transitionSetAttribute_
  :: String         -- attribute name
  -> String         -- attribute value
  -> D3Transition   -- transition to modify
  -> Effect Unit

-- | Remove elements after transition completes
-- |
-- | Calls D3's transition.remove() which schedules the elements
-- | to be removed from the DOM after the transition finishes.
foreign import transitionRemove_
  :: D3Transition   -- transition
  -> Effect Unit

-- | Convert Maybe Milliseconds to Nullable Number for FFI
maybeMillisecondsToNullable :: Maybe Milliseconds -> Nullable Number
maybeMillisecondsToNullable = toNullable <<< map unwrap

-- | Convert Maybe Easing to Nullable String for FFI
-- | Uses the Show instance which produces D3-compatible easing names
-- | (e.g., "linear", "cubicInOut", "elasticOut")
maybeEasingToNullable :: Maybe Easing -> Nullable String
maybeEasingToNullable = toNullable <<< map show
