module PSD3v2.Transition.FFI
  ( D3Transition
  , createTransition_
  , transitionSetAttribute_
  , maybeMillisecondsToNullable
  , maybeEasingToNullable
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds(..))
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
  -> Nullable String       -- optional easing function name
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

-- Helper to convert Maybe Milliseconds to Nullable Number
maybeMillisecondsToNullable :: Maybe Milliseconds -> Nullable Number
maybeMillisecondsToNullable Nothing = toNullable Nothing
maybeMillisecondsToNullable (Just (Milliseconds ms)) = toNullable (Just ms)

-- Helper to convert Maybe Easing to Nullable String
maybeEasingToNullable :: Maybe Easing -> Nullable String
maybeEasingToNullable Nothing = toNullable Nothing
maybeEasingToNullable (Just easing) = toNullable (Just (easingToD3Name easing))

-- Convert our Easing type to D3 easing function names
easingToD3Name :: Easing -> String
easingToD3Name easing = case easing of
  -- Map to d3-ease function names
  -- See: https://d3js.org/d3-ease
  _ -> show easing  -- For now, rely on Show instance matching D3 names
  -- TODO: Verify these match D3's actual function names
