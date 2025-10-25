module PSD3.Internal.Scales.Linear where

import Effect (Effect)

-- Opaque type for D3 linear scale
foreign import data LinearScale_ :: Type

-- Create a linear scale with domain and range
foreign import createLinearScale_ ::
  { domain :: Array Number, range :: Array Number } -> Effect LinearScale_

-- Apply a scale to a value
foreign import applyScale_ :: LinearScale_ -> Number -> Number

-- Get ticks from a scale for axis rendering
foreign import getTicks_ :: LinearScale_ -> Int -> Effect (Array Number)
