module D3.Scales.Linear where

import Effect (Effect)

-- Opaque type for D3 linear scale
foreign import data LinearScale :: Type

-- Create a linear scale with domain and range
foreign import createLinearScale ::
  { domain :: Array Number, range :: Array Number } -> Effect LinearScale

-- Apply a scale to a value
foreign import applyScale :: LinearScale -> Number -> Number

-- Get ticks from a scale for axis rendering
foreign import getTicks :: LinearScale -> Int -> Effect (Array Number)
