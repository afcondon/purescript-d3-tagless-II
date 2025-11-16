module PSD3.Internal.Generators.Line where

import PSD3.Internal.Scales.Linear (LinearScale_)
import Effect (Effect)

-- Opaque type for D3 line generator
foreign import data LineGenerator_ :: Type

-- Create a line generator with x and y scale functions
foreign import createLineGenerator_ ::
  { xScale :: LinearScale_
  , yScale :: LinearScale_
  } -> Effect LineGenerator_

-- Generate SVG path data from an array of data points
-- Each point should have x and y properties
foreign import generateLinePath_ :: forall a. LineGenerator_ -> Array a -> String
