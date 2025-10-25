module PSD3.Internal.Generators.Line where

import PSD3.Internal.Scales.Linear (LinearScale)
import Effect (Effect)

-- Opaque type for D3 line generator
foreign import data LineGenerator :: Type

-- Create a line generator with x and y scale functions
foreign import createLineGenerator ::
  { xScale :: LinearScale
  , yScale :: LinearScale
  } -> Effect LineGenerator

-- Generate SVG path data from an array of data points
-- Each point should have x and y properties
foreign import generateLinePath :: forall a. LineGenerator -> Array a -> String
