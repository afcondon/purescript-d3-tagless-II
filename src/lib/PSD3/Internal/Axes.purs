module D3.Axes where

import Prelude

import D3.Data.Types (D3Selection_)
import D3.Scales.Linear (LinearScale)
import Effect (Effect)

-- Opaque type for D3 axis
foreign import data Axis :: Type

-- Create axis generators
foreign import axisBottom :: LinearScale -> Axis
foreign import axisLeft :: LinearScale -> Axis

-- Call an axis on a selection (renders the axis)
foreign import callAxis :: D3Selection_ -> Axis -> Effect Unit
