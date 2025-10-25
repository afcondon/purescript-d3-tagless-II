module PSD3.Internal.Axes where

import Prelude

import PSD3.Internal.Types (D3Selection_)
import PSD3.Internal.Scales.Linear (LinearScale)
import Effect (Effect)

-- Opaque type for D3 axis
foreign import data Axis :: Type

-- Create axis generators
foreign import axisBottom :: LinearScale -> Axis
foreign import axisLeft :: LinearScale -> Axis

-- Call an axis on a selection (renders the axis)
foreign import callAxis :: D3Selection_ -> Axis -> Effect Unit
