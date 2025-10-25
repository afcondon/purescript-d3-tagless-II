module PSD3.Internal.Axes where

import Prelude

import PSD3.Internal.Types (D3Selection_)
import PSD3.Internal.Scales.Linear (LinearScale_)
import Effect (Effect)

-- Opaque type for D3 axis
foreign import data Axis_ :: Type

-- Create axis generators
foreign import axisBottom_ :: LinearScale_ -> Axis_
foreign import axisLeft_ :: LinearScale_ -> Axis_

-- Call an axis on a selection (renders the axis)
foreign import callAxis_ :: D3Selection_ -> Axis_ -> Effect Unit
