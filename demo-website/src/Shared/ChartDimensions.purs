module PSD3.Shared.ChartDimensions where

import Prelude

-- | Standard chart dimensions with margins for axes and labels
type ChartDimensions = {
    width :: Number
  , height :: Number
  , margin :: { top :: Number, right :: Number, bottom :: Number, left :: Number }
}

-- | Default dimensions suitable for most charts (800x400 with standard margins)
defaultDimensions :: ChartDimensions
defaultDimensions = {
    width: 800.0
  , height: 400.0
  , margin: { top: 20.0, right: 30.0, bottom: 30.0, left: 40.0 }
}

-- | Calculate inner width (accounting for left and right margins)
innerWidth :: ChartDimensions -> Number
innerWidth dims = dims.width - dims.margin.left - dims.margin.right

-- | Calculate inner height (accounting for top and bottom margins)
innerHeight :: ChartDimensions -> Number
innerHeight dims = dims.height - dims.margin.top - dims.margin.bottom
