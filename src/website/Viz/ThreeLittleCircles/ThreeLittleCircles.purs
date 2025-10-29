module D3.Viz.ThreeLittleCircles where

-- | Basic data binding demonstration
-- | Based on Mike Bostock's "Three Little Circles" (https://bost.ocks.org/mike/circles/)
-- | See Acknowledgements page for full credits

import PSD3.Internal.Attributes.Sugar

import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Selector)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import D3.Viz.ThreeLittleCircles.Unsafe (coerceIndex)
import Data.Int (toNumber)
import Prelude (bind, discard, negate, pure, (*), (+))

-- | Pretty much the most basic example imaginable, three ints represented by three circles
drawThreeCircles :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m D3Selection_
drawThreeCircles selector = do
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) 20.0 120.0 60.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []
  circles     <- simpleJoin circleGroup Circle [32, 57, 293] keyIsID_ 
  setAttributes circles [ fill "green"
                        , cx (\(d :: Datum_) i -> (toNumber (coerceIndex i)) * 30.0 + 10.0)
                        , cy 50.0
                        , radius 10.0 ]

  pure circles
