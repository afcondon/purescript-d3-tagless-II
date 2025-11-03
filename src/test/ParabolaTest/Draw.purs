module ParabolaTest.Draw where

import Prelude

import Effect (Effect)
import PSD3 (class SelectionM, D3Selection_, Datum_, Element(..), Index_, Selector, appendTo, attach, eval_D3M, setAttributes, simpleJoin)
import PSD3.Attributes as A
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Types (index_ToNumber)
import ParabolaTest.Model (ParabolaTestData)
import ParabolaTest.Unsafe (coerceToParabolaTestData)

-- | Accessor record for working with bound data
datum_ ::
  { x :: Datum_ -> Number
  , y :: Datum_ -> Number
  }
datum_ =
  { x: _.x <<< coerceToParabolaTestData
  , y: _.y <<< coerceToParabolaTestData
  }

-- | Main drawing function
draw :: forall m. SelectionM D3Selection_ m => Array ParabolaTestData -> Selector D3Selection_ -> m Unit
draw dataPoints selector = do
  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg
    [ A.width 800.0
    , A.height 600.0
    , A.viewBox 0.0 0.0 800.0 600.0
    ]

  -- Example: Simple data join
  circles <- simpleJoin svg Circle dataPoints keyIsID_
  setAttributes circles
    [ A.cx (\(_ :: Datum_) (i :: Index_) -> (index_ToNumber i) * 50.0)
    , A.cy 300.0
    , A.radius 20.0
    , A.fill "steelblue"
    ]

  pure unit

-- | Entry point
run :: Array ParabolaTestData -> Effect Unit
run dataPoints = eval_D3M $ draw dataPoints "#chart"
