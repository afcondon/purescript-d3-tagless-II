module ParabolaTest.Draw where

import Prelude

import PSD3
import PSD3.Attributes as A
import PSD3.Internal.FFI (keyIsID_)
import ParabolaTest.Unsafe (coerceToParabolaTestData, coerceIndex)
import ParabolaTest.Model (ParabolaTestData)
import Data.Int (toNumber)
import Effect (Effect)

-- | Accessor record for working with bound data
datum_ ::
  { x :: Datum_ -> Number
  , y :: Datum_ -> Number
  , index :: Index_ -> Int
  }
datum_ =
  {     x: _.x <<< coerceToParabolaTestData
  ,     y: _.y <<< coerceToParabolaTestData
  , index: coerceIndex
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
    [ A.cx (\(_ :: Datum_) (i :: Index_) -> toNumber (datum_.index i) * 50.0)
    , A.cy 300.0
    , A.radius 20.0
    , A.fill "steelblue"
    ]

  pure unit

-- | Entry point
run :: Array ParabolaTestData -> Effect Unit
run dataPoints = eval_D3M $ draw dataPoints "#chart"
