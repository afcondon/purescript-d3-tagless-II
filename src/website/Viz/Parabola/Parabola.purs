module D3.Viz.Parabola where

import PSD3.Internal.Attributes.Sugar

import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemePairedN_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import D3.Viz.Parabola.Unsafe (coerceDatumToInt, coerceIndex)
import Data.Int (toNumber)
import Prelude (bind, discard, negate, pure, ($), (*), (-), (/))

-- Snippet_Start
-- Name: TLCDatum_
type Model = Array Int  -- not strictly necessary in such a simple example, of course

-- | Accessor record for working with bound data
datum_ ::
  { color :: Datum_ -> String
  , x :: Datum_ -> Index_ -> Number
  , y :: Datum_ -> Number
  , index :: Index_ -> Int
  }
datum_ =
  { x:     \_ i -> (toNumber $ coerceIndex i) * 20.0
  , y:     \d   -> 100.0 - (toNumber $ coerceDatumToInt d) / 5.0
  , color: \d   -> d3SchemePairedN_ ((toNumber $ coerceDatumToInt d) / 100.0)
  , index: coerceIndex
  }
-- Snippet_End

-- Snippet_Start
-- Name: TLCParabola
drawWithData :: forall m. SelectionM D3Selection_ m => Model -> Selector D3Selection_-> m D3Selection_
drawWithData circleData selector = do
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) (-100.0) 320.0 160.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []

  circles     <- simpleJoin circleGroup Circle circleData keyIsID_ 
  setAttributes circles [ strokeColor datum_.color
                        , strokeWidth 3.0
                        , fill "none"
                        , cx datum_.x
                        , cy datum_.y
                        , radius 10.0 ]
  pure circles
-- Snippet_End
