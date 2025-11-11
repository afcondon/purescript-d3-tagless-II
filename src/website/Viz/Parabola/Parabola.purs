module D3.Viz.Parabola where

import PSD3.Internal.Attributes.Sugar

import D3.Viz.Parabola.Unsafe (coerceDatumToInt)
import Data.Int (toNumber)
import PSD3.Attributes (DatumFn(..), DatumFnI(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemePairedN_)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector, index_ToNumber)
import Prelude (bind, discard, negate, pure, ($), (*), (-), (/))

-- Snippet_Start
-- Name: ParabolaDatum_
type Model = Array Int  -- not strictly necessary in such a simple example, of course

-- | Accessor record for working with bound data
datum_ ::
  { color :: Datum_ -> String
  , x :: Datum_ -> Index_ -> Number
  , y :: Datum_ -> Number
  }
datum_ =
  { x:     \_ i -> (index_ToNumber i) * 20.0
  , y:     \d   -> 100.0 - (toNumber $ coerceDatumToInt d) / 5.0
  , color: \d   -> d3SchemePairedN_ ((toNumber $ coerceDatumToInt d) / 100.0)
  }
-- Snippet_End

-- Snippet_Start
-- Name: ParabolaDraw
drawWithData :: forall m. SelectionM D3Selection_ m => Model -> Selector (D3Selection_ Int) -> m (D3Selection_ Int)
drawWithData circleData selector = do
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) (-100.0) 320.0 160.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []

  circles     <- simpleJoin circleGroup Circle circleData keyIsID_
  -- TYPE INFERENCE TEST: Can we use typed lambdas now?
  setAttributes circles [ strokeColor (DatumFn \d -> d3SchemePairedN_ ((toNumber $ coerceDatumToInt d) / 100.0))
                        , strokeWidth 3.0
                        , fill "none"
                        , cx (DatumFnI \_ i -> (index_ToNumber i) * 20.0)
                        , cy (DatumFn \d -> 100.0 - (toNumber $ coerceDatumToInt d) / 5.0)
                        , radius 10.0 ]
  pure circles
-- Snippet_End
