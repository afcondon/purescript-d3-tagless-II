module D3.Viz.Parabola where

import PSD3.Internal.Attributes.Sugar

import Data.Int (toNumber)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemePairedN_)
import PSD3.Internal.Types (D3Selection_, Element(..), Index_, Selector, index_ToNumber)
import Prelude (bind, discard, negate, pure, ($), (*), (-), (/))

-- Snippet_Start
-- Name: ParabolaDatum_
type Model = Array Int  -- not strictly necessary in such a simple example, of course

-- | OLD APPROACH: Accessor record for working with Datum_ (not needed with clean lambdas!)
-- | With phantom types + simpleJoin, we can use typed lambdas directly.
-- | No accessor record or coercion needed!
-- Snippet_End

-- Snippet_Start
-- Name: ParabolaDraw
drawWithData :: forall m. SelectionM D3Selection_ m => Model -> Selector (D3Selection_ Int) -> m (D3Selection_ Int)
drawWithData circleData selector = do
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) (-100.0) 320.0 160.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []

  circles     <- simpleJoin circleGroup Circle circleData keyIsID_
  -- Clean typed lambdas (concrete Int type)!
  setAttributes (circles :: D3Selection_ Int)
    [ strokeColor \(d :: Int) -> d3SchemePairedN_ ((toNumber d) / 100.0)
    , strokeWidth 3.0
    , fill "none"
    , cx \(_ :: Int) (i :: Index_) -> (index_ToNumber i) * 20.0
    , cy \(d :: Int) -> 100.0 - (toNumber d) / 5.0
    , radius 10.0
    ]
  pure circles
-- Snippet_End
