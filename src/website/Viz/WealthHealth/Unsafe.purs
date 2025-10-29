module D3.Viz.WealthHealth.Unsafe where

import Prelude

import PSD3.Internal.Types (Datum_, Index_)
import Unsafe.Coerce (unsafeCoerce)

-- | Accessor functions for safely extracting NationPoint data from Datum_
datum_ ::
  { name :: Datum_ -> String
  , income :: Datum_ -> Number
  , population :: Datum_ -> Number
  , lifeExpectancy :: Datum_ -> Number
  , regionColor :: Datum_ -> String
  , region :: Datum_ -> String
  , indexNum :: Index_ -> Number
  }
datum_ =
  { name: \d -> (unsafeCoerce d).name
  , income: \d -> (unsafeCoerce d).income
  , population: \d -> (unsafeCoerce d).population
  , lifeExpectancy: \d -> (unsafeCoerce d).lifeExpectancy
  , regionColor: \d -> (unsafeCoerce d).regionColor
  , region: \d -> (unsafeCoerce d).region
  , indexNum: \i -> unsafeCoerce i
  }

-- | Key function for data joins (use nation name as unique identifier)
coerceDatumToKey :: Datum_ -> Index_
coerceDatumToKey d = unsafeCoerce (datum_.name d)
