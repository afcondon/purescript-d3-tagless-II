module ParabolaTest.Unsafe where

import PSD3.Internal.Types (Datum_, Index_)
import Unsafe.Coerce (unsafeCoerce)

-- | Data type for this visualization
type ParabolaTestData =
  {     x :: Number
  ,     y :: Number
  }

-- | Coerce Datum_ to ParabolaTestData
-- This is safe because D3's data join ensures Datum_ contains
-- the data we originally passed to simpleJoin/updateJoin
coerceToParabolaTestData :: Datum_ -> ParabolaTestData
coerceToParabolaTestData = unsafeCoerce

-- | Coerce Index_ to Int (always provided by D3)
coerceIndex :: Index_ -> Int
coerceIndex = unsafeCoerce
