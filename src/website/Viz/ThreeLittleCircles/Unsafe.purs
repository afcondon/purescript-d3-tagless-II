module D3.Viz.ThreeLittleCircles.Unsafe where

import PSD3.Internal.Types (Datum_)
import Unsafe.Coerce (unsafeCoerce)

-- | Coerce Datum_ to Int
-- This is safe because D3's data join ensures Datum_ contains
-- the data we originally passed to simpleJoin
coerceDatumToInt :: Datum_ -> Int
coerceDatumToInt = unsafeCoerce
