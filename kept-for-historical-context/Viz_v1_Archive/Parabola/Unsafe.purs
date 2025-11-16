module D3.Viz.Parabola.Unsafe where

import PSD3.Internal.Types (Datum_)
import Unsafe.Coerce (unsafeCoerce)

-- | Coerce Datum_ to Int
-- This is safe in the context of the SelectionM monad because D3's data join ensures Datum_ contains
-- the same data type we originally passed to simpleJoin
coerceDatumToInt :: Datum_ -> Int
coerceDatumToInt = unsafeCoerce
