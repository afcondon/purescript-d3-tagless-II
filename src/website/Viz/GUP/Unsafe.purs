module D3.Viz.GUP.Unsafe where

import PSD3.Internal.Types (Datum_, Index_)
import Unsafe.Coerce (unsafeCoerce)

-- | Coerce Datum_ to Char
-- This is safe because D3's data join ensures Datum_ contains
-- the data we originally passed to updateJoin
coerceDatumToChar :: Datum_ -> Char
coerceDatumToChar = unsafeCoerce

-- | Coerce Index_ to Number (always provided by D3)
coerceIndexToNumber :: Index_ -> Number
coerceIndexToNumber = unsafeCoerce

-- | For this very simple example, the data (Char) can be used directly as the key
coerceDatumToKey :: Datum_ -> Index_
coerceDatumToKey = unsafeCoerce
