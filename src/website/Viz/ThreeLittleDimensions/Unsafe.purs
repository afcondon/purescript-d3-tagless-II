module D3.Viz.ThreeLittleDimensions.Unsafe where

import PSD3.Internal.Types (Datum_, Index_)
import Unsafe.Coerce (unsafeCoerce)

-- | Coerce Datum_ to Array Int
-- This is safe because D3's data join ensures Datum_ contains
-- the data we originally passed to simpleJoin
coerceDatumToArray :: Datum_ -> Array Int
coerceDatumToArray = unsafeCoerce

-- | Coerce Datum_ to Int (for nested data)
coerceDatumToInt :: Datum_ -> Int
coerceDatumToInt = unsafeCoerce

-- | Coerce Index_ to Int (always provided by D3)
coerceIndex :: Index_ -> Int
coerceIndex = unsafeCoerce
