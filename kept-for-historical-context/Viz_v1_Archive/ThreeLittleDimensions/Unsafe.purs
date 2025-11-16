module D3.Viz.ThreeLittleDimensions.Unsafe where

import PSD3.Internal.Types (Datum_)
import Data.Set (Set)
import Unsafe.Coerce (unsafeCoerce)

-- | Coerce Datum_ to Array Int
-- This is safe because D3's data join ensures Datum_ contains
-- the data we originally passed to simpleJoin
coerceDatumToArray :: Datum_ -> Array Int
coerceDatumToArray = unsafeCoerce

-- | Coerce Datum_ to Int (for nested data)
coerceDatumToInt :: Datum_ -> Int
coerceDatumToInt = unsafeCoerce

-- | Coerce Datum_ to a Set of Strings
-- Used when cells contain Sets (unordered, unique collections)
coerceDatumToSet :: forall a. Datum_ -> Set a
coerceDatumToSet = unsafeCoerce

-- | Coerce Datum_ to String (for text content)
coerceDatumToString :: Datum_ -> String
coerceDatumToString = unsafeCoerce
