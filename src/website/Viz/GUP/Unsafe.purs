module D3.Viz.GUP.Unsafe where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import PSD3.Internal.Types (Index_)
import Data.String.CodeUnits (singleton)

-- | Convert a Char to a String key for data joins
charToKey :: Char -> String
charToKey = singleton

-- | Unsafely coerce Index_ to Number for positioning calculations
coerceIndexToNumber :: Index_ -> Number
coerceIndexToNumber = unsafeCoerce
