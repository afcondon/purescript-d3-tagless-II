module D3.Viz.GUP.Unsafe where

import PSD3.Internal.Types (Index_)
import Unsafe.Coerce (unsafeCoerce)
import Data.String.CodeUnits (singleton)

-- | Coerce Index_ to Number (for positioning)
coerceIndexToNumber :: Index_ -> Number
coerceIndexToNumber = unsafeCoerce

-- | For this simple example, convert Char to String for the key
-- Just returns String - no Index_ coercion needed!
charToKey :: Char -> String
charToKey = singleton
