module PSD3.Internal.Types
  ( -- Datum and Index types
    Datum_
  , Index_
  , index_ToInt
  , index_ToNumber
  , intToIndex_
  -- D3 opaque types
  , D3Selection_
  , D3Simulation_
  -- Type aliases
  , Selector
  , PointXY
  ) where

import Data.Int (toNumber)
import Prelude (class Eq, class Ord, compare, eq, ($))
import Unsafe.Coerce (unsafeCoerce)

-- | Opaque type representing data bound to D3 elements
-- | Used at FFI boundary when the actual data type is erased
foreign import data Datum_ :: Type

-- | Opaque type representing array indices in D3 callbacks
-- | Int under the hood, but kept opaque to match D3's conventions
foreign import data Index_ :: Type

-- Eq and Ord instances to support data joins with key functions
instance eqIndex_ :: Eq Index_ where
  eq a b = eq (index_ToInt a) (index_ToInt b)

instance ordIndex_ :: Ord Index_ where
  compare a b = compare (index_ToInt a) (index_ToInt b)

-- | Convert Index_ to Int
index_ToInt :: Index_ -> Int
index_ToInt = unsafeCoerce

-- | Convert Index_ to Number (useful for calculations)
index_ToNumber :: Index_ -> Number
index_ToNumber i = toNumber $ index_ToInt i

-- | Convert Int to Index_
intToIndex_ :: Int -> Index_
intToIndex_ = unsafeCoerce

-- | Opaque type for D3 selections (v1 style, still used by FFI)
foreign import data D3Selection_ :: Type -> Type

-- | Opaque type for D3 simulations
foreign import data D3Simulation_ :: Type

-- | CSS selector type - phantom typed but just String underneath
type Selector :: forall k. k -> Type
type Selector selection = String

-- | Simple 2D point record
type PointXY = { x :: Number, y :: Number }
