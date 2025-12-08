-- | Pie Layout - Pure PureScript Implementation
-- |
-- | Equivalent to d3.layout.pie() - converts values to arc angles.
-- | This is a pure function with no FFI dependency.
module D3.Viz.Simpsons.Pie
  ( PieSlice
  , pie
  , pieWithSort
  ) where

import Prelude

import Data.Array (foldl, sortBy)
import Data.Maybe (Maybe(..))
import Data.Number (pi)

-- | A slice of a pie chart with computed angles
-- | Compatible with D3's arc generator which expects startAngle/endAngle
type PieSlice a =
  { datum :: a           -- Original data
  , value :: Number       -- The numeric value
  , startAngle :: Number  -- Start angle in radians (0 = 12 o'clock)
  , endAngle :: Number    -- End angle in radians
  , padAngle :: Number    -- Padding between slices (usually 0)
  }

-- | Convert data to pie slices without sorting (like d3.pie().sort(null))
-- |
-- | Example:
-- | ```purescript
-- | pie _.percent
-- |   [ { label: "accepted", percent: 44.0 }
-- |   , { label: "rejected", percent: 56.0 }
-- |   ]
-- | ```
pie :: forall a. (a -> Number) -> Array a -> Array (PieSlice a)
pie getValue items = pieWithSort getValue Nothing items

-- | Convert data to pie slices with optional sorting
-- |
-- | The sort function compares values - use `Just compare` for ascending,
-- | `Just (flip compare)` for descending, or `Nothing` to preserve order.
pieWithSort
  :: forall a
   . (a -> Number)
  -> Maybe (Number -> Number -> Ordering)
  -> Array a
  -> Array (PieSlice a)
pieWithSort getValue sortFn items =
  let
    -- Calculate total for proportions
    total = foldl (\acc item -> acc + getValue item) 0.0 items

    -- Sort if requested
    sorted = case sortFn of
      Nothing -> items
      Just compareFn -> sortBy (\a b -> compareFn (getValue a) (getValue b)) items

    -- Convert to slices with cumulative angles
    tau = 2.0 * pi  -- Full circle

    -- Fold to build slices with running start angle
    result = foldl accumulate { angle: 0.0, slices: [] } sorted

    accumulate acc item =
      let
        value = getValue item
        proportion = if total > 0.0 then value / total else 0.0
        arcAngle = proportion * tau
        slice =
          { datum: item
          , value: value
          , startAngle: acc.angle
          , endAngle: acc.angle + arcAngle
          , padAngle: 0.0
          }
      in
        { angle: acc.angle + arcAngle
        , slices: acc.slices <> [slice]
        }
  in
    result.slices
