-- | SPLOM Rendering FFI
-- |
-- | Foreign interface for the D3-based SPLOM visualization.
module D3.Viz.SPLOM.Render
  ( renderSPLOM
  , clearSPLOMBrush
  , getSelectedCount
  , getTotalCount
  , SPLOMHandle
  , DimensionSpec
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import D3.Viz.SPLOM.Types (Penguin, NumericDimension, dimensionKey, dimensionLabel)

-- | Dimension specification for FFI
type DimensionSpec =
  { key :: String
  , label :: String
  }

-- | Opaque handle for SPLOM interaction
foreign import data SPLOMHandle :: Type

-- | Convert dimension to spec for FFI
toDimensionSpec :: NumericDimension -> DimensionSpec
toDimensionSpec dim =
  { key: dimensionKey dim
  , label: dimensionLabel dim
  }

-- | Render the SPLOM visualization
-- |
-- | Returns a handle for interaction (clearing brush, getting counts).
renderSPLOM
  :: String              -- ^ Container selector
  -> Array Penguin       -- ^ Data
  -> Array NumericDimension  -- ^ Dimensions to plot
  -> Effect SPLOMHandle
renderSPLOM selector data_ dimensions =
  renderSPLOM_ selector (toFFIPenguins data_) (map toDimensionSpec dimensions)

-- | Convert penguins to FFI format (with nullable fields as JS nulls)
foreign import renderSPLOM_
  :: String
  -> Array FFIPenguin
  -> Array DimensionSpec
  -> Effect SPLOMHandle

-- | Clear the brush selection
foreign import clearSPLOMBrush_ :: SPLOMHandle -> Effect Unit

-- | Get count of selected points
foreign import getSelectedCount_ :: SPLOMHandle -> Effect Int

-- | Get total point count
foreign import getTotalCount_ :: SPLOMHandle -> Effect Int

-- | Clear brush
clearSPLOMBrush :: SPLOMHandle -> Effect Unit
clearSPLOMBrush = clearSPLOMBrush_

-- | Get selected count
getSelectedCount :: SPLOMHandle -> Effect Int
getSelectedCount = getSelectedCount_

-- | Get total count
getTotalCount :: SPLOMHandle -> Effect Int
getTotalCount = getTotalCount_

-- | FFI-friendly penguin format (nullables become JS nulls)
type FFIPenguin =
  { species :: String
  , island :: String
  , billLength :: Nullable Number
  , billDepth :: Nullable Number
  , flipperLength :: Nullable Number
  , bodyMass :: Nullable Number
  , sex :: Nullable String
  , year :: Int
  }

-- | Convert penguin to FFI format
toFFIPenguin :: Penguin -> FFIPenguin
toFFIPenguin p =
  { species: p.species
  , island: p.island
  , billLength: toNullable p.billLength
  , billDepth: toNullable p.billDepth
  , flipperLength: toNullable p.flipperLength
  , bodyMass: toNullable p.bodyMass
  , sex: toNullable p.sex
  , year: p.year
  }

toFFIPenguins :: Array Penguin -> Array FFIPenguin
toFFIPenguins = map toFFIPenguin
