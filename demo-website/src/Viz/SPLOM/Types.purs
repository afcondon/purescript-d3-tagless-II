-- | SPLOM Types
-- |
-- | Types for the Scatterplot Matrix (SPLOM) visualization.
module D3.Viz.SPLOM.Types
  ( Penguin
  , NumericDimension(..)
  , dimensionLabel
  , dimensionKey
  , allDimensions
  , getDimensionValue
  , speciesColor
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- | A single penguin record (parsed from CSV)
type Penguin =
  { species :: String
  , island :: String
  , billLength :: Maybe Number    -- bill_length_mm
  , billDepth :: Maybe Number     -- bill_depth_mm
  , flipperLength :: Maybe Number -- flipper_length_mm
  , bodyMass :: Maybe Number      -- body_mass_g
  , sex :: Maybe String
  , year :: Int
  }

-- | Numeric dimensions we can plot
data NumericDimension
  = BillLength
  | BillDepth
  | FlipperLength
  | BodyMass

derive instance Eq NumericDimension
derive instance Ord NumericDimension

instance Show NumericDimension where
  show = dimensionLabel

-- | Display label for dimension
dimensionLabel :: NumericDimension -> String
dimensionLabel = case _ of
  BillLength -> "Bill Length (mm)"
  BillDepth -> "Bill Depth (mm)"
  FlipperLength -> "Flipper Length (mm)"
  BodyMass -> "Body Mass (g)"

-- | Short key for dimension
dimensionKey :: NumericDimension -> String
dimensionKey = case _ of
  BillLength -> "billLength"
  BillDepth -> "billDepth"
  FlipperLength -> "flipperLength"
  BodyMass -> "bodyMass"

-- | All available dimensions for the SPLOM
allDimensions :: Array NumericDimension
allDimensions = [BillLength, BillDepth, FlipperLength, BodyMass]

-- | Get the value for a dimension from a penguin
getDimensionValue :: NumericDimension -> Penguin -> Maybe Number
getDimensionValue dim p = case dim of
  BillLength -> p.billLength
  BillDepth -> p.billDepth
  FlipperLength -> p.flipperLength
  BodyMass -> p.bodyMass

-- | Color by species (matching Observable example)
speciesColor :: String -> String
speciesColor = case _ of
  "Adelie" -> "#1f77b4"     -- Blue
  "Gentoo" -> "#ff7f0e"     -- Orange
  "Chinstrap" -> "#2ca02c"  -- Green
  _ -> "#999"
