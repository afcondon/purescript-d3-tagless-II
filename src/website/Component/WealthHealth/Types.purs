module Component.WealthHealth.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | Geographic regions for color coding nations
data Region
  = EastAsiaAndPacific
  | Europe
  | LatinAmericaAndCaribbean
  | MiddleEastAndNorthAfrica
  | SouthAsia
  | SubSaharanAfrica
  | NorthAmerica

derive instance eqRegion :: Eq Region
derive instance ordRegion :: Ord Region
derive instance genericRegion :: Generic Region _

instance showRegion :: Show Region where
  show = genericShow

-- | Display name for a region
regionName :: Region -> String
regionName = case _ of
  EastAsiaAndPacific -> "East Asia & Pacific"
  Europe -> "Europe & Central Asia"
  LatinAmericaAndCaribbean -> "Latin America & Caribbean"
  MiddleEastAndNorthAfrica -> "Middle East & North Africa"
  SouthAsia -> "South Asia"
  SubSaharanAfrica -> "Sub-Saharan Africa"
  NorthAmerica -> "North America"

-- | Color for each region (matching D3 categorical schemes)
regionColor :: Region -> String
regionColor = case _ of
  EastAsiaAndPacific -> "#e41a1c"
  Europe -> "#377eb8"
  LatinAmericaAndCaribbean -> "#4daf4a"
  MiddleEastAndNorthAfrica -> "#984ea3"
  SouthAsia -> "#ff7f00"
  SubSaharanAfrica -> "#ffff33"
  NorthAmerica -> "#a65628"

-- | Raw nation data as loaded from nations.json
-- | Each metric is stored as an array of [year, value] pairs
type NationData =
  { name :: String
  , region :: Region
  , income :: Array (Array Number)      -- [year, income per capita]
  , population :: Array (Array Number)  -- [year, population]
  , lifeExpectancy :: Array (Array Number) -- [year, life expectancy in years]
  }

-- | Interpolated data point for a nation at a specific year
type NationPoint =
  { name :: String
  , region :: Region
  , year :: Int
  , income :: Number           -- Income per capita (PPP dollars)
  , population :: Number       -- Population count
  , lifeExpectancy :: Number   -- Life expectancy in years
  }

-- | Complete model containing all nation data
type WealthHealthModel =
  { nations :: Array NationData
  , yearRange :: { min :: Int, max :: Int }  -- Available year range (e.g., 1800-2009)
  }

-- | Bounds for visualization scales
type ScaleBounds =
  { income :: { min :: Number, max :: Number }       -- e.g., 200 - 100000
  , lifeExpectancy :: { min :: Number, max :: Number } -- e.g., 14 - 86
  , population :: { min :: Number, max :: Number }   -- Used for circle sizing
  }

-- | Default scale bounds matching the Observable notebook
defaultScaleBounds :: ScaleBounds
defaultScaleBounds =
  { income: { min: 200.0, max: 100000.0 }
  , lifeExpectancy: { min: 14.0, max: 86.0 }
  , population: { min: 0.0, max: 1400000000.0 } -- ~1.4 billion (China/India)
  }
