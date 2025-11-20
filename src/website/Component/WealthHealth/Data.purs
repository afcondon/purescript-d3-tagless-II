-- | Data loading and interpolation for Wealth & Health visualization
-- |
-- | Interpolation approach based on Mike Bostock's "The Wealth & Health of Nations"
-- | https://observablehq.com/@mbostock/the-wealth-health-of-nations
-- | Copyright (c) 2018 Mike Bostock
-- | ISC License: https://opensource.org/licenses/ISC
module Component.WealthHealth.Data where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (catMaybes, head, length, range, sortBy, (!!))
import Data.Either (Either(..))
import Data.Foldable (minimum, maximum)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (comparing)
import Effect.Aff (Aff)
import Component.WealthHealth.Types (NationData, NationPoint, Region(..), WealthHealthModel)

-- | URL for the nations.json data (hosted locally)
nationsDataUrl :: String
nationsDataUrl = "data/nations.json"

-- | Raw nation data from FFI (region is still a string)
type NationDataRaw =
  { name :: String
  , region :: String  -- Will be parsed to Region ADT
  , income :: Array (Array Number)
  , population :: Array (Array Number)
  , lifeExpectancy :: Array (Array Number)
  }

-- | Parse a region string to a Region ADT
parseRegionString :: String -> Maybe Region
parseRegionString = case _ of
  "EastAsiaAndPacific" -> Just EastAsiaAndPacific
  "Europe" -> Just Europe
  "LatinAmericaAndCaribbean" -> Just LatinAmericaAndCaribbean
  "MiddleEastAndNorthAfrica" -> Just MiddleEastAndNorthAfrica
  "SouthAsia" -> Just SouthAsia
  "SubSaharanAfrica" -> Just SubSaharanAfrica
  "NorthAmerica" -> Just NorthAmerica
  _ -> Nothing

-- | Convert raw nation data to proper NationData with parsed Region
parseNationData :: NationDataRaw -> Maybe NationData
parseNationData raw = do
  region <- parseRegionString raw.region
  pure
    { name: raw.name
    , region
    , income: raw.income
    , population: raw.population
    , lifeExpectancy: raw.lifeExpectancy
    }

-- | Foreign import to parse JSON data (returns raw data with string regions)
foreign import parseNationsJSON :: String -> Array NationDataRaw

-- | Fill in missing years in a time series by interpolating from neighboring points
-- | Returns Nothing if the data array is completely empty
fillMissingYears :: { min :: Int, max :: Int } -> Array (Array Number) -> Maybe (Array (Array Number))
fillMissingYears yearRange dataPoints =
  -- Can't interpolate from no data
  if null dataPoints then
    Nothing
  else
    let
      -- Generate all years in range
      allYears = map toNumber $ range yearRange.min yearRange.max

      -- For each year, either use existing data or interpolate
      fillYear year = case interpolateValue (floor year) dataPoints of
        Just value -> [year, value]
        Nothing -> [year, 0.0]  -- Should rarely happen with good data
    in
      Just $ map fillYear allYears
  where
    null arr = case head arr of
      Nothing -> true
      Just _ -> false

-- | Fill missing data for a nation across all metrics
-- | Returns Nothing if any metric is completely empty
fillNationData :: { min :: Int, max :: Int } -> NationData -> Maybe NationData
fillNationData yearRange nation = do
  filledIncome <- fillMissingYears yearRange nation.income
  filledPopulation <- fillMissingYears yearRange nation.population
  filledLifeExpectancy <- fillMissingYears yearRange nation.lifeExpectancy
  pure
    { name: nation.name
    , region: nation.region
    , income: filledIncome
    , population: filledPopulation
    , lifeExpectancy: filledLifeExpectancy
    }

-- | Load nations data from the Observable CDN
loadNationsData :: Aff (Either String WealthHealthModel)
loadNationsData = do
  result <- AX.get ResponseFormat.string nationsDataUrl
  pure case result of
    Left err ->
      Left $ "Failed to load nations data: " <> AX.printError err
    Right response -> do
      let rawNations = parseNationsJSON response.body
      -- Parse regions from strings to ADTs, filtering out any that fail
      let nations = catMaybes $ map parseNationData rawNations
      let yearRange = calculateYearRange nations
      -- Fill in missing years by interpolating from neighboring data
      -- Filter out nations with completely missing metrics
      let filledNations = catMaybes $ map (fillNationData yearRange) nations
      Right { nations: filledNations, yearRange }

-- | Calculate the min and max years available in the dataset
-- | Capped at 2005 to match Mike Bostock's example (data quality issues in later years)
calculateYearRange :: Array NationData -> { min :: Int, max :: Int }
calculateYearRange nations =
  let
    allYears = do
      nation <- nations
      yearValue <- nation.income
      case yearValue !! 0 of
        Just year -> pure (floor year)
        Nothing -> []

    minYear = fromMaybe 1800 (minimum allYears)
    -- Cap at 2005 as in the original Observable notebook
    maxYear = min 2005 (fromMaybe 2009 (maximum allYears))
  in
    { min: minYear, max: maxYear }

-- | Get interpolated nation data for a specific year
getNationAtYear :: Int -> NationData -> Maybe NationPoint
getNationAtYear year nation = do
  income <- interpolateValue year nation.income
  population <- interpolateValue year nation.population
  lifeExpectancy <- interpolateValue year nation.lifeExpectancy

  pure
    { name: nation.name
    , region: nation.region
    , year
    , income
    , population
    , lifeExpectancy
    }

-- | Bisect left: find the insertion point for a value in a sorted array
-- | Returns the index where value would be inserted to maintain sort order
-- | This matches D3's bisector([date]).left behavior
bisectLeft :: Number -> Array (Array Number) -> Int
bisectLeft target arr = go 0 (length arr)
  where
    go low high
      | low >= high = low
      | otherwise =
          let mid = (low + high) / 2
          in case arr !! mid >>= (_ !! 0) of
            Just midVal | midVal < target -> go (mid + 1) high
            _ -> go low mid

-- | Interpolate a value for a given year from an array of [year, value] pairs
-- | Matches Mike Bostock's valueAt function from the Observable notebook
-- | Copyright (c) 2018 Mike Bostock, ISC License
interpolateValue :: Int -> Array (Array Number) -> Maybe Number
interpolateValue targetYear dataPoints =
  case sortBy (comparing (\arr -> fromMaybe 0.0 (arr !! 0))) dataPoints of
    [] -> Nothing
    sorted -> do
      let
        yearNum = toNumber targetYear
        i = bisectLeft yearNum sorted

      -- Get point at index i (at or after target)
      a <- sorted !! i
      aYear <- a !! 0
      aValue <- a !! 1

      -- If we can interpolate with previous point
      if i > 0 then do
        b <- sorted !! (i - 1)
        bYear <- b !! 0
        bValue <- b !! 1

        -- Linear interpolation: a[1] * (1 - t) + b[1] * t
        let t = (yearNum - aYear) / (bYear - aYear)
        pure $ aValue * (1.0 - t) + bValue * t
      else
        -- Just return the value at index i
        pure aValue

-- | Get all nations' data for a specific year
-- | Filters out nations with incomplete data for the given year
getAllNationsAtYear :: Int -> WealthHealthModel -> Array NationPoint
getAllNationsAtYear year model =
  model.nations
    # map (getNationAtYear year)
    # catMaybes  -- Filter out nations with incomplete data
