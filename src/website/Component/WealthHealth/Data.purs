module PSD3.WealthHealth.Data where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (find, head, last, sortBy, (!!))
import Data.Either (Either(..))
import Data.Foldable (minimum, maximum)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (comparing)
import Effect.Aff (Aff)
import PSD3.WealthHealth.Types (NationData, NationPoint, Region(..), WealthHealthModel)

-- | URL for the nations.json data (hosted locally)
nationsDataUrl :: String
nationsDataUrl = "data/nations.json"

-- | Region constructors for FFI use
eastAsiaAndPacific :: Region
eastAsiaAndPacific = EastAsiaAndPacific

europe :: Region
europe = Europe

latinAmericaAndCaribbean :: Region
latinAmericaAndCaribbean = LatinAmericaAndCaribbean

middleEastAndNorthAfrica :: Region
middleEastAndNorthAfrica = MiddleEastAndNorthAfrica

southAsia :: Region
southAsia = SouthAsia

subSaharanAfrica :: Region
subSaharanAfrica = SubSaharanAfrica

northAmerica :: Region
northAmerica = NorthAmerica

-- | Foreign import to parse JSON data
foreign import parseNationsJSON :: String -> Array NationData

-- | Load nations data from the Observable CDN
loadNationsData :: Aff (Either String WealthHealthModel)
loadNationsData = do
  result <- AX.get ResponseFormat.string nationsDataUrl
  pure case result of
    Left err ->
      Left $ "Failed to load nations data: " <> AX.printError err
    Right response -> do
      let nations = parseNationsJSON response.body
      let yearRange = calculateYearRange nations
      Right { nations, yearRange }

-- | Calculate the min and max years available in the dataset
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
    maxYear = fromMaybe 2009 (maximum allYears)
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

-- | Interpolate a value for a given year from an array of [year, value] pairs
-- | Uses bisection and linear interpolation like the Observable notebook
interpolateValue :: Int -> Array (Array Number) -> Maybe Number
interpolateValue targetYear dataPoints =
  case sortBy (comparing (\arr -> fromMaybe 0.0 (arr !! 0))) dataPoints of
    [] -> Nothing
    sorted -> do
      -- Find the surrounding points
      let
        yearNum = toNumber targetYear
        before = findBefore yearNum sorted
        after = findAfter yearNum sorted

      case before, after of
        -- Exact match
        Just b, Just a | fromMaybe 0.0 (b !! 0) == yearNum -> b !! 1
        -- Interpolate between two points
        Just b, Just a -> do
          y0 <- b !! 0
          y1 <- a !! 0
          v0 <- b !! 1
          v1 <- a !! 1

          -- Linear interpolation
          let t = (yearNum - y0) / (y1 - y0)
          pure $ v0 + t * (v1 - v0)

        -- Use the closest available point if we're at the edges
        Just b, Nothing -> b !! 1
        Nothing, Just a -> a !! 1
        Nothing, Nothing -> Nothing

-- | Find the data point at or before the target year
findBefore :: Number -> Array (Array Number) -> Maybe (Array Number)
findBefore targetYear points =
  let
    candidates = find (\point ->
      case point !! 0 of
        Just year -> year <= targetYear
        Nothing -> false
    ) points
  in
    -- If no point before, use the first point
    case candidates of
      Nothing -> head points
      Just p -> Just p

-- | Find the data point at or after the target year
findAfter :: Number -> Array (Array Number) -> Maybe (Array Number)
findAfter targetYear points =
  let
    candidates = find (\point ->
      case point !! 0 of
        Just year -> year >= targetYear
        Nothing -> false
    ) points
  in
    -- If no point after, use the last point
    case candidates of
      Nothing -> last points
      Just p -> Just p

-- | Get all nations' data for a specific year
getAllNationsAtYear :: Int -> WealthHealthModel -> Array NationPoint
getAllNationsAtYear year model =
  model.nations
    # map (getNationAtYear year)
    # map (fromMaybe $ dummyNationPoint year)  -- Filter out failed interpolations
    # identity  -- This will need better error handling in production

-- | Dummy nation point for error cases (should not happen with good data)
dummyNationPoint :: Int -> NationPoint
dummyNationPoint year =
  { name: "Unknown"
  , region: SubSaharanAfrica
  , year
  , income: 1000.0
  , population: 1000000.0
  , lifeExpectancy: 50.0
  }
