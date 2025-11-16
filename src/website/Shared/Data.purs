module PSD3.Shared.Data where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String.CodeUnits as CodeUnits
import Data.Tree (Tree(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)

-- | Available data files with type-safe identifiers
data DataFile
  = EnergyCSV                    -- Sankey diagram data
  | FlareJSON                    -- Hierarchical data for tree/pack/partition layouts
  | MiserablesJSON               -- Character network graph
  | NationsJSON                  -- Wealth & Health of Nations
  | MetroUnemploymentCSV         -- BLS metro unemployment data
  | USPopulationStateAgeCSV      -- US population by state and age (for grouped bar chart)

derive instance Eq DataFile

-- | Get the file path for a data file
dataFilePath :: DataFile -> String
dataFilePath EnergyCSV = "./data/energy.csv"
dataFilePath FlareJSON = "./data/flare-2.json"
dataFilePath MiserablesJSON = "./data/miserables.json"
dataFilePath NationsJSON = "./data/nations.json"
dataFilePath MetroUnemploymentCSV = "./data/bls-metro-unemployment.csv"
dataFilePath USPopulationStateAgeCSV = "./data/us-population-state-age.csv"

-- | Human-readable description
dataFileDescription :: DataFile -> String
dataFileDescription EnergyCSV = "Energy flow data for Sankey diagrams"
dataFileDescription FlareJSON = "Flare visualization toolkit hierarchy (252 nodes)"
dataFileDescription MiserablesJSON = "Les Misérables character co-occurrence network (77 nodes, 254 links)"
dataFileDescription NationsJSON = "Wealth & Health of Nations time series data"
dataFileDescription MetroUnemploymentCSV = "BLS metro area unemployment rates"
dataFileDescription USPopulationStateAgeCSV = "US population by state and age group"

-- | Load a data file as a string
-- | Returns Either error string or file contents
loadDataFile :: DataFile -> Aff (Either String String)
loadDataFile dataFile = do
  let path = dataFilePath dataFile
  log $ "Loading data file: " <> path
  response <- AJAX.get ResponseFormat.string path
  case response of
    Left err -> do
      log $ "Failed to load " <> path <> ": " <> AJAX.printError err
      pure $ Left $ "Failed to load data file: " <> AJAX.printError err
    Right { body } -> do
      log $ "Successfully loaded " <> path
      pure $ Right body

-- | Simple CSV parser that handles quoted fields containing commas
-- | Good enough for our data files, not a full RFC 4180 implementation
parseCSVRow :: String -> Array String
parseCSVRow line =
  let chars = CodeUnits.toCharArray line
  in parseFields chars []
  where
    parseFields :: Array Char -> Array String -> Array String
    parseFields chars acc
      | Array.null chars = acc
      | otherwise =
          case Array.head chars of
            Just '"' ->
              -- Quoted field - find closing quote
              case findClosingQuote (Array.drop 1 chars) 0 of
                { field, rest } ->
                  let nextFields = if Array.null rest
                                   then []
                                   else Array.drop 1 rest -- skip comma
                  in parseFields nextFields (Array.snoc acc field)
            _ ->
              -- Unquoted field - read until comma or end
              let { field, rest } = readUntilComma chars
              in parseFields rest (Array.snoc acc field)

    findClosingQuote :: Array Char -> Int -> { field :: String, rest :: Array Char }
    findClosingQuote chars idx =
      case Array.index chars idx of
        Just '"' ->
          { field: CodeUnits.fromCharArray (Array.slice 0 idx chars)
          , rest: Array.drop (idx + 1) chars
          }
        Just _ -> findClosingQuote chars (idx + 1)
        Nothing ->
          -- No closing quote found, take everything
          { field: CodeUnits.fromCharArray chars
          , rest: []
          }

    readUntilComma :: Array Char -> { field :: String, rest :: Array Char }
    readUntilComma chars =
      case Array.findIndex (\c -> c == ',') chars of
        Just idx ->
          { field: CodeUnits.fromCharArray (Array.slice 0 idx chars)
          , rest: Array.drop (idx + 1) chars
          }
        Nothing ->
          { field: CodeUnits.fromCharArray chars
          , rest: []
          }

-- | Hierarchical data type (for Flare, etc.)
-- | JavaScript object with hierarchical structure (blessed JSON)
foreign import data HierData :: Type

-- | Parse JSON string into blessed HierData
foreign import parseFlareJson :: String -> HierData

-- | FFI accessors for blessed JSON data
foreign import getName :: HierData -> String
foreign import getValue :: HierData -> Number
foreign import getChildren_ :: HierData -> Nullable (Array HierData)

-- | Get children as Maybe Array
getChildren :: HierData -> Maybe (Array HierData)
getChildren = toMaybe <<< getChildren_

-- | Convert HierData to Data.Tree (done in PureScript using fromFoldable)
hierDataToTree :: HierData -> Tree { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
hierDataToTree hierData =
  let
    name = getName hierData
    value = getValue hierData
    childrenMaybe = getChildren hierData
    childrenList = case childrenMaybe of
      Nothing -> Nil
      Just childrenArray -> fromFoldable $ map hierDataToTree childrenArray
  in
    Node { name, value, x: 0.0, y: 0.0, depth: 0, height: 0 } childrenList

-- | Load and parse Flare JSON data
loadFlareData :: Aff (Either String (Tree { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }))
loadFlareData = do
  result <- loadDataFile FlareJSON
  case result of
    Left err -> pure $ Left err
    Right jsonString -> do
      let hierData = parseFlareJson jsonString
      let tree = hierDataToTree hierData
      pure $ Right tree
