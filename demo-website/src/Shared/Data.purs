module PSD3.Shared.Data where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List(..), fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Tree (Tree, mkTree)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)

-- | Available data files with type-safe identifiers
data DataFile
  = EnergyCSV -- Sankey diagram data
  | FlareJSON -- Hierarchical data for tree/pack/partition layouts
  | FlareImportsJSON -- Flare data with imports for edge bundling
  | MiserablesJSON -- Character network graph
  | NationsJSON -- Wealth & Health of Nations
  | MetroUnemploymentCSV -- BLS metro unemployment data
  | USPopulationStateAgeCSV -- US population by state and age (for grouped bar chart)
  | BridgesCSV -- Baton Rouge bridge traffic data (for chord diagram)

derive instance Eq DataFile

-- | Get the file path for a data file
dataFilePath :: DataFile -> String
dataFilePath EnergyCSV = "./data/energy.csv"
dataFilePath FlareJSON = "./data/flare-2.json"
dataFilePath FlareImportsJSON = "./data/flare-imports.json"
dataFilePath MiserablesJSON = "./data/miserables.json"
dataFilePath NationsJSON = "./data/nations.json"
dataFilePath MetroUnemploymentCSV = "./data/bls-metro-unemployment.csv"
dataFilePath USPopulationStateAgeCSV = "./data/us-population-state-age.csv"
dataFilePath BridgesCSV = "./data/bridges.csv"

-- | Human-readable description
dataFileDescription :: DataFile -> String
dataFileDescription EnergyCSV = "Energy flow data for Sankey diagrams"
dataFileDescription FlareJSON = "Flare visualization toolkit hierarchy (252 nodes)"
dataFileDescription FlareImportsJSON = "Flare data with imports for edge bundling (252 classes, ~600 dependencies)"
dataFileDescription MiserablesJSON = "Les Misérables character co-occurrence network (77 nodes, 254 links)"
dataFileDescription NationsJSON = "Wealth & Health of Nations time series data"
dataFileDescription MetroUnemploymentCSV = "BLS metro area unemployment rates"
dataFileDescription USPopulationStateAgeCSV = "US population by state and age group"
dataFileDescription BridgesCSV = "Baton Rouge bridge traffic flow data (origin;destination;value)"

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
  let
    chars = CodeUnits.toCharArray line
  in
    parseFields chars []
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
                let
                  nextFields =
                    if Array.null rest then []
                    else Array.drop 1 rest -- skip comma
                in
                  parseFields nextFields (Array.snoc acc field)
          _ ->
            -- Unquoted field - read until comma or end
            let
              { field, rest } = readUntilComma chars
            in
              parseFields rest (Array.snoc acc field)

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
    mkTree { name, value, x: 0.0, y: 0.0, depth: 0, height: 0 } childrenList

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

-- | Bridge traffic data row (origin;destination;value)
type BridgeFlowRow = { origin :: String, destination :: String, value :: Number }

-- | Bridge traffic data with matrix and labels
type BridgeData =
  { matrix :: Array (Array Number)
  , labels :: Array String
  }

-- | Parse semicolon-delimited CSV row
parseCSVRowSemicolon :: String -> Array String
parseCSVRowSemicolon line =
  let
    chars = CodeUnits.toCharArray line
  in
    parseFields chars []
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
                let
                  nextFields =
                    if Array.null rest then []
                    else Array.drop 1 rest -- skip semicolon
                in
                  parseFields nextFields (Array.snoc acc field)
          _ ->
            -- Unquoted field - read until semicolon or end
            let
              { field, rest } = readUntilSemicolon chars
            in
              parseFields rest (Array.snoc acc field)

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

  readUntilSemicolon :: Array Char -> { field :: String, rest :: Array Char }
  readUntilSemicolon chars =
    case Array.findIndex (\c -> c == ';') chars of
      Just idx ->
        { field: CodeUnits.fromCharArray (Array.slice 0 idx chars)
        , rest: Array.drop (idx + 1) chars
        }
      Nothing ->
        { field: CodeUnits.fromCharArray chars
        , rest: []
        }

-- | Parse bridges CSV data into matrix format
-- | CSV format: origin;destination;value (with optional header row)
parseBridgesCSV :: String -> Either String BridgeData
parseBridgesCSV csvContent = do
  let lines = split (Pattern "\n") csvContent
  let nonEmptyLines = Array.filter (\line -> trim line /= "") lines

  -- Skip header row if it exists (check if first row contains "From" or "To" or "Value")
  let
    dataLines = case Array.head nonEmptyLines of
      Just firstLine ->
        let
          lowerFirst = trim firstLine
        in
          if lowerFirst == "From;To;Value" || lowerFirst == "from;to;value" || lowerFirst == "Origin;Destination;Value" then Array.drop 1 nonEmptyLines
          else nonEmptyLines
      Nothing -> nonEmptyLines

  -- Parse each row
  let parsedRows = Array.mapMaybe parseRow dataLines

  -- Extract unique region labels (origins and destinations)
  let
    allRegions = Array.nub $ do
      row <- parsedRows
      [ row.origin, row.destination ]

  let labels = Array.sort allRegions
  let n = Array.length labels

  -- Create index lookup map
  let
    labelIndexMap = Map.fromFoldable $
      Array.mapWithIndex (\i label -> Tuple label i) labels

  -- Initialize matrix with zeros
  let emptyMatrix = Array.replicate n (Array.replicate n 0.0)

  -- Fill matrix with values
  let matrix = Array.foldl (fillMatrixCell labelIndexMap) emptyMatrix parsedRows

  pure { matrix, labels }
  where
  parseRow :: String -> Maybe BridgeFlowRow
  parseRow line = do
    let fields = parseCSVRowSemicolon (trim line)
    origin <- Array.index fields 0
    destination <- Array.index fields 1
    valueStr <- Array.index fields 2
    value <- Number.fromString (trim valueStr)
    pure { origin: trim origin, destination: trim destination, value }

  fillMatrixCell :: Map.Map String Int -> Array (Array Number) -> BridgeFlowRow -> Array (Array Number)
  fillMatrixCell labelMap matrix row =
    case Tuple <$> Map.lookup row.origin labelMap <*> Map.lookup row.destination labelMap of
      Just (Tuple originIdx destIdx) ->
        case Array.index matrix originIdx of
          Just rowArray ->
            let
              updatedRow = Array.updateAt destIdx row.value rowArray
            in
              case updatedRow of
                Just newRow -> case Array.updateAt originIdx newRow matrix of
                  Just newMatrix -> newMatrix
                  Nothing -> matrix
                Nothing -> matrix
          Nothing -> matrix
      Nothing -> matrix

-- | Load and parse bridges CSV data
loadBridgesData :: Aff (Either String BridgeData)
loadBridgesData = do
  result <- loadDataFile BridgesCSV
  case result of
    Left err -> pure $ Left err
    Right csvContent -> do
      let parsed = parseBridgesCSV csvContent
      case parsed of
        Right bridgeData -> do
          -- Log the parsed data for debugging
          log "=== Bridges CSV Parsed Data ==="
          log $ "Labels (" <> show (Array.length bridgeData.labels) <> " regions):"
          log $ show bridgeData.labels
          log "\nMatrix (rows = from, cols = to):"
          let indexedRows = Array.mapWithIndex (\i row -> { i, row }) bridgeData.matrix
          traverse_
            ( \{ i, row } -> do
                let
                  label = case Array.index bridgeData.labels i of
                    Just l -> l
                    Nothing -> "?"
                log $ label <> ": " <> show row
            )
            indexedRows
          log "=== End Bridges Data ==="
          pure $ Right bridgeData
        Left err -> pure $ Left err

-- | Flare imports data type (for edge bundling)
-- | JavaScript object with {name, size, imports} structure
foreign import data FlareImportNode :: Type

-- | Parse JSON string into array of FlareImportNode
foreign import parseFlareImportsJson :: String -> Array FlareImportNode

-- | FFI accessors for flare-imports data
foreign import getImportNodeName :: FlareImportNode -> String
foreign import getImportNodeSize :: FlareImportNode -> Number
foreign import getImportNodeImports :: FlareImportNode -> Array String

-- | Flare import node type (PureScript record)
type FlareImportRecord =
  { name :: String
  , size :: Number
  , imports :: Array String
  }

-- | Convert FFI node to PureScript record
flareImportNodeToRecord :: FlareImportNode -> FlareImportRecord
flareImportNodeToRecord node =
  { name: getImportNodeName node
  , size: getImportNodeSize node
  , imports: getImportNodeImports node
  }

-- | Load and parse flare-imports JSON data
loadFlareImportsData :: Aff (Either String (Array FlareImportRecord))
loadFlareImportsData = do
  result <- loadDataFile FlareImportsJSON
  case result of
    Left err -> pure $ Left err
    Right jsonString -> do
      let nodes = parseFlareImportsJson jsonString
      let records = map flareImportNodeToRecord nodes
      log $ "Loaded flare-imports: " <> show (Array.length records) <> " nodes"
      pure $ Right records
