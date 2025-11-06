module PSD3.Data.Loaders
  ( LoadError(..)
  , GraphDataJSON
  , loadGraphJSON
  , loadGraphJSON_
  , loadJSON
  , loadCSV
  , parseGraphJSON
  , unsafeBlessJSON
  , unsafeBlessGraphJSON
  ) where

import Prelude

import Affjax (URL) as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, MultipleErrors, renderForeignError, unsafeToForeign)
import Foreign as F
import Foreign.Index ((!))
import PSD3.Data.Graph (GraphModel, GraphConfig, buildGraphModel)

-- | Errors that can occur during data loading
data LoadError
  = NetworkError String
  | ParseError String
  | DecodeError String
  | ValidationError String
  | FileNotFound String

instance Show LoadError where
  show (NetworkError msg) = "Network error: " <> msg
  show (ParseError msg) = "Parse error: " <> msg
  show (DecodeError msg) = "Decode error: " <> msg
  show (ValidationError msg) = "Validation error: " <> msg
  show (FileNotFound path) = "File not found: " <> path

-- | Standard graph JSON format
type GraphDataJSON =
  { nodes :: Array Foreign
  , links :: Array Foreign
  }

-- | Load and parse a JSON file
loadJSON :: AX.URL -> Aff (Either LoadError Foreign)
loadJSON url = do
  response <- AJAX.get ResponseFormat.json url
  pure $ case response of
    Left _ ->
      Left $ NetworkError $ "Failed to fetch " <> url
    Right resp ->
      Right $ unsafeToForeign resp.body

-- | Parse graph JSON into standard format
parseGraphJSON :: Foreign -> Either LoadError GraphDataJSON
parseGraphJSON json = do
  let parsed = runExcept do
        obj <- pure json
        nodes <- obj ! "nodes" >>= F.readArray
        links <- obj ! "links" >>= F.readArray
        pure { nodes, links }

  lmap (DecodeError <<< formatErrors) parsed
  where
    formatErrors :: MultipleErrors -> String
    formatErrors errs =
      "Failed to parse graph JSON. Expected format: {nodes: [...], links: [...]}. Errors: "
      <> show (map renderForeignError errs)

-- | Load graph data from JSON and build GraphModel
loadGraphJSON :: forall node link.
  AX.URL ->
  GraphConfig node link ->
  (Foreign -> Either LoadError node) ->
  (Foreign -> Either LoadError link) ->
  Aff (Either LoadError (GraphModel node link))
loadGraphJSON url config decodeNode decodeLink = do
  jsonResult <- loadJSON url

  pure $ do
    json <- jsonResult
    graphData <- parseGraphJSON json
    nodes <- decodeArray "node" decodeNode graphData.nodes
    links <- decodeArray "link" decodeLink graphData.links
    pure $ buildGraphModel config nodes links

  where
    decodeArray :: forall a.
      String ->
      (Foreign -> Either LoadError a) ->
      Array Foreign ->
      Either LoadError (Array a)
    decodeArray typeName decoder foreigns =
      let
        decoded = decoder <$> foreigns
        failures = catMaybes $ (\e -> case e of
                                   Left err -> Just err
                                   Right _ -> Nothing) <$> decoded
        successes = catMaybes $ hush <$> decoded
      in
        if length failures == 0
          then Right successes
          else Left $ DecodeError $
            "Failed to decode " <> show (length failures)
            <> " " <> typeName <> "(s)"

-- ============================================================================
-- | Unsafe "Blessing" Functions for Performance
-- ============================================================================
-- |
-- | For large datasets, parsing JSON in PureScript can be a performance
-- | bottleneck. These unsafe functions skip validation and directly coerce
-- | the JSON to the expected types.
-- |
-- | **Use these when:**
-- | - You have large datasets (>1000 nodes)
-- | - You trust the data format
-- | - Performance is critical
-- | - You've tested the data format beforehand
-- |
-- | **Warnings:**
-- | - No error checking - malformed data causes runtime errors
-- | - No type safety - you must ensure the types match
-- | - Debugging is harder when things go wrong
-- |
-- | **Pattern:** Use safe parsing during development, switch to unsafe for
-- | production when data format is stable.

-- | Unsafe: Directly coerce Foreign JSON to your types
-- |
-- | This skips all PureScript parsing and validation. Use when you know
-- | the JSON structure matches your types exactly.
-- |
-- | Example:
-- | ```purescript
-- | -- Development (safe):
-- | nodes <- loadGraphJSON url config decodeNode decodeLink
-- |
-- | -- Production (unsafe, fast):
-- | { nodes, links } <- unsafeBlessGraphJSON <$> loadJSON url
-- | let graph = buildGraphModel config nodes links
-- | ```
unsafeBlessJSON :: forall a. Foreign -> a
unsafeBlessJSON = F.unsafeFromForeign

-- | Unsafe: Directly coerce graph JSON to {nodes, links} arrays
-- |
-- | Assumes JSON structure: `{ nodes: [...], links: [...] }`
-- | No validation, no error checking, maximum performance.
unsafeBlessGraphJSON :: forall node link.
  Foreign ->
  { nodes :: Array node, links :: Array link }
unsafeBlessGraphJSON = unsafeBlessJSON

-- | Unsafe version of loadGraphJSON - maximum performance, no validation
-- |
-- | Use this when:
-- | - Data format is known and stable
-- | - Performance is critical (large datasets)
-- | - You've already validated data in development
-- |
-- | Returns raw GraphModel without error checking.
-- |
-- | Example:
-- | ```purescript
-- | -- Safe (checks every node/link):
-- | graph <- loadGraphJSON url config decodeNode decodeLink
-- |
-- | -- Unsafe (no checks, max speed):
-- | graph <- loadGraphJSON_ url config
-- | ```
loadGraphJSON_ :: forall node link.
  AX.URL ->
  GraphConfig node link ->
  Aff (GraphModel node link)
loadGraphJSON_ url config = do
  response <- AJAX.get ResponseFormat.json url
  case response of
    Left _ ->
      -- Even unsafe version fails on network errors
      pure $ buildGraphModel config [] []
    Right resp -> do
      let json = unsafeToForeign resp.body
          { nodes, links } = unsafeBlessGraphJSON json
      pure $ buildGraphModel config nodes links

-- | Load CSV data using D3's CSV parser (via FFI)
-- |
-- | The CSV is loaded and parsed by D3, returning an array of Foreign objects.
-- | Each object represents a row with column names as keys.
-- |
-- | For simple use cases where you want the raw CSV data, you can use:
-- | ```purescript
-- | rows <- loadCSV url (Right <<< unsafeToForeign)
-- | ```
-- |
-- | For type-safe decoding, provide a decoder that extracts fields:
-- | ```purescript
-- | rows <- loadCSV url decodeMyRow
-- | ```
loadCSV :: forall a.
  AX.URL ->
  (Foreign -> Either LoadError a) ->  -- Row decoder
  Aff (Either LoadError (Array a))
loadCSV url decoder = do
  rows <- fromEffectFnAff $ loadCSVImpl url
  pure $ decodeArray "CSV row" decoder rows
  where
    decodeArray :: forall b.
      String ->
      (Foreign -> Either LoadError b) ->
      Array Foreign ->
      Either LoadError (Array b)
    decodeArray typeName dec foreigns =
      let
        decoded = dec <$> foreigns
        failures = catMaybes $ (\e -> case e of
                                   Left err -> Just err
                                   Right _ -> Nothing) <$> decoded
        successes = catMaybes $ hush <$> decoded
      in
        if length failures == 0
          then Right successes
          else Left $ DecodeError $
            "Failed to decode " <> show (length failures)
            <> " " <> typeName <> "(s)"

-- FFI imports
foreign import loadCSVImpl :: AX.URL -> EffectFnAff (Array Foreign)
