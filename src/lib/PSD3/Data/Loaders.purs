module PSD3.Data.Loaders
  ( LoadError(..)
  , GraphDataJSON
  , loadGraphJSON
  , loadJSON
  , loadCSV
  , parseGraphJSON
  ) where

import Prelude

import Affjax (Error, URL) as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json, toObject, toArray)
import Data.Argonaut.Core as Json
import Data.Array (catMaybes, foldl, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, note)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError, MultipleErrors, renderForeignError, unsafeToForeign)
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
    Left ajaxError ->
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

-- | Load CSV data (stub - to be implemented)
-- | TODO: Implement CSV parsing
loadCSV :: forall a.
  AX.URL ->
  (Array String -> Either LoadError a) ->  -- Row decoder
  Aff (Either LoadError (Array a))
loadCSV url decoder = pure $ Left $ ParseError "loadCSV not yet implemented"
