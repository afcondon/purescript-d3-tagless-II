module PSD3.Shared.DataLoader where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Data.Argonaut.Core (Json)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

-- | Configuration for data loading strategy
data LoadStrategy
  = LocalOnly       -- Load from bundled local files only
  | RemoteOnly      -- Load from remote server only
  | LocalFirst      -- Try local first, fallback to remote on failure
  | RemoteFirst     -- Try remote first, fallback to local on failure

derive instance eqLoadStrategy :: Eq LoadStrategy

-- | Configuration for data loading
type LoadConfig =
  { strategy :: LoadStrategy
  , baseUrl :: Maybe String  -- Base URL for remote server (e.g., "http://localhost:8080")
  }

-- | Default configuration: local only (current behavior)
defaultConfig :: LoadConfig
defaultConfig =
  { strategy: LocalOnly
  , baseUrl: Nothing
  }

-- | Configuration for loading from remote server
remoteConfig :: String -> LoadConfig
remoteConfig url =
  { strategy: RemoteOnly
  , baseUrl: Just url
  }

-- | Configuration that tries local first, then remote
localFirstConfig :: String -> LoadConfig
localFirstConfig url =
  { strategy: LocalFirst
  , baseUrl: Just url
  }

-- | Error type for data loading failures
data LoadError
  = NetworkError String
  | ParseError String
  | FileNotFound String
  | ConfigError String

instance showLoadError :: Show LoadError where
  show (NetworkError msg) = "Network Error: " <> msg
  show (ParseError msg) = "Parse Error: " <> msg
  show (FileNotFound path) = "File Not Found: " <> path
  show (ConfigError msg) = "Configuration Error: " <> msg

-- ============================================================================
-- JSON Loading
-- ============================================================================

-- | Load JSON from local bundled file
loadLocalJSON :: String -> Aff (Either LoadError Json)
loadLocalJSON path = do
  result <- AJAX.get ResponseFormat.json path
  pure $ case result of
    Left err ->
      Left $ NetworkError $ "Failed to load local JSON from " <> path <> ": " <> AJAX.printError err
    Right response ->
      Right response.body

-- | Load JSON from remote server
loadRemoteJSON :: String -> String -> Aff (Either LoadError Json)
loadRemoteJSON baseUrl path = do
  let fullUrl = baseUrl <> "/" <> path
  result <- AJAX.get ResponseFormat.json fullUrl
  pure $ case result of
    Left err ->
      Left $ NetworkError $ "Failed to load remote JSON from " <> fullUrl <> ": " <> AJAX.printError err
    Right response ->
      Right response.body

-- | Load JSON with configurable strategy
loadJSON :: LoadConfig -> String -> Aff (Either LoadError Json)
loadJSON config path = case config.strategy of
  LocalOnly ->
    loadLocalJSON path

  RemoteOnly -> case config.baseUrl of
    Nothing -> pure $ Left $ ConfigError "RemoteOnly strategy requires baseUrl"
    Just url -> loadRemoteJSON url path

  LocalFirst -> case config.baseUrl of
    Nothing -> loadLocalJSON path  -- No remote fallback available
    Just url -> do
      localResult <- loadLocalJSON path
      case localResult of
        Right json -> pure $ Right json
        Left _ -> loadRemoteJSON url path  -- Fallback to remote

  RemoteFirst -> case config.baseUrl of
    Nothing -> loadLocalJSON path  -- No remote available, use local
    Just url -> do
      remoteResult <- loadRemoteJSON url path
      case remoteResult of
        Right json -> pure $ Right json
        Left _ -> loadLocalJSON path  -- Fallback to local

-- | Load JSON with default fallback on error
-- | Returns a minimal error structure if all loading attempts fail
loadJSONWithFallback :: LoadConfig -> String -> Json -> Aff Json
loadJSONWithFallback config path fallback = do
  result <- loadJSON config path
  pure $ case result of
    Right json -> json
    Left _ -> fallback

-- ============================================================================
-- CSV Loading
-- ============================================================================

-- | Load CSV from local bundled file
-- | Returns array of Foreign objects (one per row)
loadLocalCSV :: String -> Aff (Either LoadError (Array Foreign))
loadLocalCSV path = do
  result <- AJAX.get ResponseFormat.json path
  pure $ case result of
    Left err ->
      Left $ NetworkError $ "Failed to load local CSV from " <> path <> ": " <> AJAX.printError err
    Right response ->
      -- CSV is expected to be parsed by d3-dsv on the server or via build process
      -- and served as JSON array
      Right $ fromFoldable $ (unsafeCoerce response.body :: Array Foreign)

-- | Load CSV from remote server
loadRemoteCSV :: String -> String -> Aff (Either LoadError (Array Foreign))
loadRemoteCSV baseUrl path = do
  let fullUrl = baseUrl <> "/" <> path
  result <- AJAX.get ResponseFormat.json fullUrl
  pure $ case result of
    Left err ->
      Left $ NetworkError $ "Failed to load remote CSV from " <> fullUrl <> ": " <> AJAX.printError err
    Right response ->
      Right $ fromFoldable $ (unsafeCoerce response.body :: Array Foreign)

-- | Load CSV with configurable strategy
loadCSV :: LoadConfig -> String -> Aff (Either LoadError (Array Foreign))
loadCSV config path = case config.strategy of
  LocalOnly ->
    loadLocalCSV path

  RemoteOnly -> case config.baseUrl of
    Nothing -> pure $ Left $ ConfigError "RemoteOnly strategy requires baseUrl"
    Just url -> loadRemoteCSV url path

  LocalFirst -> case config.baseUrl of
    Nothing -> loadLocalCSV path
    Just url -> do
      localResult <- loadLocalCSV path
      case localResult of
        Right csv -> pure $ Right csv
        Left _ -> loadRemoteCSV url path

  RemoteFirst -> case config.baseUrl of
    Nothing -> loadLocalCSV path
    Just url -> do
      remoteResult <- loadRemoteCSV url path
      case remoteResult of
        Right csv -> pure $ Right csv
        Left _ -> loadLocalCSV path

-- | Load CSV with empty array fallback on error
loadCSVWithFallback :: LoadConfig -> String -> Aff (Array Foreign)
loadCSVWithFallback config path = do
  result <- loadCSV config path
  pure $ case result of
    Right csv -> csv
    Left _ -> []

-- ============================================================================
-- Convenience Functions
-- ============================================================================

-- | Simple local JSON loader (current behavior preserved)
-- | Returns minimal fallback structure on error
simpleLoadJSON :: String -> Aff Json
simpleLoadJSON path =
  loadJSONWithFallback defaultConfig path
    (unsafeCoerce { name: "error", children: [] })

-- | Simple local CSV loader
-- | Returns empty array on error
simpleLoadCSV :: String -> Aff (Array Foreign)
simpleLoadCSV path =
  loadCSVWithFallback defaultConfig path

-- ============================================================================
-- Raw Text Loading (for CSV files that need d3 parsing, etc.)
-- ============================================================================

-- | Load raw text from local bundled file
loadLocalText :: String -> Aff (Either LoadError String)
loadLocalText path = do
  result <- AJAX.get ResponseFormat.string path
  pure $ case result of
    Left err ->
      Left $ NetworkError $ "Failed to load local text from " <> path <> ": " <> AJAX.printError err
    Right response ->
      Right response.body

-- | Load raw text from remote server
loadRemoteText :: String -> String -> Aff (Either LoadError String)
loadRemoteText baseUrl path = do
  let fullUrl = baseUrl <> "/" <> path
  result <- AJAX.get ResponseFormat.string fullUrl
  pure $ case result of
    Left err ->
      Left $ NetworkError $ "Failed to load remote text from " <> fullUrl <> ": " <> AJAX.printError err
    Right response ->
      Right response.body

-- | Load raw text with configurable strategy
loadText :: LoadConfig -> String -> Aff (Either LoadError String)
loadText config path = case config.strategy of
  LocalOnly ->
    loadLocalText path

  RemoteOnly -> case config.baseUrl of
    Nothing -> pure $ Left $ ConfigError "RemoteOnly strategy requires baseUrl"
    Just url -> loadRemoteText url path

  LocalFirst -> case config.baseUrl of
    Nothing -> loadLocalText path
    Just url -> do
      localResult <- loadLocalText path
      case localResult of
        Right text -> pure $ Right text
        Left _ -> loadRemoteText url path

  RemoteFirst -> case config.baseUrl of
    Nothing -> loadLocalText path
    Just url -> do
      remoteResult <- loadRemoteText url path
      case remoteResult of
        Right text -> pure $ Right text
        Left _ -> loadLocalText path

-- | Load raw text with empty string fallback on error
loadTextWithFallback :: LoadConfig -> String -> Aff String
loadTextWithFallback config path = do
  result <- loadText config path
  pure $ case result of
    Right text -> text
    Left _ -> ""

-- | Simple local text loader
-- | Returns empty string on error
simpleLoadText :: String -> Aff String
simpleLoadText path =
  loadTextWithFallback defaultConfig path
