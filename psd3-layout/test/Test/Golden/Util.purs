-- | Test.Golden.Util
-- |
-- | Utility functions for golden testing layout algorithms.
-- | Provides file I/O and comparison functions for deterministic output testing.
module Test.Golden.Util
  ( assertGolden
  , assertGoldenJson
  , readGoldenFile
  , writeGoldenFile
  , getTestDataPath
  , getGoldenPath
  , roundNumber
  , roundArray
  , GoldenResult(..)
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process

-- | Result of a golden test comparison
data GoldenResult
  = GoldenMatch -- Output matches golden file
  | GoldenCreated -- Golden file was created (first run)
  | GoldenMismatch String String -- Expected vs actual

derive instance eqGoldenResult :: Eq GoldenResult

instance showGoldenResult :: Show GoldenResult where
  show GoldenMatch = "GoldenMatch"
  show GoldenCreated = "GoldenCreated"
  show (GoldenMismatch expected actual) = "GoldenMismatch\nExpected:\n" <> expected <> "\nActual:\n" <> actual

-- | Get the path to test data files
-- | In a monorepo context, tests run from workspace root but files are in psd3-layout
getTestDataPath :: String -> Effect String
getTestDataPath filename = do
  cwd <- Process.cwd
  -- Check if we're in workspace root (has psd3-layout subdir) or in package dir
  let workspacePath = Path.concat [ cwd, "psd3-layout", "test", "data", filename ]
  let packagePath = Path.concat [ cwd, "test", "data", filename ]
  workspaceExists <- FS.exists (Path.concat [ cwd, "psd3-layout" ])
  pure $ if workspaceExists then workspacePath else packagePath

-- | Get the path to golden files
-- | In a monorepo context, tests run from workspace root but files are in psd3-layout
getGoldenPath :: String -> Effect String
getGoldenPath filename = do
  cwd <- Process.cwd
  -- Check if we're in workspace root (has psd3-layout subdir) or in package dir
  let workspacePath = Path.concat [ cwd, "psd3-layout", "test", "golden", filename ]
  let packagePath = Path.concat [ cwd, "test", "golden", filename ]
  workspaceExists <- FS.exists (Path.concat [ cwd, "psd3-layout" ])
  pure $ if workspaceExists then workspacePath else packagePath

-- | Read a golden file if it exists
readGoldenFile :: String -> Effect (Maybe String)
readGoldenFile filename = do
  path <- getGoldenPath filename
  exists <- FS.exists path
  if exists then do
    buffer <- FS.readFile path
    content <- Buffer.toString UTF8 buffer
    pure (Just content)
  else
    pure Nothing

-- | Write content to a golden file
writeGoldenFile :: String -> String -> Effect Unit
writeGoldenFile filename content = do
  path <- getGoldenPath filename
  buffer <- Buffer.fromString content UTF8
  FS.writeFile path buffer

-- | Assert that output matches golden file (string comparison)
-- | If golden file doesn't exist, creates it and returns GoldenCreated
-- | If UPDATE_GOLDEN env var is set, updates the golden file
assertGolden :: String -> String -> Effect GoldenResult
assertGolden filename actual = do
  maybeExpected <- readGoldenFile filename
  updateMode <- isUpdateMode
  case maybeExpected of
    Nothing -> do
      log $ "Creating golden file: " <> filename
      writeGoldenFile filename actual
      pure GoldenCreated
    Just expected ->
      if updateMode then do
        log $ "Updating golden file: " <> filename
        writeGoldenFile filename actual
        pure GoldenCreated
      else if expected == actual then
        pure GoldenMatch
      else
        pure (GoldenMismatch expected actual)

-- | Assert that JSON output matches golden file
-- | Parses both as JSON for semantic comparison (ignoring whitespace)
assertGoldenJson :: String -> Json -> Effect GoldenResult
assertGoldenJson filename actualJson = do
  let actualStr = stringify actualJson
  maybeExpected <- readGoldenFile filename
  updateMode <- isUpdateMode
  case maybeExpected of
    Nothing -> do
      log $ "Creating golden file: " <> filename
      writeGoldenFile filename actualStr
      pure GoldenCreated
    Just expectedStr ->
      if updateMode then do
        log $ "Updating golden file: " <> filename
        writeGoldenFile filename actualStr
        pure GoldenCreated
      else
        case jsonParser expectedStr of
          Left _ -> pure (GoldenMismatch expectedStr actualStr)
          Right expectedJson ->
            if stringify expectedJson == actualStr then
              pure GoldenMatch
            else
              pure (GoldenMismatch expectedStr actualStr)

-- | Check if UPDATE_GOLDEN environment variable is set
isUpdateMode :: Effect Boolean
isUpdateMode = do
  maybeVal <- Process.lookupEnv "UPDATE_GOLDEN"
  pure $ case maybeVal of
    Just "true" -> true
    Just "1" -> true
    _ -> false

-- | Round a number to fixed decimal places for stable golden comparisons
roundNumber :: Int -> Number -> Number
roundNumber decimals n =
  let str = toStringWith (fixed decimals) n
  in case parseNumber str of
    Just rounded -> rounded
    Nothing -> n

-- | Round all numbers in an array
roundArray :: Int -> Array Number -> Array Number
roundArray decimals = map (roundNumber decimals)

-- FFI helper for parsing numbers
foreign import parseNumber :: String -> Maybe Number
