module PSD3.Reference.SourceLoader where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Effect.Aff (Aff)

-- | Load a PureScript source file from docs/sources/
-- | Given a relative path like "PSD3/Types.purs", fetches from "sources/PSD3/Types.purs"
loadSourceFile :: String -> Aff (Either String String)
loadSourceFile relativePath = do
  let url = "sources/" <> relativePath
  result <- AX.get ResponseFormat.string url
  pure case result of
    Left err ->
      Left $ "Failed to load " <> relativePath <> ": " <> AX.printError err
    Right response ->
      Right response.body
