module Snippets where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Effect.Aff (Aff)

readSnippetFiles :: String -> Aff String
readSnippetFiles name = do
  response <- AJAX.get ResponseFormat.string $ "http://localhost:1234/code-examples/" <> name <> ".txt"
  case response of
    (Left _) -> pure ""
    (Right r) -> pure r.body
