module V2.Router where

import Prelude

import V2.Types (Route(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, drop, take)
import Data.String.CodeUnits (length) as String
import Data.Array (index)

-- | Parse a hash string into a Route
parseRoute :: String -> Route
parseRoute hash =
  let -- Strip leading # if present
      path = if String.length hash > 0 && take 1 hash == "#"
             then drop 1 hash
             else hash
      -- Strip leading / if present
      cleanPath = if String.length path > 0 && take 1 path == "/"
                  then drop 1 path
                  else path
      segments = split (Pattern "/") cleanPath
  in case index segments 0 of
    Nothing -> Home
    Just "" -> Home
    Just "gallery" -> Gallery
    Just "example" -> case index segments 1 of
      Just id -> Example id
      Nothing -> Gallery
    Just "spago" -> Spago
    Just "interpreters" -> Interpreters
    _ -> NotFound

-- | Convert a Route to a hash string
routeToHash :: Route -> String
routeToHash Home = "#/"
routeToHash Gallery = "#/gallery"
routeToHash (Example id) = "#/example/" <> id
routeToHash Spago = "#/spago"
routeToHash Interpreters = "#/interpreters"
routeToHash NotFound = "#/not-found"

-- | Get a user-friendly title for a route
routeTitle :: Route -> String
routeTitle Home = "PureScript Tagless D3"
routeTitle Gallery = "Examples Gallery"
routeTitle (Example _) = "Example"
routeTitle Spago = "Spago Dependency Explorer"
routeTitle Interpreters = "Alternative Interpreters"
routeTitle NotFound = "Not Found"
