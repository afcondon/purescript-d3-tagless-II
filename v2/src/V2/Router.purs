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
  let path = if String.length hash > 0 && take 1 hash == "#"
             then drop 1 hash
             else hash
      segments = split (Pattern "/") path
  in case index segments 0 of
    Nothing -> Home
    Just "" -> Home
    Just "gallery" -> Gallery
    Just "example" -> case index segments 1 of
      Just id -> Example id
      Nothing -> Gallery
    _ -> NotFound

-- | Convert a Route to a hash string
routeToHash :: Route -> String
routeToHash Home = "#/"
routeToHash Gallery = "#/gallery"
routeToHash (Example id) = "#/example/" <> id
routeToHash NotFound = "#/not-found"

-- | Get a user-friendly title for a route
routeTitle :: Route -> String
routeTitle Home = "PureScript Tagless D3"
routeTitle Gallery = "Examples Gallery"
routeTitle (Example _) = "Example"
routeTitle NotFound = "Not Found"
