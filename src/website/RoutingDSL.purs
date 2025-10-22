module PSD3.RoutingDSL where

import Prelude hiding ((/))

import PSD3.Types (Route(..))
import Routing.Match (Match, lit, str, end)
import Routing.Match (root) as Match
import Control.Alt ((<|>))

-- | Routing DSL for matching URL paths to Routes
-- |
-- | This uses purescript-routing to provide cleaner URLs without hash fragments.
-- | Routes are matched top-to-bottom, first match wins.
routing :: Match Route
routing =
  Match.root *> routes

routes :: Match Route
routes =
  home
  <|> gallery
  <|> example
  <|> spago
  <|> interpreters
  <|> notFound

-- | Match: /
home :: Match Route
home = Home <$ end

-- | Match: /gallery
gallery :: Match Route
gallery = Gallery <$ lit "gallery" <* end

-- | Match: /example/:id
example :: Match Route
example =
  lit "example" *> (Example <$> str <* end)

-- | Match: /spago
spago :: Match Route
spago = Spago <$ lit "spago" <* end

-- | Match: /interpreters
interpreters :: Match Route
interpreters = Interpreters <$ lit "interpreters" <* end

-- | Fallback: everything else is NotFound
notFound :: Match Route
notFound = pure NotFound

-- | Convert a Route back to a URL path (for links and navigation)
-- |
-- | These paths no longer use hash fragments (#) - they are clean URLs
routeToPath :: Route -> String
routeToPath Home = "/"
routeToPath Gallery = "/gallery"
routeToPath (Example id) = "/example/" <> id
routeToPath Spago = "/spago"
routeToPath Interpreters = "/interpreters"
routeToPath NotFound = "/not-found"
