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
  tutorial        -- Must come before 'about' because it's more specific
  <|> hierarchies
  <|> interpreters
  <|> codeExplorer
  <|> about
  <|> rootRedirect
  <|> notFound

-- | Match: /about
about :: Match Route
about = About <$ lit "about" <* end

-- | Match: / (redirect to /about)
rootRedirect :: Match Route
rootRedirect = About <$ end

-- | Match: /tutorial
tutorial :: Match Route
tutorial = Tutorial <$ lit "tutorial" <* end

-- | Match: /hierarchies
hierarchies :: Match Route
hierarchies = Hierarchies <$ lit "hierarchies" <* end

-- | Match: /interpreters
interpreters :: Match Route
interpreters = Interpreters <$ lit "interpreters" <* end

-- | Match: /code-explorer
codeExplorer :: Match Route
codeExplorer = CodeExplorer <$ lit "code-explorer" <* end

-- | Fallback: everything else is NotFound
notFound :: Match Route
notFound = pure NotFound

-- | Convert a Route back to a URL path (for links and navigation)
-- |
-- | These paths no longer use hash fragments (#) - they are clean URLs
routeToPath :: Route -> String
routeToPath About = "/about"
routeToPath Tutorial = "/tutorial"
routeToPath Hierarchies = "/hierarchies"
routeToPath Interpreters = "/interpreters"
routeToPath CodeExplorer = "/code-explorer"
routeToPath NotFound = "/not-found"
