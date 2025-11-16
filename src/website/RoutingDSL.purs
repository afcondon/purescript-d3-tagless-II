module PSD3.RoutingDSL where

import Prelude hiding ((/))

import PSD3.Website.Types (Route(..))
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
  home            -- Explicit home route
  <|> gettingStarted  -- Must come early - specific routes before general
  <|> wizard
  <|> howtoIndex
  <|> referenceModule  -- Must come before reference (more specific)
  <|> reference
  <|> psd3v2Examples
  <|> treeAPI
  <|> lesMisGUPTree
  <|> forceNavigator
  <|> codeExplorer
  <|> explore
  <|> wealthHealth
  <|> fpFtw
  <|> acknowledgements
  <|> rootRedirect
  <|> notFound

-- | Match: /home (landing page)
home :: Match Route
home = Home <$ lit "home" <* end

-- | Match: / (redirect to home)
rootRedirect :: Match Route
rootRedirect = Home <$ end

-- | Match: /getting-started
gettingStarted :: Match Route
gettingStarted = GettingStarted <$ lit "getting-started" <* end

-- | Match: /wizard
wizard :: Match Route
wizard = Wizard <$ lit "wizard" <* end

-- | Match: /howto
howtoIndex :: Match Route
howtoIndex = HowtoIndex <$ lit "howto" <* end

-- | Match: /reference/:moduleName
referenceModule :: Match Route
referenceModule = ReferenceModule <$> (lit "reference" *> str) <* end

-- | Match: /reference
reference :: Match Route
reference = Reference <$ lit "reference" <* end

-- | Match: /psd3v2
psd3v2Examples :: Match Route
psd3v2Examples = PSD3v2Examples <$ lit "psd3v2" <* end

-- | Match: /tree-api
treeAPI :: Match Route
treeAPI = TreeAPI <$ lit "tree-api" <* end

-- | Match: /lesmis-gup-tree
lesMisGUPTree :: Match Route
lesMisGUPTree = LesMisGUPTree <$ lit "lesmis-gup-tree" <* end

-- | Match: /force-navigator
forceNavigator :: Match Route
forceNavigator = ForceNavigator <$ lit "force-navigator" <* end

-- | Match: /code-explorer
codeExplorer :: Match Route
codeExplorer = CodeExplorer <$ lit "code-explorer" <* end

-- | Match: /explore/:snippetId
explore :: Match Route
explore = Explore <$> (lit "explore" *> str) <* end

-- | Match: /wealth-health
wealthHealth :: Match Route
wealthHealth = WealthHealth <$ lit "wealth-health" <* end

-- | Match: /fp-ftw
fpFtw :: Match Route
fpFtw = FpFtw <$ lit "fp-ftw" <* end

-- | Match: /acknowledgements
acknowledgements :: Match Route
acknowledgements = Acknowledgements <$ lit "acknowledgements" <* end

-- | Fallback: everything else is NotFound
notFound :: Match Route
notFound = pure NotFound

-- | Convert a Route back to a URL path (for links and navigation)
-- |
-- | These paths are used with hash-based routing (e.g., /#/home, /#/psd3v2)
routeToPath :: Route -> String
routeToPath Home = "/home"
routeToPath GettingStarted = "/getting-started"
routeToPath Wizard = "/wizard"
routeToPath HowtoIndex = "/howto"
routeToPath Reference = "/reference"
routeToPath (ReferenceModule moduleName) = "/reference/" <> moduleName
routeToPath PSD3v2Examples = "/psd3v2"
routeToPath TreeAPI = "/tree-api"
routeToPath LesMisGUPTree = "/lesmis-gup-tree"
routeToPath ForceNavigator = "/force-navigator"
routeToPath CodeExplorer = "/code-explorer"
routeToPath (Explore snippetId) = "/explore/" <> snippetId
routeToPath WealthHealth = "/wealth-health"
routeToPath FpFtw = "/fp-ftw"
routeToPath Acknowledgements = "/acknowledgements"
routeToPath NotFound = "/not-found"
