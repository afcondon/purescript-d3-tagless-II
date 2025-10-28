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
  <|> understandingConcepts
  <|> understandingPatterns
  <|> understandingPhilosophy
  <|> about       -- Legacy redirect to philosophy
  <|> tutorial
  <|> simpleCharts
  <|> dataFlowViz
  <|> hierarchies
  <|> interpreters
  <|> codeExplorer
  <|> explore
  <|> wealthHealth
  <|> lesMiserables
  <|> codeAtlas
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

-- | Match: /understanding/concepts
understandingConcepts :: Match Route
understandingConcepts = UnderstandingConcepts <$ lit "understanding" <* lit "concepts" <* end

-- | Match: /understanding/patterns
understandingPatterns :: Match Route
understandingPatterns = UnderstandingPatterns <$ lit "understanding" <* lit "patterns" <* end

-- | Match: /understanding/philosophy
understandingPhilosophy :: Match Route
understandingPhilosophy = UnderstandingPhilosophy <$ lit "understanding" <* lit "philosophy" <* end

-- | Match: /about (legacy redirect to philosophy)
about :: Match Route
about = UnderstandingPhilosophy <$ lit "about" <* end

-- | Match: /tutorial
tutorial :: Match Route
tutorial = Tutorial <$ lit "tutorial" <* end

-- | Match: /simple-charts
simpleCharts :: Match Route
simpleCharts = SimpleCharts <$ lit "simple-charts" <* end

-- | Match: /data-flow
dataFlowViz :: Match Route
dataFlowViz = DataFlowViz <$ lit "data-flow" <* end

-- | Match: /hierarchies
hierarchies :: Match Route
hierarchies = Hierarchies <$ lit "hierarchies" <* end

-- | Match: /interpreters
interpreters :: Match Route
interpreters = Interpreters <$ lit "interpreters" <* end

-- | Match: /code-explorer
codeExplorer :: Match Route
codeExplorer = CodeExplorer <$ lit "code-explorer" <* end

-- | Match: /explore/:snippetId
explore :: Match Route
explore = Explore <$> (lit "explore" *> str) <* end

-- | Match: /wealth-health
wealthHealth :: Match Route
wealthHealth = WealthHealth <$ lit "wealth-health" <* end

-- | Match: /les-miserables
lesMiserables :: Match Route
lesMiserables = LesMiserables <$ lit "les-miserables" <* end

-- | Match: /code-atlas
codeAtlas :: Match Route
codeAtlas = CodeAtlas <$ lit "code-atlas" <* end

-- | Fallback: everything else is NotFound
notFound :: Match Route
notFound = pure NotFound

-- | Convert a Route back to a URL path (for links and navigation)
-- |
-- | These paths are used with hash-based routing (e.g., /#/about, /#/tutorial)
routeToPath :: Route -> String
routeToPath Home = "/home"
routeToPath GettingStarted = "/getting-started"
routeToPath Wizard = "/wizard"
routeToPath HowtoIndex = "/howto"
routeToPath Reference = "/reference"
routeToPath (ReferenceModule moduleName) = "/reference/" <> moduleName
routeToPath About = "/understanding/philosophy"  -- Redirect to philosophy
routeToPath UnderstandingConcepts = "/understanding/concepts"
routeToPath UnderstandingPatterns = "/understanding/patterns"
routeToPath UnderstandingPhilosophy = "/understanding/philosophy"
routeToPath Tutorial = "/tutorial"
routeToPath SimpleCharts = "/simple-charts"
routeToPath DataFlowViz = "/data-flow"
routeToPath Hierarchies = "/hierarchies"
routeToPath Interpreters = "/interpreters"
routeToPath CodeExplorer = "/code-explorer"
routeToPath (Explore snippetId) = "/explore/" <> snippetId
routeToPath WealthHealth = "/wealth-health"
routeToPath LesMiserables = "/les-miserables"
routeToPath CodeAtlas = "/code-atlas"
routeToPath NotFound = "/not-found"
