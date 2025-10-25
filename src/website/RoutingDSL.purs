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
  gettingStarted  -- Must come early - specific routes before general
  <|> howtoIndex
  <|> reference
  <|> tutorial
  <|> simpleCharts
  <|> chordDiagram
  <|> bubbleChart
  <|> sankeyDiagram
  <|> hierarchies
  <|> interpreters
  <|> codeExplorer
  <|> explore
  <|> about
  <|> rootRedirect
  <|> notFound

-- | Match: / (home/landing page)
rootRedirect :: Match Route
rootRedirect = Home <$ end

-- | Match: /getting-started
gettingStarted :: Match Route
gettingStarted = GettingStarted <$ lit "getting-started" <* end

-- | Match: /howto
howtoIndex :: Match Route
howtoIndex = HowtoIndex <$ lit "howto" <* end

-- | Match: /reference
reference :: Match Route
reference = Reference <$ lit "reference" <* end

-- | Match: /about
about :: Match Route
about = About <$ lit "about" <* end

-- | Match: /tutorial
tutorial :: Match Route
tutorial = Tutorial <$ lit "tutorial" <* end

-- | Match: /simple-charts
simpleCharts :: Match Route
simpleCharts = SimpleCharts <$ lit "simple-charts" <* end

-- | Match: /chord-diagram
chordDiagram :: Match Route
chordDiagram = ChordDiagram <$ lit "chord-diagram" <* end

-- | Match: /bubble-chart
bubbleChart :: Match Route
bubbleChart = BubbleChart <$ lit "bubble-chart" <* end

-- | Match: /sankey-diagram
sankeyDiagram :: Match Route
sankeyDiagram = SankeyDiagram <$ lit "sankey-diagram" <* end

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

-- | Fallback: everything else is NotFound
notFound :: Match Route
notFound = pure NotFound

-- | Convert a Route back to a URL path (for links and navigation)
-- |
-- | These paths are used with hash-based routing (e.g., /#/about, /#/tutorial)
routeToPath :: Route -> String
routeToPath Home = "/"
routeToPath GettingStarted = "/getting-started"
routeToPath HowtoIndex = "/howto"
routeToPath Reference = "/reference"
routeToPath About = "/about"
routeToPath Tutorial = "/tutorial"
routeToPath SimpleCharts = "/simple-charts"
routeToPath ChordDiagram = "/chord-diagram"
routeToPath BubbleChart = "/bubble-chart"
routeToPath SankeyDiagram = "/sankey-diagram"
routeToPath Hierarchies = "/hierarchies"
routeToPath Interpreters = "/interpreters"
routeToPath CodeExplorer = "/code-explorer"
routeToPath (Explore snippetId) = "/explore/" <> snippetId
routeToPath NotFound = "/not-found"
