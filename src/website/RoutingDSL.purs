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
  <|> understandingFinallyTagless
  <|> understandingSelectionM
  <|> understandingCapabilities
  <|> understandingTypeSystem
  <|> understandingDatumPattern
  <|> understandingGrammar
  <|> understandingPhilosophy
  <|> understandingIndex
  <|> understandingConcepts   -- Legacy redirect to index
  <|> understandingPatterns   -- Legacy redirect to index
  <|> about                   -- Legacy redirect to philosophy
  <|> simpleCharts1
  <|> simpleCharts2
  <|> dataFlowViz
  <|> movement
  <|> hierarchies
  <|> interpreters
  <|> mermaidDiagrams
  <|> codeExplorer
  <|> explore
  <|> wealthHealth
  <|> codeAtlas
  <|> fpFtw
  <|> examplesGallery
  <|> example        -- Must come after examplesGallery (more specific)
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

-- | Match: /understanding (index)
understandingIndex :: Match Route
understandingIndex = UnderstandingIndex <$ lit "understanding" <* end

-- | Match: /understanding/finally-tagless
understandingFinallyTagless :: Match Route
understandingFinallyTagless = UnderstandingFinallyTagless <$ lit "understanding" <* lit "finally-tagless" <* end

-- | Match: /understanding/selectionm
understandingSelectionM :: Match Route
understandingSelectionM = UnderstandingSelectionM <$ lit "understanding" <* lit "selectionm" <* end

-- | Match: /understanding/capabilities
understandingCapabilities :: Match Route
understandingCapabilities = UnderstandingCapabilities <$ lit "understanding" <* lit "capabilities" <* end

-- | Match: /understanding/type-system
understandingTypeSystem :: Match Route
understandingTypeSystem = UnderstandingTypeSystem <$ lit "understanding" <* lit "type-system" <* end

-- | Match: /understanding/datum-pattern
understandingDatumPattern :: Match Route
understandingDatumPattern = UnderstandingDatumPattern <$ lit "understanding" <* lit "datum-pattern" <* end

-- | Match: /understanding/grammar
understandingGrammar :: Match Route
understandingGrammar = UnderstandingGrammar <$ lit "understanding" <* lit "grammar" <* end

-- | Match: /understanding/philosophy
understandingPhilosophy :: Match Route
understandingPhilosophy = UnderstandingPhilosophy <$ lit "understanding" <* lit "philosophy" <* end

-- | Match: /understanding/concepts (legacy redirect to index)
understandingConcepts :: Match Route
understandingConcepts = UnderstandingIndex <$ lit "understanding" <* lit "concepts" <* end

-- | Match: /understanding/patterns (legacy redirect to index)
understandingPatterns :: Match Route
understandingPatterns = UnderstandingIndex <$ lit "understanding" <* lit "patterns" <* end

-- | Match: /about (legacy redirect to philosophy)
about :: Match Route
about = UnderstandingPhilosophy <$ lit "about" <* end

-- | Match: /simple-charts-1
simpleCharts1 :: Match Route
simpleCharts1 = SimpleCharts1 <$ lit "simple-charts-1" <* end

-- | Match: /simple-charts-2
simpleCharts2 :: Match Route
simpleCharts2 = SimpleCharts2 <$ lit "simple-charts-2" <* end

-- | Match: /data-flow
dataFlowViz :: Match Route
dataFlowViz = DataFlowViz <$ lit "data-flow" <* end

-- | Match: /movement
movement :: Match Route
movement = Movement <$ lit "movement" <* end

-- | Match: /hierarchies
hierarchies :: Match Route
hierarchies = Hierarchies <$ lit "hierarchies" <* end

-- | Match: /interpreters
interpreters :: Match Route
interpreters = Interpreters <$ lit "interpreters" <* end

-- | Match: /mermaid-diagrams
mermaidDiagrams :: Match Route
mermaidDiagrams = MermaidDiagrams <$ lit "mermaid-diagrams" <* end

-- | Match: /code-explorer
codeExplorer :: Match Route
codeExplorer = CodeExplorer <$ lit "code-explorer" <* end

-- | Match: /explore/:snippetId
explore :: Match Route
explore = Explore <$> (lit "explore" *> str) <* end

-- | Match: /wealth-health
wealthHealth :: Match Route
wealthHealth = WealthHealth <$ lit "wealth-health" <* end

-- | Match: /code-atlas
codeAtlas :: Match Route
codeAtlas = CodeAtlas <$ lit "code-atlas" <* end

-- | Match: /fp-ftw
fpFtw :: Match Route
fpFtw = FpFtw <$ lit "fp-ftw" <* end

-- | Match: /examples
examplesGallery :: Match Route
examplesGallery = ExamplesGallery <$ lit "examples" <* end

-- | Match: /example/:exampleId
example :: Match Route
example = Example <$> (lit "example" *> str) <* end

-- | Match: /acknowledgements
acknowledgements :: Match Route
acknowledgements = Acknowledgements <$ lit "acknowledgements" <* end

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
routeToPath UnderstandingIndex = "/understanding"
routeToPath UnderstandingFinallyTagless = "/understanding/finally-tagless"
routeToPath UnderstandingSelectionM = "/understanding/selectionm"
routeToPath UnderstandingCapabilities = "/understanding/capabilities"
routeToPath UnderstandingTypeSystem = "/understanding/type-system"
routeToPath UnderstandingDatumPattern = "/understanding/datum-pattern"
routeToPath UnderstandingGrammar = "/understanding/grammar"
routeToPath UnderstandingPhilosophy = "/understanding/philosophy"
-- Legacy redirects
routeToPath UnderstandingConcepts = "/understanding"
routeToPath UnderstandingPatterns = "/understanding"
routeToPath SimpleCharts1 = "/simple-charts-1"
routeToPath SimpleCharts2 = "/simple-charts-2"
routeToPath DataFlowViz = "/data-flow"
routeToPath Movement = "/movement"
routeToPath Hierarchies = "/hierarchies"
routeToPath Interpreters = "/interpreters"
routeToPath MermaidDiagrams = "/mermaid-diagrams"
routeToPath CodeExplorer = "/code-explorer"
routeToPath (Explore snippetId) = "/explore/" <> snippetId
routeToPath WealthHealth = "/wealth-health"
routeToPath CodeAtlas = "/code-atlas"
routeToPath FpFtw = "/fp-ftw"
routeToPath ExamplesGallery = "/examples"
routeToPath (Example exampleId) = "/example/" <> exampleId
routeToPath Acknowledgements = "/acknowledgements"
routeToPath NotFound = "/not-found"
