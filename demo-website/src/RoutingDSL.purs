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
  <|> howtoTransitions      -- Must come before howtoIndex check (more specific)
  <|> howtoForceGraphs
  <|> howtoHierarchical
  <|> howtoEvents
  <|> howtoTreeAPI
  <|> howtoLoadingData
  <|> howtoAxesScales
  <|> howtoTooltips
  <|> howtoDebugging
  <|> howtoPerformance
  <|> howtoTreeExplorer
  <|> understandingGrammar  -- Must come before understanding (more specific)
  <|> understandingAttributes
  <|> understandingSelections
  <|> understandingTreeAPI
  <|> understandingScenes
  <|> understanding
  <|> referenceModule  -- Must come before reference (more specific)
  <|> reference
  <|> tourFoundations
  <|> tourProfessional
  <|> tourFlow
  <|> tourHierarchies
  <|> tourMotion
  <|> tourWealthHealth
  <|> tourInterpreters
  <|> tourFPFTW
  <|> tourGraphAlgorithms
  <|> tourLesMisGUP
  <|> tourSimpsons
  <|> tourIndex
  <|> showcase
  <|> examples
  <|> examplePage     -- Must come before other routes to match /example/:id
  <|> treeAPI
  <|> animatedTreeCluster
  <|> generalUpdatePattern
  <|> lesMisGUPTree
  <|> lesMisGUP
  <|> lesMisGUPSimple
  <|> lesMisGUPTreeAPI
  <|> lesMisGUPClean
  <|> lesMisQueryDemo
  <|> moduleGraph
  <|> mermaidTreeDemo
  <|> sceneJoinDemo
  <|> forceConfigPOC
  <|> forceConfigV2Test
  <|> simpleForceGraphRoute
  <|> sankeyDebug
  <|> forcePlayground
  <|> treeBuilder
  <|> treeBuilder2
  <|> splom
  <|> gupDebug
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

-- | Match: /howto/transitions
howtoTransitions :: Match Route
howtoTransitions = HowtoTransitions <$ lit "howto" <* lit "transitions" <* end

-- | Match: /howto/force-graphs
howtoForceGraphs :: Match Route
howtoForceGraphs = HowtoForceGraphs <$ lit "howto" <* lit "force-graphs" <* end

-- | Match: /howto/hierarchical
howtoHierarchical :: Match Route
howtoHierarchical = HowtoHierarchical <$ lit "howto" <* lit "hierarchical" <* end

-- | Match: /howto/events
howtoEvents :: Match Route
howtoEvents = HowtoEvents <$ lit "howto" <* lit "events" <* end

-- | Match: /howto/tree-api
howtoTreeAPI :: Match Route
howtoTreeAPI = HowtoTreeAPI <$ lit "howto" <* lit "tree-api" <* end

-- | Match: /howto/loading-data
howtoLoadingData :: Match Route
howtoLoadingData = HowtoLoadingData <$ lit "howto" <* lit "loading-data" <* end

-- | Match: /howto/axes-scales
howtoAxesScales :: Match Route
howtoAxesScales = HowtoAxesScales <$ lit "howto" <* lit "axes-scales" <* end

-- | Match: /howto/tooltips
howtoTooltips :: Match Route
howtoTooltips = HowtoTooltips <$ lit "howto" <* lit "tooltips" <* end

-- | Match: /howto/debugging
howtoDebugging :: Match Route
howtoDebugging = HowtoDebugging <$ lit "howto" <* lit "debugging" <* end

-- | Match: /howto/performance
howtoPerformance :: Match Route
howtoPerformance = HowtoPerformance <$ lit "howto" <* lit "performance" <* end

-- | Match: /howto/tree-explorer
howtoTreeExplorer :: Match Route
howtoTreeExplorer = HowtoTreeExplorer <$ lit "howto" <* lit "tree-explorer" <* end

-- | Match: /understanding
understanding :: Match Route
understanding = Understanding <$ lit "understanding" <* end

-- | Match: /understanding/grammar
understandingGrammar :: Match Route
understandingGrammar = UnderstandingGrammar <$ lit "understanding" <* lit "grammar" <* end

-- | Match: /understanding/attributes
understandingAttributes :: Match Route
understandingAttributes = UnderstandingAttributes <$ lit "understanding" <* lit "attributes" <* end

-- | Match: /understanding/selections
understandingSelections :: Match Route
understandingSelections = UnderstandingSelections <$ lit "understanding" <* lit "selections" <* end

-- | Match: /understanding/tree-api
understandingTreeAPI :: Match Route
understandingTreeAPI = UnderstandingTreeAPI <$ lit "understanding" <* lit "tree-api" <* end

-- | Match: /understanding/scenes
understandingScenes :: Match Route
understandingScenes = UnderstandingScenes <$ lit "understanding" <* lit "scenes" <* end

-- | Match: /reference/:moduleName
referenceModule :: Match Route
referenceModule = ReferenceModule <$> (lit "reference" *> str) <* end

-- | Match: /reference
reference :: Match Route
reference = Reference <$ lit "reference" <* end

-- | Match: /tour/foundations
tourFoundations :: Match Route
tourFoundations = TourFoundations <$ lit "tour" <* lit "foundations" <* end

-- | Match: /tour/professional
tourProfessional :: Match Route
tourProfessional = TourProfessional <$ lit "tour" <* lit "professional" <* end

-- | Match: /tour/flow
tourFlow :: Match Route
tourFlow = TourFlow <$ lit "tour" <* lit "flow" <* end

-- | Match: /tour/hierarchies
tourHierarchies :: Match Route
tourHierarchies = TourHierarchies <$ lit "tour" <* lit "hierarchies" <* end

-- | Match: /tour/motion
tourMotion :: Match Route
tourMotion = TourMotion <$ lit "tour" <* lit "motion" <* end

-- | Match: /tour/wealth-health
tourWealthHealth :: Match Route
tourWealthHealth = TourWealthHealth <$ lit "tour" <* lit "wealth-health" <* end

-- | Match: /tour/interpreters
tourInterpreters :: Match Route
tourInterpreters = TourInterpreters <$ lit "tour" <* lit "interpreters" <* end

-- | Match: /tour/fpftw
tourFPFTW :: Match Route
tourFPFTW = TourFPFTW <$ lit "tour" <* lit "fpftw" <* end

-- | Match: /tour/graph-algorithms
tourGraphAlgorithms :: Match Route
tourGraphAlgorithms = TourGraphAlgorithms <$ lit "tour" <* lit "graph-algorithms" <* end

-- | Match: /tour/lesmis-gup
tourLesMisGUP :: Match Route
tourLesMisGUP = TourLesMisGUP <$ lit "tour" <* lit "lesmis-gup" <* end

-- | Match: /tour/simpsons
tourSimpsons :: Match Route
tourSimpsons = TourSimpsons <$ lit "tour" <* lit "simpsons" <* end

-- | Match: /tour (index page - must come after specific tour routes)
tourIndex :: Match Route
tourIndex = TourIndex <$ lit "tour" <* end

-- | Match: /showcase
showcase :: Match Route
showcase = Showcase <$ lit "showcase" <* end

-- | Match: /examples
examples :: Match Route
examples = Examples <$ lit "examples" <* end

-- | Match: /example/:exampleId
examplePage :: Match Route
examplePage = Example <$> (lit "example" *> str) <* end

-- | Match: /tree-api
treeAPI :: Match Route
treeAPI = TreeAPI <$ lit "tree-api" <* end

-- | Match: /animated-tree-cluster
animatedTreeCluster :: Match Route
animatedTreeCluster = AnimatedTreeCluster <$ lit "animated-tree-cluster" <* end

-- | Match: /general-update-pattern
generalUpdatePattern :: Match Route
generalUpdatePattern = GeneralUpdatePattern <$ lit "general-update-pattern" <* end

-- | Match: /lesmis-gup-tree
lesMisGUPTree :: Match Route
lesMisGUPTree = LesMisGUPTree <$ lit "lesmis-gup-tree" <* end

-- | Match: /lesmis-gup
lesMisGUP :: Match Route
lesMisGUP = LesMisGUP <$ lit "lesmis-gup" <* end

-- | Match: /lesmis-gup-simple
lesMisGUPSimple :: Match Route
lesMisGUPSimple = LesMisGUPSimple <$ lit "lesmis-gup-simple" <* end

-- | Match: /lesmis-gup-treeapi
lesMisGUPTreeAPI :: Match Route
lesMisGUPTreeAPI = LesMisGUPTreeAPI <$ lit "lesmis-gup-treeapi" <* end

-- | Match: /lesmis-gup-clean
lesMisGUPClean :: Match Route
lesMisGUPClean = LesMisGUPClean <$ lit "lesmis-gup-clean" <* end

-- | Match: /lesmis-query-demo
lesMisQueryDemo :: Match Route
lesMisQueryDemo = LesMisQueryDemo <$ lit "lesmis-query-demo" <* end

-- | Match: /module-graph
moduleGraph :: Match Route
moduleGraph = ModuleGraph <$ lit "module-graph" <* end

-- | Match: /mermaid-tree-demo
mermaidTreeDemo :: Match Route
mermaidTreeDemo = MermaidTreeDemo <$ lit "mermaid-tree-demo" <* end

-- | Match: /scene-join-demo
sceneJoinDemo :: Match Route
sceneJoinDemo = SceneJoinDemo <$ lit "scene-join-demo" <* end

-- | Match: /force-config-poc
forceConfigPOC :: Match Route
forceConfigPOC = ForceConfigPOC <$ lit "force-config-poc" <* end

-- | Match: /force-config-v2-test
forceConfigV2Test :: Match Route
forceConfigV2Test = ForceConfigV2Test <$ lit "force-config-v2-test" <* end

-- | Match: /simple-force-graph
simpleForceGraphRoute :: Match Route
simpleForceGraphRoute = SimpleForceGraph <$ lit "simple-force-graph" <* end

-- | Match: /sankey-debug
sankeyDebug :: Match Route
sankeyDebug = SankeyDebug <$ lit "sankey-debug" <* end

-- | Match: /force-playground
forcePlayground :: Match Route
forcePlayground = ForcePlayground <$ lit "force-playground" <* end

-- | Match: /tree-builder
treeBuilder :: Match Route
treeBuilder = TreeBuilder <$ lit "tree-builder" <* end

-- | Match: /tree-builder2
treeBuilder2 :: Match Route
treeBuilder2 = TreeBuilder2 <$ lit "tree-builder2" <* end

-- | Match: /splom
splom :: Match Route
splom = SPLOM <$ lit "splom" <* end

-- | Match: /gup-debug
gupDebug :: Match Route
gupDebug = GUPDebug <$ lit "gup-debug" <* end

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
routeToPath HowtoTransitions = "/howto/transitions"
routeToPath HowtoForceGraphs = "/howto/force-graphs"
routeToPath HowtoHierarchical = "/howto/hierarchical"
routeToPath HowtoEvents = "/howto/events"
routeToPath HowtoTreeAPI = "/howto/tree-api"
routeToPath HowtoLoadingData = "/howto/loading-data"
routeToPath HowtoAxesScales = "/howto/axes-scales"
routeToPath HowtoTooltips = "/howto/tooltips"
routeToPath HowtoDebugging = "/howto/debugging"
routeToPath HowtoPerformance = "/howto/performance"
routeToPath HowtoTreeExplorer = "/howto/tree-explorer"
routeToPath Understanding = "/understanding"
routeToPath UnderstandingGrammar = "/understanding/grammar"
routeToPath UnderstandingAttributes = "/understanding/attributes"
routeToPath UnderstandingSelections = "/understanding/selections"
routeToPath UnderstandingTreeAPI = "/understanding/tree-api"
routeToPath UnderstandingScenes = "/understanding/scenes"
routeToPath Reference = "/reference"
routeToPath (ReferenceModule moduleName) = "/reference/" <> moduleName
routeToPath TourIndex = "/tour"
routeToPath TourFoundations = "/tour/foundations"
routeToPath TourProfessional = "/tour/professional"
routeToPath TourFlow = "/tour/flow"
routeToPath TourHierarchies = "/tour/hierarchies"
routeToPath TourMotion = "/tour/motion"
routeToPath TourWealthHealth = "/tour/wealth-health"
routeToPath TourInterpreters = "/tour/interpreters"
routeToPath TourFPFTW = "/tour/fpftw"
routeToPath TourGraphAlgorithms = "/tour/graph-algorithms"
routeToPath TourLesMisGUP = "/tour/lesmis-gup"
routeToPath TourSimpsons = "/tour/simpsons"
routeToPath Showcase = "/showcase"
routeToPath Examples = "/examples"
routeToPath (Example exampleId) = "/example/" <> exampleId
routeToPath TreeAPI = "/tree-api"
routeToPath AnimatedTreeCluster = "/animated-tree-cluster"
routeToPath GeneralUpdatePattern = "/general-update-pattern"
routeToPath LesMisGUPTree = "/lesmis-gup-tree"
routeToPath LesMisGUP = "/lesmis-gup"
routeToPath LesMisGUPSimple = "/lesmis-gup-simple"
routeToPath LesMisGUPTreeAPI = "/lesmis-gup-treeapi"
routeToPath LesMisGUPClean = "/lesmis-gup-clean"
routeToPath LesMisQueryDemo = "/lesmis-query-demo"
routeToPath ModuleGraph = "/module-graph"
routeToPath MermaidTreeDemo = "/mermaid-tree-demo"
routeToPath SceneJoinDemo = "/scene-join-demo"
routeToPath ForceConfigPOC = "/force-config-poc"
routeToPath ForceConfigV2Test = "/force-config-v2-test"
routeToPath SimpleForceGraph = "/simple-force-graph"
routeToPath SankeyDebug = "/sankey-debug"
routeToPath ForcePlayground = "/force-playground"
routeToPath TreeBuilder = "/tree-builder"
routeToPath TreeBuilder2 = "/tree-builder2"
routeToPath SPLOM = "/splom"
routeToPath GUPDebug = "/gup-debug"
routeToPath Acknowledgements = "/acknowledgements"
routeToPath NotFound = "/not-found"
