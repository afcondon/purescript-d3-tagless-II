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
  <|> tourFoundations
  <|> tourProfessional
  <|> tourFlow
  <|> tourHierarchies
  <|> tourMotion
  <|> tourInterpreters
  <|> tourFPFTW
  <|> tourGraphAlgorithms
  <|> tourLesMisGUP
  <|> tourShowcase
  <|> gallery
  <|> examplePage     -- Must come before other routes to match /example/:id
  <|> treeAPI
  <|> animatedTreeCluster
  <|> generalUpdatePattern
  <|> lesMisGUPTree
  <|> lesMisGUP
  <|> lesMisGUPSimple
  <|> lesMisGUPClean
  <|> lesMisQueryDemo
  <|> moduleGraph
  <|> mermaidTreeDemo
  <|> sceneJoinDemo
  <|> codeExplorer
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

-- | Match: /tour/showcase
tourShowcase :: Match Route
tourShowcase = TourShowcase <$ lit "tour" <* lit "showcase" <* end

-- | Match: /gallery
gallery :: Match Route
gallery = Gallery <$ lit "gallery" <* end

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

-- | Match: /code-explorer
codeExplorer :: Match Route
codeExplorer = CodeExplorer <$ lit "code-explorer" <* end

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
routeToPath TourFoundations = "/tour/foundations"
routeToPath TourProfessional = "/tour/professional"
routeToPath TourFlow = "/tour/flow"
routeToPath TourHierarchies = "/tour/hierarchies"
routeToPath TourMotion = "/tour/motion"
routeToPath TourInterpreters = "/tour/interpreters"
routeToPath TourFPFTW = "/tour/fpftw"
routeToPath TourGraphAlgorithms = "/tour/graph-algorithms"
routeToPath TourLesMisGUP = "/tour/lesmis-gup"
routeToPath TourShowcase = "/tour/showcase"
routeToPath Gallery = "/gallery"
routeToPath (Example exampleId) = "/example/" <> exampleId
routeToPath TreeAPI = "/tree-api"
routeToPath AnimatedTreeCluster = "/animated-tree-cluster"
routeToPath GeneralUpdatePattern = "/general-update-pattern"
routeToPath LesMisGUPTree = "/lesmis-gup-tree"
routeToPath LesMisGUP = "/lesmis-gup"
routeToPath LesMisGUPSimple = "/lesmis-gup-simple"
routeToPath LesMisGUPClean = "/lesmis-gup-clean"
routeToPath LesMisQueryDemo = "/lesmis-query-demo"
routeToPath ModuleGraph = "/module-graph"
routeToPath MermaidTreeDemo = "/mermaid-tree-demo"
routeToPath SceneJoinDemo = "/scene-join-demo"
routeToPath CodeExplorer = "/code-explorer"
routeToPath Acknowledgements = "/acknowledgements"
routeToPath NotFound = "/not-found"
