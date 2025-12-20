module PSD3.Website.Types where

import Prelude

-- | Unique identifier for an example
type ExampleId = String

-- | Difficulty level of an example
data Difficulty
  = Beginner
  | Intermediate
  | Advanced

derive instance eqDifficulty :: Eq Difficulty
derive instance ordDifficulty :: Ord Difficulty

instance showDifficulty :: Show Difficulty where
  show Beginner = "Beginner"
  show Intermediate = "Intermediate"
  show Advanced = "Advanced"

difficultyToString :: Difficulty -> String
difficultyToString = show

difficultyEmoji :: Difficulty -> String
difficultyEmoji Beginner = "🟢"
difficultyEmoji Intermediate = "🟡"
difficultyEmoji Advanced = "🔴"

-- | Category of visualization
data Category
  = BasicChart
  | AdvancedLayout
  | Interactive
  | Interpreter
  | Application

derive instance eqCategory :: Eq Category
derive instance ordCategory :: Ord Category

instance showCategory :: Show Category where
  show BasicChart = "Basic Charts"
  show AdvancedLayout = "Advanced Layouts"
  show Interactive = "Interactive"
  show Interpreter = "Alternative Interpreters"
  show Application = "Applications"

categoryToString :: Category -> String
categoryToString = show

-- | Main documentation sections
data Section
  = UnderstandingSection  -- Explanation/concept pages
  | TutorialSection       -- Getting started tutorial
  | HowToSection          -- Step-by-step guides
  | APISection            -- API documentation

derive instance eqSection :: Eq Section
derive instance ordSection :: Ord Section

instance showSection :: Show Section where
  show UnderstandingSection = "Understanding"
  show TutorialSection = "Tutorial"
  show HowToSection = "How-To"
  show APISection = "API"

-- | Metadata for a single example
type ExampleMetadata = {
  id :: ExampleId
, title :: String
, description :: String
, about :: String  -- Detailed explanation of the example
, difficulty :: Difficulty
, category :: Category
, tags :: Array String
, thumbnail :: String
, hasInteractivity :: Boolean
, hasComparison :: Boolean
}

-- | Route in the application
data Route
  = Home            -- Landing page
  | GettingStarted  -- Tutorial: installation, setup, first project (TODO: update for v2)
  | Wizard          -- Interactive wizard for scaffolding visualizations (TODO: update for v2)
  | HowtoIndex      -- How-to: index of all step-by-step guides (TODO: update for v2)
  -- How-to sub-pages
  | HowtoTransitions      -- Creating animated transitions
  | HowtoForceGraphs      -- Building force-directed graphs
  | HowtoHierarchical     -- Working with hierarchical data
  | HowtoEvents           -- Responding to user events
  | HowtoTreeAPI          -- Using the TreeAPI
  | HowtoLoadingData      -- Loading external data
  | HowtoAxesScales       -- Creating axes and scales
  | HowtoTooltips         -- Adding tooltips
  | HowtoDebugging        -- Debugging visualizations
  | HowtoPerformance      -- Performance optimization
  | HowtoTreeExplorer     -- Interactive tree layout explorer
  | Understanding   -- Conceptual overview: index page
  -- Understanding sub-pages
  | UnderstandingGrammar       -- Grammar of D3 in PSD3
  | UnderstandingAttributes    -- Type-safe attribute system with phantom types
  | UnderstandingSelections    -- Selection phantom types (Indexed Monad pattern)
  | UnderstandingTreeAPI       -- TreeAPI declarative layer
  | UnderstandingScenes        -- Scene structures for interactive visualizations
  | Reference       -- Reference: API documentation index
  | ReferenceModule String  -- Reference: individual module page
  -- Tour Pages (progressive overview of capabilities)
  | TourIndex            -- Tour: index page with hero and cards
  | TourFoundations      -- Tour 1: Three circles, matrix, parabola, bar chart
  | TourProfessional     -- Tour 2: Multiline, grouped bar, radial stacked
  | TourFlow             -- Tour 3: Chord and Sankey diagrams
  | TourHierarchies      -- Tour 4: All tree layouts, treemap, pack, icicle
  | TourMotion           -- Tour 5: Transitions and animations
  | TourWealthHealth     -- Tour 5b: Wealth & Health of Nations (animated bubble chart)
  | TourInterpreters     -- Tour 6: Alternative interpreters (Mermaid, English, generators)
  | TourSonification     -- Tour 6b: Data sonification - audio interpreter (Anscombe's Quartet as sound)
  | TourFPFTW            -- Tour 7: Functional Programming For The Win (Maps, Sets, contravariant attributes)
  | TourGraphAlgorithms  -- Tour 8: Graph Algorithms (topological sort, transitive reduction)
  | TourLesMisGUP        -- Tour 9: General Update Pattern with dynamic graphs
  | TourSimpsons         -- Tour: Simpson's Paradox visualization (classic setosa.io port)
  -- Showcase (complex app-like visualizations)
  | Showcase             -- Showcase: index page with hero and cards
  -- Tree API Examples
  | Examples        -- Examples page showing all TreeAPI examples
  | Example String  -- Individual example page by ID
  | TreeAPI         -- Tree API examples (declarative tree API)
  | AnimatedTreeCluster  -- Animated Tree ↔ Cluster transitions (flagship example from yesterday)
  | GeneralUpdatePattern  -- Classic GUP with animated letters (v1 restored)
  | LesMisGUPTree   -- Les Mis with Tree API + GUP + Dynamic Layouts (proof-of-concept)
  | LesMisGUP       -- Les Mis with declarative scene-based PSD3 + GUP (clean example)
  | LesMisGUPSimple -- Les Mis minimal GUP with UpdateNestedJoin (12-line update function!)
  | LesMisGUPTreeAPI -- Les Mis testing renderTreeWithSimulation pattern
  | LesMisGUPClean  -- Les Mis GUP with existing primitives (no wrappers!)
  | LesMisQueryDemo -- Query language demo with interactive group filtering
  | ModuleGraph     -- Module dependency graph (dogfooding!)
  | MermaidTreeDemo -- Mermaid interpreter for Tree API (visualizes tree structure)
  | UpdateJoinDemo   -- UpdateNestedJoin demonstration (GUP with type decomposition)
  | ForceConfigPOC  -- POC for testing new force configuration system
  | ForceConfigV2Test -- Minimal standalone test of V2 force configuration
  | SimpleForceGraph  -- Simplest force graph with hardcoded data
  | SankeyDebug     -- Debug testbed for Sankey layout algorithms
  | ForcePlayground -- Force simulation playground with network datasets
  | TreeBuilder    -- Interactive tree builder for Tree API visualization
  | TreeBuilder2   -- Interactive tree builder with node creation
  | TreeBuilder3   -- DSL grammar tree builder (keyboard-driven)
  | SPLOM          -- Brushable Scatterplot Matrix (Palmer Penguins)
  | GUPDebug       -- Standalone debug page for Setup.applySetupWithData GUP API
  | VizMatrix     -- Type × AST × Data matrix demo
  | AlgoraveViz   -- Algorave set visualizer - pattern trees and forest view
  -- Meta
  | Acknowledgements -- Credits and acknowledgements
  | NotFound

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show Home = "Home"
  show GettingStarted = "Getting Started"
  show Wizard = "Wizard"
  show HowtoIndex = "How-to Guides"
  show HowtoTransitions = "How-to: Transitions"
  show HowtoForceGraphs = "How-to: Force Graphs"
  show HowtoHierarchical = "How-to: Hierarchical Data"
  show HowtoEvents = "How-to: Events"
  show HowtoTreeAPI = "How-to: TreeAPI"
  show HowtoLoadingData = "How-to: Loading Data"
  show HowtoAxesScales = "How-to: Axes & Scales"
  show HowtoTooltips = "How-to: Tooltips"
  show HowtoDebugging = "How-to: Debugging"
  show HowtoPerformance = "How-to: Performance"
  show HowtoTreeExplorer = "How-to: Tree Explorer"
  show Understanding = "Understanding"
  show UnderstandingGrammar = "Understanding: Grammar"
  show UnderstandingAttributes = "Understanding: Attributes"
  show UnderstandingSelections = "Understanding: Selections"
  show UnderstandingTreeAPI = "Understanding: TreeAPI"
  show UnderstandingScenes = "Understanding: Scenes"
  show Reference = "API Reference"
  show (ReferenceModule moduleName) = "Module: " <> moduleName
  show TourIndex = "Tour"
  show TourFoundations = "Tour: Foundations"
  show TourProfessional = "Tour: Typical Charts"
  show TourFlow = "Tour: Data Flow"
  show TourHierarchies = "Tour: Hierarchies"
  show TourMotion = "Tour: Motion & Transitions"
  show TourWealthHealth = "Tour: Wealth & Health"
  show TourInterpreters = "Tour: Alternative Interpreters"
  show TourSonification = "Tour: Data Sonification"
  show TourFPFTW = "Tour: FP For The Win"
  show TourGraphAlgorithms = "Tour: Graph Algorithms"
  show TourLesMisGUP = "Tour: General Update Pattern"
  show TourSimpsons = "Tour: Simpson's Paradox"
  show Showcase = "Showcase"
  show Examples = "Examples"
  show (Example exampleId) = "Example: " <> exampleId
  show TreeAPI = "Tree API Examples"
  show AnimatedTreeCluster = "Animated Tree ↔ Cluster"
  show GeneralUpdatePattern = "General Update Pattern (Letters)"
  show LesMisGUPTree = "Les Misérables GUP (Tree API)"
  show LesMisGUP = "Les Misérables GUP (PSD3)"
  show LesMisGUPSimple = "Les Misérables GUP (Simple)"
  show LesMisGUPTreeAPI = "Les Misérables GUP (TreeAPI)"
  show LesMisGUPClean = "Les Misérables GUP (Clean)"
  show LesMisQueryDemo = "Query Language Demo"
  show ModuleGraph = "Module Graph"
  show MermaidTreeDemo = "Mermaid Tree Visualizer"
  show UpdateJoinDemo = "UpdateNestedJoin Demo"
  show ForceConfigPOC = "Force Config POC"
  show ForceConfigV2Test = "Force Config V2 Test"
  show SimpleForceGraph = "Simple Force Graph"
  show SankeyDebug = "Sankey Debug"
  show ForcePlayground = "Force Playground"
  show TreeBuilder = "Tree Builder"
  show TreeBuilder2 = "Tree Builder 2"
  show TreeBuilder3 = "DSL Tree Builder"
  show SPLOM = "Brushable SPLOM"
  show GUPDebug = "GUP Debug"
  show VizMatrix = "VizMatrix"
  show AlgoraveViz = "Algorave Visualizer"
  show Acknowledgements = "Acknowledgements"
  show NotFound = "Not Found"
