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
  | Reference       -- Reference: API documentation index
  | ReferenceModule String  -- Reference: individual module page
  -- Tour Pages (progressive overview of capabilities)
  | TourFoundations      -- Tour 1: Three circles, matrix, parabola, bar chart
  | TourProfessional     -- Tour 2: Multiline, grouped bar, radial stacked
  | TourFlow             -- Tour 3: Chord and Sankey diagrams
  | TourHierarchies      -- Tour 4: All tree layouts, treemap, pack, icicle
  | TourMotion           -- Tour 5: Transitions and animations
  | TourInterpreters     -- Tour 6: Alternative interpreters (Mermaid, English, generators)
  | TourFPFTW            -- Tour 7: Functional Programming For The Win (Maps, Sets, contravariant attributes)
  | TourGraphAlgorithms  -- Tour 8: Graph Algorithms (topological sort, transitive reduction)
  | TourShowcase         -- Tour 9: Flagship demo visualizations
  -- Tree API Examples
  | Gallery         -- Gallery of all TreeAPI examples with cards
  | Example String  -- Individual example page by ID
  | TreeAPI         -- Tree API examples (declarative tree API)
  | AnimatedTreeCluster  -- Animated Tree ↔ Cluster transitions (flagship example from yesterday)
  | LesMisGUPTree   -- Les Mis with Tree API + GUP + Dynamic Layouts (proof-of-concept)
  | ModuleGraph     -- Module dependency graph (dogfooding!)
  | MermaidTreeDemo -- Mermaid interpreter for Tree API (visualizes tree structure)
  -- Meta
  | Acknowledgements -- Credits and acknowledgements
  | NotFound

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show Home = "Home"
  show GettingStarted = "Getting Started"
  show Wizard = "Wizard"
  show HowtoIndex = "How-to Guides"
  show Reference = "API Reference"
  show (ReferenceModule moduleName) = "Module: " <> moduleName
  show TourFoundations = "Tour: Foundations"
  show TourProfessional = "Tour: Professional Charts"
  show TourFlow = "Tour: Flow Diagrams"
  show TourHierarchies = "Tour: Hierarchies"
  show TourMotion = "Tour: Motion & Transitions"
  show TourInterpreters = "Tour: Alternative Interpreters"
  show TourFPFTW = "Tour: FP For The Win"
  show TourGraphAlgorithms = "Tour: Graph Algorithms"
  show TourShowcase = "Tour: Showcase"
  show Gallery = "Examples Gallery"
  show (Example exampleId) = "Example: " <> exampleId
  show TreeAPI = "Tree API Examples"
  show AnimatedTreeCluster = "Animated Tree ↔ Cluster"
  show LesMisGUPTree = "Les Misérables GUP"
  show ModuleGraph = "Module Graph"
  show MermaidTreeDemo = "Mermaid Tree Visualizer"
  show Acknowledgements = "Acknowledgements"
  show NotFound = "Not Found"
