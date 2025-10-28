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
  = Home            -- Landing page with four documentation categories
  | GettingStarted  -- Tutorial: installation, setup, first project
  | Wizard          -- Interactive wizard for scaffolding visualizations
  | HowtoIndex      -- How-to: index of all step-by-step guides
  | Reference       -- Reference: API documentation index
  | ReferenceModule String  -- Reference: individual module page (e.g., "PSD3.Types")
  | About           -- Legacy route, redirects to UnderstandingPhilosophy
  | UnderstandingConcepts    -- Understanding: core concepts (Finally Tagless, SelectionM, etc.)
  | UnderstandingPatterns    -- Understanding: practical patterns (datum_, data joins, etc.)
  | UnderstandingPhilosophy  -- Understanding: design philosophy
  | Tutorial
  | SimpleCharts
  | DataFlowViz
  | Hierarchies
  | Interpreters
  | CodeExplorer
  | Explore String  -- Code exploration page for a specific snippet
  | WealthHealth    -- Wealth & Health of Nations visualization
  | LesMiserables   -- Les Misérables force-directed graph
  | CodeAtlas       -- Code Atlas: codebase analysis and visualization
  | NotFound

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show Home = "Home"
  show GettingStarted = "Getting Started"
  show Wizard = "Wizard"
  show HowtoIndex = "How-to Guides"
  show Reference = "API Reference"
  show (ReferenceModule moduleName) = "Module: " <> moduleName
  show About = "About"
  show UnderstandingConcepts = "Understanding: Concepts"
  show UnderstandingPatterns = "Understanding: Patterns"
  show UnderstandingPhilosophy = "Understanding: Philosophy"
  show Tutorial = "Tutorial"
  show SimpleCharts = "Simple Charts"
  show DataFlowViz = "Data Flow Visualizations"
  show Hierarchies = "Hierarchies"
  show Interpreters = "Interpreters"
  show CodeExplorer = "Code Explorer"
  show (Explore snippetId) = "Explore: " <> snippetId
  show WealthHealth = "Wealth & Health of Nations"
  show LesMiserables = "Les Misérables"
  show CodeAtlas = "Code Atlas"
  show NotFound = "Not Found"
