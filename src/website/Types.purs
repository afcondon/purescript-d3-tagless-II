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
  -- PSD3v2 Examples (Tree API based)
  | PSD3v2Examples  -- PSD3v2 examples showcase page
  | TreeAPI         -- Tree API examples (declarative tree API)
  | LesMisGUPTree   -- Les Misérables with Tree API + GUP + Dynamic Layouts (FLAGSHIP EXAMPLE)
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
  show PSD3v2Examples = "PSD3v2 Examples"
  show TreeAPI = "Tree API Examples"
  show LesMisGUPTree = "Les Mis GUP (Tree API)"
  show Acknowledgements = "Acknowledgements"
  show NotFound = "Not Found"
