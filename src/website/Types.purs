module PSD3.Types where

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
  | HowtoIndex      -- How-to: index of all step-by-step guides
  | Reference       -- Reference: API documentation
  | About
  | Tutorial
  | SimpleCharts
  | ChordDiagram
  | BubbleChart
  | SankeyDiagram
  | Hierarchies
  | Interpreters
  | CodeExplorer
  | Explore String  -- Code exploration page for a specific snippet
  | NotFound

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show Home = "Home"
  show GettingStarted = "Getting Started"
  show HowtoIndex = "How-to Guides"
  show Reference = "API Reference"
  show About = "About"
  show Tutorial = "Tutorial"
  show SimpleCharts = "Simple Charts"
  show ChordDiagram = "Chord Diagram"
  show BubbleChart = "Bubble Chart"
  show SankeyDiagram = "Sankey Diagram"
  show Hierarchies = "Hierarchies"
  show Interpreters = "Interpreters"
  show CodeExplorer = "Code Explorer"
  show (Explore snippetId) = "Explore: " <> snippetId
  show NotFound = "Not Found"
