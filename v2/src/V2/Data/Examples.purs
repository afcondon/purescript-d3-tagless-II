module V2.Data.Examples where

import Prelude
import V2.Types (Category(..), Difficulty(..), ExampleMetadata)
import Data.Array (filter, find)
import Data.Maybe (Maybe)

-- | All available examples
allExamples :: Array ExampleMetadata
allExamples =
  [ -- Basic Charts
    { id: "line-chart"
    , title: "Line Chart"
    , description: "Basic line chart with linear scales, axes, and path generation"
    , difficulty: Beginner
    , category: BasicChart
    , tags: ["scales", "axes", "paths", "2d"]
    , thumbnail: "assets/thumbnails/line-chart.svg"
    , hasInteractivity: false
    , hasComparison: true
    }
  , { id: "bar-chart"
    , title: "Bar Chart"
    , description: "Vertical bar chart demonstrating rectangles, scales, and data binding"
    , difficulty: Beginner
    , category: BasicChart
    , tags: ["scales", "axes", "rectangles", "2d"]
    , thumbnail: "assets/thumbnails/bar-chart.svg"
    , hasInteractivity: false
    , hasComparison: true
    }
  , { id: "scatter-plot"
    , title: "Scatter Plot"
    , description: "Scatter plot with circles, showing relationship between two variables"
    , difficulty: Beginner
    , category: BasicChart
    , tags: ["scales", "axes", "circles", "2d"]
    , thumbnail: "assets/thumbnails/scatter-plot.svg"
    , hasInteractivity: false
    , hasComparison: true
    }
  , { id: "scatter-quartet"
    , title: "Anscombe's Quartet"
    , description: "Four scatter plots demonstrating Anscombe's Quartet in small multiples"
    , difficulty: Intermediate
    , category: BasicChart
    , tags: ["scatter", "small-multiples", "statistics"]
    , thumbnail: "assets/thumbnails/scatter-quartet.svg"
    , hasInteractivity: false
    , hasComparison: false
    }

  -- Advanced Layouts
  , { id: "chord-diagram"
    , title: "Chord Diagram"
    , description: "Circular chord diagram showing relationships in a dependency matrix"
    , difficulty: Advanced
    , category: AdvancedLayout
    , tags: ["chord", "circular", "relationships", "arcs", "ribbons"]
    , thumbnail: "assets/thumbnails/chord-diagram.svg"
    , hasInteractivity: false
    , hasComparison: false
    }
  , { id: "bubble-chart"
    , title: "Bubble Chart (Circle Pack)"
    , description: "Hierarchical bubble chart using D3's pack layout"
    , difficulty: Intermediate
    , category: AdvancedLayout
    , tags: ["hierarchy", "circles", "pack-layout"]
    , thumbnail: "assets/thumbnails/bubble-chart.svg"
    , hasInteractivity: false
    , hasComparison: false
    }
  , { id: "sankey"
    , title: "Sankey Diagram"
    , description: "Flow diagram with configurable node and link visualization"
    , difficulty: Advanced
    , category: AdvancedLayout
    , tags: ["flow", "sankey", "interactive"]
    , thumbnail: "assets/thumbnails/sankey.svg"
    , hasInteractivity: true
    , hasComparison: false
    }
  , { id: "tree"
    , title: "Tree Layout"
    , description: "Hierarchical tree visualization with multiple layout options"
    , difficulty: Intermediate
    , category: AdvancedLayout
    , tags: ["hierarchy", "tree", "dendogram"]
    , thumbnail: "assets/thumbnails/tree.svg"
    , hasInteractivity: true
    , hasComparison: false
    }

  -- Interactive Examples
  , { id: "three-little-circles"
    , title: "Three Little Circles"
    , description: "Introduction to D3 selections and data binding"
    , difficulty: Beginner
    , category: Interactive
    , tags: ["basics", "selections", "data-binding"]
    , thumbnail: "assets/thumbnails/three-circles.svg"
    , hasInteractivity: true
    , hasComparison: true
    }
  , { id: "gup"
    , title: "General Update Pattern"
    , description: "Interactive demonstration of enter, update, and exit selections"
    , difficulty: Intermediate
    , category: Interactive
    , tags: ["data-join", "transitions", "update-pattern"]
    , thumbnail: "assets/thumbnails/gup.svg"
    , hasInteractivity: true
    , hasComparison: false
    }
  , { id: "les-mis"
    , title: "Les Misérables Network"
    , description: "Force-directed graph with drag interaction"
    , difficulty: Advanced
    , category: Interactive
    , tags: ["force", "network", "drag", "simulation"]
    , thumbnail: "assets/thumbnails/les-mis.svg"
    , hasInteractivity: true
    , hasComparison: false
    }

  -- Alternative Interpreters
  , { id: "meta-tree"
    , title: "MetaTree Visualizer"
    , description: "Visualizes the DSL syntax tree of visualizations"
    , difficulty: Advanced
    , category: Interpreter
    , tags: ["meta", "interpreter", "dsl"]
    , thumbnail: "assets/thumbnails/meta-tree.svg"
    , hasInteractivity: false
    , hasComparison: false
    }
  , { id: "print-tree"
    , title: "String Generator"
    , description: "Generates code and documentation from visualization definitions"
    , difficulty: Advanced
    , category: Interpreter
    , tags: ["meta", "interpreter", "codegen"]
    , thumbnail: "assets/thumbnails/print-tree.svg"
    , hasInteractivity: false
    , hasComparison: false
    }

  -- Applications
  , { id: "spago"
    , title: "Spago Dependency Explorer"
    , description: "Interactive application for exploring PureScript package dependencies"
    , difficulty: Advanced
    , category: Application
    , tags: ["force", "network", "interactive", "real-world"]
    , thumbnail: "assets/thumbnails/spago.svg"
    , hasInteractivity: true
    , hasComparison: false
    }
  ]

-- | Get example by ID
getExample :: String -> Maybe ExampleMetadata
getExample id = find (\ex -> ex.id == id) allExamples

-- | Get examples by category
getExamplesByCategory :: Category -> Array ExampleMetadata
getExamplesByCategory cat = filter (\ex -> ex.category == cat) allExamples

-- | Get examples by difficulty
getExamplesByDifficulty :: Difficulty -> Array ExampleMetadata
getExamplesByDifficulty diff = filter (\ex -> ex.difficulty == diff) allExamples

-- | Get interactive examples
getInteractiveExamples :: Array ExampleMetadata
getInteractiveExamples = filter (_.hasInteractivity) allExamples

-- | Get examples with JS comparison
getExamplesWithComparison :: Array ExampleMetadata
getExamplesWithComparison = filter (_.hasComparison) allExamples
