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
    , about: "A fundamental 2D chart demonstrating how to map data to visual coordinates using linear scales. Shows the use of D3's path generators to create smooth lines through data points. This example covers the core concepts of scales (domain to range mapping), axes (rendering scale ticks and labels), and SVG path generation. Perfect starting point for understanding how D3 transforms data into visual representations."
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
    , about: "Introduces the foundational D3 pattern of binding data to DOM elements. Each data point is mapped to an SVG rectangle, with height and position determined by scales. Demonstrates band scales for categorical data on the x-axis and linear scales for continuous data on the y-axis. Shows how to use data joins to create one rectangle per data point with type-safe attribute assignment."
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
    , about: "Visualizes the relationship between two continuous variables by plotting points in 2D space. Each circle's x and y coordinates are determined by linear scales mapping data values to pixel coordinates. This example demonstrates working with circles in SVG and highlights D3's ability to reveal correlations and patterns in bivariate data."
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
    , about: "Displays Anscombe's famous statistical quartet - four datasets with identical statistical properties but dramatically different distributions. Uses the 'small multiples' technique to show all four plots side by side, demonstrating why visualization is essential. Shows how to create multiple coordinated charts and the importance of always visualizing your data."
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
    , about: "Visualizes relationships between entities arranged in a circle, with ribbons connecting related items. Uses D3's chord layout to transform a dependency matrix into visual arcs and ribbons. Perfect for showing flows, dependencies, or relationships in systems. Demonstrates working with D3's arc and ribbon generators, as well as radial coordinate systems."
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
    , about: "Displays hierarchical data as nested circles, where each node is sized according to its value and positioned to minimize overlaps. Uses D3's pack layout algorithm to efficiently arrange circles in a space-filling pattern. Great for showing part-to-whole relationships and hierarchical structure simultaneously. Demonstrates working with D3 hierarchy layouts and the Finally Tagless pattern."
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
    , about: "Visualizes flow between nodes, where the width of connections represents flow magnitude. Interactive controls allow configuration of node padding, alignment, and iterations. Excellent for showing energy flows, process steps, or any system where quantity flows between stages. Demonstrates D3's sankey layout and the library's approach to providing user controls that modify visualization parameters."
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
    , about: "Displays hierarchical data as a node-link tree diagram. Provides multiple layout algorithms (tidy tree, cluster, radial) that can be toggled to show different aspects of the same data. Demonstrates D3's powerful hierarchy layouts and how the Finally Tagless pattern allows switching between different interpretations of the same visualization code."
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
    , about: "The classic 'Hello World' of D3, based on Mike Bostock's famous Three Little Circles tutorial. Demonstrates the fundamental concept of data binding: associating an array of data with DOM elements. Shows how D3 creates, updates, and removes elements based on data changes. This simple example contains the core idea that makes D3 powerful - declaratively describing what the page should look like for any given data."
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
    , about: "Interactive exploration of D3's General Update Pattern - the enter/update/exit selection pattern that handles dynamic data. Click to add or remove data points and watch as D3 smoothly transitions elements in, updates them, or removes them. This pattern is the heart of D3's data-driven approach and this example makes it tangible by letting you trigger updates and see the three selection types in action with animated transitions."
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
    , about: "Network visualization of character relationships in Les Misérables, using D3's force simulation to position nodes. Nodes attract and repel each other based on connections, creating an organic layout. Drag nodes to reposition them, and watch the physics simulation respond. Demonstrates D3's force-directed graph capabilities and how to add drag behavior. Shows integration between D3's simulation engine and the type-safe PureScript API."
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
    , about: "Demonstrates the Finally Tagless pattern by running the same visualization code through a different interpreter. Instead of rendering to SVG, this interpreter visualizes the abstract syntax tree of the visualization itself as a tree diagram. This meta-visualization shows how the same code can be interpreted in completely different ways - a key advantage of the Finally Tagless approach. Perfect for understanding the DSL structure and debugging complex visualizations."
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
    , about: "Another interpreter demonstrating Finally Tagless flexibility. This interpreter takes visualization code and generates human-readable text descriptions or code snippets. Instead of creating visual output, it produces strings documenting what the visualization does. Shows how the same high-level visualization definition can be used for documentation generation, code analysis, or teaching - all without modifying the original visualization code."
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
    , about: "A real-world application that fetches and visualizes PureScript package dependencies from your spago.dhall file. Uses force-directed layout to show package relationships, with interactive features like drag, zoom, and filtering. Demonstrates how D3 visualizations integrate into larger Halogen applications with bidirectional communication - visualization events trigger app-level actions and app state updates drive visualization changes. This is what the library was built for: serious, interactive applications."
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
