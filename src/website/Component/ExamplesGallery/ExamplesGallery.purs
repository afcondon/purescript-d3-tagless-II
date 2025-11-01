module PSD3.ExamplesGallery where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

-- | Gallery showing all available visualizations
component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }

-- | Example metadata for the gallery
type ExampleMeta =
  { id :: String
  , name :: String
  , description :: String
  , category :: String
  , route :: Route
  , thumbnailClass :: String  -- CSS class for thumbnail styling
  }

-- | All available examples organized by category
allExamples :: Array ExampleMeta
allExamples =
  [ -- Simple Charts
    { id: "three-little-circles"
    , name: "Three Little Circles"
    , description: "The simplest possible D3 example - three circles"
    , category: "Simple Charts"
    , route: Example "three-little-circles"
    , thumbnailClass: "thumb-circles"
    }
  , { id: "bar-chart"
    , name: "Bar Chart"
    , description: "Basic bar chart with axis and labels"
    , category: "Simple Charts"
    , route: Example "bar-chart"
    , thumbnailClass: "thumb-bar"
    }
  , { id: "line-chart"
    , name: "Line Chart"
    , description: "Line chart with time series data"
    , category: "Simple Charts"
    , route: Example "line-chart"
    , thumbnailClass: "thumb-line"
    }
  , { id: "scatter-plot"
    , name: "Scatter Plot"
    , description: "Scatter plot with Anscombe's Quartet"
    , category: "Simple Charts"
    , route: Example "scatter-plot"
    , thumbnailClass: "thumb-scatter"
    }
  , { id: "grouped-bar-chart"
    , name: "Grouped Bar Chart"
    , description: "Bar chart with grouped categories"
    , category: "Simple Charts"
    , route: Example "grouped-bar-chart"
    , thumbnailClass: "thumb-grouped-bar"
    }
  , { id: "multi-line-chart"
    , name: "Multi-Line Chart"
    , description: "Multiple line series on one chart"
    , category: "Simple Charts"
    , route: Example "multi-line-chart"
    , thumbnailClass: "thumb-multi-line"
    }
  , { id: "bubble-chart"
    , name: "Bubble Chart"
    , description: "Scatter plot with sized bubbles"
    , category: "Simple Charts"
    , route: Example "bubble-chart"
    , thumbnailClass: "thumb-bubble"
    }

  -- Hierarchies
  , { id: "vertical-tree"
    , name: "Vertical Tree"
    , description: "Top-down hierarchical tree layout"
    , category: "Hierarchies"
    , route: Example "vertical-tree"
    , thumbnailClass: "thumb-vtree"
    }
  , { id: "horizontal-tree"
    , name: "Horizontal Tree"
    , description: "Left-to-right hierarchical tree layout"
    , category: "Hierarchies"
    , route: Example "horizontal-tree"
    , thumbnailClass: "thumb-htree"
    }
  , { id: "radial-tree"
    , name: "Radial Tree"
    , description: "Circular hierarchical tree layout"
    , category: "Hierarchies"
    , route: Example "radial-tree"
    , thumbnailClass: "thumb-radial-tree"
    }
  , { id: "treemap"
    , name: "Treemap"
    , description: "Space-filling hierarchical visualization"
    , category: "Hierarchies"
    , route: Example "treemap"
    , thumbnailClass: "thumb-treemap"
    }
  , { id: "icicle"
    , name: "Icicle Chart"
    , description: "Hierarchical icicle/partition layout"
    , category: "Hierarchies"
    , route: Example "icicle"
    , thumbnailClass: "thumb-icicle"
    }

  -- Force-Directed
  , { id: "lesmis-force"
    , name: "Les Misérables Network"
    , description: "Character co-occurrence force-directed graph"
    , category: "Force-Directed"
    , route: Example "lesmis-force"
    , thumbnailClass: "thumb-lesmis"
    }
  , { id: "topological-sort"
    , name: "Topological Sort Layers"
    , description: "Force layout with layer constraints"
    , category: "Force-Directed"
    , route: Example "topological-sort"
    , thumbnailClass: "thumb-topo"
    }

  -- Data Flow
  , { id: "chord-diagram"
    , name: "Chord Diagram"
    , description: "Circular flow diagram showing relationships"
    , category: "Data Flow"
    , route: Example "chord-diagram"
    , thumbnailClass: "thumb-chord"
    }
  , { id: "sankey-diagram"
    , name: "Sankey Diagram"
    , description: "Flow diagram with proportional widths"
    , category: "Data Flow"
    , route: Example "sankey-diagram"
    , thumbnailClass: "thumb-sankey"
    }

  -- Rich Data Structures
  , { id: "map-quartet"
    , name: "Map-Based Quartet"
    , description: "Scatter plots using Map data structures"
    , category: "Rich Data Structures"
    , route: Example "map-quartet"
    , thumbnailClass: "thumb-map-quartet"
    }

  -- Transitions
  , { id: "three-circles-transition"
    , name: "Color Mixing Transition"
    , description: "RGB color mixing with transitions"
    , category: "Transitions"
    , route: Example "three-circles-transition"
    , thumbnailClass: "thumb-color-mix"
    }
  , { id: "general-update-pattern"
    , name: "General Update Pattern"
    , description: "Enter, update, exit pattern visualization"
    , category: "Transitions"
    , route: Example "general-update-pattern"
    , thumbnailClass: "thumb-gup"
    }
  , { id: "animated-radial-tree"
    , name: "Animated Radial Tree"
    , description: "Transitions between tree layouts"
    , category: "Transitions"
    , route: Example "animated-radial-tree"
    , thumbnailClass: "thumb-anim-radial"
    }
  ]

render :: forall m. Unit -> H.ComponentHTML Unit () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "examples-gallery-page" ] ]
    [ HH.header
        [ HP.classes [ HH.ClassName "gallery-header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "gallery-title" ] ]
            [ HH.text "Examples Gallery" ]
        , HH.p
            [ HP.classes [ HH.ClassName "gallery-subtitle" ] ]
            [ HH.text "Complete catalog of PSD3 visualization examples. Each example shows the visualization and its full source code." ]
        , HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "gallery-home-link" ]
            ]
            [ HH.text "← Back to Home" ]
        ]

    , HH.main
        [ HP.classes [ HH.ClassName "gallery-content" ] ]
        (renderCategories allExamples)
    ]

-- | Group examples by category and render
renderCategories :: forall w i. Array ExampleMeta -> Array (HH.HTML w i)
renderCategories examples =
  let
    categories = [ "Simple Charts", "Hierarchies", "Force-Directed", "Data Flow", "Rich Data Structures", "Transitions" ]
  in
    categories <#> \category ->
      let examplesInCategory = examples # filter (\ex -> ex.category == category)
      in renderCategory category examplesInCategory

-- | Render a category section
renderCategory :: forall w i. String -> Array ExampleMeta -> HH.HTML w i
renderCategory categoryName examples =
  HH.section
    [ HP.classes [ HH.ClassName "gallery-category" ] ]
    [ HH.h2
        [ HP.classes [ HH.ClassName "gallery-category-title" ] ]
        [ HH.text categoryName ]
    , HH.div
        [ HP.classes [ HH.ClassName "gallery-grid" ] ]
        (examples <#> renderExampleCard)
    ]

-- | Render an individual example card
renderExampleCard :: forall w i. ExampleMeta -> HH.HTML w i
renderExampleCard ex =
  HH.a
    [ HP.href $ "#" <> routeToPath ex.route
    , HP.classes [ HH.ClassName "example-card" ]
    ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-thumbnail", HH.ClassName ex.thumbnailClass ] ]
        []
    , HH.div
        [ HP.classes [ HH.ClassName "example-card-content" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "example-card-title" ] ]
            [ HH.text ex.name ]
        , HH.p
            [ HP.classes [ HH.ClassName "example-card-description" ] ]
            [ HH.text ex.description ]
        ]
    ]

-- | Filter examples
filter :: forall a. (a -> Boolean) -> Array a -> Array a
filter = filterImpl

foreign import filterImpl :: forall a. (a -> Boolean) -> Array a -> Array a
