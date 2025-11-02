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
  , thumbnail :: String  -- Path to thumbnail image
  }

-- | All available examples organized by category
allExamples :: Array ExampleMeta
allExamples =
  [ -- Simple Charts
    { id: "three-little-circles"
    , name: "Three Little Circles"
    , description: "The simplest possible D3 example"
    , category: "Simple Charts"
    , route: Example "three-little-circles"
    , thumbnail: "assets/example-thumbnails/three-little-circles.png"
    }
  , { id: "bar-chart"
    , name: "Bar Chart"
    , description: "Basic bar chart with axis and labels"
    , category: "Simple Charts"
    , route: Example "bar-chart"
    , thumbnail: "assets/example-thumbnails/simple-bar-chart.png"
    }
  , { id: "line-chart"
    , name: "Line Chart"
    , description: "Line chart with time series data"
    , category: "Simple Charts"
    , route: Example "line-chart"
    , thumbnail: "assets/example-thumbnails/simple-line-chart.png"
    }
  , { id: "scatter-plot"
    , name: "Scatter Plot"
    , description: "Anscombe's Quartet visualization"
    , category: "Simple Charts"
    , route: Example "scatter-plot"
    , thumbnail: "assets/example-thumbnails/anscombes-quartet.png"
    }
  , { id: "grouped-bar-chart"
    , name: "Grouped Bar Chart"
    , description: "Bar chart with grouped categories"
    , category: "Simple Charts"
    , route: Example "grouped-bar-chart"
    , thumbnail: "assets/example-thumbnails/grouped-bar-chart.png"
    }
  , { id: "multi-line-chart"
    , name: "Multi-Line Chart"
    , description: "Multiple line series on one chart"
    , category: "Simple Charts"
    , route: Example "multi-line-chart"
    , thumbnail: "assets/example-thumbnails/multiline-chart.png"
    }
  , { id: "bubble-chart"
    , name: "Bubble Chart"
    , description: "Hierarchical circle packing layout"
    , category: "Hierarchies"
    , route: Example "bubble-chart"
    , thumbnail: "assets/example-thumbnails/circle-packing.png"
    }

  -- Hierarchies
  , { id: "vertical-tree"
    , name: "Vertical Tree"
    , description: "Top-down tidy tree layout"
    , category: "Hierarchies"
    , route: Example "vertical-tree"
    , thumbnail: "assets/example-thumbnails/vertical-tidy-tree.png"
    }
  , { id: "horizontal-tree"
    , name: "Horizontal Tree"
    , description: "Left-to-right tidy tree layout"
    , category: "Hierarchies"
    , route: Example "horizontal-tree"
    , thumbnail: "assets/example-thumbnails/horizontal-tidy-tree.png"
    }
  , { id: "radial-tree"
    , name: "Radial Tree"
    , description: "Circular tidy tree layout"
    , category: "Hierarchies"
    , route: Example "radial-tree"
    , thumbnail: "assets/example-thumbnails/radial-tidy-tree.png"
    }
  , { id: "treemap"
    , name: "Treemap"
    , description: "Space-filling hierarchical visualization"
    , category: "Hierarchies"
    , route: Example "treemap"
    , thumbnail: "assets/example-thumbnails/treemap.png"
    }
  , { id: "icicle"
    , name: "Icicle Chart"
    , description: "Hierarchical partition layout"
    , category: "Hierarchies"
    , route: Example "icicle"
    , thumbnail: "assets/example-thumbnails/icicle-chart.png"
    }

  -- Force-Directed
  , { id: "lesmis-force"
    , name: "Les Misérables Network"
    , description: "Character co-occurrence graph"
    , category: "Force-Directed"
    , route: Example "lesmis-force"
    , thumbnail: "assets/example-thumbnails/les-miserables.png"
    }
  , { id: "topological-sort"
    , name: "Topological Sort"
    , description: "Force layout with layer constraints"
    , category: "Force-Directed"
    , route: Example "topological-sort"
    , thumbnail: "assets/example-thumbnails/working-with-graphs.png"
    }

  -- Data Flow
  , { id: "chord-diagram"
    , name: "Chord Diagram"
    , description: "Circular relationship diagram"
    , category: "Data Flow"
    , route: Example "chord-diagram"
    , thumbnail: "assets/example-thumbnails/chord-diagram.png"
    }
  , { id: "sankey-diagram"
    , name: "Sankey Diagram"
    , description: "Flow diagram with proportional widths"
    , category: "Data Flow"
    , route: Example "sankey-diagram"
    , thumbnail: "assets/example-thumbnails/sankey-diagram.png"
    }

  -- Rich Data Structures
  , { id: "map-quartet"
    , name: "Map Quartet"
    , description: "Sparse data with PureScript Maps"
    , category: "Rich Data Structures"
    , route: Example "map-quartet"
    , thumbnail: "assets/example-thumbnails/working-with-maps.png"
    }

  -- Transitions
  , { id: "three-circles-transition"
    , name: "Color Mixing"
    , description: "RGB color transitions"
    , category: "Transitions"
    , route: Example "three-circles-transition"
    , thumbnail: "assets/example-thumbnails/three-little-circles.png"
    }
  , { id: "general-update-pattern"
    , name: "General Update Pattern"
    , description: "Enter, update, exit pattern"
    , category: "Transitions"
    , route: Example "general-update-pattern"
    , thumbnail: "assets/example-thumbnails/general-update-pattern.png"
    }
  , { id: "animated-radial-tree"
    , name: "Animated Radial Tree"
    , description: "Transitions between tree layouts"
    , category: "Transitions"
    , route: Example "animated-radial-tree"
    , thumbnail: "assets/example-thumbnails/animating-tree.png"
    }
  ]

render :: forall m. Unit -> H.ComponentHTML Unit () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "examples-gallery-page" ] ]
    [ HH.header
        [ HP.classes [ HH.ClassName "gallery-header" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "gallery-header-content" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Home
                , HP.classes [ HH.ClassName "gallery-logo-link" ]
                ]
                [ HH.img
                    [ HP.src "assets/psd3-logo-color.svg"
                    , HP.alt "PSD3 Logo"
                    , HP.classes [ HH.ClassName "gallery-logo" ]
                    ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "gallery-header-text" ] ]
                [ HH.h1
                    [ HP.classes [ HH.ClassName "gallery-title" ] ]
                    [ HH.text "Examples" ]
                , HH.p
                    [ HP.classes [ HH.ClassName "gallery-subtitle" ] ]
                    [ HH.text "Interactive visualizations with source code" ]
                ]
            ]
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
        [ HP.classes [ HH.ClassName "example-thumbnail" ] ]
        [ HH.img
            [ HP.src ex.thumbnail
            , HP.alt ex.name
            , HP.classes [ HH.ClassName "example-thumbnail-img" ]
            ]
        ]
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
