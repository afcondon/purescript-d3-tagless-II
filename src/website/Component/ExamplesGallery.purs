module Component.ExamplesGallery where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

-- | Gallery showing all available TreeAPI visualizations
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

-- | All available TreeAPI examples organized by category
allExamples :: Array ExampleMeta
allExamples =
  [ -- Basic Examples
    { id: "three-little-circles"
    , name: "Three Little Circles"
    , description: "Data join basics - bind data to DOM elements"
    , category: "Basic Examples"
    , route: Example "three-little-circles"
    , thumbnail: "assets/example-thumbnails/three-little-circles.png"
    }
  , { id: "three-circles-transition"
    , name: "Circles Transition"
    , description: "Animated transitions with easing and delays"
    , category: "Basic Examples"
    , route: Example "three-circles-transition"
    , thumbnail: "assets/example-thumbnails/three-little-circles.png"
    }
  , { id: "simple-tree"
    , name: "Simple Tree"
    , description: "Basic nesting with named selections"
    , category: "Basic Examples"
    , route: Example "simple-tree"
    , thumbnail: "assets/example-thumbnails/simple-tree.png"
    }
  , { id: "nested-elements"
    , name: "Nested Elements"
    , description: "Multi-level nesting (Group → Circle + Text)"
    , category: "Basic Examples"
    , route: Example "nested-elements"
    , thumbnail: "assets/example-thumbnails/nested-elements.png"
    }
  , { id: "three-little-dimensions"
    , name: "Three Little Dimensions"
    , description: "Nested data joins (2D array → table)"
    , category: "Basic Examples"
    , route: Example "three-little-dimensions"
    , thumbnail: "assets/example-thumbnails/nested-data.png"
    }

  -- Chart Examples
  , { id: "bar-chart"
    , name: "Bar Chart"
    , description: "Data-driven bars with scaling"
    , category: "Chart Examples"
    , route: Example "bar-chart"
    , thumbnail: "assets/example-thumbnails/simple-bar-chart.png"
    }
  , { id: "scatter-plot"
    , name: "Scatter Plot"
    , description: "Points positioned by data coordinates"
    , category: "Chart Examples"
    , route: Example "scatter-plot"
    , thumbnail: "assets/example-thumbnails/anscombes-quartet.png"
    }
  , { id: "line-chart"
    , name: "Line Chart"
    , description: "Path element generated from data"
    , category: "Chart Examples"
    , route: Example "line-chart"
    , thumbnail: "assets/example-thumbnails/simple-line-chart.png"
    }
  , { id: "grouped-bar-chart"
    , name: "Grouped Bar Chart"
    , description: "Nested joins with multiple series"
    , category: "Chart Examples"
    , route: Example "grouped-bar-chart"
    , thumbnail: "assets/example-thumbnails/grouped-bar-chart.png"
    }

  -- Hierarchies & Simulations
  , { id: "simple-hierarchy"
    , name: "Tree Layout"
    , description: "Hierarchical node-link diagram (pure PureScript)"
    , category: "Hierarchies & Simulations"
    , route: Example "simple-hierarchy"
    , thumbnail: "assets/example-thumbnails/tree-purescript.png"
    }
  , { id: "lesmis-force"
    , name: "Les Misérables Network"
    , description: "Force-directed graph with simulation"
    , category: "Hierarchies & Simulations"
    , route: Example "lesmis-force"
    , thumbnail: "assets/example-thumbnails/les-miserables.png"
    }

  -- Advanced Examples
  , { id: "animated-tree-cluster"
    , name: "Animated Tree ↔ Cluster"
    , description: "Smooth transitions between pure PureScript layouts with proper data joins"
    , category: "Advanced Examples"
    , route: AnimatedTreeCluster
    , thumbnail: "assets/example-thumbnails/tree-purescript.png"
    }
  , { id: "lesmis-gup-tree"
    , name: "Les Misérables GUP"
    , description: "Force layout + dynamic layouts (grid, phylotaxis) + node filtering with General Update Pattern"
    , category: "Advanced Examples"
    , route: LesMisGUPTree
    , thumbnail: "assets/example-thumbnails/les-miserables-gup.png"
    }
  , { id: "module-graph"
    , name: "Module Graph"
    , description: "Visualize this codebase's own module dependencies - dogfooding!"
    , category: "Advanced Examples"
    , route: ModuleGraph
    , thumbnail: "assets/example-thumbnails/module-graph.png"
    }

  -- Alternative Interpreters
  , { id: "mermaid-tree-demo"
    , name: "Mermaid Tree Visualizer"
    , description: "Visualize Tree API structure as Mermaid diagrams - see how trees are composed!"
    , category: "Alternative Interpreters"
    , route: MermaidTreeDemo
    , thumbnail: "assets/example-thumbnails/mermaid-diagram.png"
    }
  ]

render :: forall m. Unit -> H.ComponentHTML Unit () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "examples-gallery-page" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Tree API Examples"
        }

    , HH.main
        [ HP.classes [ HH.ClassName "gallery-content" ] ]
        (renderCategories allExamples)
    ]

-- | Group examples by category and render
renderCategories :: forall w i. Array ExampleMeta -> Array (HH.HTML w i)
renderCategories examples =
  let
    categories =
      [ "Basic Examples"
      , "Chart Examples"
      , "Hierarchies & Simulations"
      , "Advanced Examples"
      , "Alternative Interpreters"
      ]
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
