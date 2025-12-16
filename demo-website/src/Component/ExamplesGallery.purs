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
  , { id: "multi-line-chart"
    , name: "Multi-Line Chart"
    , description: "Multiple data series with path generator"
    , category: "Chart Examples"
    , route: Example "multi-line-chart"
    , thumbnail: "assets/example-thumbnails/simple-line-chart.png"
    }
  , { id: "radial-stacked-bar"
    , name: "Radial Stacked Bar"
    , description: "Stacked bars in polar coordinates"
    , category: "Chart Examples"
    , route: Example "radial-stacked-bar"
    , thumbnail: "assets/example-thumbnails/radial-stacked-bar.png"
    }
  , { id: "anscombes-quartet"
    , name: "Anscombe's Quartet"
    , description: "Four datasets with same statistics, different shapes"
    , category: "Chart Examples"
    , route: Example "anscombes-quartet"
    , thumbnail: "assets/example-thumbnails/anscombes-quartet.png"
    }
  , { id: "wealth-health"
    , name: "Wealth & Health"
    , description: "Gapminder-style bubble chart"
    , category: "Chart Examples"
    , route: Example "wealth-health"
    , thumbnail: "assets/example-thumbnails/wealth-health.png"
    }

  -- Hierarchies
  , { id: "simple-hierarchy"
    , name: "Tree Layout"
    , description: "Vertical tree with node-link diagram"
    , category: "Hierarchies"
    , route: Example "simple-hierarchy"
    , thumbnail: "assets/example-thumbnails/tree-purescript.png"
    }
  , { id: "radial-tree"
    , name: "Radial Tree"
    , description: "Tree layout in polar coordinates"
    , category: "Hierarchies"
    , route: Example "radial-tree"
    , thumbnail: "assets/example-thumbnails/radial-tree.png"
    }
  , { id: "horizontal-tree"
    , name: "Horizontal Tree"
    , description: "Tree layout oriented left-to-right"
    , category: "Hierarchies"
    , route: Example "horizontal-tree"
    , thumbnail: "assets/example-thumbnails/horizontal-tree.png"
    }
  , { id: "cluster"
    , name: "Cluster Dendrogram"
    , description: "Leaf nodes aligned at same depth"
    , category: "Hierarchies"
    , route: Example "cluster"
    , thumbnail: "assets/example-thumbnails/cluster.png"
    }
  , { id: "treemap"
    , name: "Treemap"
    , description: "Nested rectangles sized by value"
    , category: "Hierarchies"
    , route: Example "treemap"
    , thumbnail: "assets/example-thumbnails/treemap.png"
    }
  , { id: "sunburst"
    , name: "Sunburst"
    , description: "Radial partition diagram"
    , category: "Hierarchies"
    , route: Example "sunburst"
    , thumbnail: "assets/example-thumbnails/sunburst.png"
    }
  , { id: "pack"
    , name: "Circle Packing"
    , description: "Nested circles sized by value"
    , category: "Hierarchies"
    , route: Example "pack"
    , thumbnail: "assets/example-thumbnails/pack.png"
    }
  , { id: "partition"
    , name: "Icicle / Partition"
    , description: "Rectangular partition diagram"
    , category: "Hierarchies"
    , route: Example "partition"
    , thumbnail: "assets/example-thumbnails/partition.png"
    }
  , { id: "animated-tree-cluster"
    , name: "Animated Tree ↔ Cluster"
    , description: "Smooth transitions between layouts"
    , category: "Hierarchies"
    , route: AnimatedTreeCluster
    , thumbnail: "assets/example-thumbnails/tree-purescript.png"
    }

  -- Relational
  , { id: "sankey"
    , name: "Sankey Diagram"
    , description: "Flow diagram with weighted links"
    , category: "Relational"
    , route: Example "sankey"
    , thumbnail: "assets/example-thumbnails/sankey.png"
    }
  , { id: "chord"
    , name: "Chord Diagram"
    , description: "Circular flow between groups"
    , category: "Relational"
    , route: Example "chord"
    , thumbnail: "assets/example-thumbnails/chord.png"
    }
  , { id: "edge-bundle"
    , name: "Edge Bundling"
    , description: "Hierarchical edge bundling"
    , category: "Relational"
    , route: Example "edge-bundle"
    , thumbnail: "assets/example-thumbnails/edge-bundle.png"
    }

  -- Force-Directed
  , { id: "simple-force"
    , name: "Simple Force Graph"
    , description: "Basic force-directed layout"
    , category: "Force-Directed"
    , route: Example "simple-force"
    , thumbnail: "assets/example-thumbnails/simple-force.png"
    }
  , { id: "les-mis"
    , name: "Les Misérables Network"
    , description: "Character co-occurrence network"
    , category: "Force-Directed"
    , route: Example "les-mis"
    , thumbnail: "assets/example-thumbnails/les-miserables.png"
    }
  , { id: "lesmis-gup-tree"
    , name: "Les Mis with GUP"
    , description: "Dynamic layouts with General Update Pattern"
    , category: "Force-Directed"
    , route: LesMisGUPTree
    , thumbnail: "assets/example-thumbnails/les-miserables-gup.png"
    }

  -- v3 DSL Examples
  , { id: "v3-parabola"
    , name: "v3 Parabola"
    , description: "Finally Tagless DSL with polymorphic expressions"
    , category: "v3 DSL Examples"
    , route: Example "v3-parabola"
    , thumbnail: "assets/example-thumbnails/parabola.png"
    }
  , { id: "v3-transition"
    , name: "v3 Transition"
    , description: "v3 expressions as animated transition targets"
    , category: "v3 DSL Examples"
    , route: Example "v3-transition"
    , thumbnail: "assets/example-thumbnails/v3-transition.png"
    }
  , { id: "v3-gup"
    , name: "v3 GUP"
    , description: "General Update Pattern with staggered transitions"
    , category: "v3 DSL Examples"
    , route: Example "v3-gup"
    , thumbnail: "assets/example-thumbnails/v3-gup.png"
    }

  -- Advanced Examples
  , { id: "splom"
    , name: "SPLOM"
    , description: "Brushable Scatterplot Matrix"
    , category: "Advanced Examples"
    , route: SPLOM
    , thumbnail: "assets/example-thumbnails/splom.png"
    }
  , { id: "module-graph"
    , name: "Module Graph"
    , description: "Visualize this codebase's module dependencies"
    , category: "Advanced Examples"
    , route: ModuleGraph
    , thumbnail: "assets/example-thumbnails/module-graph.png"
    }

  -- Alternative Interpreters
  , { id: "mermaid-tree-demo"
    , name: "Mermaid Tree Visualizer"
    , description: "Visualize Tree API as Mermaid diagrams"
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
      , "Hierarchies"
      , "Relational"
      , "Force-Directed"
      , "v3 DSL Examples"
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
