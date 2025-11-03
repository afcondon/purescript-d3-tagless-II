module PSD3.Home where -- stays at top level

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))
import PSD3.Shared.Footer as Footer

-- | Home page state
type State = Unit

-- | Home page actions
data Action = Initialize

-- | Home page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "home-page" ] ]
    [ -- Header with logo and navigation
      HH.header
        [ HP.classes [ HH.ClassName "home-header" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "home-header-content" ] ]
            [ HH.a
                [ HP.href "#hero"
                , HP.classes [ HH.ClassName "home-logo-link" ]
                ]
                [ HH.img
                    [ HP.src "assets/psd3-logo-color.svg"
                    , HP.alt "PSD3 Logo"
                    , HP.classes [ HH.ClassName "home-logo" ]
                    ]
                ]
            , HH.nav
                [ HP.classes [ HH.ClassName "home-nav" ] ]
                [ HH.a
                    [ HP.href "#docs"
                    , HP.classes [ HH.ClassName "home-nav-link" ]
                    ]
                    [ HH.text "Docs" ]
                , HH.a
                    [ HP.href "#tutorials"
                    , HP.classes [ HH.ClassName "home-nav-link" ]
                    ]
                    [ HH.text "Tour" ]
                , HH.a
                    [ HP.href "#examples"
                    , HP.classes [ HH.ClassName "home-nav-link" ]
                    ]
                    [ HH.text "Examples' code" ]
                , HH.a
                    [ HP.href $ "#" <> routeToPath Wizard
                    , HP.classes [ HH.ClassName "home-nav-link home-nav-link--cta" ]
                    ]
                    [ HH.text "Get Started" ]
                ]
            ]
        ]

    -- Hero section
    , HH.section
        [ HP.id "hero"
        , HP.classes [ HH.ClassName "home-hero" ]
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "home-hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "home-hero-title" ] ]
                [ HH.text "Type-safe, composable, maintainable, interactive data visualisation with PureScript and D3.js" ]
            , HH.div
                [ HP.classes [ HH.ClassName "home-hero-image" ] ]
                [ HH.img
                    [ HP.src "assets/example-thumbnails/code-explorer.png"
                    , HP.alt "Code Explorer visualization"
                    , HP.classes [ HH.ClassName "home-hero-img" ]
                    ]
                , HH.p
                    [ HP.classes [ HH.ClassName "home-hero-caption" ] ]
                    [ HH.text "Learn how to make powerful interactive data-driven apps like this "
                    , HH.a
                        [ HP.href $ "#" <> routeToPath CodeExplorer
                        , HP.classes [ HH.ClassName "home-hero-link" ]
                        ]
                        [ HH.text "code explorer" ]
                    ]
                ]
            ]
        ]

    -- Documentation section
    , HH.section
        [ HP.id "docs"
        , HP.classes [ HH.ClassName "home-docs" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home-section-title" ] ]
            [ HH.text "Documentation" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-docs-grid" ] ]
            [ renderDocBox
                "Getting Started"
                "Installation, setup, and your first visualization"
                (routeToPath GettingStarted)
            , renderDocBox
                "How-to Guides"
                "Step-by-step instructions for specific visualizations"
                (routeToPath HowtoIndex)
            , renderDocBoxExternal
                "API Reference"
                "Pursuit-style documentation of the library with type signatures"
                "api/index.html"
            , renderDocBox
                "Understanding"
                "Concepts, patterns, and design philosophy"
                (routeToPath UnderstandingConcepts)
            ]
        ]

    -- Tutorials section (examples in context)
    , HH.section
        [ HP.id "tutorials"
        , HP.classes [ HH.ClassName "home-tutorials" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home-section-title" ] ]
            [ HH.text "Take the Tour" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-section-description" ] ]
            [ HH.text "You can read through the following to progressively build up an understanding of the capabilities and concepts:" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-tutorials-grid" ] ]
            [ renderTutorialLink "1. Simplest Charts"
                "Start with the basics: three circles, positioning, and data binding"
                (routeToPath SimpleCharts1)
            , renderTutorialLink "2. Lines and Bars"
                "Create line and bar charts with scales, axes, and labels"
                (routeToPath SimpleCharts2)
            , renderTutorialLink "3. Hierarchies"
                "Trees, treemaps, and circle packing for hierarchical data"
                (routeToPath Hierarchies)
            , renderTutorialLink "4. Data Flow Visualizations"
                "Chord diagrams and Sankey charts for showing relationships and flows"
                (routeToPath DataFlowViz)
            , renderTutorialLink "5. Movement & Transition"
                "Animations, transitions, and the general update pattern"
                (routeToPath Movement)
            , renderTutorialLink "6. FP FTW"
                "Functional programming patterns with Maps, Sets, and type-safe graphs"
                (routeToPath FpFtw)
            , renderTutorialLink "7. Interpreters"
                "How the tagless final pattern enables framework-independent code"
                (routeToPath Interpreters)
            ]
        ]

    -- Larger examples section
    , HH.section
        [ HP.id "apps"
        , HP.classes [ HH.ClassName "home-tutorials" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home-section-title" ] ]
            [ HH.text "Interactive, data-driven apps" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-section-description" ] ]
            [ HH.text "These examples are more involved than a simple visualisation" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-tutorials-grid" ] ]
            [ renderTutorialLink "Wealth & Health of Nations"
                "Animated scatterplot showing global development over time"
                (routeToPath WealthHealth)
            , renderTutorialLink "Code Explorer"
                "Interactive force-directed graph exploring PureScript dependencies"
                (routeToPath CodeExplorer)
            , renderTutorialLink "Code Atlas"
                "Visualize codebase structure with multiple interactive views"
                (routeToPath CodeAtlas)
            ]
        ]

    -- Examples section (integrated gallery)
    , HH.section
        [ HP.id "examples"
        , HP.classes [ HH.ClassName "home-examples" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home-section-title" ] ]
            [ HH.text "Examples Side-by-side with their code" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-section-description" ] ]
            [ HH.text "Browse all the visualizations used in the tour side-by-side with their full source code" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-examples-content" ] ]
            (renderCategories allExamples)
        ]

    -- Footer
    , Footer.render
    ]

-- | Render a documentation category box with bookmark (internal route)
renderDocBox :: forall w i. String -> String -> String -> HH.HTML w i
renderDocBox title description path =
  HH.a
    [ HP.href $ "#" <> path
    , HP.classes [ HH.ClassName "home-doc-box" ]
    ]
    [ HH.div
        [ HP.classes [ HH.ClassName "home-doc-box__image-container" ] ]
        [ HH.img
            [ HP.src $ getBookmarkImage title
            , HP.alt ""
            , HP.classes [ HH.ClassName "home-doc-box__image" ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "home-doc-box__content" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "home-doc-box-title" ] ]
            [ HH.text title ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-doc-box-description" ] ]
            [ HH.text description ]
        ]
    ]

-- | Render a documentation category box with bookmark (external URL)
renderDocBoxExternal :: forall w i. String -> String -> String -> HH.HTML w i
renderDocBoxExternal title description url =
  HH.a
    [ HP.href url
    , HP.classes [ HH.ClassName "home-doc-box" ]
    ]
    [ HH.div
        [ HP.classes [ HH.ClassName "home-doc-box__image-container" ] ]
        [ HH.img
            [ HP.src $ getBookmarkImage title
            , HP.alt ""
            , HP.classes [ HH.ClassName "home-doc-box__image" ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "home-doc-box__content" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "home-doc-box-title" ] ]
            [ HH.text title ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-doc-box-description" ] ]
            [ HH.text description ]
        ]
    ]

-- | Get bookmark image path based on title
getBookmarkImage :: String -> String
getBookmarkImage = case _ of
  "Getting Started" -> "assets/bookmark-images/getting-started.jpeg"
  "How-to Guides" -> "assets/bookmark-images/howto.jpeg"
  "API Reference" -> "assets/bookmark-images/reference.jpeg"
  "Understanding" -> "assets/bookmark-images/understanding.jpeg"
  _ -> "assets/bookmark-images/howto.jpeg"

-- | Render a tutorial link card
renderTutorialLink :: forall w i. String -> String -> String -> HH.HTML w i
renderTutorialLink title description path =
  HH.a
    [ HP.href $ "#" <> path
    , HP.classes [ HH.ClassName "home-tutorial-card" ]
    ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "home-tutorial-card-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "home-tutorial-card-description" ] ]
        [ HH.text description ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

-- | Example metadata for the gallery
type ExampleMeta =
  { id :: String
  , name :: String
  , description :: String
  , category :: String
  , route :: Route
  , thumbnail :: String
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
  , { id: "radial-stacked-bar"
    , name: "Radial Stacked Bar Chart"
    , description: "Population by age and state in radial form"
    , category: "Simple Charts"
    , route: Example "radial-stacked-bar"
    , thumbnail: "assets/example-thumbnails/stacked-radial-bar-chart.png"
    }
  , { id: "parabola"
    , name: "Parabola"
    , description: "Colored circles in parabolic formation"
    , category: "Simple Charts"
    , route: Example "parabola"
    , thumbnail: "assets/example-thumbnails/parabola.png"
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
  , { id: "nested-data"
    , name: "Nested Data"
    , description: "Nested selections with 2D arrays"
    , category: "Rich Data Structures"
    , route: Example "nested-data"
    , thumbnail: "assets/example-thumbnails/nested-data.png"
    }
  , { id: "working-with-sets"
    , name: "Working with Sets"
    , description: "Nested selections with Set data structures"
    , category: "Rich Data Structures"
    , route: Example "working-with-sets"
    , thumbnail: "assets/example-thumbnails/working-with-sets.png"
    }

  -- Animations
  , { id: "wealth-health"
    , name: "Wealth & Health of Nations"
    , description: "Animated scatterplot across time"
    , category: "Animations"
    , route: Example "wealth-health"
    , thumbnail: "assets/example-thumbnails/wealth-and-health-of-nations.png"
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

-- | Group examples by category and render
renderCategories :: forall w i. Array ExampleMeta -> Array (HH.HTML w i)
renderCategories examples =
  let
    categories = [ "Simple Charts", "Hierarchies", "Force-Directed", "Data Flow", "Rich Data Structures", "Animations", "Transitions" ]
  in
    categories <#> \category ->
      let examplesInCategory = examples # filter (\ex -> ex.category == category)
      in renderCategory category examplesInCategory

-- | Render a category section
renderCategory :: forall w i. String -> Array ExampleMeta -> HH.HTML w i
renderCategory categoryName examples =
  HH.div
    [ HP.classes [ HH.ClassName "home-gallery-category" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "home-gallery-category-title" ] ]
        [ HH.text categoryName ]
    , HH.div
        [ HP.classes [ HH.ClassName "home-gallery-grid" ] ]
        (examples <#> renderExampleCard)
    ]

-- | Render an individual example card
renderExampleCard :: forall w i. ExampleMeta -> HH.HTML w i
renderExampleCard ex =
  HH.a
    [ HP.href $ "#" <> routeToPath ex.route
    , HP.classes [ HH.ClassName "home-example-card" ]
    ]
    [ HH.div
        [ HP.classes [ HH.ClassName "home-example-thumbnail" ] ]
        [ HH.img
            [ HP.src ex.thumbnail
            , HP.alt ex.name
            , HP.classes [ HH.ClassName "home-example-thumbnail-img" ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "home-example-card-content" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "home-example-card-title" ] ]
            [ HH.text ex.name ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-example-card-description" ] ]
            [ HH.text ex.description ]
        ]
    ]

-- | Filter examples
filter :: forall a. (a -> Boolean) -> Array a -> Array a
filter = filterImpl

foreign import filterImpl :: forall a. (a -> Boolean) -> Array a -> Array a
