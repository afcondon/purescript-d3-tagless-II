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
import PSD3.Shared.SiteNav as SiteNav

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
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Nothing
        }

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
                    , HP.alt "Interactive code explorer"
                    , HP.classes [ HH.ClassName "home-hero-img" ]
                    ]
                , HH.p
                    [ HP.classes [ HH.ClassName "home-hero-caption" ] ]
                    [ HH.text "Explore the "
                    , HH.a
                        [ HP.href $ "#" <> routeToPath CodeExplorer
                        , HP.classes [ HH.ClassName "home-hero-link" ]
                        ]
                        [ HH.text "interactive code explorer" ]
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
            , renderDocBox
                "API Reference"
                "Pursuit-style documentation of the library with type signatures"
                (routeToPath Reference)
            , renderDocBox
                "Understanding"
                "Conceptual overview of the project"
                (routeToPath Understanding)
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
            [ HH.text "A progressive tour of the library's capabilities, from basic visualizations to advanced techniques:" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-tutorials-grid" ] ]
            [ renderTutorialLink "1. Foundations"
                "Three circles, nested data, bar charts - the building blocks of visualization"
                (routeToPath TourFoundations)
            , renderTutorialLink "2. Typical Charts"
                "Grouped bars, multi-line, radial stacked - production-ready visualizations"
                (routeToPath TourProfessional)
            , renderTutorialLink "3. Data Flow"
                "Chord and Sankey diagrams for relationships and flows"
                (routeToPath TourFlow)
            , renderTutorialLink "4. Hierarchies"
                "Trees, dendrograms, treemaps, circle packing - all hierarchy layouts"
                (routeToPath TourHierarchies)
            , renderTutorialLink "5. Motion & Transitions"
                "Animations, General Update Pattern, force-directed graphs"
                (routeToPath TourMotion)
            , renderTutorialLink "5b. Wealth & Health"
                "Animated bubble chart - Hans Rosling's famous visualization"
                (routeToPath TourWealthHealth)
            , renderTutorialLink "6. Alternative Interpreters"
                "Mermaid diagrams, code generation - the power of Finally Tagless"
                (routeToPath TourInterpreters)
            , renderTutorialLink "7. FP For The Win"
                "Maps, Sets, contravariant functors - functional programming superpowers"
                (routeToPath TourFPFTW)
            , renderTutorialLink "8. Showcase"
                "Flagship demonstrations combining multiple techniques"
                (routeToPath Showcase)
            ]
        ]

    -- Examples section - ALL examples organized by category
    , HH.section
        [ HP.id "examples"
        , HP.classes [ HH.ClassName "home-examples" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home-section-title" ] ]
            [ HH.text "Examples Gallery" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-section-description" ] ]
            [ HH.text "Browse all TreeAPI examples organized by category, from basic data joins to advanced simulations." ]

        -- Basic Examples
        , renderExampleCategory "Basic Examples"
            [ renderExampleCard "Three Little Circles" "Data join basics - bind data to DOM elements" "assets/example-thumbnails/three-little-circles.png" (Example "three-little-circles")
            , renderExampleCard "Circles Transition" "Animated transitions with easing and delays" "assets/example-thumbnails/three-little-circles.png" (Example "three-circles-transition")
            , renderExampleCard "Simple Tree" "Basic nesting with named selections" "assets/example-thumbnails/simple-tree.png" (Example "simple-tree")
            , renderExampleCard "Nested Elements" "Multi-level nesting (Group → Circle + Text)" "assets/example-thumbnails/nested-elements.png" (Example "nested-elements")
            , renderExampleCard "Three Little Dimensions" "Nested data joins (2D array → table)" "assets/example-thumbnails/nested-data.png" (Example "three-little-dimensions")
            ]

        -- Chart Examples
        , renderExampleCategory "Chart Examples"
            [ renderExampleCard "Bar Chart" "Data-driven bars with scaling" "assets/example-thumbnails/simple-bar-chart.png" (Example "bar-chart")
            , renderExampleCard "Scatter Plot" "Points positioned by data coordinates" "assets/example-thumbnails/anscombes-quartet.png" (Example "scatter-plot")
            , renderExampleCard "Line Chart" "Path element generated from data" "assets/example-thumbnails/simple-line-chart.png" (Example "line-chart")
            , renderExampleCard "Grouped Bar Chart" "Nested joins with multiple series" "assets/example-thumbnails/grouped-bar-chart.png" (Example "grouped-bar-chart")
            ]

        -- Hierarchies & Simulations
        , renderExampleCategory "Hierarchies & Simulations"
            [ renderExampleCard "Tree Layout" "Hierarchical node-link diagram (pure PureScript)" "assets/example-thumbnails/tree-purescript.png" (Example "simple-hierarchy")
            , renderExampleCard "Les Misérables Network" "Force-directed graph with simulation" "assets/example-thumbnails/les-miserables.png" (Example "lesmis-force")
            ]

        -- Advanced Examples
        , renderExampleCategory "Advanced Examples"
            [ renderExampleCard "Animated Tree ↔ Cluster" "Smooth transitions between pure PureScript layouts" "assets/example-thumbnails/tree-purescript.png" AnimatedTreeCluster
            , renderExampleCard "Les Misérables GUP" "Force layout + dynamic layouts with General Update Pattern" "assets/example-thumbnails/les-miserables-gup.png" LesMisGUPTree
            , renderExampleCard "Module Graph" "Visualize this codebase's own module dependencies" "assets/example-thumbnails/module-graph.png" ModuleGraph
            ]

        -- Alternative Interpreters
        , renderExampleCategory "Alternative Interpreters"
            [ renderExampleCard "Mermaid Tree Visualizer" "Visualize Tree API structure as Mermaid diagrams" "assets/example-thumbnails/mermaid-diagram.png" MermaidTreeDemo
            ]
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

-- | Render a category section with examples
renderExampleCategory :: forall w i. String -> Array (HH.HTML w i) -> HH.HTML w i
renderExampleCategory categoryName examples =
  HH.div
    [ HP.classes [ HH.ClassName "home-gallery-category" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "home-gallery-category-title" ] ]
        [ HH.text categoryName ]
    , HH.div
        [ HP.classes [ HH.ClassName "home-examples-grid" ] ]
        examples
    ]

-- | Render an example card with thumbnail
renderExampleCard :: forall w i. String -> String -> String -> Route -> HH.HTML w i
renderExampleCard title description thumbnail route =
  HH.a
    [ HP.href $ "#" <> routeToPath route
    , HP.classes [ HH.ClassName "home-example-card" ]
    ]
    [ HH.div
        [ HP.classes [ HH.ClassName "home-example-thumbnail" ] ]
        [ HH.img
            [ HP.src thumbnail
            , HP.alt title
            , HP.classes [ HH.ClassName "home-example-thumbnail-img" ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "home-example-content" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "home-example-title" ] ]
            [ HH.text title ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-example-description" ] ]
            [ HH.text description ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
