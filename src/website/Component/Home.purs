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
            , renderDocBoxExternal
                "API Reference"
                "Pursuit-style documentation of the library with type signatures"
                "api/index.html"
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
            , renderTutorialLink "6. Alternative Interpreters"
                "Mermaid diagrams, code generation - the power of Finally Tagless"
                (routeToPath TourInterpreters)
            , renderTutorialLink "7. FP For The Win"
                "Maps, Sets, contravariant functors - functional programming superpowers"
                (routeToPath TourFPFTW)
            , renderTutorialLink "8. Showcase"
                "Flagship demonstrations combining multiple techniques"
                (routeToPath TourShowcase)
            ]
        ]

    -- Examples section
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
        , HH.div
            [ HP.classes [ HH.ClassName "home-gallery-cta" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Gallery
                , HP.classes [ HH.ClassName "home-gallery-link" ]
                ]
                [ HH.text "View Examples Gallery →" ]
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

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
