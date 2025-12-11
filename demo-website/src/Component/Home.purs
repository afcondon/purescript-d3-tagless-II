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
                    [ HP.src "assets/example-thumbnails/force-playground.png"
                    , HP.alt "Force Playground visualization"
                    , HP.classes [ HH.ClassName "home-hero-img" ]
                    ]
                , HH.p
                    [ HP.classes [ HH.ClassName "home-hero-caption" ] ]
                    [ HH.text "Try the "
                    , HH.a
                        [ HP.href $ "#" <> routeToPath ForcePlayground
                        , HP.classes [ HH.ClassName "home-hero-link" ]
                        ]
                        [ HH.text "Force Playground" ]
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
