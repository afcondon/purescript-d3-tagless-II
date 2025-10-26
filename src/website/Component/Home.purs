module PSD3.Home where -- stays at top level

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

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
    [ -- Hero section
      HH.section
        [ HP.classes [ HH.ClassName "home-hero" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "home-hero__title" ] ]
            [ HH.text "PS<$>D3" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-hero__subtitle" ] ]
            [ HH.text "Type-safe, composable data visualization with PureScript and D3" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-hero__description" ] ]
            [ HH.text "Finally Tagless design, functional programming principles, and the power of D3.js combined into an elegant, type-safe visualization library." ]
        ]

    -- Four documentation category boxes
    , HH.section
        [ HP.classes [ HH.ClassName "home-docs" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "home-docs__grid" ] ]
            [ -- Tutorial box (air themed - balloons) - with two action buttons
              renderGettingStartedBox

            -- How-to box (fire themed - volcano)
            , renderDocBox
                "How-to Guides"
                "Step-by-step instructions for building specific visualizations"
                (routeToPath HowtoIndex)
                "Browse Guides →"
                (Just "images/howto-bookmark-volcano.jpeg")

            -- Reference box (water themed - deep sea vent)
            , renderDocBox
                "API Reference"
                "Complete technical documentation with type signatures"
                (routeToPath Reference)
                "View API →"
                (Just "images/reference-bookmark-deepseavent.jpeg")

            -- Explanation box (earth themed - trees)
            , renderDocBox
                "Understanding"
                "Concepts, patterns, and design philosophy"
                (routeToPath UnderstandingConcepts)
                "Learn More →"
                (Just "images/understanding-bookmark-trees.jpeg")
            ]
        ]

    -- Examples section
    , HH.section
        [ HP.classes [ HH.ClassName "home-examples" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home-examples__title" ] ]
            [ HH.text "Example Visualizations" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-examples__grid" ] ]
            [ renderExampleCard
                "Basic Charts"
                "Bar charts, line charts, and scatter plots"
                (routeToPath Tutorial)
            , renderExampleCard
                "Hierarchies"
                "Trees, dendrograms, circle packing, and treemaps"
                (routeToPath Hierarchies)
            , renderExampleCard
                "Network Diagrams"
                "Chord diagrams, Sankey diagrams, and force layouts"
                (routeToPath ChordDiagram)
            , renderExampleCard
                "Code Explorer"
                "Interactive exploration of visualization code"
                (routeToPath CodeExplorer)
            , renderExampleCard
                "Wealth & Health of Nations"
                "Hans Rosling's animated exploration of income and life expectancy"
                (routeToPath WealthHealth)
            ]
        ]
    ]

-- | Render the Getting Started box with two action buttons
renderGettingStartedBox :: forall w i. HH.HTML w i
renderGettingStartedBox =
  HH.div
    [ HP.classes [ HH.ClassName "home-doc-box home-doc-box--getting-started" ] ]
    [ -- Left side: bookmark image (1/8th width)
      HH.div
        [ HP.classes [ HH.ClassName "home-doc-box__image-container" ] ]
        [ HH.img
            [ HP.src "images/tutorial-bookmark-balloons.jpeg"
            , HP.alt ""
            , HP.classes [ HH.ClassName "home-doc-box__image" ]
            ]
        ]
    -- Right side: content (7/8ths width, center-justified)
    , HH.div
        [ HP.classes [ HH.ClassName "home-doc-box__content" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "home-doc-box__title" ] ]
            [ HH.text "Getting Started" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-doc-box__actions" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath GettingStarted
                , HP.classes [ HH.ClassName "home-doc-box__action-link" ]
                ]
                [ HH.span
                    [ HP.classes [ HH.ClassName "home-doc-box__action-icon" ] ]
                    [ HH.text "📖" ]
                , HH.span_
                    [ HH.text "See how to get set up to use this library on your own system" ]
                ]
            , HH.a
                [ HP.href $ "#" <> routeToPath Wizard
                , HP.classes [ HH.ClassName "home-doc-box__action-link home-doc-box__action-link--primary" ]
                ]
                [ HH.span
                    [ HP.classes [ HH.ClassName "home-doc-box__action-icon" ] ]
                    [ HH.text "🚀" ]
                , HH.span_
                    [ HH.text "Use interactive wizard to generate and, optionally, download working code" ]
                ]
            ]
        ]
    ]

-- | Render a documentation category box
renderDocBox :: forall w i. String -> String -> String -> String -> Maybe String -> HH.HTML w i
renderDocBox title description path linkText maybeImage =
  HH.a
    [ HP.href $ "#" <> path
    , HP.classes [ HH.ClassName "home-doc-box" ]
    ]
    [ -- Left side: bookmark image (1/8th width)
      case maybeImage of
        Just imageSrc ->
          HH.div
            [ HP.classes [ HH.ClassName "home-doc-box__image-container" ] ]
            [ HH.img
                [ HP.src imageSrc
                , HP.alt ""
                , HP.classes [ HH.ClassName "home-doc-box__image" ]
                ]
            ]
        Nothing ->
          HH.div
            [ HP.classes [ HH.ClassName "home-doc-box__image-container" ] ]
            []
    -- Right side: content (7/8ths width, center-justified)
    , HH.div
        [ HP.classes [ HH.ClassName "home-doc-box__content" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "home-doc-box__title" ] ]
            [ HH.text title ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-doc-box__description" ] ]
            [ HH.text description ]
        , HH.span
            [ HP.classes [ HH.ClassName "home-doc-box__link" ] ]
            [ HH.text linkText ]
        ]
    ]

-- | Render an example card
renderExampleCard :: forall w i. String -> String -> String -> HH.HTML w i
renderExampleCard title description path =
  HH.a
    [ HP.href $ "#" <> path
    , HP.classes [ HH.ClassName "home-example-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "home-example-card__title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "home-example-card__description" ] ]
        [ HH.text description ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
