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
    [ -- Hero section
      HH.section
        [ HP.classes [ HH.ClassName "home-hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "home-hero__logo" ] ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "home-hero__logo-image" ]
                ]
            ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-hero__subtitle" ] ]
            [ HH.text "Type-safe, composable data visualization with PureScript and D3" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home-hero__description" ] ]
            [ HH.text "Finally Tagless design, functional programming principles, and the power of D3.js combined into an elegant, type-safe visualization library." ]
        ]

    -- Call to action buttons
    , HH.section
        [ HP.classes [ HH.ClassName "home-cta" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Wizard
            , HP.classes [ HH.ClassName "home-cta__button home-cta__button--primary" ]
            ]
            [ HH.span
                [ HP.classes [ HH.ClassName "home-cta__icon" ] ]
                [ HH.text "🚀" ]
            , HH.span_ [ HH.text "Start the Wizard" ]
            ]
        , HH.a
            [ HP.href "#examples"
            , HP.classes [ HH.ClassName "home-cta__button home-cta__button--secondary" ]
            ]
            [ HH.span
                [ HP.classes [ HH.ClassName "home-cta__icon" ] ]
                [ HH.text "✨" ]
            , HH.span_ [ HH.text "View Examples" ]
            ]
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
        [ HP.id "examples"
        , HP.classes [ HH.ClassName "home-examples" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home-examples__title" ] ]
            [ HH.text "Example Visualizations" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home-examples__grid" ] ]
            [ renderExampleCard
                "Simple Charts"
                "Bar charts, line charts, and scatter plots"
                (routeToPath SimpleCharts1)
            , renderExampleCard
                "Data Flow Visualizations"
                "Chord and Sankey diagrams for relationships and flows"
                (routeToPath DataFlowViz)
            , renderExampleCard
                "Hierarchies"
                "Trees, dendrograms, circle packing, and treemaps"
                (routeToPath Hierarchies)
            , renderExampleCard
                "Movement"
                "Movement, animation and transitions"
                (routeToPath Movement)
            , renderExampleCard
                "Interpreters"
                "Alternative interpreters: String and MetaTree"
                (routeToPath Interpreters)
            , renderExampleCard
                "Code Explorer"
                "Interactive dependency graph of PureScript code"
                (routeToPath CodeExplorer)
            , renderExampleCard
                "Wealth & Health"
                "Hans Rosling's animated exploration (1800-2020)"
                (routeToPath WealthHealth)
            , renderExampleCard
                "FP FTW"
                "Functional programming showcases - unique PSD3 capabilities"
                (routeToPath FpFtw)
            ]
        ]

    -- Footer
    , Footer.render
    ]

-- | Render the Getting Started box (now just links to the page, wizard is in CTAs)
renderGettingStartedBox :: forall w i. HH.HTML w i
renderGettingStartedBox =
  renderDocBox
    "Getting Started"
    "Installation, setup, and your first visualization"
    (routeToPath GettingStarted)
    "Get Started →"
    (Just "images/tutorial-bookmark-balloons.jpeg")

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
