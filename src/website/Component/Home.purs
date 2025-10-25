module PSD3.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Types (Route(..))

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
            [ -- Tutorial box (cream)
              renderDocBox
                "tutorial"
                "Getting Started"
                "Installation, setup, and your first visualization"
                (routeToPath GettingStarted)
                "Start Here →"
                Nothing

            -- How-to box (light blue)
            , renderDocBox
                "howto"
                "How-to Guides"
                "Step-by-step instructions for building specific visualizations"
                (routeToPath HowtoIndex)
                "Browse Guides →"
                (Just "cogs.jpeg")

            -- Reference box (white)
            , renderDocBox
                "reference"
                "API Reference"
                "Complete technical documentation with type signatures"
                (routeToPath Reference)
                "View API →"
                Nothing

            -- Explanation box (beige)
            , renderDocBox
                "explanation"
                "Understanding"
                "Concepts, patterns, and design philosophy"
                (routeToPath About)
                "Learn More →"
                (Just "bookmark.jpeg")
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
            ]
        ]
    ]

-- | Render a documentation category box
renderDocBox :: forall w i. String -> String -> String -> String -> String -> Maybe String -> HH.HTML w i
renderDocBox category title description path linkText maybeImage =
  HH.a
    [ HP.href $ "#" <> path
    , HP.classes [ HH.ClassName "home-doc-box", HH.ClassName $ "home-doc-box--" <> category ]
    ]
    [ case maybeImage of
        Just imageSrc ->
          HH.img
            [ HP.src imageSrc
            , HP.alt ""
            , HP.classes [ HH.ClassName "home-doc-box__image" ]
            ]
        Nothing -> HH.text ""
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
