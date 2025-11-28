module PSD3.Reference.Reference where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav

-- | Input from parent (receives current route)
type Input = Unit

-- | Reference page state
type State = Unit

-- | Reference page actions
data Action = Initialize

-- | Reference page component
component :: forall q o. H.Component q Input o Aff
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
    [ HP.classes [ HH.ClassName "docs-page", HH.ClassName "reference-index" ] ]
    [ -- Site Navigation with Reference quadrant highlighted
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadReference
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    -- Hero section
    , HH.section
        [ HP.classes [ HH.ClassName "docs-hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "docs-hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "docs-hero-title" ] ]
                [ HH.text "API Reference" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Comprehensive documentation for all PSD3 modules, types, and functions." ]
            ]
        ]

    -- API Documentation cards
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Browse Documentation" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderApiCard "Core Selection API"
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. SelectionM monad and phantom type system."

            , renderApiCard "Simulation & Forces"
                "Sed do eiusmod tempor incididunt ut labore. Force-directed layouts and physics configuration."

            , renderApiCard "TreeAPI & Declarative"
                "Ut enim ad minim veniam, quis nostrud exercitation. Declarative tree structures and interpreters."

            , renderApiCard "Data Utilities"
                "Duis aute irure dolor in reprehenderit in voluptate. Hierarchical data, scales, and transformations."
            ]
        ]
    ]

-- | Render an API card that opens docs in new tab
renderApiCard :: forall w i. String -> String -> HH.HTML w i
renderApiCard title description =
  HH.a
    [ HP.classes [ HH.ClassName "howto-card" ]
    , HP.href "api/index.html"
    , HP.target "_blank"
    ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "howto-card__title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "howto-card__description" ] ]
        [ HH.text description ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
