module PSD3.Understanding where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

-- | Understanding page state
type State = Unit

-- | Understanding page actions
data Action = Initialize

-- | Understanding page component
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
    [ HP.classes [ HH.ClassName "docs-page", HH.ClassName "understanding-index" ] ]
    [ -- Site Navigation with Understanding quadrant highlighted
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadUnderstanding
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
                [ HH.text "Understanding" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "Deep dives into the core concepts and patterns that make PSD3 a type-safe, composable framework for D3 visualizations." ]
            ]
        ]

    -- Core Concepts
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Core Concepts" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderCard UnderstandingGrammar "The Grammar of D3"
                "Four essential primitives: select, append, join, attr. Everything else builds from these."

            , renderCard UnderstandingAttributes "Type-Safe Attributes"
                "Static values, datum functions, contravariant pattern. Compile-time type safety."

            , renderCard UnderstandingSelections "Selection Phantom Types"
                "Five states: SEmpty, SPending, SBoundOwns, SBoundInherits, SExiting. Indexed Monad pattern."
            ]
        ]

    -- Architecture
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Architecture" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderCard UnderstandingTreeAPI "TreeAPI"
                "Declarative tree structures. Layer cake architecture with TreeAPI and Simulation API on SelectionM."

            , renderCard UnderstandingScenes "Scene Structures & Transitions"
                "Declarative state management for complex interactive visualizations with transition matrices."
            ]
        ]
    ]

-- | Render a card with link, title, and description
renderCard :: forall w i. Route -> String -> String -> HH.HTML w i
renderCard route title description =
  HH.a
    [ HP.classes [ HH.ClassName "howto-card" ]
    , HP.href $ "#" <> routeToPath route
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
