module PSD3.Understanding where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- Header with navigation
      HH.header
        [ HP.classes [ HH.ClassName "example-header" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "example-header-left" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Home
                , HP.classes [ HH.ClassName "example-logo-link" ]
                ]
                [ HH.img
                    [ HP.src "assets/psd3-logo-color.svg"
                    , HP.alt "PSD3 Logo"
                    , HP.classes [ HH.ClassName "example-logo" ]
                    ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "example-title-container" ] ]
                [ HH.h1
                    [ HP.classes [ HH.ClassName "example-title" ] ]
                    [ HH.text "Understanding PSD3" ]
                ]
            ]
        ]

    , HH.main_
        [ -- Introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Understanding PSD3v2" ]
            , HH.p_
                [ HH.text "Deep dives into the core concepts and patterns that make PSD3 a type-safe, composable framework for D3 visualizations. Click any card to learn more." ]
            ]

        -- Core Concepts Section
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Core Concepts" ]
            , HH.p_
                [ HH.text "Essential concepts that form the foundation of PSD3's architecture." ]

            -- Four-square navigation grid
            , HH.div
                [ HP.classes [ HH.ClassName "understanding-nav-grid" ] ]
                [ renderNavCard
                    "The Grammar of D3"
                    "Four essential primitives: select, append, join, attr. Everything else builds from these."
                    UnderstandingGrammar
                , renderNavCard
                    "Type-Safe Attributes"
                    "Static values, datum functions, contravariant pattern. Compile-time type safety."
                    UnderstandingAttributes
                , renderNavCard
                    "Selection Phantom Types"
                    "Five states: SEmpty, SPending, SBoundOwns, SBoundInherits, SExiting. Indexed Monad pattern."
                    UnderstandingSelections
                , renderNavCard
                    "TreeAPI"
                    "Declarative tree structures. Layer cake: TreeAPI and Simulation API on SelectionM."
                    UnderstandingTreeAPI
                ]
            ]

        -- Advanced Topics Section
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Advanced Topics" ]
            , HH.p_
                [ HH.text "Complex patterns for interactive, updating visualizations." ]

            , HH.div
                [ HP.classes [ HH.ClassName "understanding-nav-grid", HH.ClassName "single-column" ] ]
                [ renderNavCard
                    "Scene Structures & Transitions"
                    "Declarative state management for complex interactive visualizations. Transition matrices for multi-state apps."
                    UnderstandingScenes
                ]
            ]

        -- Quick Links Section
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Related Resources" ]
            , HH.ul_
                [ HH.li_
                    [ HH.a
                        [ HP.href "#/tour/foundations" ]
                        [ HH.text "Tour: See these concepts in action" ]
                    ]
                , HH.li_
                    [ HH.a
                        [ HP.href "#/tour/interpreters" ]
                        [ HH.text "Tour: Alternative Interpreters" ]
                    ]
                , HH.li_
                    [ HH.a
                        [ HP.href "#/reference" ]
                        [ HH.text "API Reference" ]
                    ]
                ]
            ]
        ]
    ]

-- | Render a navigation card
renderNavCard :: forall w i. String -> String -> Route -> HH.HTML w i
renderNavCard title description route =
  HH.a
    [ HP.href $ "#" <> routeToPath route
    , HP.classes [ HH.ClassName "understanding-nav-card" ]
    ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "understanding-nav-card__title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "understanding-nav-card__description" ] ]
        [ HH.text description ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
