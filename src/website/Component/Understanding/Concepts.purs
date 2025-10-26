module PSD3.Understanding.Concepts where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Understanding.UnderstandingTabs as UnderstandingTabs
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

type State = Unit

data Action = Initialize

type Slots =
  ( sectionNav :: forall q. H.Slot q Void Unit
  , tabs :: forall q. H.Slot q Void Unit
  )

_sectionNav = Proxy :: Proxy "sectionNav"
_tabs = Proxy :: Proxy "tabs"

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- TOC Panel (LHS)
      renderTOC
        { title: "Page Contents"
        , items:
            [ { anchor: "heading-finally-tagless", label: "Finally Tagless", level: 0 }
            , { anchor: "heading-selectionm", label: "The SelectionM Monad", level: 0 }
            , { anchor: "heading-capabilities", label: "Capabilities & Interpreters", level: 0 }
            , { anchor: "heading-type-safe", label: "Type-Safe Attribute System", level: 0 }
            ]
        , image: Just "images/understanding-bookmark-trees.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: UnderstandingSection
        , currentRoute: UnderstandingConcepts
        , sectionPages:
            [ { route: UnderstandingConcepts, label: "Concepts" }
            , { route: UnderstandingPatterns, label: "Patterns" }
            , { route: UnderstandingPhilosophy, label: "Philosophy" }
            ]
        , moduleCategories: Nothing
        }

    -- Main content
    , HH.div
        [ HP.classes [ HH.ClassName "explanation-content" ] ]
        [ -- Tab navigation
          HH.slot_ _tabs unit UnderstandingTabs.component UnderstandingConcepts

        -- Page title
        , HH.h1
            [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "Core Concepts" ]

        -- Finally Tagless
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-finally-tagless" ]
                [ HH.text "Finally Tagless" ]
            , HH.p_ [ HH.text "Placeholder: 1-2 paragraphs explaining Finally Tagless pattern" ]
            ]

        -- SelectionM Monad
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-selectionm" ]
                [ HH.text "The SelectionM Monad" ]
            , HH.p_ [ HH.text "Placeholder: 1-2 paragraphs explaining SelectionM" ]
            ]

        -- Capabilities/Interpreters
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-capabilities" ]
                [ HH.text "Capabilities & Interpreters" ]
            , HH.p_ [ HH.text "Placeholder: 1-2 paragraphs explaining the interpreter pattern" ]
            ]

        -- Type-safe Attributes
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-type-safe" ]
                [ HH.text "Type-Safe Attribute System" ]
            , HH.p_ [ HH.text "Placeholder: 1-2 paragraphs explaining the ToAttr typeclass" ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
