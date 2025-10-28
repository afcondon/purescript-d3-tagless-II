module PSD3.Understanding.Patterns where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Understanding.TOC (renderTOC, tocAnchor)
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
            [ tocAnchor "heading-datum-pattern" "The datum_ / Datum_ Pattern" 0
            , tocAnchor "heading-grammar" "The Grammar of D3 in SelectionM" 0
            , tocAnchor "heading-dom-to-viz" "From DOM to Visualization Elements" 0
            ]
        , image: Just "images/understanding-bookmark-trees.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: UnderstandingSection
        , currentRoute: UnderstandingPatterns
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
          HH.slot_ _tabs unit UnderstandingTabs.component UnderstandingPatterns

        -- Page title
        , HH.h1
            [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "Practical Patterns" ]

        -- Datum_ Pattern
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-datum-pattern" ]
                [ HH.text "The datum_ / Datum_ Pattern" ]
            , HH.p_ [ HH.text "Placeholder: Explanation of the datum_ accessor pattern for type-safe data access" ]
            ]

        -- SelectionM Grammar
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-grammar" ]
                [ HH.text "The Grammar of D3 in SelectionM" ]
            , HH.p_ [ HH.text "Placeholder: How SelectionM expresses D3's grammar of graphics" ]
            ]

        -- DOM to Elements Flow
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-dom-to-viz" ]
                [ HH.text "From DOM to Visualization Elements" ]
            , HH.p_ [ HH.text "Placeholder: The flow from attachment → nodes → data joins → bulk enter" ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
