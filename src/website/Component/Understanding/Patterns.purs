module PSD3.Understanding.Patterns where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
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

-- | Mermaid diagram for the Datum_ pattern flow
datumPatternDiagram :: String
datumPatternDiagram = """
sequenceDiagram
    participant PS as PureScript Code
    participant Datum as datum_ Accessor
    participant Bridge as FFI Bridge
    participant D3 as D3.js
    participant DOM as DOM Element

    PS->>Datum: Define type-safe accessor
    Note right of Datum: datum_ :: a -> b
    Datum->>Bridge: Convert to JS function
    Note right of Bridge: Type erasure
    Bridge->>D3: Pass accessor function
    D3->>DOM: Bind data to elements
    DOM->>D3: Request data value
    D3->>Bridge: Call accessor(d, i)
    Bridge->>Datum: Apply to typed data
    Datum->>PS: Extract typed field
    PS-->>D3: Return value
    D3->>DOM: Update attribute/style

    Note over PS,DOM: Bridges Type Safety and Dynamic Binding
"""

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
            , HH.p_ [ HH.text "The datum_ pattern is a critical bridge between PureScript's type-safe world and D3's dynamic data binding. It allows us to define type-safe accessor functions that extract values from data records while maintaining compatibility with D3's JavaScript API." ]

            -- Datum_ Pattern Flow Diagram
            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram datumPatternDiagram (Just "datum-pattern-diagram") ]

            , HH.p_ [ HH.text "This pattern ensures that data transformations remain type-safe on the PureScript side while seamlessly integrating with D3's dynamic attribute and style setters. The accessor functions are pure and composable, making them easy to test and reuse across different visualizations." ]
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
  Initialize -> triggerMermaidRendering
