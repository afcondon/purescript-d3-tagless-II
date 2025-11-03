module PSD3.Understanding.Patterns where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3 as D3
import PSD3.Attributes (classed, fill, fontSize, viewBox, x, y, andThen, to, remove, transitionWithDuration)
import PSD3.Data.Node (NodeID)
import PSD3.Interpreter.MermaidAST (MermaidASTM)
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3.Shared.MermaidAST as MermaidAST
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Types (Element(..))
import PSD3.Understanding.TOC (renderTOC, tocAnchor)
import PSD3.Understanding.UnderstandingTabs as UnderstandingTabs
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type State = Unit

data Action = Initialize

type Slots =
  ( sectionNav :: forall q. H.Slot q Void Unit
  , tabs :: forall q. H.Slot q Void Unit
  , mermaidAST :: MermaidAST.Slot Unit
  , docsHeader :: forall q. H.Slot q Void Unit
  )

_sectionNav = Proxy :: Proxy "sectionNav"
_tabs = Proxy :: Proxy "tabs"
_docsHeader = Proxy :: Proxy "docsHeader"

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

-- | General Update Pattern (GUP) example showing enter/update/exit
gupVisualization :: MermaidASTM NodeID
gupVisualization = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg [viewBox 0.0 100.0 800.0 350.0, classed "d3svg gup"]
  letterGroup <- D3.appendTo svg Group []

  -- Simulate the updateJoin call (this is what gets called repeatedly)
  enterSelection <- D3.openSelection letterGroup "text"
  { enter, update, exit } <- D3.updateJoin enterSelection Text [1, 2, 3] unsafeCoerce

  -- Set attributes on exit selection with transition
  let transition = transitionWithDuration $ Milliseconds 2000.0
  let exitAttrs = [classed "exit", fill "brown"] `andThen` (transition `to` [y 400.0, remove])
  D3.setAttributes exit exitAttrs

  -- Set attributes on update selection with transition
  let updateAttrs = [classed "update", fill "gray", y 200.0] `andThen` (transition `to` [x 50.0])
  D3.setAttributes update updateAttrs

  -- Append new text elements to enter selection
  newlyEntered <- D3.appendTo enter Text []
  let enterAttrs = [ classed "enter"
                    , fill "green"
                    , x 50.0
                    , y 0.0
                    , fontSize 60.0
                    ] `andThen` (transition `to` [y 200.0])
  D3.setAttributes newlyEntered enterAttrs

  pure newlyEntered

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
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just UnderstandingSection }

    -- TOC Panel (LHS)
    , renderTOC
        { title: "Page Contents"
        , items:
            [ tocAnchor "heading-datum-pattern" "The datum_ / Datum_ Pattern" 0
            , tocAnchor "heading-grammar" "The Grammar of D3 in SelectionM" 0
            , tocAnchor "heading-dom-to-viz" "From DOM to Visualization Elements" 0
            ]
        , image: Just "assets/bookmark-images/understanding.jpeg"
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
            , HH.p_ [ HH.text "SelectionM provides a compositional grammar for expressing D3 visualizations. The General Update Pattern (GUP) below demonstrates how SelectionM captures the essential operations: openSelection for nested data joins, updateJoin for synchronizing data with DOM elements, and transitions for smooth animations." ]

            -- GUP AST Diagram
            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
                    (MermaidAST.mkInput gupVisualization)
                ]

            , HH.p_ [ HH.text "This AST diagram shows how the General Update Pattern expresses the classic D3 workflow: selecting existing elements, binding new data, handling enter/update/exit selections, and applying transitions. The same SelectionM code can be interpreted multiple ways - as live D3 visualizations, as static diagrams, or as English descriptions - demonstrating the power of the finally tagless pattern." ]
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
