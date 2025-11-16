module PSD3.Understanding.UnderstandingDatumPattern where

import Prelude

import CodeSnippet (codeSnippet, triggerPrismHighlighting)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3.Website.Types (Section(..))
import Type.Proxy (Proxy(..))

type State = Unit

data Action = Initialize

type Slots =
  ( docsHeader :: forall q. H.Slot q Void Unit
  )

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

    -- Main content
    , HH.div
        [ HP.classes [ HH.ClassName "explanation-content" ] ]
        [ -- Page title
          HH.h1
            [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "The Datum_ Pattern" ]

        , HH.p_ [ HH.text "The datum_ pattern is a practical way to bridge between PureScript's type-safe world and D3's dynamic data binding. It allows us to define type-safe accessor functions that extract values from data records while maintaining compatibility with D3's JavaScript API." ]
        , HH.p_ [ HH.text "When we join some data in PS<$>D3 and evaluate that in the D3 interpreter, then all those elements of whatever the datum is are thrown over the wall to JavaScript where they lose their type information. This wouldn't matter except that we wish to write typesafe attribute functions that operate on this data, using PureScript lambdas. And, of course, we want them to be typed, it's useful, it enables us to turn run-time errors into compile time ones. " ]
        , HH.p_ [ HH.text "The way we solve this is, given that we joined some type X, write a datum_ record which contains functions to unwrap the foreign type Datum_ which we KNOW to be of type X."]

        , HH.h2
            [ HP.classes [ HH.ClassName "explanation-section"] ]
            [ HH.text "The Parabola Example"]
        , HH.p_ [ HH.text "A concrete example might help here so let's look at the code from the Parabola example."]

        , HH.p_ [ HH.text "Model and datum_ accessor for Parabola example" ]
        -- SNIPPET: parabolaDatum src/website/viz/Parabola/Parabola.purs 15-27
        , codeSnippet "parabolaDatum" "haskell"

        , HH.p_ [ HH.text "Unsafe.purs for Parabola example" ]
        -- SNIPPET: parabolaUnsafe src/website/viz/Parabola/Unsafe.purs 1-10
        , codeSnippet "parabolaUnsafe" "haskell"

        , HH.p_ [ HH.text "Model and datum_ accessor for Parabola example" ]
        -- SNIPPET: parabolaDraw src/website/viz/Parabola/Parabola.purs 32-44
        , codeSnippet "parabolaDraw" "haskell"

        -- Datum_ Pattern Flow Diagram
        , HH.div
            [ HP.classes [ HH.ClassName "diagram-container" ] ]
            [ mermaidDiagram datumPatternDiagram (Just "datum-pattern-diagram") ]

        , HH.p_ [ HH.text "The diagram above describes the type erasure and recovery that we are doing using this technique. " ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do 
    triggerMermaidRendering
    triggerPrismHighlighting