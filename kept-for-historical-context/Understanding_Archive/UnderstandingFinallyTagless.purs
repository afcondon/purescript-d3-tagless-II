module PSD3.Understanding.UnderstandingFinallyTagless where

import Prelude

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

-- | Mermaid diagram for the Finally Tagless architecture
finallyTaglessArchDiagram :: String
finallyTaglessArchDiagram = """
graph TB
    subgraph "User Code"
        UC[Visualization Code]
    end

    subgraph "Finally Tagless DSL"
        SM[SelectionM Monad]
        CM[Capabilities]
    end

    subgraph "Interpreters"
        D3I[D3 Interpreter]
        STI[String Interpreter]
        MTI[MetaTree Interpreter]
    end

    subgraph "Outputs"
        D3O[D3.js Visualization]
        STO[Debug String]
        MTO[AST Visualization]
    end

    UC --> SM
    SM --> CM
    CM --> D3I
    CM --> STI
    CM --> MTI
    D3I --> D3O
    STI --> STO
    MTI --> MTO

    style UC fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style SM fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style CM fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style D3I fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
    style STI fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
    style MTI fill:#d4c4b0,stroke:#8b7355,stroke-width:2px

    classDef tagless fill:#e8dcc6,stroke:#8b7355,stroke-width:3px,color:#2c1810
    class SM,CM tagless
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
            [ HH.text "Finally Tagless Pattern" ]

        -- Introduction
        , HH.p_
            [ HH.text "We want the library to be extensible and, in fact, D3 while it's the inspiration and a core target, is not the only possible implementation of the SelectionM monad. For this reason we choose the design pattern called Finally Tagless encoding and implement "
            , HH.em_ [ HH.text "interpreters" ]
            , HH.text " for the SelectionM."
            ]

        -- Finally Tagless Architecture Diagram
        , HH.div
            [ HP.classes [ HH.ClassName "diagram-container" ] ]
            [ mermaidDiagram finallyTaglessArchDiagram (Just "finally-tagless-diagram") ]

        , HH.p_
            [ HH.text "Furthermore, using this pattern, we can now "
            , HH.em_ [ HH.text "extend" ]
            , HH.text " the SelectionM monad as can be seen in the library where we provide a SimulationM2 monad which extends the our static DOM element visualizations by allowing them to move under the control of a Force Layout algorithm."
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> triggerMermaidRendering
