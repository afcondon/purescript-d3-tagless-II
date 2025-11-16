module PSD3.Understanding.UnderstandingTypeSystem where

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

-- | Mermaid diagram for the Type-Safe Attribute System
typeSafeAttributeDiagram :: String
typeSafeAttributeDiagram = """
graph TB
    subgraph "Attribute Types"
        Static["Static Value<br/>cx = 50<br/><i>attr 'cx' 50</i>"]
        Datum["Datum Function<br/>cx = λd → d.x<br/><i>attr 'cx' (datum_ _.x)</i>"]
        DatumIdx["Datum+Index Function<br/>cx = λd i → d.x + i * 10<br/><i>attr 'cx' (\\d i -> ...)</i>"]
    end

    subgraph "Type Classes"
        AttrClass[IsAttribute typeclass]
        SetAttr[SetAttribute instance]
    end

    subgraph "Runtime"
        Resolve[Type Resolution]
        Apply[Apply to Elements]
    end

    Static --> AttrClass
    Datum --> AttrClass
    DatumIdx --> AttrClass

    AttrClass --> SetAttr
    SetAttr --> Resolve
    Resolve --> Apply

    style Static fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style Datum fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style DatumIdx fill:#d4c4b0,stroke:#8b7355,stroke-width:2px

    classDef typesafe fill:#e8dcc6,stroke:#8b7355,stroke-width:3px,color:#2c1810
    class AttrClass,SetAttr typesafe
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
            [ HH.text "Type-Safe Attribute System" ]

        , HH.p_
            [ HH.text "A key innovation of D3 is the easy, natural way one can go from static to data-driven attributes, for example going from putting "
            , HH.em_ [ HH.text "n" ]
            , HH.text " identical circles into the DOM vs putting "
            , HH.em_ [ HH.text "n" ]
            , HH.text " circles of differing colours and sizes into the DOM. This is the essence (with join/simulation) of Data Drive Documents, ie D3."
            ]
        , HH.p_
            [ HH.text "In PureScript we need a little bit of typeclass trickery to get the same clean syntax, and we use it to enable interchangeable, type-safe attribute setters. For example, we can use an attribute "
            , HH.em_ [ HH.text "cx" ]
            , HH.text " (the center x position of a circle) which can take:"
            ]
        , HH.ul_
            [ HH.li_ [ HH.text "a numeric value OR," ]
            , HH.li_ [ HH.text "a lambda function which takes the datum we are using to a numeric value OR," ]
            , HH.li_ [ HH.text "a lambda function which takes the datum AND the index of that datum in the selection" ]
            ]

        -- Type-Safe Attribute System Diagram
        , HH.div
            [ HP.classes [ HH.ClassName "diagram-container" ] ]
            [ mermaidDiagram typeSafeAttributeDiagram (Just "type-safe-attribute-diagram") ]

        , HH.p_
            [ HH.text "This type system ensures that attributes are always used correctly at compile time, preventing runtime errors while maintaining the flexibility that makes D3 so powerful." ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> triggerMermaidRendering
