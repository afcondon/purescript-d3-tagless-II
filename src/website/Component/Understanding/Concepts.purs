module PSD3.Understanding.Concepts where

import Prelude

import CodeSnippet (codeSnippet, triggerPrismHighlighting)
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

-- | Mermaid diagram for the SelectionM grammar state machine
selectionMGrammarDiagram :: String
selectionMGrammarDiagram = """
stateDiagram-v2
    [*] --> Attach: attach
    Attach --> Build: appendTo
    Build --> Build: appendTo
    Build --> Attributes: attr/style
    Attributes --> Attributes: attr/style
    Attributes --> Behaviors: on/drag/zoom
    Behaviors --> Behaviors: on/drag/zoom
    Build --> DataJoin: join
    Attributes --> DataJoin: join
    Behaviors --> DataJoin: join
    DataJoin --> Enter: enter
    Enter --> Build: append
    Enter --> Attributes: attr/style
    DataJoin --> Update: selectAll
    Update --> Attributes: attr/style
    DataJoin --> Exit: exit
    Exit --> Remove: remove
    Remove --> [*]

    note right of Attach
        Hook into DOM element
        e.g., div#viz
    end note

    note right of Build
        Create structure
        svg, g, etc.
    end note

    note right of DataJoin
        Bind data array
        Creates enter/update/exit
    end note
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
            [ tocAnchor "heading-finally-tagless" "Finally Tagless" 0
            , tocAnchor "heading-selectionm" "The SelectionM Monad" 0
            , tocAnchor "heading-capabilities" "Capabilities & Interpreters" 0
            , tocAnchor "heading-type-safe" "Type-Safe Attribute System" 0
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

        -- SelectionM Monad
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-selectionm" ]
                [ HH.text "The SelectionM Monad" ]
            , HH.h3_ [ HH.text "Grammar" ]
            , HH.p_
                [ HH.text "D3.js is, naturally, a JavaScript idiomatic library, making use of function chaining as a readable and easily-learnt API." ]
            , HH.p_
                [ HH.text "PS<$>D3 formalizes this slightly while retaining the general shape of the pattern and core concepts such as Selection, Attribute." ]
            , HH.p_ [ HH.text "The basic grammar of PS<$>D3 is just this:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "we can ", HH.em_ [ HH.text "attach" ], HH.text " our visualisation into the DOM, typically hooking into something like <div id=\"viz\">" ]
                , HH.li_ [ HH.text "we can build up whatever additional structure we want inside that, using ", HH.em_ [ HH.text "appendTo" ], HH.text " to for example put an <svg> with some <g>'s inside the <div id=\"viz\">" ]
                , HH.li_ [ HH.text "we can apply ", HH.em_ [ HH.text "attributes" ], HH.text " to the elements we've created: styles, ids, css classes and such" ]
                , HH.li_ [ HH.text "we can apply ", HH.em_ [ HH.text "behaviors" ], HH.text " to the elements we've created: things like zoom, drag and click event-handlers" ]
                , HH.li_ [ HH.text "we can ", HH.em_ [ HH.text "join" ], HH.text " some array of data at this point, such that when we next ", HH.em_ [ HH.text "add" ], HH.text " something, say a <circle>, we will put ", HH.em_ [ HH.text "n" ], HH.text " <circle>s in, not just one." ]
                ]

            -- SelectionM Grammar State Machine Diagram
            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram selectionMGrammarDiagram (Just "selectionm-grammar-diagram") ]

            , HH.p_ [ HH.text "In D3 the thing that is being acted on in this chain of functions is The Selection..." ]
            , HH.h3_ [ HH.text "The SelectionM Monad" ]
            , HH.p_
                [ HH.text "Where functions would be chained in JavaScript, the natural counterpart in PureScript is a Monad, specifically the SelectionM Monad which embodies the core functions above and a few others." ]
            , HH.p_ [ HH.text "It looks like this:" ]
            -- SNIPPET: selectionMClass src/lib/PSD3/Capabilities/Selection.purs 85-258
            , codeSnippet "selectionMClass" "haskell"
            ]

        -- Capabilities/Interpreters
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-capabilities" ]
                [ HH.text "Capabilities & Interpreters" ]
            , HH.p_ [ HH.text "The SimulationM2 monad looks like:" ]
            -- SNIPPET: simulationM2Class src/lib/PSD3/Capabilities/Simulation.purs 254-320
            , codeSnippet "simulationM2Class" "haskell"
            ]

        -- Type-safe Attributes
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-type-safe" ]
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
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    triggerPrismHighlighting
    triggerMermaidRendering
