module Component.Understanding.UnderstandingGrammar where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

-- | State for the Grammar page
type State = Unit

-- | Actions for the Grammar page
data Action = Initialize

-- | Grammar page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> liftEffect triggerMermaidRendering

-- | Mermaid diagram showing static visualization structure
staticVizDiagram :: String
staticVizDiagram = """
graph TD
    subgraph "Static Visualization"
        A[select container] --> B[appendChild SVG]
        B --> C[appendData Circle]
        C --> D[setAttrs]
    end

    style A fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style B fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style C fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style D fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
"""

-- | Mermaid diagram showing GUP flow
gupFlowDiagram :: String
gupFlowDiagram = """
graph TD
    subgraph "General Update Pattern"
        A[openSelection/selectAll] --> B[joinData]
        B --> C{JoinResult}
        C --> D[enter: append]
        C --> E[update: setAttrs]
        C --> F[exit: remove]
        D --> G[merge]
        E --> G
    end

    style A fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style B fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style C fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
    style D fill:#c4e8d3,stroke:#558b73,stroke-width:2px
    style E fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
    style F fill:#e8d4d4,stroke:#8b5555,stroke-width:2px
"""

-- | Mermaid diagram showing TreeAPI
treeAPIDiagram :: String
treeAPIDiagram = """
graph TD
    subgraph "TreeAPI - Declarative Structure"
        A[renderTree] --> B[Tree datum]
        B --> C[Node]
        B --> D[Join]
        B --> E[SceneJoin]
        C --> F[name, elemType, attrs, children]
        D --> G[template: datum -> Tree]
        E --> H[enter/update/exit behaviors]
    end

    style A fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style B fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style C fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
    style D fill:#c4e8d3,stroke:#558b73,stroke-width:2px
    style E fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
"""

-- | Mermaid diagram showing selection state machine
selectionStateDiagram :: String
selectionStateDiagram = """
stateDiagram-v2
    [*] --> SEmpty: select
    SEmpty --> SBoundOwns: appendData/renderData
    SEmpty --> JoinResult: joinData

    JoinResult --> SPending: .enter
    JoinResult --> SBoundOwns: .update
    JoinResult --> SExiting: .exit

    SPending --> SBoundOwns: append
    SBoundOwns --> SBoundOwns: setAttrs
    SBoundOwns --> SBoundInherits: appendChildInheriting
    SExiting --> [*]: remove

    note right of SEmpty: No data bound yet
    note right of SPending: Data waiting for elements
    note right of SBoundOwns: Elements with data
    note right of SExiting: Elements to remove
"""

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- Header with navigation
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadUnderstanding
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    , HH.main_
        [ -- Introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "The Grammar of D3" ]
            , HH.p_
                [ HH.text "D3 can be understood as an embedded Domain Specific Language for data visualization. While the full API has a large surface area, the essential grammar is remarkably small. PSD3v2 captures this grammar in a type-safe, composable way." ]

            -- Section navigation cards
            , HH.div
                [ HP.classes [ HH.ClassName "section-nav-cards" ] ]
                [ renderNavCard "Core Primitives" "#core-primitives" "The four essential operations"
                , renderNavCard "Selection States" "#selection-states" "Phantom types for safety"
                , renderNavCard "Static vs GUP" "#static-vs-gup" "One-time vs updating"
                , renderNavCard "TreeAPI" "#tree-api" "Declarative alternative"
                ]
            ]

        -- Section 1: Core Primitives
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "core-primitives"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Core Primitives" ]
            , HH.p_
                [ HH.text "The grammar of D3 visualizations consists of four essential primitives. Everything else is built from these:" ]

            , HH.table
                [ HP.classes [ HH.ClassName "tutorial-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Primitive" ]
                        , HH.th_ [ HH.text "Function" ]
                        , HH.th_ [ HH.text "PSD3v2 API" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "select" ] ]
                        , HH.td_ [ HH.text "Find an entry point in the DOM" ]
                        , HH.td_
                            [ HH.code_ [ HH.text "select" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "selectAll" ]
                            ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "append" ] ]
                        , HH.td_ [ HH.text "Add a DOM element to a selection" ]
                        , HH.td_
                            [ HH.code_ [ HH.text "appendChild" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "append" ]
                            ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "join" ] ]
                        , HH.td_
                            [ HH.text "For each datum "
                            , HH.strong_ [ HH.text "d" ]
                            , HH.text ", create an element "
                            , HH.strong_ [ HH.text "e" ]
                            ]
                        , HH.td_
                            [ HH.code_ [ HH.text "appendData" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "joinData" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "renderData" ]
                            ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "attr" ] ]
                        , HH.td_ [ HH.text "Apply attributes to current elements" ]
                        , HH.td_
                            [ HH.code_ [ HH.text "setAttrs" ]
                            , HH.text " (array of Attribute)"
                            ]
                        ]
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key insight: "
                , HH.text "Attributes can be static values or functions of the datum, enabling Data Driven Documents."
                ]
            ]

        -- Section 2: Selection States
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "selection-states"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Selection States (Phantom Types)" ]
            , HH.p_
                [ HH.text "PSD3v2 uses phantom types to track what operations are legal on a selection. This catches errors at compile time that D3 would only catch at runtime:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram selectionStateDiagram (Just "selection-state-diagram") ]

            , HH.table
                [ HP.classes [ HH.ClassName "tutorial-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "State" ]
                        , HH.th_ [ HH.text "Meaning" ]
                        , HH.th_ [ HH.text "Legal Operations" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SEmpty" ] ]
                        , HH.td_ [ HH.text "Selection has elements but no data" ]
                        , HH.td_ [ HH.text "appendChild, appendData, joinData" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SPending" ] ]
                        , HH.td_ [ HH.text "Data waiting for elements (enter)" ]
                        , HH.td_ [ HH.text "append" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SBoundOwns" ] ]
                        , HH.td_ [ HH.text "Elements with bound data" ]
                        , HH.td_ [ HH.text "setAttrs, appendChildInheriting, merge" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SExiting" ] ]
                        , HH.td_ [ HH.text "Elements to be removed (exit)" ]
                        , HH.td_ [ HH.text "setAttrsExit, remove" ]
                        ]
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Type safety: "
                , HH.text "Trying to call "
                , HH.code_ [ HH.text "setAttrs" ]
                , HH.text " on an "
                , HH.code_ [ HH.text "SEmpty" ]
                , HH.text " selection is a compile error, not a runtime bug."
                ]
            ]

        -- Section 3: Static vs GUP
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "static-vs-gup"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Static Visualizations vs General Update Pattern" ]
            , HH.p_
                [ HH.text "There are two fundamental patterns for D3 visualizations:" ]

            , HH.h3_ [ HH.text "Static: One-time render" ]
            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram staticVizDiagram (Just "static-viz-diagram") ]

            , HH.p_
                [ HH.text "Use "
                , HH.code_ [ HH.text "appendData" ]
                , HH.text " for simple visualizations that render once with fixed data. This is perfect for charts, static hierarchies, and basic data displays."
                ]

            , HH.h3_ [ HH.text "GUP: Dynamic updates" ]
            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram gupFlowDiagram (Just "gup-flow-diagram") ]

            , HH.p_
                [ HH.text "Use "
                , HH.code_ [ HH.text "joinData" ]
                , HH.text " when data changes over time. The join splits data into three disjoint sets:"
                ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Enter" ]
                    , HH.text " - new data items that need elements created"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Update" ]
                    , HH.text " - existing elements that should be updated"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Exit" ]
                    , HH.text " - old elements that should be removed"
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "GUP enables: "
                , HH.text "Animated transitions, live data feeds, user interactions, and smooth state changes."
                ]
            ]

        -- Section 4: TreeAPI
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "tree-api"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "TreeAPI: Declarative Alternative" ]
            , HH.p_
                [ HH.text "For static visualizations, the TreeAPI provides a more declarative, succinct syntax. Instead of imperative method chaining, you declare the desired tree structure:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram treeAPIDiagram (Just "tree-api-diagram") ]

            , HH.p_
                [ HH.text "TreeAPI variants handle different use cases:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "Node" ]
                    , HH.text " - static elements with children"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "Join" ]
                    , HH.text " - data-driven element creation (N elements from N data)"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "NestedJoin" ]
                    , HH.text " - nested data with type decomposition"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "SceneJoin" ]
                    , HH.text " - declarative GUP with enter/update/exit behaviors"
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Note: "
                , HH.text "TreeAPI excels at static visualizations. For complex interactive visualizations with simulations, the underlying Selection API provides more control."
                ]
            ]

        -- Summary
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Summary" ]
            , HH.p_
                [ HH.text "The D3 grammar in PSD3v2:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Four primitives: select, append, join, attr" ]
                , HH.li_ [ HH.text "Phantom types enforce correct operation ordering" ]
                , HH.li_ [ HH.text "Static visualizations use appendData or TreeAPI" ]
                , HH.li_ [ HH.text "Dynamic visualizations use joinData with GUP" ]
                , HH.li_ [ HH.text "TreeAPI provides declarative syntax for tree structures" ]
                ]
            ]
        ]
    ]

-- | Render a navigation card
renderNavCard :: forall w i. String -> String -> String -> HH.HTML w i
renderNavCard title href description =
  HH.a
    [ HP.href href
    , HP.classes [ HH.ClassName "section-nav-card" ]
    ]
    [ HH.h3_ [ HH.text title ]
    , HH.p_ [ HH.text description ]
    ]

-- | Render header with navigation
renderHeader :: forall w i. HH.HTML w i
renderHeader =
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
        , HH.a
            [ HP.href "#/understanding"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "Understanding" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text "The Grammar of D3" ]
            ]
        ]
    ]
