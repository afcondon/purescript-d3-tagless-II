module Component.Understanding.UnderstandingTreeAPI where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

-- | State for the TreeAPI page
type State = Unit

-- | Actions for the TreeAPI page
data Action = Initialize

-- | TreeAPI page component
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

-- | Layer cake diagram showing API architecture
layerCakeDiagram :: String
layerCakeDiagram = """
graph TB
    subgraph "High-Level APIs"
        TreeAPI["TreeAPI<br/><i>Declarative tree structures</i>"]
        SimAPI["Simulation API<br/><i>Force-directed layouts</i>"]
    end

    subgraph "Core Layer"
        SelectionM["SelectionM<br/><i>Type-safe selections</i>"]
    end

    subgraph "Interpreters"
        D3v2["D3v2 Interpreter<br/><i>DOM manipulation</i>"]
        Mermaid["Mermaid Interpreter<br/><i>Diagram generation</i>"]
        String["String Interpreter<br/><i>Debug output</i>"]
    end

    TreeAPI --> SelectionM
    SimAPI --> SelectionM
    SelectionM --> D3v2
    SelectionM --> Mermaid
    SelectionM --> String

    style TreeAPI fill:#c4e8d3,stroke:#558b73,stroke-width:2px
    style SimAPI fill:#c4e8d3,stroke:#558b73,stroke-width:2px
    style SelectionM fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
    style D3v2 fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style Mermaid fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style String fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
"""

-- | Tree structure diagram
treeStructureDiagram :: String
treeStructureDiagram = """
graph TD
    subgraph "Tree datum"
        Node["Node<br/><i>Static element</i>"]
        Join["Join<br/><i>Data-driven N elements</i>"]
        NestedJoin["NestedJoin<br/><i>Type decomposition</i>"]
        SceneJoin["SceneJoin<br/><i>GUP behaviors</i>"]
    end

    Node --> Children["children: Array Tree"]
    Join --> Template["template: datum -> Tree"]
    NestedJoin --> Decompose["decompose + template"]
    SceneJoin --> Behaviors["enter/update/exit"]

    style Node fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style Join fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style NestedJoin fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
    style SceneJoin fill:#c4e8d3,stroke:#558b73,stroke-width:2px
"""

-- | Comparison diagram
comparisonDiagram :: String
comparisonDiagram = """
graph LR
    subgraph "Imperative Style"
        Imp1["select"]
        Imp2["appendChild"]
        Imp3["appendChild"]
        Imp4["joinData"]
        Imp5["append"]
        Imp6["setAttrs"]
    end

    subgraph "Declarative Style"
        Dec1["renderTree"]
        Dec2["Tree structure"]
    end

    Imp1 --> Imp2
    Imp2 --> Imp3
    Imp3 --> Imp4
    Imp4 --> Imp5
    Imp5 --> Imp6

    Dec1 --> Dec2

    style Imp1 fill:#f8d7da,stroke:#721c24,stroke-width:1px
    style Dec1 fill:#d4edda,stroke:#155724,stroke-width:1px
    style Dec2 fill:#d4edda,stroke:#155724,stroke-width:1px
"""

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- Header with navigation
      renderHeader

    , HH.main_
        [ -- Introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "TreeAPI: Declarative Visualization" ]
            , HH.p_
                [ HH.text "TreeAPI provides a declarative way to build visualization structures. Instead of imperative method chains, you declare the tree structure you want and PSD3v2 renders it. This makes static visualizations extremely succinct while maintaining full type safety." ]

            -- Section navigation cards
            , HH.div
                [ HP.classes [ HH.ClassName "section-nav-cards" ] ]
                [ renderNavCard "Architecture" "#architecture" "API layer cake"
                , renderNavCard "Tree Variants" "#tree-variants" "Node, Join, SceneJoin"
                , renderNavCard "Static vs GUP" "#static-vs-gup" "When to use what"
                , renderNavCard "Limitations" "#limitations" "When to use Selection API"
                ]
            ]

        -- Section 1: Architecture
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "architecture"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Architecture: The Layer Cake" ]
            , HH.p_
                [ HH.text "PSD3v2 is organized in layers. TreeAPI and Simulation API are high-level, declarative APIs that sit on top of the core SelectionM layer:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram layerCakeDiagram (Just "layer-cake-diagram") ]

            , HH.p_
                [ HH.text "Key points:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "TreeAPI" ]
                    , HH.text " - Declarative tree structures for static visualizations"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Simulation API" ]
                    , HH.text " - Force-directed layouts with physics"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "SelectionM" ]
                    , HH.text " - Core selection operations with phantom types"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Interpreters" ]
                    , HH.text " - Multiple backends via Finally Tagless"
                    ]
                ]
            ]

        -- Section 2: Tree Variants
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "tree-variants"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Tree Variants" ]
            , HH.p_
                [ HH.text "TreeAPI provides different tree node types for different use cases:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram treeStructureDiagram (Just "tree-structure-diagram") ]

            , HH.table
                [ HP.classes [ HH.ClassName "tutorial-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Variant" ]
                        , HH.th_ [ HH.text "Use Case" ]
                        , HH.th_ [ HH.text "Smart Constructor" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "Node" ] ]
                        , HH.td_ [ HH.text "Static elements with children" ]
                        , HH.td_ [ HH.code_ [ HH.text "named, elem" ] ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "Join" ] ]
                        , HH.td_ [ HH.text "N elements from N data items" ]
                        , HH.td_ [ HH.code_ [ HH.text "joinData" ] ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "NestedJoin" ] ]
                        , HH.td_ [ HH.text "Nested data with type decomposition" ]
                        , HH.td_ [ HH.code_ [ HH.text "nestedJoin" ] ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SceneJoin" ] ]
                        , HH.td_ [ HH.text "GUP with enter/update/exit behaviors" ]
                        , HH.td_ [ HH.code_ [ HH.text "sceneJoin, sceneNestedJoin" ] ]
                        ]
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Composition: "
                , HH.code_ [ HH.text "withChild" ]
                , HH.text " and "
                , HH.code_ [ HH.text "withChildren" ]
                , HH.text " build up tree structures."
                ]
            ]

        -- Section 3: Static vs GUP
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "static-vs-gup"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Static vs General Update Pattern" ]
            , HH.p_
                [ HH.text "TreeAPI shines for static visualizations with its succinct, declarative syntax:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram comparisonDiagram (Just "comparison-diagram") ]

            , HH.p_
                [ HH.text "For updating visualizations, use "
                , HH.code_ [ HH.text "SceneJoin" ]
                , HH.text " which declaratively specifies enter/update/exit behaviors:"
                ]

            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Enter" ]
                    , HH.text " - Initial attributes and optional transition for new elements"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Update" ]
                    , HH.text " - Attributes and transition for existing elements"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Exit" ]
                    , HH.text " - Attributes and transition before removal"
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "SceneNestedJoin: "
                , HH.text "Combines type decomposition with GUP - the most powerful variant."
                ]
            ]

        -- Section 4: Limitations
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "limitations"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "When to Use the Selection API" ]
            , HH.p_
                [ HH.text "TreeAPI is excellent for many use cases, but the underlying Selection API provides more control for:" ]

            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Complex interactions" ]
                    , HH.text " - Fine-grained control over event handlers"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Dynamic structures" ]
                    , HH.text " - Tree shape that changes based on data"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Custom transitions" ]
                    , HH.text " - Coordinated animations across elements"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Force simulations" ]
                    , HH.text " - Physics-based layouts with tick functions"
                    ]
                ]

            , HH.p_
                [ HH.text "The good news: both APIs work with the same SelectionM foundation, so you can mix them as needed. Use TreeAPI for structure, Selection API for behavior."
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Tip: "
                , HH.text "Start with TreeAPI. Drop to Selection API when you need more control."
                ]
            ]

        -- Summary
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Summary" ]
            , HH.p_
                [ HH.text "TreeAPI provides:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Declarative tree structures instead of imperative chains" ]
                , HH.li_ [ HH.text "Four variants: Node, Join, NestedJoin, SceneJoin" ]
                , HH.li_ [ HH.text "Succinct static visualizations" ]
                , HH.li_ [ HH.text "SceneJoin for declarative GUP with transitions" ]
                , HH.li_ [ HH.text "Compatible with underlying Selection API" ]
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
                [ HH.text "TreeAPI" ]
            ]
        ]
    ]
