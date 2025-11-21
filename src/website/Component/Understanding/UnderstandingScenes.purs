module Component.Understanding.UnderstandingScenes where

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

-- | State for the Scenes page
type State = Unit

-- | Actions for the Scenes page
data Action = Initialize

-- | Scenes page component
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

-- | Scene complexity diagram
complexityDiagram :: String
complexityDiagram = """
graph TB
    subgraph "Interactive Visualization Complexity"
        User["User Actions"]
        State["Application State"]
        Sim["Simulation Physics"]
        Trans["Transitions"]
        DOM["DOM Updates"]
    end

    User --> State
    State --> Sim
    Sim --> Trans
    Trans --> DOM
    State --> DOM

    style User fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style State fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
    style Sim fill:#c4e8d3,stroke:#558b73,stroke-width:2px
    style Trans fill:#e8d3f5,stroke:#735b8b,stroke-width:2px
    style DOM fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
"""

-- | Scene structure diagram
sceneStructureDiagram :: String
sceneStructureDiagram = """
graph TD
    subgraph "Scene Data Structure"
        Scene["Scene"]
        Data["Visualization Data"]
        Config["Configuration"]
        Behaviors["Enter/Update/Exit"]
    end

    Scene --> Data
    Scene --> Config
    Scene --> Behaviors

    Behaviors --> Enter["Enter Behavior<br/><i>initialAttrs, transition</i>"]
    Behaviors --> Update["Update Behavior<br/><i>attrs, transition</i>"]
    Behaviors --> Exit["Exit Behavior<br/><i>attrs, transition</i>"]

    style Scene fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
    style Data fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style Config fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style Enter fill:#c4e8d3,stroke:#558b73,stroke-width:2px
    style Update fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
    style Exit fill:#f8d7da,stroke:#721c24,stroke-width:2px
"""

-- | Transition matrix diagram
transitionMatrixDiagram :: String
transitionMatrixDiagram = """
graph LR
    subgraph "State Transition Matrix"
        S1["Scene A"]
        S2["Scene B"]
        S3["Scene C"]
    end

    S1 -->|"transition AB"| S2
    S2 -->|"transition BC"| S3
    S3 -->|"transition CA"| S1

    note["Each transition defines:<br/>- Data changes<br/>- Element changes<br/>- Animation timing"]

    style S1 fill:#c4e8d3,stroke:#558b73,stroke-width:2px
    style S2 fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
    style S3 fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
"""

-- | Declarative vs imperative diagram
declarativeVsDiagram :: String
declarativeVsDiagram = """
graph TB
    subgraph "Imperative (D3)"
        Imp1["Check current state"]
        Imp2["Calculate diff"]
        Imp3["Apply changes"]
        Imp4["Manage transitions"]
        Imp5["Handle cleanup"]
    end

    subgraph "Declarative (PSD3v2)"
        Dec1["Declare desired state"]
        Dec2["Framework handles rest"]
    end

    Imp1 --> Imp2
    Imp2 --> Imp3
    Imp3 --> Imp4
    Imp4 --> Imp5

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
                [ HH.text "Scene Structures & Transitions" ]
            , HH.p_
                [ HH.text "Complex interactive visualizations like force-directed graphs involve managing multiple concerns: user input, simulation physics, state transitions, and DOM updates. PSD3v2's Scene structures make this complexity declarative." ]

            -- Section navigation cards
            , HH.div
                [ HP.classes [ HH.ClassName "section-nav-cards" ] ]
                [ renderNavCard "The Challenge" "#challenge" "Why interactive viz is hard"
                , renderNavCard "Scene Structures" "#scene-structures" "Declarative state management"
                , renderNavCard "Transitions" "#transitions" "Smooth state changes"
                , renderNavCard "Code Explorer" "#code-explorer" "Flagship example"
                ]
            ]

        -- Section 1: The Challenge
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "challenge"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "The Challenge of Interactive Visualizations" ]
            , HH.p_
                [ HH.text "Interactive, updating visualizations with simulations are among the most complex frontend code to write correctly. They involve:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram complexityDiagram (Just "complexity-diagram") ]

            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "User actions" ]
                    , HH.text " - clicks, drags, hovers that trigger state changes"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "State management" ]
                    , HH.text " - tracking what's visible, selected, filtered"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Simulation physics" ]
                    , HH.text " - continuous force calculations and position updates"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Transitions" ]
                    , HH.text " - smooth animations between states"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "DOM synchronization" ]
                    , HH.text " - keeping the view in sync with the model"
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Problem: "
                , HH.text "Imperative D3 code becomes tangled, hard to reason about, and prone to subtle bugs."
                ]
            ]

        -- Section 2: Scene Structures
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "scene-structures"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Scene Structures" ]
            , HH.p_
                [ HH.text "A Scene bundles together all the data needed to render a visualization state:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram sceneStructureDiagram (Just "scene-structure-diagram") ]

            , HH.p_
                [ HH.text "Each scene contains:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Data" ]
                    , HH.text " - the nodes, links, or other data to visualize"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Configuration" ]
                    , HH.text " - layout parameters, force strengths, etc."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Behaviors" ]
                    , HH.text " - how elements enter, update, and exit"
                    ]
                ]

            , HH.p_
                [ HH.text "When the scene changes, PSD3v2 automatically:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Calculates the diff between old and new data" ]
                , HH.li_ [ HH.text "Creates enter selections for new data" ]
                , HH.li_ [ HH.text "Updates existing elements" ]
                , HH.li_ [ HH.text "Removes exiting elements with transitions" ]
                ]
            ]

        -- Section 3: Transitions
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "transitions"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Transition Matrix" ]
            , HH.p_
                [ HH.text "Complex applications have multiple scenes (states) with transitions between them:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram transitionMatrixDiagram (Just "transition-matrix-diagram") ]

            , HH.p_
                [ HH.text "Each transition defines:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "TransitionConfig" ]
                    , HH.text " - duration, easing, delay"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Initial attributes" ]
                    , HH.text " - starting state for entering elements"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Target attributes" ]
                    , HH.text " - ending state to animate toward"
                    ]
                ]

            , HH.p_
                [ HH.text "This declarative approach makes complex multi-state visualizations much easier to reason about:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram declarativeVsDiagram (Just "declarative-vs-diagram") ]
            ]

        -- Section 4: Code Explorer
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "code-explorer"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Flagship Example: Code Explorer" ]
            , HH.p_
                [ HH.text "The Code Explorer demonstrates Scene structures in action. It's an interactive force-directed graph of PureScript module dependencies with:" ]

            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Multiple scenes" ]
                    , HH.text " - different views of the module graph"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Smooth transitions" ]
                    , HH.text " - animated changes between states"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "User interactions" ]
                    , HH.text " - click, drag, zoom, expand/collapse"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Force simulation" ]
                    , HH.text " - physics-based layout"
                    ]
                ]

            , HH.p_
                [ HH.text "Despite this complexity, the core update logic is declarative - you specify the target scene and PSD3v2 handles the rest."
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key insight: "
                , HH.text "Think in terms of \"what state do I want?\" not \"what updates do I need to apply?\""
                ]
            ]

        -- Summary
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Summary" ]
            , HH.p_
                [ HH.text "Scene structures enable:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Declarative state management for complex visualizations" ]
                , HH.li_ [ HH.text "Automatic diff calculation and DOM updates" ]
                , HH.li_ [ HH.text "Transition matrix for multi-state applications" ]
                , HH.li_ [ HH.text "Clean separation of data, configuration, and behavior" ]
                , HH.li_ [ HH.text "Easier reasoning about complex interactive visualizations" ]
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
                [ HH.text "Scene Structures" ]
            ]
        ]
    ]
