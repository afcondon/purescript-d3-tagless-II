module Component.Tour.TourShowcase where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Tour page state
type State = Unit

-- | Tour page actions
data Action = Initialize

-- | Tour page component
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
  Initialize -> do
    -- No examples to render on this page
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourShowcase
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Showcase: Flagship Demonstrations" ]
            , HH.p_
                [ HH.text "This page highlights the most impressive and complete demonstrations of the PS<$>D3 library. These examples combine multiple techniques from the tour to create sophisticated, production-ready visualizations." ]
            , HH.p_
                [ HH.text "Each showcase example demonstrates the full power of combining PureScript's type safety with D3's visualization capabilities, showing what's possible when you bring together advanced programming language features and data visualization." ]
            ]

        -- Animated Tree ↔ Cluster
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "animated-tree-cluster"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Animated Tree ↔ Cluster Transitions" ]
            , HH.p_
                [ HH.text "This flagship demonstration shows smooth, bidirectional animations between Tree and Cluster layouts. It visualizes 252 nodes from the Flare visualization toolkit, demonstrating:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Smooth transitions using D3's data join with identity-based keys" ]
                , HH.li_ [ HH.text "Height-based child sorting to eliminate crossovers during animation" ]
                , HH.li_ [ HH.text "Extent-based centering for balanced layout" ]
                , HH.li_ [ HH.text "Complex hierarchical data with 252 nodes transitioning smoothly" ]
                ]
            , HH.p_
                [ HH.a
                    [ HP.href $ "#" <> routeToPath AnimatedTreeCluster ]
                    [ HH.text "View the Animated Tree ↔ Cluster demonstration →" ]
                ]
            , HH.p_
                [ HH.text "This example showcases the TreeAPI's capability to handle complex layout transitions with proper enter/update/exit patterns, ensuring every node maintains its identity across layout changes for smooth, comprehensible animations." ]
            ]

        -- Les Mis GUP + Tree
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "lesmis-gup-tree"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Les Misérables with General Update Pattern" ]
            , HH.p_
                [ HH.text "A proof-of-concept combining the General Update Pattern (GUP) with Tree API and dynamic layouts. This demonstration shows character relationships from Victor Hugo's Les Misérables with dynamic updates and transitions." ]
            , HH.p_
                [ HH.a
                    [ HP.href $ "#" <> routeToPath LesMisGUPTree ]
                    [ HH.text "View the Les Misérables GUP demonstration →" ]
                ]
            , HH.p_
                [ HH.text "This example demonstrates how the library can combine multiple advanced techniques: hierarchical layouts, force-directed positioning, and the General Update Pattern for smooth enter/update/exit transitions." ]
            ]

        -- Module Dependency Graph
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "module-graph"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Module Dependency Graph (Dogfooding)" ]
            , HH.p_
                [ HH.text "A self-referential visualization showing the module dependencies of the PS<$>D3 library itself. This is \"dogfooding\" - using our library to visualize its own structure." ]
            , HH.p_
                [ HH.a
                    [ HP.href $ "#" <> routeToPath ModuleGraph ]
                    [ HH.text "View the Module Dependency Graph →" ]
                ]
            , HH.p_
                [ HH.text "This example shows:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "How the library can visualize its own architecture" ]
                , HH.li_ [ HH.text "Hierarchical relationships between modules" ]
                , HH.li_ [ HH.text "The power of tree layouts for understanding code structure" ]
                , HH.li_ [ HH.text "Self-hosting capabilities of the visualization system" ]
                ]
            ]

        -- Gallery
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "gallery"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Full Examples Gallery" ]
            , HH.p_
                [ HH.text "For a complete collection of all working examples with source code, visit the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath Gallery ]
                    [ HH.text "Examples Gallery" ]
                , HH.text ". The gallery provides a comprehensive view of all TreeAPI examples, organized by category and difficulty level."
                ]
            , HH.p_
                [ HH.text "Each example in the gallery includes:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Full source code with syntax highlighting" ]
                , HH.li_ [ HH.text "Interactive demonstrations" ]
                , HH.li_ [ HH.text "Detailed explanations of techniques used" ]
                , HH.li_ [ HH.text "Links to relevant API documentation" ]
                ]
            ]

        -- Future Showcase
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Coming Soon" ]
            , HH.p_
                [ HH.text "As more sophisticated examples are implemented in TreeAPI, they will be featured here in the showcase. Future additions may include:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Interactive chord diagrams with relationship filtering" ]
                , HH.li_ [ HH.text "Animated Sankey diagrams showing flow changes over time" ]
                , HH.li_ [ HH.text "Force-directed graphs with custom physics simulations" ]
                , HH.li_ [ HH.text "Multi-view coordinated visualizations" ]
                , HH.li_ [ HH.text "Real-time data streaming visualizations" ]
                , HH.li_ [ HH.text "Complex interactive dashboards combining multiple visualization types" ]
                ]
            , HH.p_
                [ HH.text "The goal is to demonstrate that PureScript + D3 can handle any visualization challenge, with better type safety, composability, and maintainability than pure JavaScript solutions." ]
            ]
        ]
    ]
