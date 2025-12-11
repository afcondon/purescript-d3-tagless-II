module Component.HowTo.HowtoForceGraphs where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

type State = Unit

data Action = Initialize

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
  Initialize -> pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadHowTo
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    , HH.main_
        [ -- Intro
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Building Force-Directed Graphs" ]
            , HH.p_
                [ HH.strong_ [ HH.text "Recommended: " ]
                , HH.text "Use the SimulationManager pattern. See "
                , HH.a [ HP.href "#/force-playground" ] [ HH.text "Force Playground" ]
                , HH.text " for an interactive example."
                ]
            , HH.p_
                [ HH.text "The documentation below covers the type-class based API (SimulationM/SimulationM2) which is still available." ]
            ]

        -- Initialize Simulation
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Initialize Simulation" ]

            , HH.p_ [ HH.text "Create a force simulation with nodes and links:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Capabilities.Simulation (class SimulationM, init, addTickFunction, start, Step(..))

{ nodes, links } <- init
  { nodes: myNodeData
  , links: myLinkData
  , forces: [centerForce w h, chargeForce (const (-100.0)), linkForce]
  , activeForces: Set.fromFoldable ["center", "charge", "link"]
  , config: defaultSimConfig
  , keyFn: _.id
  , ticks: Map.empty
  }""" ]
                ]
            ]

        -- Tick Functions
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Tick Functions" ]

            , HH.p_ [ HH.text "Register callbacks to update DOM on each simulation tick:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Update node positions
addTickFunction "nodes" $ Step nodeCircles
  [ cx (_.x)
  , cy (_.y)
  ]

-- Update link positions
addTickFunction "links" $ Step linkLines
  [ x1 (\\l -> l.source.x)
  , y1 (\\l -> l.source.y)
  , x2 (\\l -> l.target.x)
  , y2 (\\l -> l.target.y)
  ]

start -- Begin animation""" ]
                ]
            ]

        -- Forces
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Available Forces" ]

            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "centerForce" ], HH.text " - Keep nodes centered" ]
                , HH.li_ [ HH.strong_ [ HH.text "chargeForce" ], HH.text " - Repulsion/attraction between nodes" ]
                , HH.li_ [ HH.strong_ [ HH.text "linkForce" ], HH.text " - Springs between connected nodes" ]
                , HH.li_ [ HH.strong_ [ HH.text "collideForce" ], HH.text " - Prevent node overlap" ]
                , HH.li_ [ HH.strong_ [ HH.text "forceX/Y" ], HH.text " - Push toward position" ]
                , HH.li_ [ HH.strong_ [ HH.text "radialForce" ], HH.text " - Push toward circle" ]
                ]
            ]

        -- Dynamic Updates
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Dynamic Updates" ]

            , HH.p_ [ HH.text "Use SimulationM2 for dynamic data changes:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Capabilities.Simulation (class SimulationM2, update, reheat)

{ nodes, links } <- update
  { nodes: Just newNodeArray
  , links: Just newLinkArray
  , nodeFilter: Nothing
  , linkFilter: Nothing
  , activeForces: Nothing
  , config: Nothing
  , keyFn: _.id
  }

reheat 0.7  -- Re-energize simulation
start       -- Begin animation""" ]
                ]
            ]

        -- Drag Behavior
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Node Dragging" ]

            , HH.p_ [ HH.text "Enable simulation-aware drag:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (Drag(..), simulationDrag)

-- Drag reheats simulation automatically
_ <- on (Drag (simulationDrag "my-simulation-id")) nodeCircles""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "SimulationM" ], HH.text " - Static simulations" ]
                , HH.li_ [ HH.strong_ [ HH.text "SimulationM2" ], HH.text " - Adds update/reheat for dynamic data" ]
                , HH.li_ [ HH.strong_ [ HH.text "Step" ], HH.text " - Maps selection to tick attributes" ]
                , HH.li_ [ HH.strong_ [ HH.text "activeForces" ], HH.text " - Control which forces are enabled" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Examples" ]
            , HH.p_ [ HH.text "See force simulation in action:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.a [ HP.href "#/force-playground" ] [ HH.text "Force Playground" ]
                    , HH.text " - Interactive force simulation with multiple datasets"
                    ]
                , HH.li_
                    [ HH.a [ HP.href "#/simple-force-graph" ] [ HH.text "Simple Force Graph" ]
                    , HH.text " - Minimal force-directed graph example"
                    ]
                ]
            ]
        ]
    ]

renderHeader :: forall w i. String -> HH.HTML w i
renderHeader title =
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
            [ HP.href "#/howto"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "How-to" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text title ]
            ]
        ]
    ]
