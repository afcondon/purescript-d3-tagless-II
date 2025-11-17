module Component.Tour.TourLesMisGUP where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import D3.Viz.FPFTW.LesMisGUPAuto as LesMisGUPAuto
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import PSD3v2.Interpreter.D3v2 as D3v2
import Utility (getWindowWidthHeight)

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
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Les Misérables data
    lesMisResponse <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    case lesMisResponse of
      Left _ -> pure unit  -- Fail silently
      Right response -> do
        let graph = readGraphFromFileContents (Right response)
        Tuple width height <- liftEffect getWindowWidthHeight

        -- Create force library and initial state
        let manyBodyForce = createForce "charge" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-30.0) ]
            centerForce = createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
            linksForce = createLinkForce allNodes [ F.distanceVal 30.0 ]
            collisionForce = createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]
            forceLibrary = initialize [ manyBodyForce, centerForce, linksForce, collisionForce ]
            initialState = { simulation: initialSimulationState forceLibrary }

        -- Start auto-cycling GUP demo
        liftEffect $ void $ D3v2.runD3v2SimM initialState $ LesMisGUPAuto.drawLesMisGUPAuto graph.nodes graph.links "#lesmis-gup-auto-viz" width height

    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourLesMisGUP
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "General Update Pattern: Dynamic Graphs" ]
            , HH.p_
                [ HH.text "The "
                , HH.strong_ [ HH.text "General Update Pattern (GUP)" ]
                , HH.text " is D3's approach to handling changing data. When your dataset changes, you need to:"
                ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Enter: " ], HH.text "Create new elements for new data" ]
                , HH.li_ [ HH.strong_ [ HH.text "Update: " ], HH.text "Modify existing elements for persisting data" ]
                , HH.li_ [ HH.strong_ [ HH.text "Exit: " ], HH.text "Remove old elements for deleted data" ]
                ]
            , HH.p_
                [ HH.text "Below, watch as nodes randomly enter and exit the graph every few seconds. The TreeAPI handles all the complexity automatically:" ]
            ]

        -- Live demo
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "lesmis-gup-demo"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Watch the Pattern in Action" ]
            , HH.div
                [ HP.id "lesmis-gup-auto-viz"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center; min-height: 600px;"
                ]
                []
            , HH.div
                [ HP.classes [ HH.ClassName "gup-legend" ]
                , HP.style "text-align: center; margin: 20px 0;"
                ]
                [ HH.div
                    [ HP.style "display: inline-block; margin: 0 20px;" ]
                    [ HH.span
                        [ HP.style "display: inline-block; width: 12px; height: 12px; background: green; border-radius: 50%; margin-right: 8px;" ]
                        []
                    , HH.text "Entering"
                    ]
                , HH.div
                    [ HP.style "display: inline-block; margin: 0 20px;" ]
                    [ HH.span
                        [ HP.style "display: inline-block; width: 12px; height: 12px; background: #666; border-radius: 50%; margin-right: 8px;" ]
                        []
                    , HH.text "Staying"
                    ]
                , HH.div
                    [ HP.style "display: inline-block; margin: 0 20px;" ]
                    [ HH.span
                        [ HP.style "display: inline-block; width: 12px; height: 12px; background: brown; border-radius: 50%; margin-right: 8px;" ]
                        []
                    , HH.text "Exiting"
                    ]
                ]
            , HH.p_
                [ HH.text "Notice how:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Entering nodes" ]
                    , HH.text " appear in "
                    , HH.span [ HP.style "color: green;" ] [ HH.text "green" ]
                    , HH.text " at their initial positions, then transition to their group color"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Staying nodes" ]
                    , HH.text " maintain their group colors and smoothly adjust positions"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Exiting nodes" ]
                    , HH.text " turn "
                    , HH.span [ HP.style "color: brown;" ] [ HH.text "brown" ]
                    , HH.text ", lose their links, and fly away before being removed"
                    ]
                ]
            ]

        -- How it works
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "How TreeAPI Handles GUP" ]
            , HH.p_
                [ HH.text "With traditional D3, you manually handle enter/update/exit with verbose code. With TreeAPI, you just call "
                , HH.code_ [ HH.text "joinData" ]
                , HH.text " and get three selections back:"
                ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto;"
                ]
                [ HH.code_
                    [ HH.text """-- Join new data with existing elements
JoinResult { enter, update, exit } <- joinData newNodes "circle" nodeGroup

-- ENTER: Create new elements (green initially)
enterNodes <- renderData Circle
  [ fill "green", radius 5.0, ... ]
  enter

-- Transition enter nodes to final color
withTransition enterTransition enterNodes
  [ fill \\n -> groupColor n.group ]

-- UPDATE: Update existing elements
setAttrs [ fill \\n -> groupColor n.group ] update

-- EXIT: Transition out and remove
setAttrsExit [ fill "brown" ] exit
withTransitionExit exitTransition exit []"""
                    ]
                ]
            , HH.p_
                [ HH.text "The TreeAPI:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Automatically tracks which elements correspond to which data (via key function)" ]
                , HH.li_ [ HH.text "Separates new data (enter), existing data (update), and removed data (exit)" ]
                , HH.li_ [ HH.text "Handles transitions and removals declaratively" ]
                , HH.li_ [ HH.text "Updates the simulation state to match the new graph structure" ]
                ]
            ]

        -- Why this matters
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Why GUP Matters" ]
            , HH.p_
                [ HH.text "The General Update Pattern is essential for real-world visualizations:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Streaming data: " ]
                    , HH.text "Real-time dashboards where data arrives continuously"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "User interactions: " ]
                    , HH.text "Filtering, searching, or drilling down changes what's displayed"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Animations: " ]
                    , HH.text "Smooth transitions make changes comprehensible instead of jarring"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Code clarity: " ]
                    , HH.text "Declarative approach is easier to understand and maintain"
                    ]
                ]
            , HH.p_
                [ HH.text "This pattern works for any visualization - bar charts, network graphs, maps, timelines. Master it once, use it everywhere." ]
            ]

        -- Next steps
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Explore More" ]
            , HH.p_
                [ HH.text "See the full-featured version with manual controls, grid layout, and phylotaxis at "
                , HH.a
                    [ HP.href $ "#" <> routeToPath LesMisGUPTree ]
                    [ HH.text "Les Misérables GUP Tree" ]
                , HH.text "."
                ]
            , HH.p_
                [ HH.text "Or continue the tour to see more advanced examples in "
                , HH.a
                    [ HP.href $ "#" <> routeToPath TourShowcase ]
                    [ HH.text "Showcase" ]
                , HH.text "."
                ]
            ]
        ]
    ]
