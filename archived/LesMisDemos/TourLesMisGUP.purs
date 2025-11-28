module Component.Tour.TourLesMisGUP where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import D3.Viz.LesMisGUPV2 as LesMisGUPV2
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import D3.Viz.LesMiserables.Model (LesMisSimNode, LesMisLink)
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import PSD3v2.Interpreter.D3v2 as D3v2

-- | Forces configuration
forces :: { center :: Force LesMisSimNode, collision :: Force LesMisSimNode, links :: Force LesMisSimNode, manyBodyNeg :: Force LesMisSimNode }
forces =
  { manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  , collision: createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]
  , center: createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links: createLinkForce allNodes []
  }

forceLibrary :: Map.Map String (Force LesMisSimNode)
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

-- | Tour page state
type State =
  { simulation :: D3SimulationState_ LesMisSimNode
  , originalGraph :: Maybe { nodes :: Array LesMisSimNode, links :: Array LesMisLink }
  }

-- | Tour page actions
data Action
  = Initialize
  | FilterNodes Int

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { simulation: initialSimulationState forceLibrary
      , originalGraph: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    log "TourLesMisGUP: Initializing"
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Les Misérables data
    lesMisResponse <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    case lesMisResponse of
      Left _ -> log "Failed to load Les Misérables data"
      Right response -> do
        let graph = readGraphFromFileContents (Right response)
        let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
            activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]

        -- Store original graph for filtering
        H.modify_ \s -> s { originalGraph = Just graph }

        state <- H.get
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          _ <- LesMisGUPV2.drawLesMisGUPV2 forcesArray activeForces graph "#lesmis-gup-viz"
          pure unit
        H.modify_ \s -> s { simulation = newState.simulation }
        log "TourLesMisGUP: Initialized successfully"

    pure unit

  FilterNodes minGroup -> do
    log $ "Filtering nodes to group >= " <> show minGroup
    state <- H.get
    case state.originalGraph of
      Nothing -> log "Error: Original graph data not loaded"
      Just graph -> do
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          LesMisGUPV2.filterByGroupWithOriginal minGroup graph
        H.modify_ \s -> s { simulation = newState.simulation }

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
                [ HH.text "Try the Pattern Yourself" ]
            , HH.p_
                [ HH.text "Click the buttons below to filter nodes by group. Watch how nodes enter (appear), update (stay), and exit (disappear):" ]
            , HH.div
                [ HP.classes [ HH.ClassName "button-group" ]
                , HP.style "text-align: center; margin: 20px 0;"
                ]
                [ HH.button
                    [ HP.classes [ HH.ClassName "control-button" ]
                    , HE.onClick \_ -> FilterNodes 0
                    ]
                    [ HH.text "All Nodes (77)" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "control-button" ]
                    , HE.onClick \_ -> FilterNodes 5
                    ]
                    [ HH.text "Group ≥ 5" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "control-button" ]
                    , HE.onClick \_ -> FilterNodes 8
                    ]
                    [ HH.text "Group ≥ 8" ]
                ]
            , HH.div
                [ HP.id "lesmis-gup-viz"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center; min-height: 600px;"
                ]
                []
            , HH.p_
                [ HH.text "Notice how:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Entering nodes" ]
                    , HH.text " appear at phylotaxis (spiral) positions and animate into place"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Staying nodes" ]
                    , HH.text " maintain their positions and group colors"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Exiting nodes" ]
                    , HH.text " and their links are removed from the simulation"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Links" ]
                    , HH.text " are automatically filtered to only connect visible nodes"
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
