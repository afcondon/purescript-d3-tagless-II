module Component.LesMisGUPClean where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Array as Array
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import D3.Viz.LesMis.LesMisGUPClean as LesMisGUPClean
import D3.Viz.LesMiserables.Model (LesMisSimNode)
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import PSD3.Data.Node (D3Link_Unswizzled)
import PSD3v2.Interpreter.D3v2 as D3v2
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.FFI (linksForceName_)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.Map as Map

type State =
  { lesMisGUPSimulation :: D3SimulationState_ LesMisSimNode
  , originalGraph :: Maybe { nodes :: Array LesMisSimNode, links :: Array D3Link_Unswizzled }
  }

data Action
  = Initialize
  | FilterNodes Int

-- | Forces configuration for LesMis
forces :: { center :: Force LesMisSimNode, collision :: Force LesMisSimNode, links :: Force LesMisSimNode, manyBodyNeg :: Force LesMisSimNode }
forces =
  { manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  , collision: createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]
  , center: createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links: createLinkForce allNodes []
  }

forceLibrary :: Map.Map String (Force LesMisSimNode)
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { lesMisGUPSimulation: initialSimulationState forceLibrary
        , originalGraph: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ HP.class_ (HH.ClassName "lesmis-gup-clean-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Les Misérables - Clean GUP (No Wrappers!)" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Force simulation using existing SelectionM primitives. No SceneNestedJoin, no SceneData, no KeyedNode - just arrays and functions!" ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.h3_ [ HH.text "Node Filtering (GUP)" ]
            , HH.p_ [ HH.text "Test enter/update/exit pattern by filtering nodes:" ]
            , HH.div [ HP.class_ (HH.ClassName "button-group") ]
                [ HH.button
                    [ HP.class_ (HH.ClassName "control-button")
                    , HE.onClick \_ -> FilterNodes 0
                    ]
                    [ HH.text "All Nodes (77)" ]
                , HH.button
                    [ HP.class_ (HH.ClassName "control-button")
                    , HE.onClick \_ -> FilterNodes 5
                    ]
                    [ HH.text "Group ≥ 5" ]
                , HH.button
                    [ HP.class_ (HH.ClassName "control-button")
                    , HE.onClick \_ -> FilterNodes 8
                    ]
                    [ HH.text "Group ≥ 8" ]
                ]
            ]
        ]

    , HH.div
        [ HP.id "lesmis-gup-clean-viz"
        , HP.class_ (HH.ClassName "viz-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "What This Demonstrates" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Direct use of joinDataWithKey - no wrapper types needed!" ]
            , HH.li_ [ HH.text "No SceneData, no KeyedNode - just raw Array LesMisSimNode" ]
            , HH.li_ [ HH.text "Uses existing SelectionM primitives: joinDataWithKey + append + setAttrs + merge" ]
            , HH.li_ [ HH.text "Update function: rejoin data and merge enter + update selections" ]
            , HH.li_ [ HH.text "Tree API only for static structure (SVG container)" ]
            ]
        , HH.h3_ [ HH.text "Key Code Pattern" ]
        , HH.pre [ HP.class_ (HH.ClassName "code-snippet") ]
            [ HH.text """-- Join data directly (no wrappers!)
JoinResult { enter, update, exit } <-
  joinDataWithKey nodes (_.id) "circle" nodesGroup

-- Handle each phase
enterBound <- append Circle [] enter
_ <- setAttrs [...] enterBound
_ <- remove exit
nodeCircles <- merge enterBound update

-- Update: just rejoin!
updateNodes newNodes = do
  { nodes } <- update { nodes: Just newNodes, ... }
  nodesGroup <- select "#nodes"
  JoinResult { enter, update, exit } <-
    joinDataWithKey nodes (_.id) "circle" nodesGroup
  -- ... same pattern ...
""" ]
        ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "LesMisGUPClean: Initializing"
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
        newState <- H.liftEffect $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
          _ <- LesMisGUPClean.drawLesMisGUPClean forcesArray activeForces graph "#lesmis-gup-clean-viz"
          pure unit
        H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }
        log "LesMisGUPClean: Initialized successfully"

    pure unit

  FilterNodes minGroup -> do
    log $ "Filtering nodes to group >= " <> show minGroup
    state <- H.get
    case state.originalGraph of
      Nothing -> log "Error: Original graph data not loaded"
      Just graph -> do
        let filteredNodes = if minGroup == 0
              then graph.nodes
              else Array.filter (\n -> n.group >= minGroup) graph.nodes

        log $ "Filtered to " <> show (Array.length filteredNodes) <> " nodes"

        newState <- H.liftEffect $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
          LesMisGUPClean.updateNodes filteredNodes
        H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }
