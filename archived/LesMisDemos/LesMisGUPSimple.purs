module Component.LesMisGUPSimple where

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
import D3.Viz.LesMis.LesMisGUPSimple as LesMisGUPSimple
import D3.Viz.LesMiserables.Model (LesMisSimNode, LesMisLink)
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
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
  , originalGraph :: Maybe { nodes :: Array LesMisSimNode, links :: Array LesMisLink }
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
  HH.div [ HP.class_ (HH.ClassName "lesmis-gup-simple-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Les Misérables - Minimal GUP Demo (UpdateNestedJoin)" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Minimal force simulation demonstrating UpdateNestedJoin with declarative enter/update/exit behaviors. Colors based on node group." ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.h3_ [ HH.text "Node Filtering (GUP)" ]
            , HH.p_ [ HH.text "Demonstrate enter/update/exit pattern by filtering nodes:" ]
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
        [ HP.id "lesmis-gup-simple-viz"
        , HP.class_ (HH.ClassName "viz-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "What This Demonstrates" ]
        , HH.ul_
            [ HH.li_ [ HH.text "UpdateNestedJoin - declarative GUP with type decomposition (SceneData → KeyedNode)" ]
            , HH.li_ [ HH.text "12-line update function - just call renderTree with new scene data!" ]
            , HH.li_ [ HH.text "Datum-based colors - no functions in scene data (like Letters GUP)" ]
            , HH.li_ [ HH.text "Phylotaxis initial layout - sunflower spiral positioning" ]
            , HH.li_ [ HH.text "Interactive behaviors - drag nodes, zoom/pan" ]
            ]
        , HH.h3_ [ HH.text "Implementation Highlights" ]
        , HH.pre [ HP.class_ (HH.ClassName "code-snippet") ]
            [ HH.text """-- Define scene structure ONCE:
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree scene =
  T.updateNestedJoin "nodeElements" "circle"
    [scene]
    (_.nodes >>> map KeyedNode)
    template
    { enterBehavior: ..., updateBehavior: ..., exitBehavior: ... }

-- Update is just 12 lines:
updateNodes :: Array LesMisSimNode -> D3v2SimM () LesMisSimNode Unit
updateNodes newNodes = do
  { nodes } <- update { nodes: Just newNodes, ... }
  nodesGroup <- select "#nodes"
  _ <- renderTree nodesGroup (createNodesTree (SceneData { nodes }))
  pure unit""" ]
        ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "LesMisGUPSimple: Initializing"
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
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
          _ <- LesMisGUPSimple.drawLesMisGUPSimple forcesArray activeForces graph "#lesmis-gup-simple-viz"
          pure unit
        H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }
        log "LesMisGUPSimple: Initialized successfully"

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

        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
          LesMisGUPSimple.updateNodes filteredNodes
        H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }
