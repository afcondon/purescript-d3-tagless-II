module Component.LesMisGUPTree where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import D3.Viz.LesMisGUPV2 as LesMisGUPV2
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
  | MoveToGrid
  | MoveToPhylotaxis
  | UnpinNodes
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
  HH.div [ HP.class_ (HH.ClassName "lesmis-gup-tree-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Les Misérables - Tree API + GUP + Dynamic Layouts" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Full-featured force simulation using declarative Tree API with General Update Pattern (enter/update/exit) and dynamic layout transitions." ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.h3_ [ HH.text "Layout Transitions" ]
            , HH.p_ [ HH.text "Transform node positions with smooth D3 transitions:" ]
            , HH.div [ HP.class_ (HH.ClassName "button-group") ]
                [ HH.button
                    [ HP.class_ (HH.ClassName "control-button")
                    , HE.onClick \_ -> MoveToGrid
                    ]
                    [ HH.text "Grid Layout" ]
                , HH.button
                    [ HP.class_ (HH.ClassName "control-button")
                    , HE.onClick \_ -> MoveToPhylotaxis
                    ]
                    [ HH.text "Phylotaxis (Spiral)" ]
                , HH.button
                    [ HP.class_ (HH.ClassName "control-button")
                    , HE.onClick \_ -> UnpinNodes
                    ]
                    [ HH.text "Force-Directed" ]
                ]
            ]

        , HH.div [ HP.class_ (HH.ClassName "control-section") ]
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
        [ HP.id "lesmis-gup-tree-viz"
        , HP.class_ (HH.ClassName "viz-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "What This Demonstrates" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Declarative Tree API - entire DOM structure defined as a tree" ]
            , HH.li_ [ HH.text "General Update Pattern - Tree API handles enter/update/exit automatically" ]
            , HH.li_ [ HH.text "Dynamic layouts - Grid, Phylotaxis (sunflower spiral), Force-directed" ]
            , HH.li_ [ HH.text "Simulation state management - forces, nodes, links updated declaratively" ]
            , HH.li_ [ HH.text "Interactive behaviors - drag nodes, zoom/pan, simulation drag" ]
            ]
        , HH.h3_ [ HH.text "Check Browser Console" ]
        , HH.p_ [ HH.text "Open the browser console to see detailed logging of simulation state, node counts, and GUP operations." ]
        ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "LesMisGUPTree: Initializing"
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
          _ <- LesMisGUPV2.drawLesMisGUPV2 forcesArray activeForces graph "#lesmis-gup-tree-viz"
          pure unit
        H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }
        log "LesMisGUPTree: Initialized successfully"

    pure unit

  MoveToGrid -> do
    log "Moving to grid layout"
    state <- H.get
    newState <- H.liftEffect $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
      LesMisGUPV2.moveToGrid 30.0
    H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }

  MoveToPhylotaxis -> do
    log "Moving to phylotaxis layout"
    state <- H.get
    newState <- H.liftEffect $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
      LesMisGUPV2.moveToPhylotaxis
    H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }

  UnpinNodes -> do
    log "Unpinning nodes (returning to force layout)"
    state <- H.get
    newState <- H.liftEffect $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
      LesMisGUPV2.unpinNodes
    H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }

  FilterNodes minGroup -> do
    log $ "Filtering nodes to group >= " <> show minGroup
    state <- H.get
    case state.originalGraph of
      Nothing -> log "Error: Original graph data not loaded"
      Just graph -> do
        newState <- H.liftEffect $ D3v2.execD3v2SimM { simulation: state.lesMisGUPSimulation } do
          LesMisGUPV2.filterByGroupWithOriginal minGroup graph
        H.modify_ \s -> s { lesMisGUPSimulation = newState.simulation }
