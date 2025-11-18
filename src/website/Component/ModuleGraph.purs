module Component.ModuleGraph where

import Prelude

import D3.Viz.ModuleGraph.LoadModuleGraph (loadModuleGraph)
import D3.Viz.ModuleGraph.Model (ModuleSimNode)
import D3.Viz.ModuleGraph.Viz as ModuleGraphViz
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3v2.Interpreter.D3v2 as D3v2

type State =
  { simulation :: D3SimulationState_ ModuleSimNode
  , moduleCount :: Maybe Int
  , linkCount :: Maybe Int
  }

data Action
  = Initialize

-- | Forces configuration for module graph
forces :: { center :: Force ModuleSimNode, collision :: Force ModuleSimNode, links :: Force ModuleSimNode, manyBodyNeg :: Force ModuleSimNode }
forces =
  { manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-30.0) ]
  , collision: createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 8.0 ]
  , center: createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links: createLinkForce allNodes [ F.distanceVal 30.0 ]
  }

forceLibrary :: Map.Map String (Force ModuleSimNode)
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { simulation: initialSimulationState forceLibrary
        , moduleCount: Nothing
        , linkCount: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "module-graph-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Module Dependency Graph" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Force-directed visualization of local module dependencies in this codebase. "
            , HH.text "Each node is a PureScript module, colored by top-level namespace. "
            , HH.text "Links show import relationships."
            ]
        , case state.moduleCount, state.linkCount of
            Just modules, Just links ->
              HH.p [ HP.class_ (HH.ClassName "stats") ]
                [ HH.text $ show modules <> " modules, " <> show links <> " dependencies" ]
            _, _ -> HH.text ""
        ]

    , HH.div
        [ HP.id "module-graph-viz"
        , HP.class_ (HH.ClassName "viz-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "Module Groups (by color)" ]
        , HH.ul_
            [ HH.li_ [ colorBox 0, HH.text "PSD3.Internal - Core library internals" ]
            , HH.li_ [ colorBox 1, HH.text "PSD3v2 - Tree API implementation" ]
            , HH.li_ [ colorBox 2, HH.text "PSD3.Layout - Layout algorithms (Sankey, Hierarchy)" ]
            , HH.li_ [ colorBox 3, HH.text "PSD3.Shared - Shared website utilities" ]
            , HH.li_ [ colorBox 4, HH.text "PSD3.* - Other library modules" ]
            , HH.li_ [ colorBox 5, HH.text "Component.* - Halogen components" ]
            , HH.li_ [ colorBox 6, HH.text "D3.Viz - Visualization implementations" ]
            , HH.li_ [ colorBox 7, HH.text "D3.* - Other viz modules" ]
            , HH.li_ [ colorBox 8, HH.text "Utility - Utility functions" ]
            ]
        , HH.h3_ [ HH.text "Interactions" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Drag nodes to reposition them" ]
            , HH.li_ [ HH.text "Zoom and pan with mouse wheel / trackpad" ]
            , HH.li_ [ HH.text "Watch for clusters and orphan modules" ]
            ]
        ]
    ]
  where
    colorBox :: forall w i. Int -> HH.HTML w i
    colorBox group =
      HH.span
        [ HP.class_ (HH.ClassName "color-box")
        , HP.style $ "background-color: " <> getGroupColor group <> ";"
        ]
        []

    getGroupColor :: Int -> String
    getGroupColor 0 = "#1f77b4"
    getGroupColor 1 = "#ff7f0e"
    getGroupColor 2 = "#2ca02c"
    getGroupColor 3 = "#d62728"
    getGroupColor 4 = "#9467bd"
    getGroupColor 5 = "#8c564b"
    getGroupColor 6 = "#e377c2"
    getGroupColor 7 = "#7f7f7f"
    getGroupColor 8 = "#bcbd22"
    getGroupColor _ = "#17becf"

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "ModuleGraph: Initializing"
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load module graph data
    graphResult <- H.liftAff loadModuleGraph
    case graphResult of
      Left err -> log $ "Failed to load module graph: " <> err
      Right graph -> do
        log $ "Loaded " <> show (Array.length graph.nodes) <> " modules"

        H.modify_ _
          { moduleCount = Just (Array.length graph.nodes)
          , linkCount = Just (Array.length graph.links)
          }

        let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
            activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]

        state <- H.get
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          ModuleGraphViz.drawModuleGraph forcesArray activeForces graph "#module-graph-viz"
        H.modify_ \s -> s { simulation = newState.simulation }
        log "ModuleGraph: Initialized successfully"

    pure unit
