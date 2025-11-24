-- | CodeExplorerV2 - Clean rebuild using scene-based architecture
-- |
-- | This component demonstrates the scene-based approach:
-- | - Forces defined in Forces.purs (type-safe, no strings)
-- | - Each scene has its own config file
-- | - Orchestration handles transitions
-- | - Component just manages state and user interaction
module Component.CodeExplorerV2 where

import Prelude

import Component.CodeExplorerV2.Orchestration as Orchestration
import Component.CodeExplorerV2.Scenes.Types (Scene(..))
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.CodeExplorer.Data (readModelData)
import Data.Map as Map
import PSD3.Internal.Simulation.Types (D3SimulationState_, initialSimulationState)
import PSD3v2.Interpreter.D3v2 as D3v2

-- | Component state
type State =
  { simulation :: D3SimulationState_ SpagoSimNode
  , model :: Maybe SpagoModel
  , currentScene :: Scene
  }

data Action
  = Initialize
  | FormTree
  | ActivateForceLayout

-- | Initial simulation state
initialSimState :: D3SimulationState_ SpagoSimNode
initialSimState = initialSimulationState Map.empty

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { simulation: initialSimState
        , model: Nothing
        , currentScene: Orbit
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "code-explorer-v2-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Code Explorer V2" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Scene-based architecture with type-safe forces." ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.h3_ [ HH.text "Scene Transitions" ]
            , HH.div [ HP.class_ (HH.ClassName "button-group") ]
                [ HH.button
                    [ HP.class_ (HH.ClassName "control-button primary")
                    , HE.onClick \_ -> FormTree
                    ]
                    [ HH.text "Form Tree" ]
                , HH.button
                    [ HP.class_ (HH.ClassName "control-button")
                    , HE.onClick \_ -> ActivateForceLayout
                    ]
                    [ HH.text "Force Layout" ]
                ]
            , HH.p [ HP.class_ (HH.ClassName "scene-status") ]
                [ HH.text $ "Current: " <> show state.currentScene ]
            ]
        ]

    , HH.div
        [ HP.id "code-explorer-v2-viz"
        , HP.class_ (HH.ClassName "viz-container svg-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "Architecture" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Forces.purs - type-safe force definitions" ]
            , HH.li_ [ HH.text "Scenes/*.purs - config per scene" ]
            , HH.li_ [ HH.text "Orchestration.purs - transitions" ]
            , HH.li_ [ HH.text "No string-based force lookups" ]
            ]
        ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "CodeExplorerV2: Initializing"
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Spago model data
    maybeModel <- H.liftAff readModelData
    case maybeModel of
      Nothing -> log "Failed to load Spago model data"
      Just model -> do
        H.modify_ _ { model = Just model }

        state <- H.get
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Orchestration.initialize model "#code-explorer-v2-viz"

        H.modify_ \s -> s { simulation = newState.simulation }
        log "CodeExplorerV2: Initialized"

    pure unit

  FormTree -> do
    log "CodeExplorerV2: Forming tree"
    state <- H.get
    case state.model of
      Nothing -> log "Error: Model not loaded"
      Just model -> do
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Orchestration.transitionToTreeReveal model

        H.modify_ \s -> s { simulation = newState.simulation, currentScene = TreeReveal }
        log "Tree formation started"

    pure unit

  ActivateForceLayout -> do
    log "CodeExplorerV2: Activating force layout"
    state <- H.get
    case state.model of
      Nothing -> log "Error: Model not loaded"
      Just model -> do
        -- Transition to force graph
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Orchestration.transitionToForceGraph model

        H.modify_ \s -> s { simulation = newState.simulation, currentScene = ForceGraph }
        log "Graph links added, waiting before starting simulation..."

        -- Wait 1 second for links to appear, then start simulation
        H.liftAff $ delay (Milliseconds 1000.0)

        finalState <- H.get
        newState2 <- H.liftAff $ D3v2.execD3v2SimM { simulation: finalState.simulation } do
          Orchestration.startSimulation

        H.modify_ \s -> s { simulation = newState2.simulation }
        log "Force layout activated"

    pure unit
