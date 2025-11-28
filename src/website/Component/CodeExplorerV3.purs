-- | CodeExplorerV3 - Clean implementation using our own force engine
-- |
-- | This component demonstrates the new architecture:
-- | - SimulationManager owns the force loop (not D3)
-- | - SceneConfigs define forces per scene
-- | - OrchestrationV2 handles transitions
-- | - Full control, full debuggability
module Component.CodeExplorerV3 where

import Prelude

import Component.CodeExplorerV2.OrchestrationV2 as Orch
import Component.CodeExplorerV2.SceneConfigs (SceneConfig)
import D3.Viz.Spago.Model (SpagoModel)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.CodeExplorer.Data (readModelData)

-- =============================================================================
-- Types
-- =============================================================================

-- | Component state
type State =
  { model :: Maybe SpagoModel
  , currentScene :: String
  , initialized :: Boolean
  }

-- | Actions
data Action
  = Initialize
  | GoToOrbit
  | GoToTree
  | GoToForceGraph
  | GoToBubblePack
  | Reheat
  | Stop

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { model: Nothing
        , currentScene: "Loading"
        , initialized: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "code-explorer-v3") ]
    [ -- Header
      HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Code Explorer V3" ]
        , HH.p_ [ HH.text "Using our own force engine - full control, full debuggability" ]
        ]

    -- Controls
    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.h3_ [ HH.text "Scene Transitions" ]
        , HH.div [ HP.class_ (HH.ClassName "button-group") ]
            [ button "Orbit" GoToOrbit (state.currentScene == "Orbit")
            , button "Tree" GoToTree (state.currentScene == "Tree")
            , button "Force Graph" GoToForceGraph (state.currentScene == "ForceGraph")
            , button "Bubble Pack" GoToBubblePack (state.currentScene == "BubblePack")
            ]
        , HH.div [ HP.class_ (HH.ClassName "button-group") ]
            [ HH.button
                [ HP.class_ (HH.ClassName "control-button")
                , HE.onClick \_ -> Reheat
                ]
                [ HH.text "Reheat" ]
            , HH.button
                [ HP.class_ (HH.ClassName "control-button")
                , HE.onClick \_ -> Stop
                ]
                [ HH.text "Stop" ]
            ]
        , HH.p [ HP.class_ (HH.ClassName "scene-status") ]
            [ HH.text $ "Current: " <> state.currentScene ]
        ]

    -- Visualization container
    , HH.div
        [ HP.id "code-explorer-v3-viz"
        , HP.class_ (HH.ClassName "viz-container svg-container")
        ]
        []

    -- Info
    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "New Architecture" ]
        , HH.ul_
            [ HH.li_ [ HH.text "SimulationManager - our own force loop" ]
            , HH.li_ [ HH.text "No D3 simulation wrapper" ]
            , HH.li_ [ HH.text "Forces created from pure config" ]
            , HH.li_ [ HH.text "Type-safe swizzling" ]
            , HH.li_ [ HH.text "Tick callback for DOM updates" ]
            ]
        ]
    ]

button :: forall m. String -> Action -> Boolean -> H.ComponentHTML Action () m
button label action active =
  HH.button
    [ HP.classes
        [ HH.ClassName "control-button"
        , HH.ClassName if active then "active" else ""
        ]
    , HE.onClick \_ -> action
    ]
    [ HH.text label ]

-- =============================================================================
-- Action Handler
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[V3] Initializing..."
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load model data
    maybeModel <- H.liftAff readModelData
    case maybeModel of
      Nothing -> do
        log "[V3] Failed to load model data"
        H.modify_ _ { currentScene = "Error: No data" }

      Just model -> do
        log "[V3] Model loaded"
        H.modify_ _ { model = Just model }

        -- Initialize visualization
        _ <- liftEffect $ Orch.initialize model "#code-explorer-v3-viz"

        H.modify_ _
          { currentScene = "Orbit"
          , initialized = true
          }
        log "[V3] Initialized"

  GoToOrbit -> do
    log "[V3] Going to Orbit"
    liftEffect Orch.transitionToOrbit
    H.modify_ _ { currentScene = "Orbit" }

  GoToTree -> do
    log "[V3] Going to Tree"
    liftEffect Orch.transitionToTree
    H.modify_ _ { currentScene = "Tree" }

  GoToForceGraph -> do
    log "[V3] Going to ForceGraph"
    liftEffect Orch.transitionToForceGraph
    H.modify_ _ { currentScene = "ForceGraph" }

  GoToBubblePack -> do
    log "[V3] Going to BubblePack"
    liftEffect Orch.transitionToBubblePack
    H.modify_ _ { currentScene = "BubblePack" }

  Reheat -> do
    log "[V3] Reheating"
    liftEffect Orch.reheatSimulation

  Stop -> do
    log "[V3] Stopping"
    liftEffect Orch.stopSimulation
