-- | Force Configuration POC
-- |
-- | A minimal proof-of-concept component to test the new force configuration system.
-- |
-- | Features:
-- | - 3 forces: charge, collision, center
-- | - 2 scenes: compact (weak forces) and spread (strong forces)
-- | - Scene transition button
-- | - Parameter display to verify values
-- |
-- | Success criteria:
-- | - Switching scenes resets force parameters correctly
-- | - Console logs show correct parameter values being applied
module Component.ForceConfigPOC where

import Prelude

import Data.Array as A
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Config.Apply (applySceneConfig)
import PSD3.Config.Force (AttrValue(..), ForceConfig(..), centerForce, collideForce, manyBodyForce, withRadius, withStrength)
import PSD3.Config.Scene (SceneConfig(..), defaultSimParams, forceCount, scene, withAlpha)
import PSD3.Data.Node (SimulationNode)
import PSD3.Internal.FFI (SimulationVariables, d3Append_, d3DataWithKeyFunction_, d3EnterAndAppend_, d3SelectFirstInDOM_, d3SelectionSelectAll_, d3SetAttr_, defaultNodeTick_, initSimulation_, setNodes_, startSimulation_)
import PSD3.Internal.Types (D3Selection_, D3Simulation_)
import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Force Definitions
-- =============================================================================

-- Weak charge force (for compact layout)
chargeWeak :: ForceConfig
chargeWeak = manyBodyForce "charge"
  # withStrength (-30.0)

-- Strong charge force (for spread layout)
chargeStrong :: ForceConfig
chargeStrong = manyBodyForce "charge"
  # withStrength (-200.0)

-- Small collision radius (for compact layout)
collisionSmall :: ForceConfig
collisionSmall = collideForce "collision" (StaticValue 5.0)
  # withStrength 1.0

-- Large collision radius (for spread layout)
collisionLarge :: ForceConfig
collisionLarge = collideForce "collision" (StaticValue 20.0)
  # withStrength 1.0

-- Center force (same for both scenes)
center :: ForceConfig
center = centerForce "center"
  # withStrength 0.1

-- =============================================================================
-- Scene Definitions
-- =============================================================================

compactScene :: SceneConfig
compactScene = scene "Compact" [chargeWeak, collisionSmall, center]
  # withAlpha 0.3

spreadScene :: SceneConfig
spreadScene = scene "Spread" [chargeStrong, collisionLarge, center]
  # withAlpha 0.3

-- =============================================================================
-- Test Data
-- =============================================================================

type TestNode = ( id :: String, label :: String, r :: Number )

-- Create 20 test nodes in a grid-like pattern
testNodes :: Array (SimulationNode TestNode)
testNodes =
  map (\i -> {
    id: "node-" <> show i
  , label: "N" <> show i
  , r: 8.0
  , x: 0.0
  , y: 0.0
  , vx: 0.0
  , vy: 0.0
  , fx: null
  , fy: null
  }) (A.range 1 20)

-- Simulation configuration matching our scene parameters
simConfig :: SimulationVariables
simConfig = {
    alpha: 1.0
  , alphaTarget: 0.0
  , alphaMin: 0.001
  , alphaDecay: 0.0228
  , velocityDecay: 0.4
}

-- Key function for node identification
nodeKey :: forall r. SimulationNode r -> String
nodeKey node =
  let nodeWithId = unsafeCoerce node :: { id :: String }
  in nodeWithId.id

-- =============================================================================
-- Component
-- =============================================================================

type State = {
    currentScene :: SceneConfig
  , sceneName :: String
  , simulation :: Maybe D3Simulation_
}

data Action
  = Initialize
  | SwitchToCompact
  | SwitchToSpread

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent {
    initialState: \_ -> {
      currentScene: compactScene
    , sceneName: "Compact"
    , simulation: Nothing
    }
  , render
  , eval: H.mkEval $ H.defaultEval {
      handleAction = handleAction
    , initialize = Just Initialize
    }
}

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "force-config-poc") ]
    [ HH.h1_ [ HH.text "Force Configuration POC" ]

    , HH.div
        [ HP.class_ (HH.ClassName "scene-info") ]
        [ HH.h2_ [ HH.text $ "Current Scene: " <> state.sceneName ]
        , HH.p_ [ HH.text $ "Force count: " <> show (forceCount state.currentScene) ]
        ]

    , HH.div
        [ HP.class_ (HH.ClassName "scene-controls") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "scene-btn")
            , HE.onClick \_ -> SwitchToCompact
            , HP.disabled (state.sceneName == "Compact")
            ]
            [ HH.text "Compact Scene" ]

        , HH.button
            [ HP.class_ (HH.ClassName "scene-btn")
            , HE.onClick \_ -> SwitchToSpread
            , HP.disabled (state.sceneName == "Spread")
            ]
            [ HH.text "Spread Scene" ]
        ]

    , HH.div
        [ HP.class_ (HH.ClassName "force-info") ]
        [ HH.h3_ [ HH.text "Expected Force Parameters:" ]
        , HH.div
            [ HP.class_ (HH.ClassName "params") ]
            (renderSceneParams state.currentScene)
        ]

    , HH.div
        [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.p_ [ HH.text "1. Open browser console (F12)" ]
        , HH.p_ [ HH.text "2. Switch between scenes using buttons above" ]
        , HH.p_ [ HH.text "3. Verify console logs show correct parameter values" ]
        , HH.p_ [ HH.text "4. Check that parameters match 'Expected' section" ]
        ]

    , HH.div
        [ HP.id "poc-svg-container"
        , HP.class_ (HH.ClassName "svg-container")
        , HP.attr (HH.AttrName "style") "position: relative; margin-top: 20px; border: 1px solid #ccc;"
        ]
        []  -- SVG created programmatically in Initialize
    ]

renderSceneParams :: forall w i. SceneConfig -> Array (HH.HTML w i)
renderSceneParams (SceneConfig config) =
  map renderForce config.forces
  where
    renderForce (ForceConfig force) =
      HH.div
        [ HP.class_ (HH.ClassName "force-params") ]
        [ HH.strong_ [ HH.text $ force.name <> ": " ]
        , HH.text $ show force.params
        ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect $ logMessage "POC: Initializing..."

    -- Create simulation
    let simulation = initSimulation_ simConfig nodeKey
    liftEffect $ logMessage "POC: Created simulation"

    -- Set nodes
    let _ = setNodes_ simulation testNodes
    liftEffect $ logMessage $ "POC: Added " <> show (A.length testNodes) <> " nodes"

    -- Set up D3 visualization with dimensions
    let svg = d3SelectFirstInDOM_ "#poc-svg-container"
    let svgEl = d3Append_ "svg" svg
    let _ = d3SetAttr_ "width" (unsafeCoerce 800) svgEl
    let _ = d3SetAttr_ "height" (unsafeCoerce 600) svgEl
    let _ = d3SetAttr_ "style" (unsafeCoerce "display: block; background-color: #f9f9f9;") svgEl
    let g = d3Append_ "g" svgEl
    let _ = d3SetAttr_ "transform" (unsafeCoerce "translate(400,300)") g  -- Center the group
    liftEffect $ logMessage "POC: Set up SVG (800x600)"

    -- Create circles with data binding (enter pattern)
    liftEffect $ logMessage "POC: Starting circle creation..."
    let circleSelection = d3SelectionSelectAll_ "circle" g
    liftEffect $ logMessage "POC: Selected circles (empty)"
    let dataSelection = d3DataWithKeyFunction_ testNodes nodeKey circleSelection
    liftEffect $ logMessage "POC: Bound data to selection"
    let circles = d3EnterAndAppend_ "circle" dataSelection
    liftEffect $ logMessage "POC: Created enter circles"
    let _ = d3SetAttr_ "r" (unsafeCoerce 8.0) circles  -- Set radius
    let _ = d3SetAttr_ "fill" (unsafeCoerce "#69b3a2") circles  -- Set color
    liftEffect $ logMessage "POC: Set attributes on circles"

    -- Store simulation in state
    H.modify_ _ { simulation = Just simulation }

    -- Apply initial scene
    liftEffect $ logMessage "POC: Applying initial Compact scene"
    liftEffect $ logMessage "  Expected: charge strength = -30, collision radius = 5"
    liftEffect $ applySceneConfig compactScene simulation

    -- Set up tick function for visualization
    let _ = defaultNodeTick_ "poc-tick" simulation circles

    -- Start simulation
    let _ = startSimulation_ simulation
    liftEffect $ logMessage "POC: Simulation started!"

  SwitchToCompact -> do
    liftEffect $ logMessage "POC: Switching to Compact scene"
    liftEffect $ logMessage "  Expected: charge strength = -30, collision radius = 5"
    H.modify_ _ { currentScene = compactScene, sceneName = "Compact" }
    applyCurrentScene

  SwitchToSpread -> do
    liftEffect $ logMessage "POC: ===== SPREAD BUTTON CLICKED ====="
    liftEffect $ logMessage "POC: Switching to Spread scene"
    liftEffect $ logMessage "  Expected: charge strength = -200, collision radius = 20"
    H.modify_ _ { currentScene = spreadScene, sceneName = "Spread" }
    liftEffect $ logMessage "POC: Updated state to Spread scene"
    applyCurrentScene
    liftEffect $ logMessage "POC: Scene application complete"

-- Apply the current scene to the simulation
applyCurrentScene :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
applyCurrentScene = do
  liftEffect $ logMessage "POC: applyCurrentScene called"
  state <- H.get
  liftEffect $ logMessage $ "POC: Current state sceneName: " <> state.sceneName

  case state.simulation of
    Nothing -> do
      liftEffect $ logMessage "POC: ERROR - No simulation found in state!"

    Just simulation -> do
      liftEffect $ logMessage "POC: Found simulation, applying config..."
      liftEffect $ applySceneConfig state.currentScene simulation
      liftEffect $ logMessage "POC: Scene config applied successfully!"
      liftEffect $ logMessage "POC: Reheating simulation..."
      let _ = startSimulation_ simulation
      liftEffect $ logMessage "POC: Simulation restarted!"

-- =============================================================================
-- FFI Helpers
-- =============================================================================

foreign import getSimulationFromWindow :: Effect (Maybe Foreign)
foreign import logMessage :: String -> Effect Unit

type Foreign = forall a. a
