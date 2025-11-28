-- | Minimal standalone test of V2 force configuration system
-- | No old force system, no complex setup - just pure V2
module Component.ForceConfigV2Test where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Config.Apply (applySceneConfig)
import PSD3.Config.Force (ForceConfig(..), ForceType(..), ForceParams(..), AttrValue(..))
import PSD3.Config.Scene as CFG
import PSD3.Internal.FFI (SimulationVariables, d3Append_, d3SelectFirstInDOM_, d3SetAttr_, d3SelectionSelectAll_, defaultNodeTick_, initSimulation_, setNodes_, startSimulation_)
import Unsafe.Coerce (unsafeCoerce)

-- Simple test node type with D3 simulation properties
type TestNode =
  { id :: Int
  , x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , fx :: Nullable Number
  , fy :: Nullable Number
  , r :: Number
  }

-- Test data: 5 nodes in a simple pattern
testNodes :: Array TestNode
testNodes =
  [ { id: 0, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null, r: 10.0 }
  , { id: 1, x: 100.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null, r: 10.0 }
  , { id: 2, x: 100.0, y: 100.0, vx: 0.0, vy: 0.0, fx: null, fy: null, r: 10.0 }
  , { id: 3, x: 0.0, y: 100.0, vx: 0.0, vy: 0.0, fx: null, fy: null, r: 10.0 }
  , { id: 4, x: 50.0, y: 50.0, vx: 0.0, vy: 0.0, fx: null, fy: null, r: 15.0 }
  ]

-- Simple scene config: just center and collision forces
-- Center at (400, 300) which is the middle of the 800x600 SVG
testSceneConfig :: CFG.SceneConfig
testSceneConfig = CFG.scene "V2Test" [
    ForceConfig {
      name: "center"
    , forceType: ForceCenter
    , params: CenterParams {
        x: StaticValue 400.0  -- Center of 800px width
      , y: StaticValue 300.0  -- Center of 600px height
      , strength: StaticValue 0.1
      }
    , filter: Nothing
    }
  , ForceConfig {
      name: "collision"
    , forceType: ForceCollide
    , params: CollideParams {
        radius: StaticValue 12.0  -- Fixed radius for all nodes (slightly larger than node circles)
      , strength: StaticValue 1.0
      , iterations: StaticValue 1.0
      }
    , filter: Nothing
    }
  ]
  # CFG.withDescription "Simple test: center + collision"
  # CFG.withAlpha 0.3

-- Component
type State = Unit

data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.h1_ [ HH.text "Force Config V2 Test" ]
    , HH.p_ [ HH.text "Minimal standalone test of the new V2 force configuration system." ]
    , HH.div
        [ HP.attr (HH.AttrName "id") "v2-test-container"
        , HP.attr (HH.AttrName "class") "svg-container"
        ]
        []
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    log "ForceConfigV2Test: Initializing"

    liftEffect $ do
      -- 1. Select container and create SVG (pure D3 operations)
      let container = d3SelectFirstInDOM_ "#v2-test-container"
      let svg0 = d3Append_ "svg" container
      let svg1 = d3SetAttr_ "width" (unsafeCoerce "800") svg0
      let svg = d3SetAttr_ "height" (unsafeCoerce "600") svg1
      Console.log "Created SVG"

      -- 2. Create simulation with default config
      let simConfig :: SimulationVariables
          simConfig =
            { alpha: 1.0
            , alphaTarget: 0.0
            , alphaMin: 0.001
            , alphaDecay: 0.0228
            , velocityDecay: 0.4
            }

      let nodeKey = \n -> n.id
      let simulation = initSimulation_ simConfig nodeKey
      Console.log "Created simulation"

      -- 3. Add nodes to simulation
      let _ = setNodes_ simulation testNodes
      Console.log $ "Added " <> show (Array.length testNodes) <> " nodes to simulation"

      -- 4. Apply V2 scene config
      Console.log "Applying V2 scene config..."
      applySceneConfig testSceneConfig simulation
      Console.log "V2 scene config applied"

      -- 5. Create circles for nodes (pure D3 operations)
      let circles = d3Append_ "g" svg
      let _ = d3SetAttr_ "class" (unsafeCoerce "nodes") circles

      -- Add a circle for each node
      let renderCircle n =
            let circle = d3Append_ "circle" circles
                _ = d3SetAttr_ "r" (unsafeCoerce $ show n.r) circle
                _ = d3SetAttr_ "fill" (unsafeCoerce "steelblue") circle
                _ = d3SetAttr_ "stroke" (unsafeCoerce "white") circle
                _ = d3SetAttr_ "stroke-width" (unsafeCoerce "2") circle
            in unit

      let _ = map renderCircle testNodes
      Console.log "Created circle elements"

      -- 6. Set up tick handler to update circle positions
      let allCircles = d3SelectionSelectAll_ "circle" svg
      let _ = defaultNodeTick_ "test-tick" simulation allCircles
      Console.log "Tick handler registered"

      -- 7. Start simulation
      let _ = startSimulation_ simulation
      Console.log "Simulation started"

      Console.log "ForceConfigV2Test: Initialization complete"

    pure unit
