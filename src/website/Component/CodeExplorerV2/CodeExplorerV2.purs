-- | CodeExplorerV2 - Clean rebuild following LesMisGUP patterns
-- |
-- | This is a simplified version of CodeExplorer that:
-- | - Uses consistent ID types (Int) throughout
-- | - Follows the declarative genericUpdateSimulation pattern
-- | - Starts minimal and builds up incrementally
module Component.CodeExplorerV2 where

import Prelude

import Component.CodeExplorerV2.Draw as Draw
import Component.CodeExplorerV2.Types (Scene(..))
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isModule, isPackage, isUsedModule)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Set as Set
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.CodeExplorer.Data (readModelData)
import PSD3.CodeExplorer.Forces (gridPointX)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceFilter(..), ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3v2.Interpreter.D3v2 as D3v2
import Unsafe.Coerce (unsafeCoerce)

-- | Simple state following LesMisGUPTree pattern
type State =
  { simulation :: D3SimulationState_ SpagoSimNode
  , model :: Maybe SpagoModel
  , currentScene :: Scene
  }

data Action
  = Initialize
  | FormTree
  | ActivateForceLayout
  | SwitchScene Scene

-- | Forces for Spago visualization
forces :: { center :: Force SpagoSimNode
          , collision :: Force SpagoSimNode
          , charge :: Force SpagoSimNode
          , links :: Force SpagoSimNode
          , packageOrbit :: Force SpagoSimNode
          , moduleOrbit :: Force SpagoSimNode
          , clusterX_M :: Force SpagoSimNode
          , clusterY_M :: Force SpagoSimNode
          }
forces =
  { charge: createForce "charge" (RegularForce ForceManyBody) allNodes [ F.strengthVal (300.0) ]
  , collision: createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 20.0 ]
  , center: createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links: createLinkForce allNodes [ F.distanceVal 50.0 ]
  -- Radial forces for orbit view
  , packageOrbit: createForce "packageOrbit" (RegularForce ForceRadial) packagesOnly
      [ F.strengthVal 0.7, F.xVal 0.0, F.yVal 0.0, F.radiusVal 900.0 ]
  , moduleOrbit: createForce "moduleOrbit" (RegularForce ForceRadial) modulesOnly
      [ F.strengthVal 0.8, F.xVal 0.0, F.yVal 0.0, F.radiusVal 900.0 ]
  , clusterX_M: createForce "clusterX_M"     (RegularForce ForceX)        modulesOnly [ F.strengthVal 0.2, F.xFn (\d _ -> gridPointX (unsafeCoerce d)) ]
  , clusterY_M: createForce "clusterY_M"     (RegularForce ForceY)        modulesOnly [ F.strengthVal 0.2, F.yFn (\d _ -> gridPointY (unsafeCoerce d)) ]

  }
  where
    packagesOnly = Just $ ForceFilter "packages" (isPackage <<< unsafeCoerce)
    modulesOnly = Just $ ForceFilter "modules" (isModule <<< unsafeCoerce)
    usedModulesOnly = Just $ ForceFilter "used modules" (isUsedModule <<< unsafeCoerce)
    gridPointX :: SpagoSimNode -> Number
    gridPointX d = fromMaybe d.x $ map _.x $ toMaybe d.gridXY

    gridPointY :: SpagoSimNode -> Number
    gridPointY d = fromMaybe d.y $ map _.y $ toMaybe d.gridXY


forceLibrary :: Map.Map String (Force SpagoSimNode)
forceLibrary = initialize
  [ forces.charge
  , forces.collision
  , forces.center
  , forces.links
  , forces.packageOrbit
  , forces.moduleOrbit
  , forces.clusterX_M
  , forces.clusterY_M
  ]

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { simulation: initialSimulationState forceLibrary
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
            [ HH.text "Clean rebuild of Code Explorer using LesMisGUP patterns." ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.h3_ [ HH.text "Tree Reveal Animation" ]
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
        [ HH.h3_ [ HH.text "About This Demo" ]
        , HH.p_ [ HH.text "This is a clean rebuild of Code Explorer following the patterns established in LesMisGUP." ]
        , HH.ul_
            [ HH.li_ [ HH.text "Consistent Int IDs for nodes and links" ]
            , HH.li_ [ HH.text "Simple scene switching with proper GUP" ]
            , HH.li_ [ HH.text "Uses genericUpdateSimulation correctly" ]
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

        -- Orbit scene: radial forces for packages and modules
        let activeForces = Set.fromFoldable ["collision", "clusterX_M", "clusterY_M"]
            forcesArray = [ forces.charge, forces.collision, forces.center, forces.links
                          , forces.packageOrbit, forces.moduleOrbit, forces.clusterX_M, forces.clusterY_M ]

        state <- H.get
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Draw.initialize forcesArray activeForces model model.links "#code-explorer-v2-viz" Orbit

        H.modify_ \s -> s { simulation = newState.simulation }
        log $ "CodeExplorerV2: Initialized with " <> show (Array.length model.nodes) <> " nodes"

    pure unit

  FormTree -> do
    log "CodeExplorerV2: Forming tree"
    state <- H.get
    case state.model of
      Nothing -> log "Error: Model not loaded"
      Just model -> do
        -- Staggered transition to tree positions
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Draw.staggeredTreeReveal model

        H.modify_ \s -> s { simulation = newState.simulation, currentScene = TreeReveal }
        log "Tree formation started"

    pure unit

  ActivateForceLayout -> do
    log "CodeExplorerV2: Activating force layout"
    state <- H.get
    case state.model of
      Nothing -> log "Error: Model not loaded"
      Just model -> do
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Draw.activateForceTree model model.links

        H.modify_ \s -> s { simulation = newState.simulation, currentScene = ForceTree }
        log "Force layout activated"

    pure unit

  SwitchScene targetScene -> do
    log $ "CodeExplorerV2: Switching to " <> show targetScene
    state <- H.get
    case state.model of
      Nothing -> log "Error: Model not loaded"
      Just model -> do
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Draw.updateScene model model.links targetScene Set.empty

        H.modify_ \s -> s
          { simulation = newState.simulation
          , currentScene = targetScene
          }

    pure unit
