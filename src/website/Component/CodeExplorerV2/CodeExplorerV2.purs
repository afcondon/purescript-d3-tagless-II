-- | CodeExplorerV2 - Clean rebuild following LesMisGUP patterns
-- |
-- | This is a simplified version of CodeExplorer that:
-- | - Uses consistent ID types (Int) throughout
-- | - Follows the declarative genericUpdateSimulation pattern
-- | - Starts minimal and builds up incrementally
module Component.CodeExplorerV2 where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- Spago data types and loading
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode)
import PSD3.CodeExplorer.Data (readModelData)

-- Simulation infrastructure
import PSD3v2.Interpreter.D3v2 as D3v2
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.FFI (linksForceName_)

-- V2 visualization
import Component.CodeExplorerV2.Draw as Draw
import Component.CodeExplorerV2.Types (Scene(..))

-- Data imports
import Data.Map as Map

-- | Simple state following LesMisGUPTree pattern
type State =
  { simulation :: D3SimulationState_ SpagoSimNode
  , model :: Maybe SpagoModel
  , currentScene :: Scene
  }

data Action
  = Initialize
  | SwitchScene Scene

-- | Forces for Spago visualization
-- | Start with basic forces, can add more later
forces :: { center :: Force SpagoSimNode
          , collision :: Force SpagoSimNode
          , charge :: Force SpagoSimNode
          , links :: Force SpagoSimNode
          }
forces =
  { charge: createForce "charge" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-300.0) ]
  , collision: createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 20.0 ]
  , center: createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links: createLinkForce allNodes [ F.distanceVal 50.0 ]
  }

forceLibrary :: Map.Map String (Force SpagoSimNode)
forceLibrary = initialize [ forces.charge, forces.collision, forces.center, forces.links ]

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { simulation: initialSimulationState forceLibrary
        , model: Nothing
        , currentScene: PackageGraph
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
            [ HH.h3_ [ HH.text "Scenes" ]
            , HH.div [ HP.class_ (HH.ClassName "button-group") ]
                [ HH.button
                    [ HP.class_ (HH.ClassName $ "control-button" <> if state.currentScene == PackageGraph then " active" else "")
                    , HE.onClick \_ -> SwitchScene PackageGraph
                    ]
                    [ HH.text "Package Graph" ]
                , HH.button
                    [ HP.class_ (HH.ClassName $ "control-button" <> if state.currentScene == HorizontalTree then " active" else "")
                    , HE.onClick \_ -> SwitchScene HorizontalTree
                    ]
                    [ HH.text "Horizontal Tree" ]
                ]
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

        let activeForces = Set.fromFoldable ["charge", "collision", "center", linksForceName_]
            forcesArray = [ forces.charge, forces.collision, forces.center, forces.links ]

        state <- H.get
        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Draw.initialize forcesArray activeForces model model.links "#code-explorer-v2-viz" PackageGraph

        H.modify_ \s -> s { simulation = newState.simulation }
        log $ "CodeExplorerV2: Initialized with " <> show (Array.length model.nodes) <> " nodes"

    pure unit

  SwitchScene targetScene -> do
    log $ "CodeExplorerV2: Switching to " <> show targetScene
    state <- H.get
    case state.model of
      Nothing -> log "Error: Model not loaded"
      Just model -> do
        -- Determine active forces based on scene
        let activeForces = case targetScene of
              PackageGraph -> Set.fromFoldable ["charge", "collision", "center", linksForceName_]
              HorizontalTree -> Set.fromFoldable ["collision"]  -- Tree layouts use minimal forces

        newState <- H.liftAff $ D3v2.execD3v2SimM { simulation: state.simulation } do
          Draw.updateScene model model.links targetScene activeForces

        H.modify_ \s -> s
          { simulation = newState.simulation
          , currentScene = targetScene
          }

    pure unit
