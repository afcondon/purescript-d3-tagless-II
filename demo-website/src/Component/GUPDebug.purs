-- | GUP Debug Component - Halogen-First Architecture
-- |
-- | Standalone debug page for testing GUP (General Update Pattern) with
-- | the Les Misérables force-directed graph.
-- |
-- | ALL STATE LIVES IN HALOGEN:
-- | - fullModel: The complete dataset
-- | - enteringProgress: Map of node name -> progress for entering nodes
-- | - exitingNodes: Array of nodes with frozen positions, animating out
-- | - simulation: The D3 force simulation handle (opaque)
-- |
-- | GUPDemo provides STATELESS functions:
-- | - createSimulation: Creates simulation, returns handle
-- | - subscribeToTick: Sets up tick callback
-- | - buildSceneData: Pure function combining nodes + transitions
-- | - renderScene: Renders scene to DOM
-- |
-- | TODO: Consider the interaction between visual interpolation and physics.
-- | Currently, visual property transitions (radius, color, opacity) run
-- | concurrently with the physics simulation. This works well for GUP where
-- | we only interpolate visuals, not positions. However, if position
-- | interpolation were needed (like in SceneEngine), the simulation would
-- | overwrite interpolated positions. For library users, this could lead to
-- | confusing bugs. Consider:
-- | - Documenting this behavior clearly
-- | - Providing a mode that pauses physics during transitions
-- | - Or making the interpolation/physics relationship more explicit in the API
module Component.GUPDebug where

import Prelude

import Control.Monad (void)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.DataLoader (simpleLoadJSON)
import PSD3.Transition.Tick as Tick
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Setup as Setup
import PSD3.ForceEngine.Events (SimulationEvent(..), defaultCallbacks)
import PSD3.ForceEngine.Halogen (subscribeToSimulation)
import Unsafe.Coerce (unsafeCoerce)
import D3.Viz.LesMisV3.Model as LesMisModel
import D3.Viz.LesMisV3.Model (LesMisModel)
import D3.Viz.LesMisV3.GUPDemo as GUPDemo
import D3.Viz.LesMisV3.GUPDemo (ExitingNode, LesMisSimulation)
import PSD3.ForceEngine.Links (filterLinksToSubset)

-- =============================================================================
-- Component State
-- =============================================================================

-- | All state lives in Halogen
type State =
  { fullModel :: Maybe LesMisModel        -- The complete dataset
  , visibleIds :: Set String              -- IDs of visible nodes (subset of fullModel)
  , enteringProgress :: Map String Tick.Progress  -- Nodes animating in
  , exitingNodes :: Array ExitingNode     -- Nodes animating out with frozen positions
  , simulation :: Maybe LesMisSimulation  -- D3 force simulation handle
  }

data Action
  = Initialize
  | Finalize
  | SimTick                     -- Simulation tick event from D3
  | AddNodes
  | RemoveNodes
  | ResetFull

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { fullModel: Nothing
      , visibleIds: Set.empty
      , enteringProgress: Map.empty
      , exitingNodes: []
      , simulation: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Les Misérables data
    lesMisJson <- H.liftAff $ simpleLoadJSON "./data/miserables.json"
    let rawModel :: LesMisModel.LesMisRawModel
        rawModel = unsafeCoerce lesMisJson
        model = LesMisModel.processRawModel rawModel

    -- Initialize all nodes as visible
    let allNodeIds = Set.fromFoldable $ map _.name model.nodes

    -- Create callbacks for Halogen subscription
    callbacks <- liftEffect defaultCallbacks

    -- Create simulation with callbacks
    simulation <- liftEffect $ GUPDemo.createSimulationWithCallbacks callbacks model
    liftEffect $ GUPDemo.renderSVGContainer "#gup-viz-container"

    -- Subscribe to simulation events via Halogen subscription
    -- This allows tick events to flow through Halogen, enabling state access
    emitter <- liftEffect $ subscribeToSimulation simulation
    void $ H.subscribe $ emitter <#> \event -> case event of
      Tick -> SimTick
      _ -> SimTick  -- Ignore other events, just render

    -- Store state
    H.modify_ _
      { fullModel = Just model
      , visibleIds = allNodeIds
      , enteringProgress = Map.empty
      , exitingNodes = []
      , simulation = Just simulation
      }

    -- Do initial render
    doRender

  Finalize -> do
    -- Cleanup if needed (simulation will stop on its own)
    pure unit

  SimTick -> do
    -- On each tick from the simulation:
    -- 1. Advance transition progress for entering/exiting nodes
    -- 2. Remove completed transitions
    -- 3. Render scene with current state
    state <- H.get
    case state.simulation of
      Nothing -> pure unit
      Just sim -> do
        -- Advance entering progress (0.0 → 1.0)
        let delta = GUPDemo.transitionDelta
            { active: stillEntering } = Tick.tickProgressMap delta state.enteringProgress

        -- Advance exiting transitions (0.0 → 1.0)
        let { active: stillExiting } = Tick.tickTransitions delta state.exitingNodes

        -- Update state with advanced progress
        H.modify_ _
          { enteringProgress = stillEntering
          , exitingNodes = stillExiting
          }

        -- Build scene with current Halogen state and render
        liftEffect do
          currentNodes <- Sim.getNodes sim
          currentLinks <- Sim.getLinks sim
          let scene = GUPDemo.buildSceneData
                currentNodes
                stillEntering    -- Use current transition state
                stillExiting     -- Use current exiting state
                currentLinks
          GUPDemo.renderScene scene

  AddNodes -> do
    state <- H.get
    case state.fullModel, state.simulation of
      Just model, Just sim -> do
        -- Pick 5 random nodes from hidden nodes to add
        let hiddenNodes = Array.filter (\n -> not (Set.member n.name state.visibleIds)) model.nodes

        nodesToAdd <- liftEffect $ GUPDemo.pickRandom 5 hiddenNodes
        let newVisibleIds = state.visibleIds <> (Set.fromFoldable $ map _.name nodesToAdd)

        -- Build desired state
        let desiredNodes = Array.filter (\n -> Set.member n.name newVisibleIds) model.nodes
            desiredLinks = filterLinksToSubset _.id desiredNodes model.links

        -- Apply GUP with the shared setup
        result <- liftEffect $ Setup.applySetupWithData GUPDemo.lesMisSetup desiredNodes desiredLinks sim

        -- Start entering transitions for new nodes
        let enteringNames = map _.name result.nodes.entered
            newEntering = Tick.startProgress enteringNames state.enteringProgress

        -- Reheat simulation
        liftEffect $ Sim.reheat sim

        H.modify_ _
          { visibleIds = newVisibleIds
          , enteringProgress = newEntering
          }

        Console.log $ "Added " <> show (Array.length nodesToAdd) <> " nodes"

      _, _ -> pure unit

  RemoveNodes -> do
    state <- H.get
    case state.fullModel, state.simulation of
      Just model, Just sim -> do
        -- Pick 5 random visible nodes to remove
        let visibleNodes = Array.filter (\n -> Set.member n.name state.visibleIds) model.nodes

        nodesToRemove <- liftEffect $ GUPDemo.pickRandom 5 visibleNodes
        let nodesToRemoveIds = Set.fromFoldable $ map _.name nodesToRemove
            newVisibleIds = Set.difference state.visibleIds nodesToRemoveIds

        -- Build desired state
        let desiredNodes = Array.filter (\n -> Set.member n.name newVisibleIds) model.nodes
            desiredLinks = filterLinksToSubset _.id desiredNodes model.links

        -- Apply GUP
        result <- liftEffect $ Setup.applySetupWithData GUPDemo.lesMisSetup desiredNodes desiredLinks sim

        -- Start exiting transitions for removed nodes (freeze their current positions)
        let newExiting = Tick.startTransitions result.nodes.exited

        -- Reheat simulation
        liftEffect $ Sim.reheat sim

        H.modify_ _
          { visibleIds = newVisibleIds
          , exitingNodes = state.exitingNodes <> newExiting
          }

        Console.log $ "Removed " <> show (Array.length nodesToRemove) <> " nodes"

      _, _ -> pure unit

  ResetFull -> do
    state <- H.get
    case state.fullModel, state.simulation of
      Just model, Just sim -> do
        -- Reset to full model
        let allNodeIds = Set.fromFoldable $ map _.name model.nodes

        -- Apply GUP with full data
        result <- liftEffect $ Setup.applySetupWithData GUPDemo.lesMisSetup model.nodes model.links sim

        -- Start entering transitions for newly visible nodes
        let enteringNames = map _.name result.nodes.entered
            newEntering = Tick.startProgress enteringNames Map.empty

        -- Reheat simulation
        liftEffect $ Sim.reheat sim

        H.modify_ _
          { visibleIds = allNodeIds
          , enteringProgress = newEntering
          , exitingNodes = []
          }

        Console.log "Reset to full dataset"

      _, _ -> pure unit

-- =============================================================================
-- Rendering Helpers
-- =============================================================================

-- | Internal: Render current state to DOM (used for initial render)
doRender :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
doRender = do
  state <- H.get
  case state.simulation of
    Just sim -> liftEffect do
      -- Get current nodes and links from simulation
      currentNodes <- Sim.getNodes sim
      currentLinks <- Sim.getLinks sim

      -- Build scene data combining:
      -- - Current nodes with positions
      -- - Entering progress map
      -- - Exiting nodes with frozen positions
      -- - Current links
      let scene = GUPDemo.buildSceneData
            currentNodes
            state.enteringProgress
            state.exitingNodes
            currentLinks

      -- Render to DOM (stateless)
      GUPDemo.renderScene scene

    _ -> pure unit

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "gup-debug-page" ] ]
    [ HH.h1_ [ HH.text "GUP Debug - Les Miserables" ]
    , HH.p_ [ HH.text "Halogen-first architecture: All state lives in Halogen." ]
    , HH.p_ [ HH.text "GUPDemo provides stateless visualization functions." ]

    -- Visualization container
    , HH.div
        [ HP.id "gup-viz-container"
        , HP.classes [ HH.ClassName "viz-container" ]
        , HP.style "width: 100%; height: 600px; border: 1px solid #ccc; background: #fafafa;"
        ]
        []

    -- Button row
    , HH.div
        [ HP.classes [ HH.ClassName "button-row" ]
        , HP.style "margin-top: 1rem; display: flex; gap: 0.5rem; flex-wrap: wrap;"
        ]
        [ HH.button
            [ HP.classes [ HH.ClassName "debug-button" ]
            , HP.style "padding: 0.5rem 1rem; cursor: pointer;"
            , HE.onClick \_ -> AddNodes
            ]
            [ HH.text "Add 5 Nodes" ]
        , HH.button
            [ HP.classes [ HH.ClassName "debug-button" ]
            , HP.style "padding: 0.5rem 1rem; cursor: pointer;"
            , HE.onClick \_ -> RemoveNodes
            ]
            [ HH.text "Remove 5 Nodes" ]
        , HH.button
            [ HP.classes [ HH.ClassName "debug-button", HH.ClassName "secondary" ]
            , HP.style "padding: 0.5rem 1rem; cursor: pointer;"
            , HE.onClick \_ -> ResetFull
            ]
            [ HH.text "Reset All" ]
        ]

    -- Instructions
    , HH.div
        [ HP.style "margin-top: 2rem; padding: 1rem; background: #f0f0f0; border-radius: 4px;" ]
        [ HH.h3_ [ HH.text "Architecture" ]
        , HH.ul_
            [ HH.li_ [ HH.text "State: fullModel, visibleIds, enteringProgress, exitingNodes, simulation" ]
            , HH.li_ [ HH.text "Simulation tick callback drives position updates" ]
            , HH.li_ [ HH.text "applySetupWithData handles GUP semantics" ]
            , HH.li_ [ HH.text "Transitions are tick-driven via Tick module" ]
            ]
        , HH.h3_ [ HH.text "How to Use" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Add/Remove Nodes: Triggers GUP transitions (green=enter, gray=update, brown=exit)" ]
            , HH.li_ [ HH.text "Reset All: Restores the full dataset" ]
            , HH.li_ [ HH.text "Drag nodes to reposition them; the simulation will respond" ]
            ]
        ]
    ]
