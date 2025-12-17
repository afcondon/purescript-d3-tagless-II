-- | Force Playground Component - Halogen-First Architecture
-- |
-- | Interactive force-directed graph visualization using procedurally
-- | generated networks with rich attributes.
-- |
-- | ALL STATE LIVES IN HALOGEN:
-- | - model: The generated dataset
-- | - visibleNodeIds: IDs of visible nodes (for category filtering)
-- | - enabledForces: Set of currently enabled forces
-- | - enteringProgress: Map of node ID -> progress for entering nodes
-- | - exitingNodes: Array of nodes with frozen positions, animating out
-- | - simulation: The D3 force simulation handle (opaque)
-- |
-- | Simple provides STATELESS functions:
-- | - createSimulationWithCallbacks: Creates simulation, returns handle
-- | - buildSceneData: Pure function combining nodes + transitions
-- | - renderScene: Renders scene to DOM
-- |
-- | Styled as a fullscreen showcase with floating control panel.
module Component.ForcePlayground where

import Prelude

import Data.Array (filter, length)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Events (SimulationEvent(..), defaultCallbacks)
import PSD3.ForceEngine.Halogen (subscribeToSimulation)
import PSD3.Transition.Tick as Tick
import PSD3.Transform (clearContainer)
import D3.Viz.ForcePlayground.Model (NetworkModel, fromGeneratedGraph)
import D3.Viz.ForcePlayground.Simple as Simple
import D3.Viz.ForcePlayground.Simple (ForceId(..), NetworkSimulation, ExitingNode)
import D3.Viz.ForcePlayground.Generator as Gen

-- | Force presets - pre-configured combinations
data ForcePreset
  = PresetStandard     -- All forces enabled (default)
  | PresetClustered    -- No X/Y centering - clusters drift apart
  | PresetMinimal      -- Only links and collide - organic layout

derive instance eqForcePreset :: Eq ForcePreset

presetLabel :: ForcePreset -> String
presetLabel = case _ of
  PresetStandard -> "Standard"
  PresetClustered -> "Clustered"
  PresetMinimal -> "Minimal"

presetTooltip :: ForcePreset -> String
presetTooltip = case _ of
  PresetStandard -> "All forces enabled - balanced layout"
  PresetClustered -> "No centering - clusters float freely"
  PresetMinimal -> "Links and collision only - organic shapes"

presetForces :: ForcePreset -> Set ForceId
presetForces = case _ of
  PresetStandard -> Simple.allForces
  PresetClustered -> Set.fromFoldable [ForceCharge, ForceCollide, ForceLink]
  PresetMinimal -> Set.fromFoldable [ForceCollide, ForceLink]

-- | All categories visible by default
allCategories :: Set Int
allCategories = Set.fromFoldable [0, 1, 2, 3]

-- | Component state - ALL STATE LIVES HERE
type State =
  { model :: Maybe NetworkModel           -- The generated dataset
  , simulation :: Maybe NetworkSimulation -- D3 force simulation handle
  , enabledForces :: Set ForceId          -- Track for UI rendering
  , activePreset :: Maybe ForcePreset     -- Track current preset (Nothing if custom)
  , shownCategories :: Set Int            -- Which node categories are visible (0-3)
  , visibleNodeIds :: Set Int             -- IDs of visible nodes (for filtering)
  , enteringProgress :: Map Int Tick.Progress  -- Nodes animating in
  , exitingNodes :: Array ExitingNode     -- Nodes animating out with frozen positions
  }

-- | Component actions
data Action
  = Initialize
  | Finalize
  | SimTick                     -- Simulation tick event from D3
  | RegenerateGraph
  | ToggleForce ForceId
  | ApplyPreset ForcePreset
  | ToggleCategory Int
  | ShowAllCategories

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { model: Nothing
      , simulation: Nothing
      , enabledForces: Simple.allForces
      , activePreset: Just PresetStandard
      , shownCategories: allCategories
      , visibleNodeIds: Set.empty
      , enteringProgress: Map.empty
      , exitingNodes: []
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

-- =============================================================================
-- Render - Fullscreen Layout with Floating Panel
-- =============================================================================

render :: forall w. State -> HH.HTML w Action
render state =
  HH.div
    [ HP.classes [ HH.ClassName "force-playground-page" ] ]
    [ -- Site Navigation (top)
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Force Playground"
        }

    -- Fullscreen visualization container
    , HH.div
        [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "force-playground-viz" ] ]
        [ -- Main viz area
          HH.div
            [ HP.id "force-playground-container"
            , HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "svg-container" ]
            ]
            []

        -- Floating control panel (top-right)
        , renderControlPanel state
        ]
    ]

-- | Floating control panel
renderControlPanel :: forall w. State -> HH.HTML w Action
renderControlPanel state =
  HH.div
    [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "floating-panel--medium", HH.ClassName "force-playground-panel" ] ]
    [ -- Panel title
      HH.h2
        [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
        [ HH.text "Controls" ]

    -- Regenerate button
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Network" ]
        , HH.button
            [ HP.classes [ HH.ClassName "control-button", HH.ClassName "control-button--secondary" ]
            , HE.onClick \_ -> RegenerateGraph
            ]
            [ HH.text "Regenerate" ]
        , HH.p
            [ HP.classes [ HH.ClassName "control-description" ] ]
            [ HH.text "Generate a new random network with clusters" ]
        ]

    -- Force Presets
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Presets" ]
        , HH.div
            [ HP.classes [ HH.ClassName "button-row" ] ]
            [ presetButton state PresetStandard
            , presetButton state PresetClustered
            , presetButton state PresetMinimal
            ]
        ]

    -- Force Toggles
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Forces" ]
        , HH.div
            [ HP.classes [ HH.ClassName "toggle-grid" ] ]
            [ forceToggle state ForceCharge "Charge"
            , forceToggle state ForceCollide "Collide"
            , forceToggle state ForceLink "Links"
            , forceToggle state ForceX "X Center"
            , forceToggle state ForceY "Y Center"
            ]
        ]

    -- Category Filters
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Filter Categories" ]
        , HH.div
            [ HP.classes [ HH.ClassName "category-filters" ] ]
            [ categoryToggle state 0 "Research" "#1f77b4"
            , categoryToggle state 1 "Industry" "#ff7f0e"
            , categoryToggle state 2 "Government" "#2ca02c"
            , categoryToggle state 3 "Community" "#d62728"
            ]
        , if state.shownCategories /= allCategories
            then HH.button
              [ HP.classes [ HH.ClassName "control-button", HH.ClassName "control-button--link" ]
              , HE.onClick \_ -> ShowAllCategories
              ]
              [ HH.text "Show All" ]
            else HH.text ""
        ]

    -- Instructions
    , HH.div
        [ HP.classes [ HH.ClassName "panel-info-box" ] ]
        [ HH.p_ [ HH.text "Scroll to zoom, drag to pan." ]
        , HH.p_ [ HH.text "Click-drag nodes to pin them." ]
        , HH.p_ [ HH.text "Tiny drag to unpin." ]
        ]
    ]

-- | Render a preset button
presetButton :: forall w. State -> ForcePreset -> HH.HTML w Action
presetButton state preset =
  let isActive = state.activePreset == Just preset
  in HH.button
      [ HP.classes $
          [ HH.ClassName "control-button" ] <>
          if isActive then [ HH.ClassName "control-button--active" ] else []
      , HP.title (presetTooltip preset)
      , HE.onClick \_ -> ApplyPreset preset
      ]
      [ HH.text (presetLabel preset) ]

-- | Render a force toggle button
forceToggle :: forall w. State -> ForceId -> String -> HH.HTML w Action
forceToggle state forceId label =
  let isEnabled = Set.member forceId state.enabledForces
  in HH.button
      [ HP.classes $
          [ HH.ClassName "toggle-button" ] <>
          if isEnabled then [ HH.ClassName "toggle-button--on" ] else [ HH.ClassName "toggle-button--off" ]
      , HE.onClick \_ -> ToggleForce forceId
      ]
      [ HH.text label ]

-- | Render a category filter toggle with color indicator
categoryToggle :: forall w. State -> Int -> String -> String -> HH.HTML w Action
categoryToggle state catId label color =
  let isVisible = Set.member catId state.shownCategories
  in HH.button
      [ HP.classes $
          [ HH.ClassName "category-button" ] <>
          if isVisible then [ HH.ClassName "category-button--active" ] else [ HH.ClassName "category-button--inactive" ]
      , HP.style $ "border-color: " <> color <> "; " <> if isVisible then "background-color: " <> color <> ";" else ""
      , HP.title $ "Toggle " <> label <> " nodes"
      , HE.onClick \_ -> ToggleCategory catId
      ]
      [ HH.text label ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction RegenerateGraph

  Finalize -> do
    pure unit

  SimTick -> do
    state <- H.get
    case state.simulation, state.model of
      Just sim, Just model -> do
        -- Advance entering progress (0.0 -> 1.0)
        let delta = Simple.transitionDelta
            { active: stillEntering } = Tick.tickProgressMap delta state.enteringProgress

        -- Advance exiting transitions (0.0 -> 1.0)
        let { active: stillExiting } = Tick.tickTransitions delta state.exitingNodes

        -- Update state with advanced progress
        H.modify_ _
          { enteringProgress = stillEntering
          , exitingNodes = stillExiting
          }

        -- Build scene with current Halogen state and render
        liftEffect do
          currentNodes <- Sim.getNodes sim
          let visibleIds = Array.fromFoldable state.visibleNodeIds
              scene = Simple.buildSceneData
                currentNodes
                visibleIds
                stillEntering
                stillExiting
                model.links
          Simple.renderScene scene

      _, _ -> pure unit

  RegenerateGraph -> do
    liftEffect $ clearContainer "#force-playground-container"

    -- Generate new network
    liftEffect $ Console.log "Generating random network..."
    generated <- liftEffect $ Gen.generateGraph Gen.defaultConfig
    let model = fromGeneratedGraph generated
    liftEffect $ Console.log $ "Generated: " <> show (length model.nodes) <> " nodes, " <> show (length model.links) <> " links"

    -- Create callbacks for Halogen subscription
    callbacks <- liftEffect defaultCallbacks

    -- Create simulation with callbacks (Halogen-first pattern)
    simulation <- liftEffect $ Simple.createSimulationWithCallbacks callbacks model

    -- Render SVG container
    liftEffect $ Simple.renderSVGContainer "#force-playground-container"

    -- Subscribe to simulation events via Halogen subscription
    emitter <- liftEffect $ subscribeToSimulation simulation
    void $ H.subscribe $ emitter <#> \event -> case event of
      Tick -> SimTick
      _ -> SimTick

    -- All nodes visible initially
    let allNodeIds = Set.fromFoldable $ map _.id model.nodes

    H.modify_ _
      { model = Just model
      , simulation = Just simulation
      , enabledForces = Simple.allForces
      , activePreset = Just PresetStandard
      , shownCategories = allCategories
      , visibleNodeIds = allNodeIds
      , enteringProgress = Map.empty
      , exitingNodes = []
      }

    -- Do initial render
    doRender

  ToggleForce forceId -> do
    state <- H.get
    case state.simulation of
      Nothing -> pure unit
      Just sim -> do
        let isEnabled = Set.member forceId state.enabledForces
        if isEnabled
          then do
            liftEffect $ Sim.removeForce (Simple.forceIdName forceId) sim
            H.modify_ _ { enabledForces = Set.delete forceId state.enabledForces, activePreset = Nothing }
          else do
            liftEffect $ Sim.addForce (Simple.forceIdSpec forceId) sim
            H.modify_ _ { enabledForces = Set.insert forceId state.enabledForces, activePreset = Nothing }
        liftEffect $ Sim.reheat sim

  ApplyPreset preset -> do
    state <- H.get
    case state.simulation of
      Nothing -> pure unit
      Just sim -> do
        let targetForces = presetForces preset

        -- Remove forces not in preset
        let toRemove = Set.difference state.enabledForces targetForces
        liftEffect $ traverse_ (\f -> Sim.removeForce (Simple.forceIdName f) sim) (Set.toUnfoldable toRemove :: Array ForceId)

        -- Add forces in preset but not currently enabled
        let toAdd = Set.difference targetForces state.enabledForces
        liftEffect $ traverse_ (\f -> Sim.addForce (Simple.forceIdSpec f) sim) (Set.toUnfoldable toAdd :: Array ForceId)

        liftEffect $ Sim.reheat sim

        H.modify_ _ { enabledForces = targetForces, activePreset = Just preset }
    where
      traverse_ :: forall a. (a -> Effect Unit) -> Array a -> Effect Unit
      traverse_ f = void <<< Array.foldM (\_ a -> f a $> unit) unit

  ToggleCategory catId -> do
    state <- H.get
    case state.simulation, state.model of
      Just sim, Just model -> do
        let isCurrentlyShown = Set.member catId state.shownCategories
            newShownCategories = if isCurrentlyShown
              then Set.delete catId state.shownCategories
              else Set.insert catId state.shownCategories

        -- Calculate new visible node IDs based on category filter
        let newVisibleIds = Set.fromFoldable $ map _.id $
              filter (\n -> Set.member n.group newShownCategories) model.nodes

        -- Find nodes being removed (start exit transitions)
        let removedIds = Set.difference state.visibleNodeIds newVisibleIds

        -- Get current positions for exiting nodes
        currentNodes <- liftEffect $ Sim.getNodes sim
        let removedNodes = filter (\n -> Set.member n.id removedIds) currentNodes
            newExiting = Tick.startTransitions removedNodes

        -- Find nodes being added (start enter transitions)
        let addedIds = Set.difference newVisibleIds state.visibleNodeIds
            newEntering = Tick.startProgress (Array.fromFoldable addedIds) state.enteringProgress

        liftEffect $ Sim.reheat sim

        H.modify_ _
          { shownCategories = newShownCategories
          , visibleNodeIds = newVisibleIds
          , enteringProgress = newEntering
          , exitingNodes = state.exitingNodes <> newExiting
          }

      _, _ -> pure unit

  ShowAllCategories -> do
    state <- H.get
    case state.simulation, state.model of
      Just sim, Just model -> do
        let allNodeIds = Set.fromFoldable $ map _.id model.nodes
            addedIds = Set.difference allNodeIds state.visibleNodeIds
            newEntering = Tick.startProgress (Array.fromFoldable addedIds) state.enteringProgress

        liftEffect $ Sim.reheat sim

        H.modify_ _
          { shownCategories = allCategories
          , visibleNodeIds = allNodeIds
          , enteringProgress = newEntering
          }

      _, _ -> pure unit

-- =============================================================================
-- Rendering Helpers
-- =============================================================================

doRender :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
doRender = do
  state <- H.get
  case state.simulation, state.model of
    Just sim, Just model -> liftEffect do
      currentNodes <- Sim.getNodes sim
      let visibleIds = Array.fromFoldable state.visibleNodeIds
          scene = Simple.buildSceneData
            currentNodes
            visibleIds
            state.enteringProgress
            state.exitingNodes
            model.links
      Simple.renderScene scene

    _, _ -> pure unit
