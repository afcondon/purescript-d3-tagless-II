-- | The Code Explorer Component - A Complex Force-Directed Graph Visualization
-- |
-- | This module demonstrates the "MiseEnScene" pattern for building interactive
-- | force simulations with multiple views/scenes. Key architectural elements:
-- |
-- | **Scene Management**: Each "scene" is a complete configuration (MiseEnScene)
-- | that specifies data filters, force settings, visual styling, and initialization.
-- | Switching scenes is as simple as setting up the config and calling runSimulation.
-- |
-- | **State Structure**:
-- | - `model`: Immutable source data (graph structure, node/link properties)
-- | - `staging`: Filtered/transformed data ready for D3 (nodes, links, selections)
-- | - `simulation`: D3 simulation state (managed by library)
-- | - `scene`: Current MiseEnScene configuration
-- |
-- | **Event Flow**:
-- | 1. User interaction in Halogen UI → Action
-- | 2. Action handler updates State (filters, forces, styling)
-- | 3. `runSimulation` stages data and updates D3 visualization
-- | 4. D3 events (clicks, drags) → VizEvent → Halogen Action (bidirectional!)
-- |
-- | **The runSimulation Pattern**:
-- | This is the key function that bridges Halogen state to D3 rendering:
-- | 1. Apply filters to model data → staging
-- | 2. Run node initialization functions (positioning, pinning)
-- | 3. Stop simulation
-- | 4. Update forces based on scene configuration
-- | 5. Call updateSimulation (D3 General Update Pattern)
-- | 6. Restart simulation
module PSD3.CodeExplorer where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array ((:))
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Effect.Class.Console (log)
import PSD3.Data.Tree (TreeLayout(..))
import D3.Viz.Spago.Draw (getVizEventFromClick)
import D3.Viz.Spago.Draw as Graph
import D3.Viz.Spago.Files (NodeType(..))
import D3.Viz.Spago.Model (SpagoModel, isPackage, isPackageOrVisibleModule, transitionNodesToPinnedPositions_)
import PSD3.Data.Node (D3_SimulationNode(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, liftEffect)
import Halogen as H
import Halogen.Subscription as HS
import PSD3.Capabilities.Selection as PSD3Selection
import PSD3.Capabilities.Simulation (start, stop)
import PSD3.Capabilities.Simulation as PSD3Simulation
import PSD3.Internal.Attributes.Sugar (onMouseEventEffectful, x)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Internal.Simulation.Types (initialSimulationState)
import PSD3.Internal.Types (MouseEvent(..))
import PSD3.Interpreter.D3 (evalEffectSimulation, runWithD3_Simulation)
import PSD3.Simulation.RunSimulation as LibRunSim
import PSD3.CodeExplorer.Actions (Action(..), FilterData(..), Scene(..), StyleChange(..), VizEvent(..))
import PSD3.CodeExplorer.Data (readModelData)
import PSD3.CodeExplorer.Forces (forceLibrary)
import PSD3.CodeExplorer.HTML (render)
import PSD3.CodeExplorer.Scenes (horizontalTreeScene, layerSwarmScene, packageGraphScene, packageGridScene, radialTreeScene, verticalTreeScene)
import PSD3.CodeExplorer.State (State, applySceneConfig, clearAllTags, getModelLinks, getModelNodes, initialScene, setChooseNodes, setCssClass, setLinksActive, setLinksShown, setSceneAttributes, tagNodes, toggleForce)

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize }
  }
  where

  initialState :: State
  initialState = do
    { model: Nothing
    , staging: { selections: { nodes: Nothing, links: Nothing }, linksWithForce: const true, rawdata: { nodes: [], links: [] } }
    , simulation: initialSimulationState forceLibrary
    , scene: initialScene forceLibrary
    , eventListener: Nothing
    , transitionListener: Nothing
    , tags: Map.empty
    , showWelcome: true
    }

simulationEvent :: HS.Listener Action -> SelectionAttribute
simulationEvent l = onMouseEventEffectful MouseClick (\e d t -> liftEffect $ HS.notify l (EventFromVizualization (getVizEventFromClick e d t)))

-- | Main action handler - processes all user interactions and internal events
handleAction :: forall t316 t317 t318.
  MonadAff t318 =>
  Action ->
  HalogenM State Action t316 t317 t318 Unit
handleAction = case _ of

  Initialize -> do
    -- 1. Load model data from JSON files (async)
    (maybeModel :: Maybe SpagoModel) <- H.liftAff readModelData
    H.modify_ _ { model = maybeModel }

    -- 2. Initialize D3 structure (one-time SVG setup)
    openSelections <- evalEffectSimulation Graph.initialize
    H.modify_ \s -> s {
      staging = s.staging {
        selections = {
          nodes: openSelections.nodes
        , links: openSelections.links
        }
      }
    }

    -- 3. Set up bidirectional event flow: D3 → Halogen
    -- Create emitter/listener pair for D3 click events to trigger Halogen actions
    { emitter, listener } <- liftEffect $ HS.create
    void $ H.subscribe emitter  -- Subscribe Halogen to the emitter
    H.modify_ _ { eventListener = Just listener }

    -- 4. Set up transition completion flow: D3 transition → Halogen
    -- Create separate emitter/listener pair for transition completion callbacks
    { emitter: transEmitter, listener: transListener } <- liftEffect $ HS.create
    void $ H.subscribe transEmitter  -- Subscribe Halogen to transition events
    H.modify_ _ { transitionListener = Just transListener }

    pure unit

  Finalize -> pure unit

  EventFromVizualization ve -> do
    case ve of
      NodeClick (IsPackage _) id -> handleAction $ ToggleChildrenOfNode id
      NodeClick (IsModule _)  id -> handleAction $ SpotlightNode id

  ToggleChildrenOfNode id -> do
    H.modify_ $ setChooseNodes (isPackageOrVisibleModule id)
    runSimulation

  UnToggleChildrenOfNode _ -> do
    H.modify_ $ setChooseNodes isPackage
    runSimulation

  SpotlightNode _ -> runWithD3_Simulation stop

  -- | Scene Switching Pattern - Declarative Scene Configuration
  -- |
  -- | NEW SIMPLIFIED APPROACH: Scene definitions live in Scenes.purs as constants.
  -- | Scene handlers simply apply the pre-defined configuration.
  -- | This eliminates:
  -- | - Lens imports and operators
  -- | - State mutation boilerplate
  -- | - Duplication of scene configuration
  -- |
  -- | Benefits:
  -- | - Scenes can be easily shared, tested, or composed
  -- | - Scene configuration is declarative and self-documenting
  -- | - No lens knowledge required
  Scene PackageGrid -> do
    -- TRANSITION SCENE PATTERN
    -- Phase 1: Visual transition (simulation OFF)
    -- Phase 2: Scene activation (simulation ON) - triggered by callback

    -- Apply scene configuration to state
    H.modify_ $ applySceneConfig packageGridScene
    state <- H.get

    case state.model of
      Just model -> runWithD3_Simulation do
        -- PHASE 1: TRANSITION (Simulation OFF)
        log "PackageGrid: Starting transition phase (sim OFF)"

        -- Stop simulation - it stays OFF during entire transition
        PSD3Simulation.stop

        -- Calculate target pinned node positions
        let pinnedNodes = foldr (\initFn nodes -> initFn nodes) model.nodes state.scene.nodeInitializerFunctions

        -- Start D3 visual transition (1500ms)
        -- Callback triggers Phase 2 when animation completes
        case state.transitionListener of
          Just listener -> do
            liftEffect $ transitionNodesToPinnedPositions_
              "div.svg-container svg"
              "g.nodes > g"
              "g.links > line"
              pinnedNodes
              (HS.notify listener (ActivateSceneAfterTransition PackageGrid))  -- Phase 2 trigger
          Nothing ->
            log "PackageGrid: No transition listener available"

        -- DON'T call updateSimulation or start simulation here!
        -- That happens in Phase 2 (ActivateSceneAfterTransition handler)

      Nothing -> log "PackageGrid: No model data available"

  Scene PackageGraph -> do
    H.modify_ $ applySceneConfig packageGraphScene
    runSimulation

  Scene LayerSwarm -> do
    H.modify_ $ applySceneConfig layerSwarmScene
    runSimulation

  Scene (ModuleTree Radial) -> do
    -- Apply scene configuration
    H.modify_ $ applySceneConfig radialTreeScene
    state <- H.get

    -- Gentle transition pattern for pinned tree layout
    case state.model of
      Just model -> runWithD3_Simulation do
        PSD3Simulation.stop

        -- Calculate pinned tree positions
        let pinnedNodes = foldr (\initFn nodes -> initFn nodes) model.nodes state.scene.nodeInitializerFunctions

        -- Get DOM selections
        root <- PSD3Selection.attach "div.svg-container"
        nodesGroup <- PSD3Selection.selectUnder root "g.nodes"
        linksGroup <- PSD3Selection.selectUnder root "g.links"

        -- Start D3 transition
        liftEffect $ transitionNodesToPinnedPositions_
          "div.svg-container svg" "g.nodes > g" "g.links > line"
          pinnedNodes
          (log "Radial tree transition complete")

        -- Update simulation with pinned nodes
        let callback = case state.eventListener of
              Just listener -> simulationEvent listener
              Nothing -> x 0.0
            enhancedAttrs = state.scene.attributes {
              circles = callback : state.scene.attributes.circles
            , tagMap = Just state.tags
            }

        Graph.updateSimulation
          { nodes: Just nodesGroup, links: Just linksGroup }
          { allNodes: pinnedNodes
          , allLinks: model.links
          , nodeFilter: state.scene.chooseNodes
          , linkFilter: Just state.scene.linksShown
          , nodeInitializers: []
          , activeForces: state.scene.activeForces
          , linksWithForce: state.scene.linksActive
          }
          enhancedAttrs

        -- Restart simulation so tick function positions nodes
        PSD3Simulation.start
      Nothing -> log "RadialTree: No model data available"

  Scene (ModuleTree Horizontal) -> do
    -- Apply scene configuration
    H.modify_ $ applySceneConfig horizontalTreeScene
    state <- H.get

    -- Gentle transition pattern for pinned tree layout
    case state.model of
      Just model -> runWithD3_Simulation do
        PSD3Simulation.stop

        -- Calculate pinned tree positions
        let pinnedNodes = foldr (\initFn nodes -> initFn nodes) model.nodes state.scene.nodeInitializerFunctions

        -- Get DOM selections
        root <- PSD3Selection.attach "div.svg-container"
        nodesGroup <- PSD3Selection.selectUnder root "g.nodes"
        linksGroup <- PSD3Selection.selectUnder root "g.links"

        -- Start D3 transition
        liftEffect $ transitionNodesToPinnedPositions_
          "div.svg-container svg" "g.nodes > g" "g.links > line"
          pinnedNodes
          (log "Horizontal tree transition complete")

        -- Update simulation with pinned nodes
        let callback = case state.eventListener of
              Just listener -> simulationEvent listener
              Nothing -> x 0.0
            enhancedAttrs = state.scene.attributes {
              circles = callback : state.scene.attributes.circles
            , tagMap = Just state.tags
            }

        Graph.updateSimulation
          { nodes: Just nodesGroup, links: Just linksGroup }
          { allNodes: pinnedNodes
          , allLinks: model.links
          , nodeFilter: state.scene.chooseNodes
          , linkFilter: Just state.scene.linksShown
          , nodeInitializers: []
          , activeForces: state.scene.activeForces
          , linksWithForce: state.scene.linksActive
          }
          enhancedAttrs

        -- Restart simulation so tick function positions nodes
        PSD3Simulation.start
      Nothing -> log "HorizontalTree: No model data available"

  Scene (ModuleTree Vertical) -> do
    -- Apply scene configuration
    H.modify_ $ applySceneConfig verticalTreeScene
    state <- H.get

    -- Gentle transition pattern for pinned tree layout
    case state.model of
      Just model -> runWithD3_Simulation do
        PSD3Simulation.stop

        -- Calculate pinned tree positions
        let pinnedNodes = foldr (\initFn nodes -> initFn nodes) model.nodes state.scene.nodeInitializerFunctions

        -- Get DOM selections
        root <- PSD3Selection.attach "div.svg-container"
        nodesGroup <- PSD3Selection.selectUnder root "g.nodes"
        linksGroup <- PSD3Selection.selectUnder root "g.links"

        -- Start D3 transition
        liftEffect $ transitionNodesToPinnedPositions_
          "div.svg-container svg" "g.nodes > g" "g.links > line"
          pinnedNodes
          (log "Vertical tree transition complete")

        -- Update simulation with pinned nodes
        let callback = case state.eventListener of
              Just listener -> simulationEvent listener
              Nothing -> x 0.0
            enhancedAttrs = state.scene.attributes {
              circles = callback : state.scene.attributes.circles
            , tagMap = Just state.tags
            }

        Graph.updateSimulation
          { nodes: Just nodesGroup, links: Just linksGroup }
          { allNodes: pinnedNodes
          , allLinks: model.links
          , nodeFilter: state.scene.chooseNodes
          , linkFilter: Just state.scene.linksShown
          , nodeInitializers: []
          , activeForces: state.scene.activeForces
          , linksWithForce: state.scene.linksActive
          }
          enhancedAttrs

        -- Restart simulation so tick function positions nodes
        PSD3Simulation.start
      Nothing -> log "VerticalTree: No model data available"

  -- Activate scene after transition completes
  -- This is triggered by the D3 transition completion callback
  -- Scene config is already applied to state, just need to run simulation
  ActivateSceneAfterTransition scene -> do
    log $ "Activating scene after transition: " <> show scene
    runSimulation

  ToggleForce label -> do
    H.modify_ $ toggleForce label
    runSimulation

  Filter (LinkShowFilter filterFn) -> do
    H.modify_ $ setLinksShown filterFn
    runSimulation

  Filter (LinkForceFilter filterFn) -> do
    H.modify_ $ setLinksActive filterFn
    runSimulation

  Filter (NodeFilter filterFn) -> do
    H.modify_ $ setChooseNodes filterFn
    runSimulation

  ChangeStyling (TopLevelCSS style) -> do
    H.modify_ $ setCssClass style

  ChangeStyling (GraphStyle attributes) -> do
    H.modify_ $ setSceneAttributes attributes
    runSimulation

  ChangeSimConfig c -> do
    -- TODO: Use update API
    pure unit -- runWithD3_Simulation $ setConfigVariable c

  StartSim -> do
    runWithD3_Simulation start
    -- TODO: Use update to set alpha first

  StopSim -> runWithD3_Simulation stop

  TagHalogen -> do
    state <- H.get
    let allNodes = getModelNodes state
        -- Tag all packages whose name contains "halogen"
        isHalogenPackage (D3SimNode n) = case n.nodetype of
          IsPackage _ -> String.contains (String.Pattern "halogen") (String.toLower n.name)
          _ -> false
    H.modify_ $ tagNodes "halogen" isHalogenPackage allNodes
    runSimulation

  ClearTags -> do
    H.modify_ clearAllTags
    runSimulation

  DismissWelcome -> do
    H.modify_ _ { showWelcome = false }

-- | The core simulation orchestrator - bridges Halogen state to D3 rendering
-- |
-- | ## The Filter → Initialize → Simulate Pipeline
-- |
-- | This function implements the correct ordering for complex force simulations
-- | that use alternative layout algorithms (trees, grids, etc.):
-- |
-- | **Step 1: Filter** - Apply scene's node filter predicate to model data
-- | ```purescript
-- | let filteredNodes = filter nodeFilter allModelNodes  -- e.g., only modules in tree
-- | ```
-- |
-- | **Step 2: Initialize** - Run node initializers on filtered data
-- | ```purescript
-- | let initializedNodes = foldl (\nodes fn -> fn nodes) filteredNodes [
-- |       unpinAllNodes,           -- Clear any previous pins
-- |       treeNodesToTreeXY_H,     -- Position nodes via tree layout
-- |       fixNamedNodeTo "Main"    -- Pin root node
-- |     ]
-- | ```
-- |
-- | **Step 3: Simulate** - Pass initialized nodes to SimulationM2
-- | ```purescript
-- | update
-- |   { nodes: Just initializedNodes   -- Already filtered and initialized
-- |   , nodeFilter: Nothing             -- Don't filter again!
-- |   , ...
-- |   }
-- | ```
-- |
-- | ## Why This Order Matters
-- |
-- | Node initializers (especially tree layouts) expect homogeneous, pre-filtered data.
-- | For example, `treeNodesToTreeXY_H` expects only the nodes in the dependency tree,
-- | not the full 884-node mixed dataset of modules and packages.
-- |
-- | During debugging, we discovered that when filtering moved into SimulationM2
-- | (after initializers), the tree layout received unfiltered data. The layout algorithm
-- | couldn't handle the mixed data and only generated coordinates for 1 node (the root),
-- | causing tree scenes to render only 1 node instead of ~90.
-- |
-- | ## The Fix
-- |
-- | The solution was to restore the original ordering: filter first, initialize second,
-- | then pass to SimulationM2. This is why we pass `nodeFilter: Nothing` to SimulationM2 -
-- | the filtering has already happened, and we don't want to filter again.
-- |
-- | ## Related Bug: changeLinkType
-- |
-- | Another bug was discovered at the same time: the `changeLinkType` function in Tree.purs
-- | was using incorrect PureScript record update syntax, creating new objects with ONLY
-- | the `linktype` field and losing `source` and `target`. This prevented tree building:
-- |
-- | ```purescript
-- | -- BROKEN (lost source/target):
-- | changeLinkType linktype link =
-- |   unsafeCoerce $ (unsafeCoerce link :: { linktype :: t }) { linktype = linktype }
-- |
-- | -- FIXED (preserves all fields):
-- | changeLinkType newLinktype link =
-- |   let oldLink = unsafeCoerce link :: { source :: Int, target :: Int, linktype :: t, inSim :: Boolean }
-- |   in unsafeCoerce $ oldLink { linktype = newLinktype }
-- | ```
-- |
-- | Without source/target, `buildTree` couldn't build the dependency tree, the tree layout
-- | only generated data for 1 node, and only 1 node got `connected: true` set.
runSimulation :: forall m.
  MonadEffect m =>
  MonadState State m =>
  m Unit
runSimulation = runWithD3_Simulation do
  LibRunSim.runSimulationFromState
    -- Extract selections from state
    (_.staging.selections)
    -- Extract scene configuration from state
    (_.scene)
    -- Extract model nodes from state
    getModelNodes
    -- Extract model links from state
    getModelLinks
    -- Enhance attributes with callbacks and tags
    (\attrs state -> do
      let callback = case state.eventListener of
            Just listener -> simulationEvent listener
            Nothing -> x 0.0  -- dummy during initialization
      attrs {
        circles = callback : attrs.circles
      , tagMap = Just state.tags  -- Pass tags for automatic CSS class propagation
      }
    )
    -- Visualization-specific updateSimulation
    Graph.updateSimulation
