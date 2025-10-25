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
module PSD3.Spago where

import Prelude

import Control.Monad.State (class MonadState, get)
import PSD3.Internal.Attributes.Sugar (onMouseEventEffectful, x)
import PSD3.Data.Tree (TreeLayout(..))
import PSD3.Internal.Types (MouseEvent(..))
import D3.Viz.Spago.Draw (getVizEventFromClick)
import D3.Viz.Spago.Draw as Graph
import D3.Viz.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Viz.Spago.Files (NodeType(..), isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Viz.Spago.Model (SpagoModel, allNodes, fixNamedNodeTo, isPackage, isPackageOrVisibleModule, isUsedModule, moduleNodesToContainerXY, packageNodesToGridXY, packagesNodesToPhyllotaxis, sourcePackageIs, treeNodesToTreeXY_R, unpinAllNodes)
import PSD3.Internal.FFI (linksForceName)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Internal.Simulation.Types (SimVariable(..), initialSimulationState)
import Data.Set as Set
import PSD3.Capabilities.Simulation (actualizeForces, setConfigVariable, start, stop)
import PSD3.Interpreter.D3 (evalEffectSimulation, runWithD3_Simulation)
import Data.Array (filter, foldl, (:))
import Data.Lens (use, view, (%=), (.=))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, liftEffect)
import Halogen as H
import Halogen.Subscription as HS
import PSD3.CodeExplorer.Actions (Action(..), FilterData(..), Scene(..), StyleChange(..), VizEvent(..))
import PSD3.CodeExplorer.Data (readModelData)
import PSD3.CodeExplorer.Forces (forceLibrary)
import PSD3.CodeExplorer.HTML (render)
import PSD3.CodeExplorer.State (State, _activeForces, _chooseNodes, _cssClass, _enterselections, _eventListener, _links, _linksActive, _linksShown, _model, _modelLinks, _modelNodes, _nodeInitializerFunctions, _nodes, _sceneAttributes, _staging, _stagingLinkFilter, _stagingLinks, _stagingNodes, initialScene)

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
    -- read various JSON files and synthesize a Model
    (maybeModel :: Maybe SpagoModel) <- H.liftAff readModelData
    _model %= (const maybeModel)

    -- 2. Initialize D3 structure (one-time SVG setup)
    openSelections <- evalEffectSimulation Graph.initialize
    (_staging <<< _enterselections <<< _nodes) %= (const $ openSelections.nodes)
    (_staging <<< _enterselections <<< _links) %= (const $ openSelections.links)

    -- 3. Set up bidirectional event flow: D3 → Halogen
    -- Create emitter/listener pair for D3 click events to trigger Halogen actions
    { emitter, listener } <- liftEffect $ HS.create
    void $ H.subscribe emitter  -- Subscribe Halogen to the emitter
    _eventListener .= Just listener  -- Store listener in component state (not scene config)

    pure unit

  Finalize -> pure unit

  EventFromVizualization ve -> do
    case ve of
      NodeClick (IsPackage _) id -> handleAction $ ToggleChildrenOfNode id
      NodeClick (IsModule _)  id -> handleAction $ SpotlightNode id

  ToggleChildrenOfNode id -> do
    _chooseNodes .= (isPackageOrVisibleModule id)
    runSimulation

  UnToggleChildrenOfNode _ -> do
    _chooseNodes .= isPackage
    runSimulation

  SpotlightNode _ -> runWithD3_Simulation stop

  -- | Scene Switching Pattern - demonstrates the MiseEnScene approach
  -- | Each scene handler:
  -- | 1. Sets node/link filters (_chooseNodes, _linksShown, _linksActive)
  -- | 2. Configures forces (_activeForces: Set of force labels to enable)
  -- | 3. Sets visual style (_cssClass, _sceneAttributes)
  -- | 4. Specifies initialization functions (_nodeInitializerFunctions)
  -- | 5. Calls runSimulation to apply the configuration
  Scene PackageGrid -> do
    _chooseNodes     .= allNodes
    _linksShown      .= isM2P_Link
    _linksActive     .= const true
    _cssClass        .= "cluster"
    _sceneAttributes .= clusterSceneAttributes
    _activeForces    .= Set.fromFoldable [ "clusterx_P", "clustery_P", "clusterx_M", "clustery_M", "collide2" ]
    _nodeInitializerFunctions .= [ unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY ]
    runSimulation

  Scene PackageGraph -> do
    _chooseNodes     .= isPackage
    _linksShown      .= isP2P_Link
    _linksActive     .= (sourcePackageIs "my-project")
    _activeForces    .= Set.fromFoldable ["center", "collide2", "charge2", "packageOrbit", linksForceName ]
    _cssClass        .= "graph"
    _sceneAttributes .= graphSceneAttributes
    _nodeInitializerFunctions .= [ unpinAllNodes, packagesNodesToPhyllotaxis, fixNamedNodeTo "my-project" { x: 0.0, y: 0.0 } ]
    runSimulation

  Scene LayerSwarm -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .= const true
    _cssClass        .= "tree"
    _sceneAttributes .= treeSceneAttributes
    _activeForces    .= Set.fromFoldable [ "htreeNodesX", "collide1", "y", linksForceName ]
    _nodeInitializerFunctions .= [ unpinAllNodes ]
    runSimulation

  Scene (ModuleTree Radial) -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .= const true
    _cssClass        .= "tree radial"
    _sceneAttributes .= treeSceneAttributes
    _activeForces    .= Set.fromFoldable [ "center", "collide2", "chargetree", "charge2", linksForceName ]
    _nodeInitializerFunctions .= [ unpinAllNodes, treeNodesToTreeXY_R, fixNamedNodeTo "Main" { x: 0.0, y: 0.0 } ]
    runSimulation

  Scene (ModuleTree Horizontal) -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .= const false
    _cssClass        .= "tree horizontal"
    _sceneAttributes .= treeSceneAttributes
    _activeForces    .= Set.fromFoldable [ "htreeNodesX", "htreeNodesY", "charge1", "collide2" ]
    _nodeInitializerFunctions .= [ unpinAllNodes ]
    runSimulation

  Scene (ModuleTree Vertical) -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .=  const false
    _cssClass        .= "tree vertical"
    _sceneAttributes .= treeSceneAttributes
    _activeForces    .= Set.fromFoldable [ "vtreeNodesX", "vtreeNodesY", "charge1", "collide2" ]
    _nodeInitializerFunctions .= [ unpinAllNodes ]
    runSimulation

  ToggleForce label -> do
    _activeForces %= \forces ->
      if Set.member label forces
        then Set.delete label forces
        else Set.insert label forces
    runSimulation

  Filter (LinkShowFilter filterFn) -> do
    _linksShown .= filterFn
    runSimulation

  Filter (LinkForceFilter filterFn) -> do
    _linksActive .= filterFn
    runSimulation

  Filter (NodeFilter filterFn) -> do
    _chooseNodes .= filterFn
    runSimulation

  ChangeStyling (TopLevelCSS style) -> do
    _cssClass .= style

  ChangeStyling (GraphStyle attributes) -> do
    _sceneAttributes .= attributes
    runSimulation

  ChangeSimConfig c -> do
    runWithD3_Simulation $ setConfigVariable c

  StartSim -> do
    runWithD3_Simulation do
      setConfigVariable $ Alpha 1.0
      start

  StopSim -> runWithD3_Simulation $
    do
      setConfigVariable $ Alpha 0.0
      stop

-- | Prepare model data for visualization by applying filters and transformations
-- | This is the bridge between immutable model data and mutable staging data
stageDataFromModel :: forall m.
  MonadState State m =>
  m Unit
stageDataFromModel = do
  state       <- get
  linksShown  <- use _linksShown
  linksActive <- use _linksActive
  chooseNodes <- use _chooseNodes
  nodeInitializerFunctions <- use _nodeInitializerFunctions

  _stagingLinks      .= (filter linksShown $ view _modelLinks state)
  _stagingLinkFilter .= linksActive
  let rawnodes = filter chooseNodes $ view _modelNodes state
      initializedNodes = foldl (\b a -> a b) rawnodes nodeInitializerFunctions

  _stagingNodes      .= initializedNodes

-- | The core simulation orchestrator - bridges Halogen state to D3 rendering
-- |
-- | This is the key pattern for updating force simulations:
-- | 1. Stage data from model (apply filters, run initializers)
-- | 2. Stop the running simulation
-- | 3. Activate/deactivate forces based on scene config
-- | 4. Update D3 visualization (General Update Pattern)
-- | 5. Restart simulation with new alpha
-- |
-- | Called whenever scene configuration changes or data is filtered
runSimulation :: forall m.
  MonadEffect m =>
  MonadState State m =>
  m Unit
runSimulation = do
  stageDataFromModel
  staging         <- use _staging
  maybeListener   <- use _eventListener
  sceneAttributes <- use _sceneAttributes
  activeForces    <- use _activeForces

  -- Construct callback from listener (or dummy if not yet initialized)
  let callback = case maybeListener of
        Just listener -> simulationEvent listener
        Nothing -> x 0.0  -- dummy during initialization
      attributesWithCallback = sceneAttributes { circles = callback : sceneAttributes.circles }

  runWithD3_Simulation do
    stop
    actualizeForces activeForces
    Graph.updateSimulation staging attributesWithCallback
    setConfigVariable $ Alpha 1.0
    start
