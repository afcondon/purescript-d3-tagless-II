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
import D3.Viz.Spago.Draw as Graph
import D3.Viz.Spago.Files (NodeType(..))
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isPackage, isPackageOrVisibleModule)
import Data.Array ((:), filter)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (HalogenM, liftEffect)
import Halogen as H
import Halogen.Subscription as HS
import PSD3.Capabilities.Selection as PSD3Selection
import PSD3v2.Capabilities.Simulation (start, stop)
import PSD3.CodeExplorer.Actions (Action(..), FilterData(..), Scene(..), StyleChange(..), VizEvent(..))
import PSD3.CodeExplorer.Data (readModelData)
import PSD3.CodeExplorer.Forces (forceLibrary)
import PSD3.CodeExplorer.HTML (render)
import PSD3.CodeExplorer.Scenes (horizontalTreeScene, layerSwarmScene, packageGraphScene, packageGridScene, radialTreeScene, verticalTreeScene)
import PSD3.CodeExplorer.State (State, TransitionMatrix, applySceneConfig, applySceneWithTransition, clearAllTags, getModelLinks, getModelNodes, getSelections, initialScene, setChooseNodes, setCssClass, setLinksActive, setLinksShown, setSceneAttributes, tagNodes, toggleForce, updateScene)
import PSD3.Data.Tree (TreeLayout(..))
import PSD3.Internal.Simulation.Types (initialSimulationState)
import PSD3.Interpreter.D3 (evalEffectSimulation)
import PSD3v2.Interpreter.D3v2 (evalD3v2SimM)
import PSD3v2.Simulation.RunSimulation as LibRunSim
import PSD3v2.Simulation.Scene (smoothTransition, smoothTransitionPinned)
import Unsafe.Coerce (unsafeCoerce)

-- | Default transition matrix: declarative specification of scene choreography
-- |
-- | This matrix defines how scene transitions animate. Key design decisions:
-- | - **Initial setup is instant**: (Nothing, _) transitions not specified → instant
-- | - **Between-scene transitions are smooth**: Most scene switches use smoothTransition
-- | - **Sparse specification**: Only define non-instant transitions
-- |
-- | The matrix enables asymmetric transitions. For example:
-- | - PackageGrid → PackageGraph: smooth (expanding view)
-- | - PackageGraph → PackageGrid: could be quick (collapsing view)
-- |
-- | Benefits:
-- | - Fully declarative: No manual sequencing in action handlers
-- | - Type-safe: Compiler ensures all scenes are valid
-- | - Maintainable: All choreography in one place
-- | - Testable: Matrix can be inspected and validated independently
defaultTransitionMatrix :: TransitionMatrix
defaultTransitionMatrix = Map.fromFoldable
  -- Between-scene transitions: all smooth
  [ Tuple (Tuple PackageGrid PackageGraph) smoothTransition
  , Tuple (Tuple PackageGraph PackageGrid) smoothTransition
  , Tuple (Tuple PackageGrid LayerSwarm) smoothTransition
  , Tuple (Tuple LayerSwarm PackageGrid) smoothTransition
  , Tuple (Tuple PackageGraph LayerSwarm) smoothTransition
  , Tuple (Tuple LayerSwarm PackageGraph) smoothTransition
  -- To tree layouts: use pinned transition (nodes lock at tree positions)
  , Tuple (Tuple PackageGrid (ModuleTree Radial)) smoothTransitionPinned
  , Tuple (Tuple PackageGrid (ModuleTree Horizontal)) smoothTransitionPinned
  , Tuple (Tuple PackageGrid (ModuleTree Vertical)) smoothTransitionPinned
  , Tuple (Tuple PackageGraph (ModuleTree Radial)) smoothTransitionPinned
  , Tuple (Tuple PackageGraph (ModuleTree Horizontal)) smoothTransitionPinned
  , Tuple (Tuple PackageGraph (ModuleTree Vertical)) smoothTransitionPinned
  , Tuple (Tuple LayerSwarm (ModuleTree Radial)) smoothTransitionPinned
  , Tuple (Tuple LayerSwarm (ModuleTree Horizontal)) smoothTransitionPinned
  , Tuple (Tuple LayerSwarm (ModuleTree Vertical)) smoothTransitionPinned
  -- From tree layouts: use regular transition (nodes become unpinned, forces take over)
  , Tuple (Tuple (ModuleTree Radial) PackageGrid) smoothTransition
  , Tuple (Tuple (ModuleTree Radial) PackageGraph) smoothTransition
  , Tuple (Tuple (ModuleTree Radial) LayerSwarm) smoothTransition
  , Tuple (Tuple (ModuleTree Horizontal) PackageGrid) smoothTransition
  , Tuple (Tuple (ModuleTree Horizontal) PackageGraph) smoothTransition
  , Tuple (Tuple (ModuleTree Horizontal) LayerSwarm) smoothTransition
  , Tuple (Tuple (ModuleTree Vertical) PackageGrid) smoothTransition
  , Tuple (Tuple (ModuleTree Vertical) PackageGraph) smoothTransition
  , Tuple (Tuple (ModuleTree Vertical) LayerSwarm) smoothTransition
  -- Between tree orientations: use pinned (trees stay locked)
  , Tuple (Tuple (ModuleTree Radial) (ModuleTree Horizontal)) smoothTransitionPinned
  , Tuple (Tuple (ModuleTree Radial) (ModuleTree Vertical)) smoothTransitionPinned
  , Tuple (Tuple (ModuleTree Horizontal) (ModuleTree Radial)) smoothTransitionPinned
  , Tuple (Tuple (ModuleTree Horizontal) (ModuleTree Vertical)) smoothTransitionPinned
  , Tuple (Tuple (ModuleTree Vertical) (ModuleTree Radial)) smoothTransitionPinned
  , Tuple (Tuple (ModuleTree Vertical) (ModuleTree Horizontal)) smoothTransitionPinned
  -- Initial setup: not specified, defaults to instant (no delay on first scene)
  ]

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
    , currentScene: EmptyScene  -- No scene active initially
    , transitionMatrix: defaultTransitionMatrix
    , eventListener: Nothing
    , transitionListener: Nothing
    , tags: Map.empty
    , showWelcome: true
    }

-- | Main action handler - processes all user interactions and internal events
handleAction :: forall t316 t317 t318.
  MonadAff t318 =>
  Action SpagoSimNode ->
  HalogenM State (Action SpagoSimNode) t316 t317 t318 Unit
handleAction = case _ of

  Initialize -> do
    -- 1. Load model data from JSON files (async)
    (maybeModel :: Maybe SpagoModel) <- H.liftAff readModelData
    H.modify_ _ { model = maybeModel }

    -- 2. Initialize D3 structure (one-time SVG setup)
    state <- H.get
    openSelections <- H.liftAff $ evalD3v2SimM state Graph.initialize
    H.modify_ \s -> s {
      staging = s.staging {
        selections = {
          nodes: Just (unsafeCoerce openSelections.nodes)
        , links: Just (unsafeCoerce openSelections.links)
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

  SpotlightNode _ -> do
    state <- H.get
    H.liftAff $ evalD3v2SimM state stop

  -- | Scene Switching Pattern - Declarative Scene Configuration with Transition Matrix
  -- |
  -- | TRANSITION MATRIX APPROACH: Each scene handler:
  -- | 1. Looks up (currentScene, targetScene) in transition matrix
  -- | 2. Applies scene config with appropriate transition
  -- | 3. Runs simulation (which executes transition if specified)
  -- | 4. Updates currentScene to track state
  -- |
  -- | This enables:
  -- | - **Instant initial setup**: (Nothing, Scene) → instant (no delay on first scene)
  -- | - **Smooth scene switching**: (Scene, Scene) → smooth transition
  -- | - **Asymmetric choreography**: Different animations A→B vs B→A
  -- | - **Fully declarative**: No manual sequencing, just lookup and apply
  -- |
  -- | Benefits:
  -- | - Zero user orchestration: No stop/transition/start sequencing
  -- | - Type-safe: Compiler ensures valid scene transitions
  -- | - Maintainable: All choreography in defaultTransitionMatrix
  -- | - Testable: Matrix can be validated independently

  Scene EmptyScene -> do
    pure unit

  Scene PackageGrid -> do
    H.modify_ $ applySceneWithTransition PackageGrid packageGridScene
    runSimulation
    H.modify_ _ { currentScene = PackageGrid }

  Scene PackageGraph -> do
    H.modify_ $ applySceneWithTransition PackageGraph packageGraphScene
    runSimulation
    H.modify_ _ { currentScene = PackageGraph }

  Scene LayerSwarm -> do
    H.modify_ $ applySceneWithTransition LayerSwarm layerSwarmScene
    runSimulation
    H.modify_ _ { currentScene = LayerSwarm }

  Scene (ModuleTree Radial) -> do
    H.modify_ $ applySceneWithTransition (ModuleTree Radial) radialTreeScene
    runSimulation
    H.modify_ _ { currentScene = (ModuleTree Radial) }

  Scene (ModuleTree Horizontal) -> do
    H.modify_ $ applySceneWithTransition (ModuleTree Horizontal) horizontalTreeScene
    runSimulation
    H.modify_ _ { currentScene = (ModuleTree Horizontal) }

  Scene (ModuleTree Vertical) -> do
    H.modify_ $ applySceneWithTransition (ModuleTree Vertical) verticalTreeScene
    runSimulation
    H.modify_ _ { currentScene = (ModuleTree Vertical) }

  ToggleForce label -> do
    H.modify_ $ toggleForce label
    -- Clear transition for instant feedback on force toggles
    H.modify_ $ updateScene \s -> s { transitionConfig = Nothing }
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
    state <- H.get
    H.liftAff $ evalD3v2SimM state start
    -- TODO: Use update to set alpha first

  StopSim -> do
    state <- H.get
    H.liftAff $ evalD3v2SimM state stop

  TagHalogen -> do
    state <- H.get
    let allNodes = getModelNodes state
        -- Tag all packages whose name contains "halogen"
        isHalogenPackage n = case n.nodetype of
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
  MonadAff m =>
  MonadState State m =>
  m Unit
runSimulation = do
  state <- H.get
  H.liftAff $ evalD3v2SimM state do
    LibRunSim.runSimulationFromState
      -- Extract selections from state (unwraps Maybe for v2 API)
      getSelections
      -- Extract scene configuration from state
      (_.scene)
      -- Extract model nodes from state
      getModelNodes
      -- Extract model links from state
      getModelLinks
      -- Enhance attributes with tags and click handler
      (\attrs st -> attrs {
          tagMap = Just st.tags  -- Pass tags for automatic CSS class propagation
        , nodeClick = case st.eventListener of
            Just listener -> Just \node ->
              HS.notify listener (EventFromVizualization (NodeClick node.nodetype node.id))
            Nothing -> Nothing
        }
      )
      -- Visualization-specific updateSimulation
      Graph.updateSimulation
