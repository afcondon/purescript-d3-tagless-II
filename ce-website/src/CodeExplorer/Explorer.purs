-- | Code Explorer using Scene Engine
-- |
-- | Clean implementation using the compositional scene architecture.
-- | Each scene is a self-contained config; transitions are handled by the Engine.
module CodeExplorer.Explorer
  ( initExplorer
  , initExplorerWithCallbacks
  , goToScene
  , SceneId(..) -- Export ADT and constructors for type-safe scene selection
  , reloadWithProject
  , setViewState -- Public API for changing view state
  , executeRadialTreeWaypoint -- Waypoint for Force view (Halogen calls this)
  , setNeighborhoodViewType -- Switch between Bubbles/Chord/Matrix in neighborhood view
  , navigateBack
  , restoreFromFocus -- Restore full view from Halogen-owned focus state
  , navigateToModuleByName -- Navigate to module neighborhood by name (for search)
  , getModuleNames -- Get all module names for search
  , getOriginView -- Get the origin view for back navigation
  , updateNodeColors
    -- Re-exports from State
  , module State
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, minimum, maximum)
import Data.Int (toNumber)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable as Nullable
import Data.Traversable (traverse, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import Data.Loader (loadModel, loadModelForProject, LoadedModel, DeclarationsMap, FunctionCallsMap, fetchBatchDeclarations, fetchBatchFunctionCalls)
import CodeExplorer.State as State
import CodeExplorer.BubblePack (renderModulePackWithCallbacks, highlightCallGraph, clearCallGraphHighlight, drawFunctionEdges, clearFunctionEdges, drawModuleEdges, highlightModuleCallGraph, ModuleEdge, DeclarationClickCallback, DeclarationHoverCallback, clearBubblePacks)
import CodeExplorer.ChordDiagram (renderNeighborhoodChord, clearChordDiagram)
import CodeExplorer.AdjacencyMatrix (renderNeighborhoodMatrix, clearAdjacencyMatrix)
import CodeExplorer.TriptychView (renderTriptychWithDeclarations, clearTriptych)
-- CallGraphPopup is now a Halogen component (Component.CallGraphPopup)
-- NarrativePanel is now a Halogen component (Component.NarrativePanel)
-- It polls State.globalViewStateRef and State.globalModelInfoRef directly
import CodeExplorer.ViewState (ViewState(..), OverviewView(..), DetailView(..), NeighborhoodViewType(..), getNeighborhoodModule, neighborhoodViewLabel)
import CodeExplorer.ViewTransition as VT
import Data.ColorPalette (getNodeStroke, getNodeFill) as ColorPalette
import CodeExplorer.Treemap (recalculateTreemapPositions, renderWatermark, clearWatermark)
import PSD3v2.Tooltip (hideTooltip) as Tooltip
import CodeExplorer.Scene as Scene
import CodeExplorer.Scenes as Scenes
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Links (swizzleLinks)
import PSD3.ForceEngine.Render (GroupId(..), updateGroupPositions, updateLinkPositions)
import PSD3.ForceEngine.Simulation as Sim
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom, onMouseEnter, onMouseLeave, onClickWithDatum)
import PSD3v2.Capabilities.Selection (select, selectAll, on, renderTree)
import PSD3v2.VizTree.Tree as T
import PSD3v2.Interpreter.D3v2 (getElementsD3v2) as D3v2
import PSD3v2.Classify (classifyElements, clearClasses)
import Data.Set as Set
import PSD3v2.Tooltip (onTooltip)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SEmpty)
import Types (SimNode, SimLink, NodeType(..), LinkType, isTreeLink)
import CodeExplorer.ViewBox as ViewBox
import Viz.CodeExplorer.TreeLinks (verticalLinkPath)
import Web.DOM.Element (Element, classList, toNode)
import Web.DOM.Node as Node
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)
import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Scene ID (Type-Safe Scene Selection)
-- =============================================================================

-- | Type-safe scene identifiers
-- | Replaces string-based scene selection for compile-time safety
data SceneId
  = TreemapForm -- ^ Static treemap/grid layout
  | TreeForm -- ^ Static tree layout (bezier links)
  | RadialTreeForm -- ^ Static radial tree layout (waypoint before Force)
  | TreeRun -- ^ Force-directed tree (physics enabled)
  | TopoForm -- ^ Package DAG with topological positions

derive instance eqSceneId :: Eq SceneId

-- | Get scene configuration for a SceneId
sceneConfigFor :: SceneId -> Scene.SceneConfig SimNode
sceneConfigFor TreemapForm = Scenes.treemapFormScene
sceneConfigFor TreeForm = Scenes.treeFormScene
sceneConfigFor RadialTreeForm = Scenes.radialTreeFormScene
sceneConfigFor TreeRun = Scenes.treeRunScene
sceneConfigFor TopoForm = Scenes.topoFormScene

-- =============================================================================
-- Public API: setViewState
-- =============================================================================

-- | Is this view module-centric (shows modules)?
isModuleCentric :: OverviewView -> Boolean
isModuleCentric TreeView = true
isModuleCentric ForceView = true
isModuleCentric _ = false

-- | Is this view package-centric (Topo)?
isPackageCentric :: OverviewView -> Boolean
isPackageCentric TopoView = true
isPackageCentric _ = false

-- | Get the overview from a ViewState, if any
getOverview :: ViewState -> Maybe OverviewView
getOverview (Overview ov) = Just ov
getOverview (Detail _) = Nothing

-- | Set the current view state and trigger appropriate scene transitions.
-- | This is the public API for changing views - Halogen calls this.
-- |
-- | Waypoint logic has been moved to Halogen - this function does direct transitions only.
-- | Halogen is responsible for:
-- | - Detecting when waypoints are needed (e.g., Tree→Topo needs Treemap first)
-- | - Storing the pending target view
-- | - Calling setViewState again on transition completion
-- |
-- | This function handles:
-- | - Updating the internal view state ref (will be removed once Halogen owns all state)
-- | - Updating node colors to match the new view
-- | - Triggering scene transitions (TreemapForm/TreeForm/TreeRun/TopoForm)
-- | - Notifying Halogen via callback
setViewState :: ViewState -> Effect Unit
setViewState newView = do
  -- Get current view from SceneState for logging
  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Just stateRef -> do
      currentView <- Scene.getViewState stateRef
      log $ "[Explorer] setViewState: " <> showViewState currentView <> " -> " <> showViewState newView
    Nothing -> log $ "[Explorer] setViewState: (no state) -> " <> showViewState newView
  executeViewChange newView

-- | Execute radial tree waypoint (internal scene, no ViewState change)
-- | This is used as an intermediate step before transitioning to ForceView
-- | Called by Halogen when it determines a waypoint is needed
executeRadialTreeWaypoint :: Effect Unit
executeRadialTreeWaypoint = do
  log "[Explorer] executeRadialTreeWaypoint: transitioning to radial tree positions"

  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Just stateRef -> do
      -- Clear any existing links and set up for radial view
      clearTreeLinks
      setTreeSceneClass true -- Fade packages/non-tree like TreeView

      -- Trigger the radial tree scene transition
      goToScene RadialTreeForm stateRef

      -- Reheat simulation to drive the transition
      sceneState <- Ref.read stateRef
      Sim.reheat sceneState.simulation
    Nothing -> pure unit

-- | Execute the actual view change (called directly or after waypoint)
executeViewChange :: ViewState -> Effect Unit
executeViewChange newView = do
  log $ "[Explorer] executeViewChange: " <> showViewState newView

  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Just stateRef -> do
      -- Update ViewState and TransitionState in SceneState
      -- (setSceneViewState computes the new transition internally)
      Scene.setSceneViewState newView stateRef

      -- Log transition info for debugging
      newTransition <- Scene.getTransitionState stateRef
      log $ "[Explorer] Transition computed - entering: "
        <> show (Map.size newTransition.enteringProgress)
        <> ", exiting: "
        <> show (Array.length newTransition.exitingNodes)

      -- Update node colors
      updateNodeColors newView

      -- Trigger scene transition if needed
      case newView of
        Overview TreemapView -> goToScene TreemapForm stateRef
        Overview TreeView -> goToScene TreeForm stateRef
        Overview ForceView -> goToScene TreeRun stateRef
        Overview TopoView -> goToScene TopoForm stateRef
        Detail _ -> pure unit -- Detail views handled separately

      -- Always reheat simulation on view change to ensure ticks keep running
      -- This is needed when returning from Static scenes (Tree, Treemap)
      sceneState <- Ref.read stateRef
      Sim.reheat sceneState.simulation
    Nothing -> pure unit

  -- Notify Halogen via callback
  State.notifyViewStateChanged newView

-- | Tick handler wrapper that notifies Halogen on transition completion
-- | This wraps the normal Scene tick and, when a transition completes,
-- | calls the onTransitionComplete callback so Halogen can handle waypoints.
tickWithTransitionCallback :: Ref Scene.SceneState -> String -> Effect Unit
tickWithTransitionCallback stateRef nodesSelector = do
  -- Get state before tick to detect transition completion
  wasTransitioning <- Scene.isTransitioning stateRef

  -- Run the normal tick (ViewState and TransitionState now stored in SceneState)
  Scene.onTickWithViewTransition stateRef nodesSelector

  -- Update package label positions if any exist (for TopoForm transition)
  VT.updatePackageLabelPositions

  -- Check if transition just completed
  stillTransitioning <- Scene.isTransitioning stateRef

  -- If transition just completed, notify Halogen via callback
  -- Halogen handles waypoint logic (checking for pending target view)
  when (wasTransitioning && not stillTransitioning) do
    State.notifyTransitionComplete

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the explorer
initExplorer :: String -> Effect Unit
initExplorer containerSelector = do
  -- Note: ViewState is initialized inside initWithModel when SceneState is created
  let initialView = Overview TreemapView
  log $ "[Explorer] Initializing with view: " <> showViewState initialView

  -- Then load data asynchronously
  launchAff_ do
    log "[Explorer] BUILD: 2025-12-03 - Immediate TangleJS init"
    log "[Explorer] Loading data..."
    result <- loadModel
    case result of
      Left err -> liftEffect $ log $ "[Explorer] Error: " <> err
      Right model -> do
        liftEffect $ log $ "[Explorer] Loaded: " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
        -- Store links and declarations for later use
        liftEffect $ Ref.write model.links State.globalLinksRef
        liftEffect $ Ref.write model.declarations State.globalDeclarationsRef

        -- Initialize visualization immediately
        -- Function calls are loaded on-demand when drilling into neighborhoods
        stateRef <- liftEffect $ initWithModel model containerSelector
        liftEffect $ Ref.write (Just stateRef) State.globalStateRef
        liftEffect $ log "[Explorer] Ready (function calls loaded on-demand per neighborhood)"

-- | Initialize the explorer with callbacks
-- | This is the preferred API - callbacks notify the Halogen component of state changes
-- | instead of requiring polling.
initExplorerWithCallbacks :: String -> State.ExplorerCallbacks -> Effect Unit
initExplorerWithCallbacks containerSelector callbacks = do
  -- Store callbacks for use by other Explorer functions
  Ref.write (Just callbacks) State.globalCallbacksRef

  -- Note: ViewState is initialized inside initWithModel when SceneState is created
  -- Notify Halogen of initial view now so it can display immediately
  let initialView = Overview TreemapView
  callbacks.onViewStateChanged initialView
  log "[Explorer] Initializing with callback notification"

  -- Then load data asynchronously
  launchAff_ do
    log "[Explorer] BUILD: 2025-12-05 - Callback-based initialization"
    log "[Explorer] Loading data..."
    result <- loadModel
    case result of
      Left err -> liftEffect $ log $ "[Explorer] Error: " <> err
      Right model -> do
        liftEffect $ log $ "[Explorer] Loaded: " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
        -- Store links and declarations for later use
        liftEffect $ Ref.write model.links State.globalLinksRef
        liftEffect $ Ref.write model.declarations State.globalDeclarationsRef

        -- Initialize visualization immediately
        -- Function calls are loaded on-demand when drilling into neighborhoods
        stateRef <- liftEffect $ initWithModel model containerSelector
        liftEffect $ Ref.write (Just stateRef) State.globalStateRef

        -- Notify model loaded via callback
        let modelInfo = { projectName: "purescript-d3-dataviz", moduleCount: model.moduleCount, packageCount: model.packageCount }
        liftEffect $ callbacks.onModelLoaded modelInfo

        liftEffect $ log "[Explorer] Ready with callback notifications"

-- | Initialize with loaded model
initWithModel :: LoadedModel -> String -> Effect (Ref Scene.SceneState)
initWithModel model containerSelector = do
  log "[Explorer] Initializing with new Scene Engine"

  -- Store model info for narrative
  Ref.write { projectName: "purescript-d3-dataviz", moduleCount: model.moduleCount, packageCount: model.packageCount } State.globalModelInfoRef

  -- Color palette is now generated by Halogen NarrativePanel based on packageCount

  -- Calculate treemap-based positions (replaces grid layout)
  let treemapNodes = recalculateTreemapPositions model.nodes

  -- Randomize module starting positions
  randomizedNodes <- randomizeModulePositions treemapNodes
  let nodesWithFix = map fixPackagePosition randomizedNodes

  -- Create simulation with default config (faster cooling for Treemap)
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodesWithFix sim

  -- Add forces
  addGridForces nodesWithFix sim

  -- Get nodes from simulation for initial render
  simNodes <- Sim.getNodes sim

  -- Initial view is Treemap
  let initialView = Overview TreemapView

  -- Render SVG structure first
  _ <- runD3v2M $ renderSVG initialView containerSelector simNodes

  -- Render treemap watermark (behind nodes)
  renderWatermark model.nodes

  -- Create scene state (includes ViewState and TransitionState)
  sceneState <- Scene.mkSceneState sim State.nodesGroupId initialView simNodes
  stateRef <- Ref.new sceneState

  -- Set up tick callback - includes view transition progress advancement and visual updates
  let nodesSelector = "#explorer-nodes" -- Selector for the nodes group
  -- Wrap the scene tick to notify Halogen on transition completion
  Sim.onTick (tickWithTransitionCallback stateRef nodesSelector) sim

  -- Start simulation - let forces do the initial clustering animation
  -- (no transition on startup - nodes start randomized, forces pull them in)
  Sim.start sim

  -- Mark TreeForm as current scene (we start with treemap-based positions)
  Scene.setCurrentScene Scenes.treeFormScene stateRef

  pure stateRef

-- =============================================================================
-- Public API
-- =============================================================================

-- | Reload the explorer with a different project's data
-- | Clears existing visualization and loads new project data
reloadWithProject :: Int -> Effect Unit
reloadWithProject projectId = do
  log $ "[Explorer] Reloading with project: " <> show projectId

  -- Clear existing state
  Ref.write [] State.globalLinksRef
  Ref.write Object.empty State.globalDeclarationsRef
  Ref.write Object.empty State.globalFunctionCallsRef
  Ref.write [] State.globalNavigationStackRef
  Ref.write { focusedNodeId: Nothing, fullNodes: [], originView: Nothing } State.globalFocusRef

  -- Clear tree links if any
  clearTreeLinks
  setTreeSceneClass false

  -- Clear existing DOM elements
  clearNodesGroup

  -- Load new project data asynchronously
  launchAff_ do
    log $ "[Explorer] Loading data for project " <> show projectId <> "..."
    result <- loadModelForProject projectId
    case result of
      Left err -> liftEffect $ log $ "[Explorer] Error loading project: " <> err
      Right model -> do
        liftEffect $ log $ "[Explorer] Loaded: " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
        -- Store new links and declarations
        liftEffect $ Ref.write model.links State.globalLinksRef
        liftEffect $ Ref.write model.declarations State.globalDeclarationsRef

        -- Get existing stateRef and update with new data
        mStateRef <- liftEffect $ Ref.read State.globalStateRef
        case mStateRef of
          Nothing -> do
            -- Initialize fresh if no existing state
            stateRef <- liftEffect $ initWithModel model "#viz"
            liftEffect $ Ref.write (Just stateRef) State.globalStateRef
          Just stateRef -> do
            -- Update existing state with new model
            liftEffect $ updateWithModel model stateRef

        liftEffect $ log "[Explorer] Project switch complete"

-- | Update existing state with a new model (for project switching)
updateWithModel :: LoadedModel -> Ref Scene.SceneState -> Effect Unit
updateWithModel model stateRef = do
  log "[Explorer] Updating visualization with new model"

  -- Update model info
  Ref.write { projectName: "project", moduleCount: model.moduleCount, packageCount: model.packageCount } State.globalModelInfoRef

  -- Calculate treemap-based positions (replaces grid layout)
  let treemapNodes = recalculateTreemapPositions model.nodes

  -- Randomize module starting positions
  randomizedNodes <- randomizeModulePositions treemapNodes
  let nodesWithFix = map fixPackagePosition randomizedNodes

  -- Get current state
  state <- Ref.read stateRef

  -- Update simulation with new nodes
  Sim.setNodes nodesWithFix state.simulation

  -- Reapply grid forces
  addGridForces nodesWithFix state.simulation

  -- Get updated nodes
  simNodes <- Sim.getNodes state.simulation

  -- Re-render nodes using existing renderNodesOnly
  currentView <- Ref.read State.globalViewStateRef
  _ <- runD3v2M $ renderNodesOnly currentView simNodes

  -- Update watermark for new project
  clearWatermark
  renderWatermark model.nodes

  -- Reset to TreeForm scene
  Scene.setCurrentScene Scenes.treeFormScene stateRef

  -- Restart simulation
  Sim.start state.simulation

-- | Go to a scene using type-safe scene ID
goToScene :: SceneId -> Ref Scene.SceneState -> Effect Unit
goToScene sceneId stateRef = do
  let scene = sceneConfigFor sceneId

  -- Handle TreemapForm: clear links, restore grid forces, remove CSS class
  when (sceneId == TreemapForm) do
    state <- Ref.read stateRef
    nodes <- Sim.getNodes state.simulation
    clearTreeLinks -- Remove any tree/force links
    restoreGridForces nodes state.simulation -- Restore grid forces
    Scene.clearLinksGroupId stateRef -- Disable link updates
    Scene.clearTreeLinksGroupId stateRef -- Disable tree link updates
    setTreeSceneClass false -- Show all nodes normally
    VT.clearPackageLabels -- Remove TopoGraph package labels if present

  -- Handle TreeForm: render tree bezier links
  when (sceneId == TreeForm) do
    state <- Ref.read stateRef
    nodes <- Sim.getNodes state.simulation
    clearTreeLinks -- Clear any existing links
    renderTreeLinks nodes
    Scene.setTreeLinksGroupId State.treeLinksGroupId stateRef -- Enable tree link updates during transition
    setTreeSceneClass true
    VT.clearPackageLabels -- Remove TopoGraph package labels if present

  -- Handle RadialTreeForm: waypoint to radial positions (no links)
  -- Used as intermediate step before Force view
  when (sceneId == RadialTreeForm) do
    clearTreeLinks -- Remove any existing links
    Scene.clearLinksGroupId stateRef -- Disable link updates
    Scene.clearTreeLinksGroupId stateRef -- Disable tree link updates
    setTreeSceneClass true -- Keep packages/non-tree faded
    VT.clearPackageLabels -- Remove TopoGraph package labels if present

  -- Handle TreeRun: force-directed tree with link forces
  when (sceneId == TreeRun) do
    state <- Ref.read stateRef
    nodes <- Sim.getNodes state.simulation
    links <- Ref.read State.globalLinksRef
    clearTreeLinks -- Remove any existing links
    addTreeForces nodes links state.simulation
    renderForceLinks nodes links -- Render straight line links
    Scene.setLinksGroupId State.forceLinksGroupId stateRef -- Enable link updates
    Scene.clearTreeLinksGroupId stateRef -- Disable tree link updates (using force links now)
    setTreeSceneClass true -- Keep packages/non-tree faded
    VT.clearPackageLabels -- Remove TopoGraph package labels if present

  -- Handle TopoForm: package DAG with topological positions
  -- TODO: Could add package dependency links here in future
  when (sceneId == TopoForm) do
    state <- Ref.read stateRef
    nodes <- Sim.getNodes state.simulation
    let packages = Array.filter (\n -> n.nodeType == PackageNode) nodes
    clearTreeLinks -- Remove any tree/force links
    Scene.clearLinksGroupId stateRef -- Disable link updates
    Scene.clearTreeLinksGroupId stateRef -- Disable tree link updates
    setTreeSceneClass false -- Show all nodes (packages prominent)
    VT.renderPackageLabels packages -- Add package name labels

  Scene.transitionTo scene stateRef

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Fix package positions (pin at grid)
fixPackagePosition :: SimNode -> SimNode
fixPackagePosition node = case node.nodeType of
  PackageNode -> node { fx = Nullable.notNull node.gridX, fy = Nullable.notNull node.gridY }
  ModuleNode -> node

-- | Randomize module starting positions (near their package)
randomizeModulePositions :: Array SimNode -> Effect (Array SimNode)
randomizeModulePositions nodes = traverse randomizeIfModule nodes
  where
  packagePositions = Object.fromFoldable $ Array.mapMaybe getPackagePos nodes

  getPackagePos n = case n.nodeType of
    PackageNode -> Just (Tuple (show n.id) { x: n.gridX, y: n.gridY })
    ModuleNode -> Nothing

  randomizeIfModule n = case n.nodeType of
    PackageNode -> pure n
    ModuleNode -> do
      rx <- random
      ry <- random
      let
        { x: pkgX, y: pkgY } = case Object.lookup (show n.cluster) packagePositions of
          Just pos -> pos
          Nothing -> { x: 0.0, y: 0.0 }
      let jitterX = (rx - 0.5) * 100.0
      let jitterY = (ry - 0.5) * 100.0
      pure $ n { x = pkgX + jitterX, y = pkgY + jitterY }

-- | Add Grid forces to simulation
addGridForces :: Array SimNode -> Scene.CESimulation -> Effect Unit
addGridForces nodes sim = do
  let collideHandle = Core.createCollideGrid 5.0 0.7 1
  _ <- Core.initializeForce collideHandle nodes
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  let forceXHandle = Core.createForceXGrid 0.5
  _ <- Core.initializeForce forceXHandle nodes
  Ref.modify_ (Map.insert "gridX" forceXHandle) sim.forces

  let forceYHandle = Core.createForceYGrid 0.5
  _ <- Core.initializeForce forceYHandle nodes
  Ref.modify_ (Map.insert "gridY" forceYHandle) sim.forces

  log "[Explorer] Grid forces added"

-- | Add Tree forces to simulation (for force-directed tree)
-- | Based on Observable's force-directed tree: https://observablehq.com/@d3/force-directed-tree
addTreeForces :: Array SimNode -> Array SimLink -> Scene.CESimulation -> Effect Unit
addTreeForces nodes links sim = do
  -- Filter to tree links only
  let treeLinks = Array.filter isTreeLink links
  log $ "[Explorer] Setting up force-directed tree with " <> show (Array.length treeLinks) <> " links"

  -- Clear existing forces
  Ref.write Map.empty sim.forces

  -- Link force - looser binding for more radial spread
  let linkHandle = Core.createLink { distance: 30.0, strength: 0.7, iterations: 1 }
  _ <- Core.initializeLinkForce linkHandle nodes treeLinks
  Ref.modify_ (Map.insert "link" linkHandle) sim.forces

  -- Many-body (charge) force - stronger repulsion for better spacing
  let manyBodyHandle = Core.createManyBody { strength: -100.0, theta: 0.9, distanceMin: 1.0, distanceMax: 1.0e10 }
  _ <- Core.initializeForce manyBodyHandle nodes
  Ref.modify_ (Map.insert "charge" manyBodyHandle) sim.forces

  -- X/Y positioning forces (like Observable's forceX/forceY for centering)
  let forceXHandle = Core.createForceX { x: 0.0, strength: 0.1 }
  _ <- Core.initializeForce forceXHandle nodes
  Ref.modify_ (Map.insert "x" forceXHandle) sim.forces

  let forceYHandle = Core.createForceY { y: 0.0, strength: 0.1 }
  _ <- Core.initializeForce forceYHandle nodes
  Ref.modify_ (Map.insert "y" forceYHandle) sim.forces

  log "[Explorer] Tree forces added (link, charge, x, y)"

-- | Restore Grid forces (when leaving tree scene)
restoreGridForces :: Array SimNode -> Scene.CESimulation -> Effect Unit
restoreGridForces nodes sim = do
  log "[Explorer] Restoring grid forces"
  -- Clear existing forces
  Ref.write Map.empty sim.forces
  -- Add grid forces back
  addGridForces nodes sim

-- | Add Orbit forces - free-floating with radial centering and repulsion
-- | Modules cluster by package via many-body but no grid constraints
addOrbitForces :: Array SimNode -> Scene.CESimulation -> Effect Unit
addOrbitForces nodes sim = do
  log "[Explorer] Setting up orbit forces"
  -- Clear existing forces
  Ref.write Map.empty sim.forces

  -- Collision detection
  let collideHandle = Core.createCollide { radius: 8.0, strength: 0.7, iterations: 1 }
  _ <- Core.initializeForce collideHandle nodes
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  -- Many-body repulsion - keeps nodes spread out
  let manyBodyHandle = Core.createManyBody { strength: -80.0, theta: 0.9, distanceMin: 1.0, distanceMax: 1.0e10 }
  _ <- Core.initializeForce manyBodyHandle nodes
  Ref.modify_ (Map.insert "charge" manyBodyHandle) sim.forces

  -- Gentle centering force - keeps the cloud centered
  let forceXHandle = Core.createForceX { x: 0.0, strength: 0.03 }
  _ <- Core.initializeForce forceXHandle nodes
  Ref.modify_ (Map.insert "x" forceXHandle) sim.forces

  let forceYHandle = Core.createForceY { y: 0.0, strength: 0.03 }
  _ <- Core.initializeForce forceYHandle nodes
  Ref.modify_ (Map.insert "y" forceYHandle) sim.forces

  log "[Explorer] Orbit forces added (collide, charge, x, y)"

-- =============================================================================
-- Tooltip Formatting
-- =============================================================================

-- | Format tooltip HTML for a node
formatNodeTooltip :: SimNode -> String
formatNodeTooltip node =
  "<div class=\"tooltip-header\">" <> node.name <> "</div>"
    <> "<div class=\"tooltip-package\">"
    <> node.package
    <> "</div>"
    <> "<div class=\"tooltip-metrics\">"
    <> metric "Type" (nodeTypeLabel node.nodeType)
    <> metric "Dependencies" (show (Array.length node.targets))
    <> metric "Dependents" (show (Array.length node.sources))
    <>
      "</div>"
  where
  metric label value =
    "<div class=\"tooltip-metric\">"
      <> "<span class=\"metric-label\">"
      <> label
      <> "</span>"
      <> "<span class=\"metric-value\">"
      <> value
      <> "</span>"
      <>
        "</div>"

  nodeTypeLabel :: NodeType -> String
  nodeTypeLabel PackageNode = "Package"
  nodeTypeLabel ModuleNode = "Module"

-- =============================================================================
-- Rendering
-- =============================================================================

renderSVG :: ViewState -> String -> Array SimNode -> D3v2M { nodeSel :: D3v2Selection_ SBoundOwns Element SimNode }
renderSVG currentView containerSelector nodes = do
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Build the structure tree
  let structureTree :: T.Tree Unit
      structureTree =
        T.named SVG "explorer-svg"
          [ v3AttrStr "viewBox" (str (show ((-ViewBox.viewBoxWidth) / 2.0) <> " " <> show ((-ViewBox.viewBoxHeight) / 2.0) <> " " <> show ViewBox.viewBoxWidth <> " " <> show ViewBox.viewBoxHeight))
          , v3AttrStr "id" (str "explorer-svg")
          , v3AttrStr "class" (str "ce-viz")
          ]
          `T.withChildren`
            [ T.named Group "explorer-zoom-group"
                [ v3AttrStr "id" (str "explorer-zoom-group") ]
                `T.withChildren`
                  [ T.named Group "treemap-watermark" [ v3AttrStr "id" (str "treemap-watermark"), v3AttrStr "class" (str "watermark") ]
                  , T.named Group "explorer-links" [ v3AttrStr "id" (str "explorer-links") ]
                  , T.named Group "explorer-nodes" [ v3AttrStr "id" (str "explorer-nodes") ]
                  ]
            ]

  -- Render the structure
  selections <- renderTree container structureTree

  -- Reselect groups for data binding and behaviors
  svg <- liftEffect $ reselectD3v2 "explorer-svg" selections
  nodesGroup <- liftEffect $ reselectD3v2 "explorer-nodes" selections

  -- Render nodes with data binding
  let nodesTree :: T.Tree SimNode
      nodesTree =
        T.joinData "nodes-data" "circle" nodes $ \node ->
          T.elem Circle
            [ v3Attr "cx" (lit node.x)
            , v3Attr "cy" (lit node.y)
            , v3Attr "r" (lit node.r)
            , v3AttrStr "fill" (str (ColorPalette.getNodeFill currentView node))
            , v3AttrStr "stroke" (str (ColorPalette.getNodeStroke currentView node))
            , v3Attr "stroke-width" (lit 1.0) -- Slightly thicker stroke for visibility
            , v3AttrStr "class" (str (nodeClass node)) -- CSS class for type-based styling/transitions
            ]

  nodeSelections <- renderTree nodesGroup nodesTree
  -- Get the nodes selection from the map (already has data bound as SBoundOwns)
  case Map.lookup "nodes-data" nodeSelections of
    Nothing -> pure { nodeSel: unsafeCoerce unit } -- Should never happen
    Just nodeSel -> do
      -- Add zoom behavior
      _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#explorer-zoom-group") svg

      -- Add highlight on hover (mouse enter highlights connected nodes)
      let highlightClasses = [ "highlighted-source", "highlighted-upstream", "highlighted-downstream", "dimmed" ]
      _ <- on
        ( onMouseEnter \node -> do
            -- Clear previous highlight classes first
            clearClasses "#explorer-nodes" "circle" highlightClasses
            -- Then apply new classification
            let targetSet = Set.fromFoldable node.targets
            let sourceSet = Set.fromFoldable node.sources
            classifyElements "#explorer-nodes" "circle" \n ->
              if n.id == node.id then "highlighted-source"
              else if Set.member n.id targetSet then "highlighted-upstream"
              else if Set.member n.id sourceSet then "highlighted-downstream"
              else "dimmed"
        )
        nodeSel

      -- Add combined mouse leave handler (clear highlight AND hide tooltip)
      -- Note: D3 replaces event handlers, so we must combine both actions into one handler
      _ <- on
        ( onMouseLeave \_ -> do
            clearClasses "#explorer-nodes" "circle" highlightClasses
            Tooltip.hideTooltip
        )
        nodeSel

      -- Add tooltip on hover
      _ <- on (onTooltip formatNodeTooltip) nodeSel

      -- Add click to focus on neighborhood
      _ <- on (onClickWithDatum toggleFocus) nodeSel

      pure { nodeSel }

-- | Update node colors based on current ViewState
-- | This should be called whenever transitioning between views
-- | Uses the library's principled approach: re-render nodes with new color functions
updateNodeColors :: ViewState -> Effect Unit
updateNodeColors viewState = do
  -- Get current nodes from simulation
  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Just stateRef -> do
      sceneState <- Ref.read stateRef
      nodes <- Sim.getNodes sceneState.simulation
      -- Clear and re-render nodes with current ViewState's colors
      clearNodesGroup
      void $ runD3v2M $ renderNodesOnly viewState nodes
    Nothing -> log "[Explorer] No simulation state available for color update"

-- | CSS class based on node type for styling and transitions
nodeClass :: SimNode -> String
nodeClass n = case n.nodeType of
  PackageNode -> "node package-node"
  ModuleNode
    | n.isInTree -> "node tree-module"
    | otherwise -> "node non-tree-module"

-- =============================================================================
-- Link Interconnectivity Scoring
-- =============================================================================

-- | Calculate interconnectivity score for a link based on node degrees
-- | Returns a normalized score between 0.0 and 1.0
linkInterconnectivity :: SimNode -> SimNode -> Number
linkInterconnectivity sourceNode targetNode =
  let
    -- Total connections for each node
    sourceDegree = Array.length sourceNode.targets + Array.length sourceNode.sources
    targetDegree = Array.length targetNode.targets + Array.length targetNode.sources

    -- Combined degree (how "important" are both ends of this connection)
    combinedDegree = toNumber (sourceDegree + targetDegree)

    -- Normalize to 0-1 range (assuming max degree around 50 for typical modules)
    maxExpectedDegree = 100.0
    normalized = min 1.0 (combinedDegree / maxExpectedDegree)
  in
    normalized

-- | Log connectivity statistics for a set of links
logConnectivityStats :: String -> Array Number -> Effect Unit
logConnectivityStats label scores =
  case Array.uncons scores of
    Nothing -> log $ "[" <> label <> "] No connectivity scores"
    Just _ -> do
      let sortedScores = Array.sort scores
      let count = Array.length scores
      let minScore = fromMaybe 0.0 (Array.head sortedScores)
      let maxScore = fromMaybe 0.0 (Array.last sortedScores)
      let avgScore = (foldl (+) 0.0 scores) / toNumber count
      let medianScore = fromMaybe 0.0 (Array.index sortedScores (count / 2))
      log $ "[" <> label <> "] Connectivity scores - count: " <> show count
        <> ", min: "
        <> show minScore
        <> ", max: "
        <> show maxScore
        <> ", avg: "
        <> show avgScore
        <> ", median: "
        <> show medianScore

-- | Map interconnectivity score to stroke width
-- | Returns width between 0.5 (weak connection) and 3.5 (strong connection)
linkStrokeWidth :: Number -> Number
linkStrokeWidth score = 0.5 + (score * 3.0)

-- | Map interconnectivity score to color
-- | Use white for now (color channel reserved for future use)
linkStrokeColor :: Number -> String
linkStrokeColor _score = "white"

-- | Map interconnectivity score to opacity
-- | More interconnected links are more opaque (visible)
linkOpacity :: Number -> Number
linkOpacity score = 0.3 + (score * 0.5) -- Range: 0.3 to 0.8

-- =============================================================================
-- Tree Link Rendering
-- =============================================================================

-- | Render tree links as vertical bezier paths (root at top)
renderTreeLinks :: Array SimNode -> Effect Unit
renderTreeLinks nodes = do
  links <- Ref.read State.globalLinksRef
  let treeLinks = Array.filter isTreeLink links
  log $ "[Explorer] Rendering " <> show (Array.length treeLinks) <> " tree links"

  -- Build node map for position lookup
  let nodeMap = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes

  -- Render links via D3
  _ <- runD3v2M $ renderTreeLinksD3 nodeMap treeLinks
  pure unit

-- | Render tree links using D3 selection
renderTreeLinksD3 :: Map.Map Int SimNode -> Array SimLink -> D3v2M Unit
renderTreeLinksD3 nodeMap links = do
  linksGroup <- select "#explorer-links" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Log connectivity statistics for debugging
  let scores = Array.mapMaybe (computeLinkScore nodeMap) links
  liftEffect $ logConnectivityStats "Tree links" scores

  -- Build tree of path elements for each tree link with dynamic attributes
  let linksTree :: T.Tree SimLink
      linksTree =
        T.joinData "tree-links-data" "path" links $ \link ->
          let
            d = linkPathFn nodeMap link
            strokeColor = linkStrokeFn nodeMap link
            strokeW = linkWidthFn nodeMap link
            opac = linkOpacityFn nodeMap link
          in
            T.elem Path
              [ v3AttrStr "d" (str d)
              , v3AttrStr "fill" (str "none")
              , v3AttrStr "stroke" (str strokeColor)
              , v3Attr "stroke-width" (lit strokeW)
              , v3Attr "opacity" (lit opac)
              , v3AttrStr "class" (str "tree-link")
              ]

  _ <- renderTree linksGroup linksTree
  pure unit
  where
  computeLinkScore :: Map.Map Int SimNode -> SimLink -> Maybe Number
  computeLinkScore nm link =
    case Map.lookup link.source nm, Map.lookup link.target nm of
      Just srcNode, Just tgtNode -> Just (linkInterconnectivity srcNode tgtNode)
      _, _ -> Nothing

  -- Compute stroke color based on interconnectivity
  linkStrokeFn :: Map.Map Int SimNode -> SimLink -> String
  linkStrokeFn nm link =
    case Map.lookup link.source nm, Map.lookup link.target nm of
      Just srcNode, Just tgtNode ->
        let
          score = linkInterconnectivity srcNode tgtNode
        in
          linkStrokeColor score
      _, _ -> "white" -- Fallback

  -- Compute stroke width based on interconnectivity
  linkWidthFn :: Map.Map Int SimNode -> SimLink -> Number
  linkWidthFn nm link =
    case Map.lookup link.source nm, Map.lookup link.target nm of
      Just srcNode, Just tgtNode ->
        let
          score = linkInterconnectivity srcNode tgtNode
        in
          linkStrokeWidth score
      _, _ -> 1.5 -- Fallback

  -- Compute opacity based on interconnectivity
  linkOpacityFn :: Map.Map Int SimNode -> SimLink -> Number
  linkOpacityFn nm link =
    case Map.lookup link.source nm, Map.lookup link.target nm of
      Just srcNode, Just tgtNode ->
        let
          score = linkInterconnectivity srcNode tgtNode
        in
          linkOpacity score
      _, _ -> 0.5 -- Fallback

-- | Generate path string for a link (vertical tree layout)
linkPathFn :: Map.Map Int SimNode -> SimLink -> String
linkPathFn nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      verticalLinkPath sourceNode.treeX sourceNode.treeY targetNode.treeX targetNode.treeY
    _, _ -> ""

-- =============================================================================
-- Force Link Rendering (for Tree)
-- =============================================================================

-- | Swizzled link type for D3 rendering
-- | source/target are node references (with x, y) instead of integer IDs
type SwizzledLink =
  { source :: SimNode
  , target :: SimNode
  , linkType :: LinkType
  }

-- | Render force links as straight lines
-- | These are updated on each tick by the Scene engine
renderForceLinks :: Array SimNode -> Array SimLink -> Effect Unit
renderForceLinks nodes links = do
  let treeLinks = Array.filter isTreeLink links
  log $ "[Explorer] Rendering " <> show (Array.length treeLinks) <> " force links"

  -- Swizzle links: convert integer IDs to node references
  let
    swizzled = swizzleLinks nodes treeLinks \src tgt _i link ->
      { source: src, target: tgt, linkType: link.linkType }

  -- Render via D3
  _ <- runD3v2M $ renderForceLinksD3 swizzled
  pure unit

-- | D3 rendering for force links
renderForceLinksD3 :: Array SwizzledLink -> D3v2M Unit
renderForceLinksD3 links = do
  linksGroup <- select "#explorer-links" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Build tree of line elements for each link with dynamic attributes
  let linksTree :: T.Tree SwizzledLink
      linksTree =
        T.joinData "force-links-data" "line" links $ \link ->
          let
            score = linkInterconnectivity link.source link.target
          in
            T.elem Line
              [ v3Attr "x1" (lit link.source.x)
              , v3Attr "y1" (lit link.source.y)
              , v3Attr "x2" (lit link.target.x)
              , v3Attr "y2" (lit link.target.y)
              , v3AttrStr "stroke" (str (linkStrokeColor score))
              , v3Attr "stroke-width" (lit (linkStrokeWidth score))
              , v3Attr "opacity" (lit (linkOpacity score))
              , v3AttrStr "class" (str "force-link")
              ]

  _ <- renderTree linksGroup linksTree
  pure unit

-- =============================================================================
-- CSS Scene Class Management
-- =============================================================================

-- | Clear tree links (remove bezier paths from DOM)
clearTreeLinks :: Effect Unit
clearTreeLinks = do
  win <- window
  doc <- document win
  let parentNode = toParentNode doc
  mElement <- querySelector (QuerySelector "#explorer-links") parentNode
  case mElement of
    Just linksGroup -> do
      -- Remove all children from links group
      clearElement linksGroup
      log "[Explorer] Cleared tree links"
    Nothing -> log "[Explorer] Could not find #explorer-links"

-- | Clear all children from an element using web-dom
-- | (Pure PureScript implementation, no FFI needed)
clearElement :: Element -> Effect Unit
clearElement element = clearNode (toNode element)
  where
  clearNode :: Node.Node -> Effect Unit
  clearNode node = do
    mChild <- Node.firstChild node
    case mChild of
      Just child -> do
        _ <- Node.removeChild child node
        clearNode node -- recurse until no more children
      Nothing -> pure unit

-- | Add or remove the tree-scene class from the nodes group to trigger CSS fade
setTreeSceneClass :: Boolean -> Effect Unit
setTreeSceneClass shouldAdd = do
  win <- window
  doc <- document win
  let parentNode = toParentNode doc
  mElement <- querySelector (QuerySelector "#explorer-nodes") parentNode
  case mElement of
    Just element -> do
      classes <- classList element
      if shouldAdd then DOMTokenList.add classes "tree-scene"
      else DOMTokenList.remove classes "tree-scene"
      log $ "[Explorer] Set tree-scene class: " <> show shouldAdd
    Nothing -> log "[Explorer] Could not find #explorer-nodes for CSS class"

-- =============================================================================
-- Neighborhood Focus
-- =============================================================================

-- | Show ViewState for logging
showViewState :: ViewState -> String
showViewState (Overview TreemapView) = "Overview(Treemap)"
showViewState (Overview TreeView) = "Overview(Tree)"
showViewState (Overview ForceView) = "Overview(Force)"
showViewState (Overview TopoView) = "Overview(Topo)"
showViewState (Detail (NeighborhoodDetail name _)) = "Detail(Neighborhood:" <> name <> ")"
showViewState (Detail (PackageNeighborhoodDetail name)) = "Detail(PackageNeighborhood:" <> name <> ")"
showViewState (Detail (FunctionCallsDetail name)) = "Detail(FunctionCalls:" <> name <> ")"

-- | Toggle focus on a node's neighborhood
-- | Click once to drill down to neighborhood, click again to restore full view
toggleFocus :: SimNode -> Effect Unit
toggleFocus clickedNode = do
  focus <- Ref.read State.globalFocusRef
  mStateRef <- Ref.read State.globalStateRef

  case mStateRef of
    Nothing -> log "[Explorer] No state ref available"
    Just stateRef -> do
      state <- Ref.read stateRef

      case focus.focusedNodeId of
        -- Currently focused on this node - go back via navigation
        Just fid | fid == clickedNode.id -> do
          log $ "[Explorer] Unfocusing from node " <> show clickedNode.id <> " (via navigateBack)"
          _ <- navigateBack
          pure unit

        -- Not focused, or focused on different node - focus on this neighborhood
        _ -> do
          -- Get current nodes before filtering
          currentNodes <- Sim.getNodes state.simulation

          -- Store full nodes if not already stored
          let nodesToStore = if Array.null focus.fullNodes then currentNodes else focus.fullNodes

          -- Build neighborhood based on node type
          case clickedNode.nodeType of
            PackageNode -> do
              -- Package click: show packages in adjacent topological layers
              -- This shows the clicked package's layer ±1 (three layers total, or fewer at edges)
              let packageName = clickedNode.name
              let clickedLayer = clickedNode.topoLayer

              -- Find all packages (to determine min/max layer)
              let allPackages = Array.filter (\n -> n.nodeType == PackageNode) nodesToStore
              let minLayer = fromMaybe 0 $ minimum (map _.topoLayer allPackages)
              let maxLayer = fromMaybe 0 $ maximum (map _.topoLayer allPackages)

              -- Build neighborhood: packages within ±1 layer of clicked package
              -- At edges (minLayer or maxLayer), only show 2 layers instead of 3
              let
                neighborhoodNodes = Array.filter
                  ( \n ->
                      n.nodeType == PackageNode
                        && n.topoLayer >= (clickedLayer - 1)
                        &&
                          n.topoLayer <= (clickedLayer + 1)
                  )
                  nodesToStore

              log $ "[Explorer] Focusing on package " <> packageName
                <> " (layer "
                <> show clickedLayer
                <> ", range "
                <> show minLayer
                <> "-"
                <> show maxLayer
                <> ", neighborhood: "
                <> show (Array.length neighborhoodNodes)
                <> " packages)"

              -- Record the origin view (must be done before view change)
              currentView <- Ref.read State.globalViewStateRef
              let
                origin = case currentView of
                  Overview ov -> Just ov
                  Detail _ -> focus.originView

              -- Notify Halogen of focus state change
              let focusInfo = { focusedNodeId: Just clickedNode.id, fullNodes: nodesToStore, originView: origin }
              State.notifyFocusChanged focusInfo
              -- Also update local ref (will be removed once Halogen fully owns this)
              Ref.write focusInfo State.globalFocusRef

              -- Notify Halogen to push current view to navigation stack
              State.notifyNavigationPush currentView
              -- Also update local ref (will be removed once Halogen fully owns this)
              Ref.modify_ (Array.cons currentView) State.globalNavigationStackRef

              -- Update ViewState for package neighborhood view
              let packageView = Detail (PackageNeighborhoodDetail packageName)
              Ref.write packageView State.globalViewStateRef
              State.notifyViewStateChanged packageView

              -- Update simulation and DOM with package-specific rendering
              focusOnPackageNeighborhood packageName neighborhoodNodes state.simulation

            -- EXPERIMENTAL: Module-level package view (commented out - too dense for large packages)
            -- This shows all modules in a package + their external module dependencies
            -- Could be useful with additional work (filtering, clustering, etc.)
            --
            -- let modulesInPackage = Array.filter (\n -> n.nodeType == ModuleNode && n.package == packageName) nodesToStore
            -- let moduleIds = Set.fromFoldable $ map _.id modulesInPackage
            -- let externalDepIds = Set.fromFoldable $ Array.concatMap (\m -> m.targets <> m.sources) modulesInPackage
            -- let neighborhoodIds = Set.union moduleIds externalDepIds
            -- let neighborhoodNodes = Array.filter (\n -> n.nodeType == ModuleNode && Set.member n.id neighborhoodIds) nodesToStore

            ModuleNode -> do
              -- Module click: show this module + its direct dependencies
              let
                neighborhoodIds = Set.fromFoldable $
                  [ clickedNode.id ] <> clickedNode.targets <> clickedNode.sources

              let neighborhoodNodes = Array.filter (\n -> Set.member n.id neighborhoodIds) currentNodes

              log $ "[Explorer] Focusing on node " <> show clickedNode.id
                <> " (neighborhood: "
                <> show (Array.length neighborhoodNodes)
                <> " nodes)"

              -- Record the origin view (must be done before view change)
              currentView <- Ref.read State.globalViewStateRef
              let
                origin = case currentView of
                  Overview ov -> Just ov
                  Detail _ -> focus.originView -- Keep existing origin if already in detail

              -- Notify Halogen of focus state change
              let focusInfo = { focusedNodeId: Just clickedNode.id, fullNodes: nodesToStore, originView: origin }
              State.notifyFocusChanged focusInfo
              -- Also update local ref (will be removed once Halogen fully owns this)
              Ref.write focusInfo State.globalFocusRef

              -- Notify Halogen to push current view to navigation stack
              State.notifyNavigationPush currentView
              -- Also update local ref (will be removed once Halogen fully owns this)
              Ref.modify_ (Array.cons currentView) State.globalNavigationStackRef

              -- Update ViewState for neighborhood view (always triptych now)
              let neighborhoodView = Detail (NeighborhoodDetail clickedNode.name TriptychView)
              Ref.write neighborhoodView State.globalViewStateRef
              State.notifyViewStateChanged neighborhoodView

              -- Update simulation and DOM (renders bubble packs)
              focusOnNeighborhood neighborhoodNodes state.simulation

              -- Wrap with triptych layout (adds chord + matrix panels)
              renderTriptychWithDeclarations clickedNode.name neighborhoodNodes

-- | Focus on a subset of nodes
focusOnNeighborhood :: Array SimNode -> Scene.CESimulation -> Effect Unit
focusOnNeighborhood nodes sim = do
  log $ "[Explorer] Focusing on neighborhood with " <> show (Array.length nodes) <> " nodes"

  -- Hide any existing tooltip
  Tooltip.hideTooltip

  -- Clear any existing DOM elements
  clearTreeLinks
  clearNodesGroup

  -- Unpin all nodes (clear fx/fy so simulation can position them)
  let unpinnedNodes = map unpinNode nodes

  -- Update simulation with unpinned neighborhood nodes
  Sim.setNodes unpinnedNodes sim

  -- Get LIVE nodes from simulation (these are the objects D3 will mutate)
  liveNodes <- Sim.getNodes sim

  -- Set up neighborhood forces
  setNeighborhoodForces liveNodes sim

  -- Fetch declarations and function calls via batch endpoints (single request each)
  -- This replaces N individual requests with 2 batch requests
  launchAff_ do
    -- Extract module names from nodes (filter out packages which have empty names)
    let moduleNames = Array.mapMaybe (\n -> if n.name /= "" then Just n.name else Nothing) liveNodes
    log $ "[Explorer] Fetching batch data for " <> show (Array.length moduleNames) <> " modules"

    -- Batch fetch declarations (single request)
    declResult <- fetchBatchDeclarations moduleNames
    case declResult of
      Left err -> log $ "[Explorer] Batch declarations error: " <> err
      Right _ -> pure unit
    let
      declarations = case declResult of
        Right decls -> decls
        Left _ -> Object.empty

    -- Batch fetch function calls (single request)
    fnCallResult <- fetchBatchFunctionCalls moduleNames
    case fnCallResult of
      Left err -> log $ "[Explorer] Batch function calls error: " <> err
      Right _ -> pure unit
    let
      functionCalls = case fnCallResult of
        Right fnCalls -> fnCalls
        Left _ -> Object.empty

    -- Update global function calls ref so click/hover handlers can use it
    liftEffect $ Ref.write functionCalls State.globalFunctionCallsRef

    -- Log fetch summary
    liftEffect $ log $ "[Explorer] Batch fetched: " <> show (Object.size declarations) <> " modules with declarations, "
      <> show (Object.size functionCalls)
      <> " function entries"

    -- Render bubble packs with fetched declarations
    liftEffect do
      for_ liveNodes \node -> do
        _ <- renderModulePackWithCallbacks declarations onDeclarationClick onDeclarationHover onDeclarationLeave onModuleHover node
        pure unit

      -- Color legend is now handled by Halogen NarrativePanel (switches to declaration types automatically)

      -- Attach drag behavior using library API
      -- Select all module-pack groups and get their DOM elements
      packElements <- runD3v2M do
        nodesContainer <- select "#explorer-nodes"
        packGroups <- selectAll "g.module-pack" nodesContainer
        pure $ D3v2.getElementsD3v2 packGroups

      -- Attach group drag with reheat callback
      -- Use #explorer-zoom-group as coordinate reference (handles viewBox transforms)
      Core.attachGroupDragWithReheat packElements "#explorer-zoom-group" (Sim.reheat sim)

      -- Render links between neighborhood modules
      renderNeighborhoodLinks liveNodes

      -- Set up custom tick callback for bubble pack mode
      -- This updates group transforms and link positions
      Sim.onTick (bubblePackTick) sim

      -- Reheat simulation to animate
      Sim.reheat sim

  pure unit

-- | Focus on a package neighborhood (packages in adjacent topo layers)
-- | Unlike focusOnNeighborhood, this renders packages as simple circles with labels
-- | positioned by their topological layer
focusOnPackageNeighborhood :: String -> Array SimNode -> Scene.CESimulation -> Effect Unit
focusOnPackageNeighborhood clickedPackageName nodes sim = do
  log $ "[Explorer] Focusing on package neighborhood: " <> clickedPackageName
    <> " with "
    <> show (Array.length nodes)
    <> " packages"

  -- Hide any existing tooltip
  Tooltip.hideTooltip

  -- Clear any existing DOM elements
  clearTreeLinks
  clearNodesGroup

  -- Find the clicked package's layer and the min/max layers in neighborhood
  let clickedNode = Array.find (\n -> n.name == clickedPackageName) nodes
  let clickedLayer = maybe 0 _.topoLayer clickedNode
  let allLayers = map _.topoLayer nodes
  let minLayer = fromMaybe 0 $ minimum allLayers
  let maxLayer = fromMaybe 0 $ maximum allLayers

  -- Calculate layered positions (x spread within layer, y by layer)
  -- Group packages by layer for horizontal distribution
  let
    layerGroups = Array.groupBy (\a b -> a.topoLayer == b.topoLayer) $
      Array.sortBy (comparing _.topoLayer) nodes

  -- Viewport dimensions - viewBox is centered at origin (-1200 -800 2400 1600)
  let viewWidth = ViewBox.viewBoxWidth
  let viewHeight = ViewBox.viewBoxHeight
  let centerX = 0.0 -- Origin is center
  let centerY = 0.0

  -- Calculate layer spacing (use ~half viewport height for compact layout)
  let numLayers = maxLayer - minLayer + 1
  let totalLayerSpan = viewHeight / 2.0 -- Half the viewport height
  let
    layerHeight =
      if numLayers > 1 then totalLayerSpan / toNumber (numLayers - 1)
      else 0.0

  -- Position nodes by layer (top = high layer, bottom = low layer)
  let
    positionedNodes = Array.concat $ Array.mapWithIndex
      ( \_ group ->
          let
            groupArray = Array.fromFoldable group
            layerNum = maybe minLayer _.topoLayer (Array.head groupArray)
            -- Y position: higher layer = higher on screen (lower Y)
            yPos = centerY - (toNumber (layerNum - clickedLayer)) * layerHeight
            -- X positions: centered, spread evenly
            numInLayer = Array.length groupArray
            nodeSpacing = 120.0 -- Space between nodes
            rowWidth = toNumber (numInLayer - 1) * nodeSpacing
            xStart = centerX - rowWidth / 2.0 -- Center the row
          in
            Array.mapWithIndex
              ( \nodeIdx node ->
                  let
                    xPos = xStart + toNumber nodeIdx * nodeSpacing
                  in
                    node { x = xPos, y = yPos, fx = Nullable.notNull xPos, fy = Nullable.notNull yPos }
              )
              groupArray
      )
      layerGroups

  -- Update simulation with positioned nodes (pinned so they don't move)
  Sim.setNodes positionedNodes sim

  -- Get LIVE nodes from simulation
  liveNodes <- Sim.getNodes sim

  -- Render package circles with labels
  renderPackageNeighborhoodNodes clickedPackageName liveNodes clickedLayer

  -- No simulation needed - nodes are pinned in place
  pure unit

-- | Render package nodes for the neighborhood view
-- | Packages are rendered as circles with labels, highlighted if clicked
-- | Uses FFI for direct DOM manipulation since PSD3v2 doesn't have simple append
renderPackageNeighborhoodNodes :: String -> Array SimNode -> Int -> Effect Unit
renderPackageNeighborhoodNodes clickedPkg nodes clickedLayer = do
  for_ nodes \node -> do
    let isClicked = node.name == clickedPkg
    let isClickedLayer = node.topoLayer == clickedLayer
    let r = if isClicked then 35.0 else 25.0
    let
      fillColor =
        if isClicked then "#fbbf24" -- Amber for clicked
        else if isClickedLayer then "#60a5fa" -- Blue for same layer
        else "#94a3b8" -- Gray for other layers
    let strokeColor = if isClicked then "#f59e0b" else "#475569"
    let sw = if isClicked then "3" else "2"
    let fw = if isClicked then "600" else "400"
    renderPackageNodeFFI node.x node.y node.name node.topoLayer r fillColor strokeColor sw fw

-- | FFI for rendering a package node with circle and labels
foreign import renderPackageNodeFFI :: Number -> Number -> String -> Int -> Number -> String -> String -> String -> String -> Effect Unit

-- | Tick callback for bubble pack mode
-- | Updates group transforms and link positions
bubblePackTick :: Effect Unit
bubblePackTick = do
  -- Update bubble pack group positions
  updateGroupPositions State.nodesGroupId

  -- Update link positions
  updateLinkPositions State.forceLinksGroupId

-- | Pin a node at its current position (set fx/fy to current x/y)
pinNodeAtCurrent :: SimNode -> SimNode
pinNodeAtCurrent n = n { fx = Nullable.notNull n.x, fy = Nullable.notNull n.y }

-- | Unpin a node (clear fx/fy so simulation can move it)
unpinNode :: SimNode -> SimNode
unpinNode n = n { fx = Nullable.null, fy = Nullable.null }

-- | Swizzled link for neighborhood rendering
type NeighborhoodLink =
  { source :: SimNode
  , target :: SimNode
  , linkType :: LinkType
  }

-- | Directional link for colored bidirectional rendering
-- | Each link direction is rendered separately with offset
type DirectionalLink =
  { source :: SimNode
  , target :: SimNode
  , isOutgoing :: Boolean -- True = green (imports), False = orange (imported by)
  }

-- | Render links between neighborhood nodes (full graph edges, not tree)
-- | Now renders bidirectional colored links (green for outgoing, orange for incoming)
renderNeighborhoodLinks :: Array SimNode -> Effect Unit
renderNeighborhoodLinks nodes = do
  allLinks <- Ref.read State.globalLinksRef

  -- Build set of node IDs in neighborhood
  let nodeIdSet = Set.fromFoldable $ map _.id nodes

  -- Build map from ID to SimNode for swizzling
  let nodeMap = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes

  -- Filter to links where BOTH source and target are in neighborhood
  let
    neighborhoodLinks = Array.filter
      (\link -> Set.member link.source nodeIdSet && Set.member link.target nodeIdSet)
      allLinks

  log $ "[Explorer] Rendering " <> show (Array.length neighborhoodLinks) <> " neighborhood links as bidirectional"

  -- Convert SimLinks to DirectionalLinks
  -- Each SimLink becomes one directional link (source imports target = outgoing)
  -- We also need to check for reverse links and create inbound markers
  let
    -- Build a set of (source, target) pairs for quick lookup
    linkPairSet = Set.fromFoldable $ map (\l -> Tuple l.source l.target) neighborhoodLinks

    -- Create directional links from the raw links
    directionalLinks = Array.mapMaybe (toDirectionalLink nodeMap linkPairSet) neighborhoodLinks

  log $ "[Explorer] Created " <> show (Array.length directionalLinks) <> " directional links"

  -- Render as colored offset lines
  _ <- runD3v2M $ renderDirectionalLinksD3 directionalLinks
  pure unit

-- | Convert a SimLink to DirectionalLink, marking direction based on link orientation
toDirectionalLink :: Map.Map Int SimNode -> Set.Set (Tuple Int Int) -> SimLink -> Maybe DirectionalLink
toDirectionalLink nodeMap _linkPairSet link = do
  srcNode <- Map.lookup link.source nodeMap
  tgtNode <- Map.lookup link.target nodeMap
  -- Each link represents source importing target (outgoing)
  -- All neighborhood links are green (outgoing direction)
  pure { source: srcNode, target: tgtNode, isOutgoing: true }

-- | Render static links for bubble pack view (no simulation updates)
-- | Uses node x/y directly to create line coordinates
renderStaticNeighborhoodLinks :: Array SimNode -> Effect Unit
renderStaticNeighborhoodLinks nodes = do
  allLinks <- Ref.read State.globalLinksRef

  -- Build map of node ID -> node for position lookup
  let nodeMap = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes
  let nodeIdSet = Set.fromFoldable $ map _.id nodes

  -- Filter to links where BOTH source and target are in neighborhood
  let
    neighborhoodLinks = Array.filter
      (\link -> Set.member link.source nodeIdSet && Set.member link.target nodeIdSet)
      allLinks

  log $ "[Explorer] Rendering " <> show (Array.length neighborhoodLinks) <> " static neighborhood links"

  -- Render as straight lines using node positions
  _ <- runD3v2M $ renderStaticLinksD3 nodeMap neighborhoodLinks
  pure unit

-- | Static link type with pre-computed coordinates
type StaticLink =
  { x1 :: Number
  , y1 :: Number
  , x2 :: Number
  , y2 :: Number
  }

-- | D3 rendering for static links
renderStaticLinksD3 :: Map.Map Int SimNode -> Array SimLink -> D3v2M Unit
renderStaticLinksD3 nodeMap links = do
  linksGroup <- select "#explorer-links" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Convert links to static coordinates
  let staticLinks = Array.mapMaybe (toStaticLink nodeMap) links

  let linksTree :: T.Tree StaticLink
      linksTree =
        T.joinData "static-links-data" "line" staticLinks $ \link ->
          T.elem Line
            [ v3Attr "x1" (lit link.x1)
            , v3Attr "y1" (lit link.y1)
            , v3Attr "x2" (lit link.x2)
            , v3Attr "y2" (lit link.y2)
            , v3AttrStr "stroke" (str "white")
            , v3Attr "stroke-width" (lit 1.5)
            , v3Attr "opacity" (lit 0.6)
            , v3AttrStr "class" (str "neighborhood-link")
            ]

  _ <- renderTree linksGroup linksTree
  pure unit

-- | Convert a SimLink to StaticLink using node positions
toStaticLink :: Map.Map Int SimNode -> SimLink -> Maybe StaticLink
toStaticLink nodeMap link = do
  srcNode <- Map.lookup link.source nodeMap
  tgtNode <- Map.lookup link.target nodeMap
  pure { x1: srcNode.x, y1: srcNode.y, x2: tgtNode.x, y2: tgtNode.y }

-- | D3 rendering for neighborhood links
renderNeighborhoodLinksD3 :: Array NeighborhoodLink -> D3v2M Unit
renderNeighborhoodLinksD3 links = do
  linksGroup <- select "#explorer-links" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Log connectivity statistics for debugging
  let scores = map (\link -> linkInterconnectivity link.source link.target) links
  liftEffect $ logConnectivityStats "Neighborhood links" scores

  let linksTree :: T.Tree NeighborhoodLink
      linksTree =
        T.joinData "neighborhood-links-data" "line" links $ \link ->
          let
            score = linkInterconnectivity link.source link.target
          in
            T.elem Line
              [ v3Attr "x1" (lit link.source.x)
              , v3Attr "y1" (lit link.source.y)
              , v3Attr "x2" (lit link.target.x)
              , v3Attr "y2" (lit link.target.y)
              , v3AttrStr "stroke" (str (linkStrokeColor score))
              , v3Attr "stroke-width" (lit (linkStrokeWidth score))
              , v3Attr "opacity" (lit (linkOpacity score))
              , v3AttrStr "class" (str "neighborhood-link")
              ]

  _ <- renderTree linksGroup linksTree
  pure unit

-- | D3 rendering for directional links with bidirectional coloring
-- | Green = outgoing (source imports target), Orange = incoming (the reverse link)
renderDirectionalLinksD3 :: Array DirectionalLink -> D3v2M Unit
renderDirectionalLinksD3 links = do
  linksGroup <- select "#explorer-links" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Each link is rendered with its directional color
  -- Green (#4ade80) for outgoing, matching the adjacency matrix
  let linksTree :: T.Tree DirectionalLink
      linksTree =
        T.joinData "directional-links-data" "line" links $ \link ->
          let
            score = linkInterconnectivity link.source link.target
            strokeColor = if link.isOutgoing then "#4ade80" else "#f97316"
            className = if link.isOutgoing then "neighborhood-link outgoing-link" else "neighborhood-link incoming-link"
          in
            T.elem Line
              [ v3Attr "x1" (lit link.source.x)
              , v3Attr "y1" (lit link.source.y)
              , v3Attr "x2" (lit link.target.x)
              , v3Attr "y2" (lit link.target.y)
              , v3AttrStr "stroke" (str strokeColor)
              , v3Attr "stroke-width" (lit (linkStrokeWidth score))
              , v3Attr "opacity" (lit 0.7)
              , v3AttrStr "class" (str className)
              ]

  _ <- renderTree linksGroup linksTree
  pure unit

-- =============================================================================
-- Navigation
-- =============================================================================

-- | Get the origin view (where the user came from before entering Detail view)
-- | Returns the overview view they were on, or TreemapView as fallback
getOriginView :: Effect OverviewView
getOriginView = do
  focus <- Ref.read State.globalFocusRef
  pure $ fromMaybe TreemapView focus.originView

-- | Get all module names for search functionality
getModuleNames :: Effect (Array String)
getModuleNames = do
  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Nothing -> pure []
    Just stateRef -> do
      state <- Ref.read stateRef
      nodes <- Sim.getNodes state.simulation
      let moduleNames = Array.mapMaybe (\n -> if n.nodeType == ModuleNode && n.name /= "" then Just n.name else Nothing) nodes
      pure $ Array.sort moduleNames

-- | Navigate to a module neighborhood by name (for search)
-- | Returns true if module was found and navigation happened
navigateToModuleByName :: String -> Effect Boolean
navigateToModuleByName moduleName = do
  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Nothing -> do
      log $ "[Explorer] Cannot navigate - no state ref"
      pure false
    Just stateRef -> do
      state <- Ref.read stateRef
      focus <- Ref.read State.globalFocusRef

      -- Get current nodes
      currentNodes <- Sim.getNodes state.simulation
      -- Use fullNodes if available (may be in a detail view already)
      let nodesPool = if Array.null focus.fullNodes then currentNodes else focus.fullNodes

      -- Find the module by name
      case Array.find (\n -> n.name == moduleName && n.nodeType == ModuleNode) nodesPool of
        Nothing -> do
          log $ "[Explorer] Module not found: " <> moduleName
          pure false
        Just targetNode -> do
          log $ "[Explorer] Navigating to module: " <> moduleName

          -- Build neighborhood: target node + its targets + its sources
          let
            neighborhoodIds = Set.fromFoldable $
              [ targetNode.id ] <> targetNode.targets <> targetNode.sources
            neighborhoodNodes = Array.filter (\n -> Set.member n.id neighborhoodIds) nodesPool

          log $ "[Explorer] Found " <> show (Array.length neighborhoodNodes) <> " nodes in neighborhood"

          -- Record the origin view (must be done before view change)
          currentView <- Ref.read State.globalViewStateRef
          let
            origin = case currentView of
              Overview ov -> Just ov
              Detail _ -> focus.originView -- Keep existing origin if already in detail

          -- Notify Halogen of focus state change
          let focusInfo = { focusedNodeId: Just targetNode.id, fullNodes: nodesPool, originView: origin }
          State.notifyFocusChanged focusInfo
          -- Also update local ref (will be removed once Halogen fully owns this)
          Ref.write focusInfo State.globalFocusRef

          -- Notify Halogen to push current view to navigation stack
          State.notifyNavigationPush currentView
          -- Also update local ref (will be removed once Halogen fully owns this)
          Ref.modify_ (Array.cons currentView) State.globalNavigationStackRef

          -- Update ViewState for neighborhood view (always triptych now)
          let neighborhoodView = Detail (NeighborhoodDetail targetNode.name TriptychView)
          Ref.write neighborhoodView State.globalViewStateRef
          State.notifyViewStateChanged neighborhoodView

          -- Update simulation and DOM (renders bubble packs)
          focusOnNeighborhood neighborhoodNodes state.simulation

          -- Wrap with triptych layout (adds chord + matrix panels)
          renderTriptychWithDeclarations targetNode.name neighborhoodNodes

          pure true

-- | Navigate back to the previous view in the navigation stack
-- | Returns true if navigation happened, false if stack was empty
navigateBack :: Effect Boolean
navigateBack = do
  stack <- Ref.read State.globalNavigationStackRef
  case Array.uncons stack of
    Nothing -> do
      log "[Explorer] Navigation stack empty, nothing to go back to"
      pure false
    Just { head: previousView, tail: remainingStack } -> do
      log $ "[Explorer] Navigating back to: " <> showViewState previousView
      -- Pop the stack
      Ref.write remainingStack State.globalNavigationStackRef
      -- Restore the previous view
      restoreToView previousView
      pure true

-- | Restore to a specific view (used by navigateBack)
restoreToView :: ViewState -> Effect Unit
restoreToView targetView = do
  focus <- Ref.read State.globalFocusRef
  mStateRef <- Ref.read State.globalStateRef

  case mStateRef of
    Nothing -> log "[Explorer] No state ref to restore"
    Just stateRef -> do
      state <- Ref.read stateRef

      case targetView of
        Overview _ -> restoreFullView focus.fullNodes targetView state.simulation
        Detail _ -> do
          -- Re-entering a detail view - just update the view state
          -- (the DOM is already showing the detail)
          Ref.write targetView State.globalViewStateRef
          State.notifyViewStateChanged targetView

-- | Restore full view
restoreFullView :: Array SimNode -> ViewState -> Scene.CESimulation -> Effect Unit
restoreFullView fullNodes targetView sim = do
  -- Update simulation with full nodes
  Sim.setNodes fullNodes sim

  -- Clear neighborhood-specific visualizations
  clearTreeLinks
  clearChordDiagram
  clearAdjacencyMatrix
  clearBubblePacks
  clearTriptych

  -- Color legend is handled by Halogen NarrativePanel (switches back to packages automatically)

  -- Re-render DOM with circles
  clearNodesGroup
  _ <- runD3v2M $ renderNodesOnly targetView fullNodes

  -- Restore the original Scene tick handler and trigger appropriate scene
  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Just stateRef -> do
      Scene.clearLinksGroupId stateRef
      -- Restore tick handler with view transition support
      let nodesSelector = "#explorer-nodes"
      Sim.onTick (tickWithTransitionCallback stateRef nodesSelector) sim
      -- Trigger appropriate scene based on target view
      case targetView of
        Overview TreemapView -> goToScene TreemapForm stateRef
        Overview TreeView -> goToScene TreeForm stateRef
        Overview ForceView -> goToScene TreeRun stateRef
        Overview TopoView -> goToScene TopoForm stateRef
        Detail _ -> pure unit -- Detail views shouldn't reach this path
    Nothing -> pure unit

  -- Update ViewState and notify via callback
  Ref.write targetView State.globalViewStateRef
  State.notifyViewStateChanged targetView

  -- Ensure colors are correct for the restored view
  -- (renderNodesOnly should set them, but this ensures they're applied)
  updateNodeColors targetView

  -- Clear focus state (local ref - Halogen clears its own)
  Ref.write { focusedNodeId: Nothing, fullNodes: [], originView: Nothing } State.globalFocusRef

  Sim.reheat sim
  pure unit

-- | Restore from Halogen-owned focus state
-- | Called by Halogen when navigating back - takes focus info from Halogen state
restoreFromFocus :: State.FocusInfo -> ViewState -> Effect Unit
restoreFromFocus focusInfo targetView = do
  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Nothing -> log "[Explorer] No state ref to restore"
    Just stateRef -> do
      state <- Ref.read stateRef
      case targetView of
        Overview _ -> restoreFullView focusInfo.fullNodes targetView state.simulation
        Detail _ -> do
          -- Re-entering a detail view - just update the view state
          Ref.write targetView State.globalViewStateRef
          State.notifyViewStateChanged targetView

-- | Set forces appropriate for neighborhood view (spread out, no grid)
-- | Uses stronger forces than full graph view since neighborhood has fewer nodes
setNeighborhoodForces :: Array SimNode -> Scene.CESimulation -> Effect Unit
setNeighborhoodForces nodes sim = do
  -- Clear existing forces
  Ref.write Map.empty sim.forces

  -- Many-body repulsion to spread nodes out (stronger for small neighborhoods)
  let manyBodyHandle = Core.createManyBody { strength: -400.0, theta: 0.9, distanceMin: 1.0, distanceMax: 1.0e10 }
  _ <- Core.initializeForce manyBodyHandle nodes
  Ref.modify_ (Map.insert "charge" manyBodyHandle) sim.forces

  -- Collision to prevent overlap (larger radius for bubble packs)
  let collideHandle = Core.createCollideGrid 20.0 0.7 1
  _ <- Core.initializeForce collideHandle nodes
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  -- Centering force to keep neighborhood on screen
  let forceXHandle = Core.createForceX { x: 0.0, strength: 0.05 }
  _ <- Core.initializeForce forceXHandle nodes
  Ref.modify_ (Map.insert "x" forceXHandle) sim.forces

  let forceYHandle = Core.createForceY { y: 0.0, strength: 0.05 }
  _ <- Core.initializeForce forceYHandle nodes
  Ref.modify_ (Map.insert "y" forceYHandle) sim.forces

  log "[Explorer] Neighborhood forces set (stronger charge=-400, collide=20)"

-- | Clear the nodes group (remove all circles)
clearNodesGroup :: Effect Unit
clearNodesGroup = do
  win <- window
  doc <- document win
  let parentNode = toParentNode doc
  mElement <- querySelector (QuerySelector "#explorer-nodes") parentNode
  case mElement of
    Just nodesGroup -> clearElement nodesGroup
    Nothing -> log "[Explorer] Could not find #explorer-nodes"

-- | Render nodes only (without full SVG setup)
-- | ViewState passed as parameter for color functions
renderNodesOnly :: ViewState -> Array SimNode -> D3v2M Unit
renderNodesOnly currentView nodes = do
  nodesGroup <- select "#explorer-nodes" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- ViewState now passed as parameter (no global ref read)

  -- Render nodes with data binding
  let nodesTree :: T.Tree SimNode
      nodesTree =
        T.joinData "nodes-data" "circle" nodes $ \node ->
          T.elem Circle
            [ v3Attr "cx" (lit node.x)
            , v3Attr "cy" (lit node.y)
            , v3Attr "r" (lit node.r)
            , v3AttrStr "fill" (str (ColorPalette.getNodeFill currentView node))
            , v3AttrStr "stroke" (str (ColorPalette.getNodeStroke currentView node))
            , v3Attr "stroke-width" (lit 1.0) -- Slightly thicker stroke for visibility
            , v3AttrStr "class" (str (nodeClass node))
            ]

  nodeSelections <- renderTree nodesGroup nodesTree
  -- Get the nodes selection from the map (already has data bound as SBoundOwns)
  case Map.lookup "nodes-data" nodeSelections of
    Nothing -> pure unit -- Should never happen
    Just nodeSel -> do
      log "[Explorer] Reattaching event handlers to circles"

      -- Re-attach highlight behavior
      let highlightClasses = [ "highlighted-source", "highlighted-upstream", "highlighted-downstream", "dimmed" ]
      _ <- on
        ( onMouseEnter \node -> do
            clearClasses "#explorer-nodes" "circle" highlightClasses
            let targetSet = Set.fromFoldable node.targets
            let sourceSet = Set.fromFoldable node.sources
            classifyElements "#explorer-nodes" "circle" \n ->
              if n.id == node.id then "highlighted-source"
              else if Set.member n.id targetSet then "highlighted-upstream"
              else if Set.member n.id sourceSet then "highlighted-downstream"
              else "dimmed"
        )
        nodeSel

      -- Combined mouse leave handler (clear highlight AND hide tooltip)
      _ <- on
        ( onMouseLeave \_ -> do
            clearClasses "#explorer-nodes" "circle" highlightClasses
            Tooltip.hideTooltip
        )
        nodeSel

      -- Re-attach tooltip
      _ <- on (onTooltip formatNodeTooltip) nodeSel

      -- Re-attach click handler
      _ <- on (onClickWithDatum toggleFocus) nodeSel

      pure unit

-- =============================================================================
-- Declaration Click Handler
-- =============================================================================

-- | Handle click on a declaration circle
-- | Shows the call graph popup with function call information and source code
onDeclarationClick :: DeclarationClickCallback
onDeclarationClick moduleName declarationName kind = do
  log $ "[Explorer] Declaration clicked: " <> moduleName <> "." <> declarationName <> " (kind: " <> kind <> ")"

  -- Notify the Halogen component to show the call graph popup
  -- The popup component will fetch its own data from the API
  State.notifyShowCallGraphPopup moduleName declarationName

-- | Handle hover on a declaration circle
-- | Highlights individual function circles based on call graph
onDeclarationHover :: DeclarationHoverCallback
onDeclarationHover moduleName declarationName _kind = do
  -- Look up function calls for this declaration
  fnCalls <- Ref.read State.globalFunctionCallsRef
  let sourceFunc = moduleName <> "." <> declarationName
  case Object.lookup sourceFunc fnCalls of
    Nothing -> pure unit -- No call data, no highlighting
    Just fnInfo -> do
      -- Build full "Module.funcName" strings for callers and callees
      -- fnInfo.calls contains { target, targetModule, ... } - build "targetModule.target"
      let calleeFuncs = map (\c -> c.targetModule <> "." <> c.target) fnInfo.calls
      -- fnInfo.calledBy is already in "Module.funcName" format
      let callerFuncs = fnInfo.calledBy
      -- Highlight individual function circles
      highlightCallGraph sourceFunc callerFuncs calleeFuncs
      -- Draw edges between function circles
      drawFunctionEdges sourceFunc callerFuncs calleeFuncs

-- Hover hint could be added via a globalHintRef if needed

-- | Handle mouse leave from a declaration
onDeclarationLeave :: Effect Unit
onDeclarationLeave = do
  clearCallGraphHighlight
  clearFunctionEdges

-- | Handle hover on a module (the outer circle)
-- | Shows all function-to-function edges for the entire module
onModuleHover :: String -> Effect Unit
onModuleHover moduleName = do
  fnCalls <- Ref.read State.globalFunctionCallsRef
  -- Find all functions in this module from the function calls map
  let allFuncs = Object.keys fnCalls
  let moduleFuncs = Array.filter (startsWith (moduleName <> ".")) allFuncs

  -- Build edges and collect caller/callee info
  let { edges, callerFuncs, calleeFuncs } = buildModuleEdgesAndFuncs moduleName moduleFuncs fnCalls

  -- Apply highlighting
  highlightModuleCallGraph moduleName moduleFuncs callerFuncs calleeFuncs
  -- Draw all edges
  drawModuleEdges edges

-- | Build all edges for a module and collect callers/callees
buildModuleEdgesAndFuncs
  :: String
  -> Array String
  -> FunctionCallsMap
  -> { edges :: Array ModuleEdge, callerFuncs :: Array String, calleeFuncs :: Array String }
buildModuleEdgesAndFuncs moduleName moduleFuncs fnCalls =
  let
    -- Process each function in the module
    processFunc :: String -> { edges :: Array ModuleEdge, callers :: Array String, callees :: Array String }
    processFunc funcKey = case Object.lookup funcKey fnCalls of
      Nothing -> { edges: [], callers: [], callees: [] }
      Just fnInfo ->
        let
          -- Outgoing edges (this function calls others)
          outEdges = map
            ( \c ->
                { source: funcKey
                , target: c.targetModule <> "." <> c.target
                , isOutgoing: true
                }
            )
            fnInfo.calls
          -- Incoming edges (others call this function)
          inEdges = map
            ( \caller ->
                { source: caller
                , target: funcKey
                , isOutgoing: false
                }
            )
            fnInfo.calledBy
          -- Collect callee and caller function names
          calleeNames = map (\c -> c.targetModule <> "." <> c.target) fnInfo.calls
          callerNames = fnInfo.calledBy
        in
          { edges: outEdges <> inEdges, callers: callerNames, callees: calleeNames }

    results = map processFunc moduleFuncs
    allEdges = Array.concatMap _.edges results
    allCallers = Array.nub $ Array.concatMap _.callers results
    allCallees = Array.nub $ Array.concatMap _.callees results
    -- Filter out self-module references for callers/callees (they're already in moduleFuncs)
    externalCallers = Array.filter (not <<< startsWith (moduleName <> ".")) allCallers
    externalCallees = Array.filter (not <<< startsWith (moduleName <> ".")) allCallees
  in
    { edges: allEdges, callerFuncs: externalCallers, calleeFuncs: externalCallees }

-- | Helper: check if string starts with prefix
startsWith :: String -> String -> Boolean
startsWith prefix str = SCU.take (SCU.length prefix) str == prefix

-- | Extract module name from "Module.name" string
-- | Splits "Foo.Bar.baz" into Just "Foo.Bar" (module part)
extractModuleName :: String -> Maybe String
extractModuleName fullName = do
  lastDotIdx <- String.lastIndexOf (Pattern ".") fullName
  let modulePart = SCU.take lastDotIdx fullName
  if modulePart == "" then Nothing else Just modulePart

-- =============================================================================
-- Neighborhood View Type Switching
-- =============================================================================

-- | Switch between neighborhood view types (Bubbles, Chord, Matrix)
-- | This only works when already in a NeighborhoodDetail view
setNeighborhoodViewType :: NeighborhoodViewType -> Effect Unit
setNeighborhoodViewType newViewType = do
  currentView <- Ref.read State.globalViewStateRef
  case getNeighborhoodModule currentView of
    Nothing -> do
      log "[Explorer] setNeighborhoodViewType called but not in neighborhood view"
      pure unit
    Just moduleName -> do
      log $ "[Explorer] Switching neighborhood view to: " <> neighborhoodViewLabel newViewType

      -- Get current nodes from simulation
      mStateRef <- Ref.read State.globalStateRef
      case mStateRef of
        Nothing -> log "[Explorer] No state ref available"
        Just stateRef -> do
          state <- Ref.read stateRef
          nodes <- Sim.getNodes state.simulation

          -- Clear current view-specific elements
          clearBubblePacks
          clearChordDiagram
          clearAdjacencyMatrix
          clearTriptych
          clearTreeLinks

          -- Update view state
          let newView = Detail (NeighborhoodDetail moduleName newViewType)
          Ref.write newView State.globalViewStateRef
          State.notifyViewStateChanged newView

          -- Render the new view type
          case newViewType of
            BubblePackView -> do
              -- Re-render bubble packs (similar to focusOnNeighborhood)
              renderBubblePackView nodes
            ChordView -> do
              -- Render chord diagram
              renderNeighborhoodChord moduleName nodes ViewBox.viewBoxWidth ViewBox.viewBoxHeight
            MatrixView -> do
              -- Render adjacency matrix
              renderNeighborhoodMatrix moduleName nodes ViewBox.viewBoxWidth ViewBox.viewBoxHeight
            TriptychView -> do
              -- First render bubble packs normally
              renderBubblePackView nodes
              -- Then wrap them with triptych layout and add chord + matrix panels
              renderTriptychWithDeclarations moduleName nodes

-- | Render bubble pack view for neighborhood
-- | This is a simplified version of focusOnNeighborhood for view switching
renderBubblePackView :: Array SimNode -> Effect Unit
renderBubblePackView nodes = do
  declarations <- Ref.read State.globalDeclarationsRef
  for_ nodes \node -> do
    _ <- renderModulePackWithCallbacks declarations onDeclarationClick onDeclarationHover onDeclarationLeave onModuleHover node
    pure unit

  -- Attach drag behavior
  packElements <- runD3v2M do
    nodesContainer <- select "#explorer-nodes"
    packGroups <- selectAll "g.module-pack" nodesContainer
    pure $ D3v2.getElementsD3v2 packGroups

  mStateRef <- Ref.read State.globalStateRef
  case mStateRef of
    Just stateRef -> do
      state <- Ref.read stateRef
      Core.attachGroupDragWithReheat packElements "#explorer-zoom-group" (Sim.reheat state.simulation)
    Nothing -> pure unit

  -- Render neighborhood links
  renderNeighborhoodLinks nodes
