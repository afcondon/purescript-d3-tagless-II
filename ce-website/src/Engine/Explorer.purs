-- | Code Explorer using Scene Engine
-- |
-- | Clean implementation using the compositional scene architecture.
-- | Each scene is a self-contained config; transitions are handled by the Engine.
module Engine.Explorer
  ( initExplorer
  , initExplorerWithCallbacks
  , ExplorerCallbacks
  , ModelInfo
  , goToScene
  , SceneId(..)  -- Export ADT and constructors for type-safe scene selection
  , reloadWithProject
  , setViewState  -- Public API for changing view state
  , navigateBack
  , updateNodeColors
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
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
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Data.Loader (loadModel, loadModelForProject, LoadedModel, DeclarationsMap, FunctionCallsMap, fetchBatchDeclarations, fetchBatchFunctionCalls)
import Engine.BubblePack (renderModulePackWithCallbacks, highlightCallGraph, clearCallGraphHighlight, DeclarationClickCallback, DeclarationHoverCallback)
-- CallGraphPopup is now a Halogen component (Component.CallGraphPopup)
-- NarrativePanel is now a Halogen component (Component.NarrativePanel)
-- It polls globalViewStateRef and globalModelInfoRef directly
import Engine.ViewState (ViewState(..), ScopeFilter(..))
import Data.ColorPalette (getNodeStroke, getNodeFill) as ColorPalette
import Engine.Treemap as Treemap
import Engine.Treemap (recalculateTreemapPositions, renderWatermark, clearWatermark)
import PSD3v2.Tooltip (hideTooltip) as Tooltip
import Engine.Scene as Scene
import Engine.Scenes as Scenes
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Links (swizzleLinks, swizzleLinksByIndex)
import PSD3.ForceEngine.Render (GroupId(..), updateGroupPositions, updateLinkPositions)
import PSD3.ForceEngine.Simulation as Sim
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, viewBox, d, opacity, x1, x2, y1, y2)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom, onMouseEnter, onMouseLeave, onClickWithDatum)
import PSD3v2.Capabilities.Selection (select, selectAll, appendChild, appendData, on)
import PSD3v2.Interpreter.D3v2 (getElementsD3v2) as D3v2
import PSD3v2.Classify (classifyElements, clearClasses)
import Data.Set as Set
import PSD3v2.Tooltip (onTooltip)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns)
import Types (SimNode, SimLink, NodeType(..), LinkType, isTreeLink)
import Engine.ViewBox as ViewBox
import Viz.SpagoGridTest.TreeLinks (verticalLinkPath)
import Web.DOM.Element (Element, classList, toNode)
import Web.DOM.Node as Node
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)

-- =============================================================================
-- Scene ID (Type-Safe Scene Selection)
-- =============================================================================

-- | Type-safe scene identifiers
-- | Replaces string-based scene selection for compile-time safety
data SceneId
  = TreeForm     -- ^ Static tree layout (bezier links)
  | TreeRun      -- ^ Force-directed tree (physics enabled)

derive instance eqSceneId :: Eq SceneId

-- | Get scene configuration for a SceneId
sceneConfigFor :: SceneId -> Scene.SceneConfig SimNode
sceneConfigFor TreeForm = Scenes.treeFormScene
sceneConfigFor TreeRun = Scenes.treeRunScene

-- =============================================================================
-- Public API: setViewState
-- =============================================================================

-- | Set the current view state and trigger appropriate scene transitions.
-- | This is the public API for changing views - replaces direct Ref writes.
-- |
-- | Handles:
-- | - Updating the internal view state ref
-- | - Updating node colors to match the new view
-- | - Triggering scene transitions (TreeForm/TreeRun)
-- | - Notifying Halogen via callback
setViewState :: ViewState -> Effect Unit
setViewState newView = do
  log $ "[Explorer] setViewState: " <> showViewState newView

  -- Update internal state
  Ref.write newView globalViewStateRef

  -- Update node colors
  updateNodeColors newView

  -- Trigger scene transition if needed
  mStateRef <- Ref.read globalStateRef
  case mStateRef of
    Just stateRef ->
      case newView of
        Treemap _ -> pure unit  -- Treemap is static, no scene transition
        TreeLayout _ _ -> goToScene TreeForm stateRef
        ForceLayout _ _ -> goToScene TreeRun stateRef
        Neighborhood _ -> pure unit  -- Neighborhood handled separately
        FunctionCalls _ -> pure unit  -- FunctionCalls handled separately
    Nothing -> pure unit

  -- Notify Halogen via callback
  notifyViewStateChanged newView

-- =============================================================================
-- Global State
-- =============================================================================

-- | Global ref for external access (UI buttons, etc.)
globalStateRef :: Ref (Maybe (Ref Scene.SceneState))
globalStateRef = unsafePerformEffect $ Ref.new Nothing

-- | Global ref for links data
globalLinksRef :: Ref (Array SimLink)
globalLinksRef = unsafePerformEffect $ Ref.new []

-- | Global ref for declarations (for bubble packs)
globalDeclarationsRef :: Ref DeclarationsMap
globalDeclarationsRef = unsafePerformEffect $ Ref.new Object.empty

-- | Global ref for function calls (for atomic view)
globalFunctionCallsRef :: Ref FunctionCallsMap
globalFunctionCallsRef = unsafePerformEffect $ Ref.new Object.empty

-- | Model info for narrative panel
type ModelInfo =
  { projectName :: String
  , moduleCount :: Int
  , packageCount :: Int
  }

-- | Global ref for model info (for narrative)
globalModelInfoRef :: Ref ModelInfo
globalModelInfoRef = unsafePerformEffect $ Ref.new { projectName: "", moduleCount: 0, packageCount: 0 }

-- | Global ref for current view state
globalViewStateRef :: Ref ViewState
globalViewStateRef = unsafePerformEffect $ Ref.new (Treemap ProjectAndLibraries)

-- | Navigation stack for zoom out (history of previous views)
-- | Stack grows from left: newest at head, oldest at tail
globalNavigationStackRef :: Ref (Array ViewState)
globalNavigationStackRef = unsafePerformEffect $ Ref.new []

-- | Focus state for neighborhood drill-down
type FocusState =
  { focusedNodeId :: Maybe Int -- Currently focused node (Nothing = full view)
  , fullNodes :: Array SimNode -- Original full node set for restoration
  }

-- | Global ref for focus state
globalFocusRef :: Ref FocusState
globalFocusRef = unsafePerformEffect $ Ref.new { focusedNodeId: Nothing, fullNodes: [] }

-- =============================================================================
-- Callback-Based Notification System
-- =============================================================================

-- | Callbacks for Explorer events
-- | These allow the Halogen component to be notified of state changes
-- | instead of polling global refs.
type ExplorerCallbacks =
  { onViewStateChanged :: ViewState -> Effect Unit
      -- ^ Called when ViewState changes (navigation, drill-down, etc.)
  , onModelLoaded :: ModelInfo -> Effect Unit
      -- ^ Called when model data is loaded (provides package count for palette)
  , onShowCallGraphPopup :: String -> String -> Effect Unit
      -- ^ Called when a declaration is clicked (moduleName, declarationName)
  , onHideCallGraphPopup :: Effect Unit
      -- ^ Called when popup should be hidden (e.g., view change)
  }

-- | Global ref for callbacks (set by initExplorerWithCallbacks)
-- | This pattern allows Explorer functions to invoke callbacks without
-- | threading them through every function.
globalCallbacksRef :: Ref (Maybe ExplorerCallbacks)
globalCallbacksRef = unsafePerformEffect $ Ref.new Nothing

-- | Notify ViewState change via callback (if set)
notifyViewStateChanged :: ViewState -> Effect Unit
notifyViewStateChanged newView = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onViewStateChanged newView
    Nothing -> pure unit  -- No callbacks registered, silent no-op

-- | Notify model loaded via callback (if set)
notifyModelLoaded :: ModelInfo -> Effect Unit
notifyModelLoaded modelInfo = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onModelLoaded modelInfo
    Nothing -> pure unit  -- No callbacks registered, silent no-op

-- | Notify show call graph popup via callback (if set)
notifyShowCallGraphPopup :: String -> String -> Effect Unit
notifyShowCallGraphPopup moduleName declarationName = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onShowCallGraphPopup moduleName declarationName
    Nothing -> pure unit  -- No callbacks registered, silent no-op

-- =============================================================================
-- Constants
-- =============================================================================

nodesGroupId :: GroupId
nodesGroupId = GroupId "#explorer-nodes"

forceLinksGroupId :: GroupId
forceLinksGroupId = GroupId "#explorer-links"

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the explorer
initExplorer :: String -> Effect Unit
initExplorer containerSelector = do
  -- Initialize ViewState (Halogen NarrativePanel polls this)
  let initialView = Treemap ProjectAndLibraries
  Ref.write initialView globalViewStateRef
  log "[Explorer] ViewState initialized (Halogen NarrativePanel polls this)"

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
        liftEffect $ Ref.write model.links globalLinksRef
        liftEffect $ Ref.write model.declarations globalDeclarationsRef

        -- Initialize visualization immediately
        -- Function calls are loaded on-demand when drilling into neighborhoods
        stateRef <- liftEffect $ initWithModel model containerSelector
        liftEffect $ Ref.write (Just stateRef) globalStateRef
        liftEffect $ log "[Explorer] Ready (function calls loaded on-demand per neighborhood)"

-- | Initialize the explorer with callbacks
-- | This is the preferred API - callbacks notify the Halogen component of state changes
-- | instead of requiring polling.
initExplorerWithCallbacks :: String -> ExplorerCallbacks -> Effect Unit
initExplorerWithCallbacks containerSelector callbacks = do
  -- Store callbacks for use by other Explorer functions
  Ref.write (Just callbacks) globalCallbacksRef

  -- Initialize ViewState and notify via callback
  let initialView = Treemap ProjectAndLibraries
  Ref.write initialView globalViewStateRef
  callbacks.onViewStateChanged initialView
  log "[Explorer] ViewState initialized with callback notification"

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
        liftEffect $ Ref.write model.links globalLinksRef
        liftEffect $ Ref.write model.declarations globalDeclarationsRef

        -- Initialize visualization immediately
        -- Function calls are loaded on-demand when drilling into neighborhoods
        stateRef <- liftEffect $ initWithModel model containerSelector
        liftEffect $ Ref.write (Just stateRef) globalStateRef

        -- Notify model loaded via callback
        let modelInfo = { projectName: "purescript-d3-dataviz", moduleCount: model.moduleCount, packageCount: model.packageCount }
        liftEffect $ callbacks.onModelLoaded modelInfo

        liftEffect $ log "[Explorer] Ready with callback notifications"

-- | Initialize with loaded model
initWithModel :: LoadedModel -> String -> Effect (Ref Scene.SceneState)
initWithModel model containerSelector = do
  log "[Explorer] Initializing with new Scene Engine"

  -- Store model info for narrative
  Ref.write { projectName: "purescript-d3-dataviz", moduleCount: model.moduleCount, packageCount: model.packageCount } globalModelInfoRef

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

  -- Render SVG structure first (initial view is Treemap)
  let initialView = Treemap ProjectAndLibraries
  _ <- runD3v2M $ renderSVG initialView containerSelector simNodes

  -- Render treemap watermark (behind nodes)
  renderWatermark model.nodes

  -- Create scene state
  let sceneState = Scene.mkSceneState sim nodesGroupId
  stateRef <- Ref.new sceneState

  -- Set up tick callback - the Engine handles everything
  Sim.onTick (Scene.onTick stateRef) sim

  -- Start simulation - let forces do the initial clustering animation
  -- (no transition on startup - nodes start randomized, forces pull them in)
  Sim.start sim

  -- Mark TreeForm as current scene (we start with treemap-based positions)
  Ref.modify_ (_ { currentScene = Just Scenes.treeFormScene }) stateRef

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
  Ref.write [] globalLinksRef
  Ref.write Object.empty globalDeclarationsRef
  Ref.write Object.empty globalFunctionCallsRef
  Ref.write [] globalNavigationStackRef
  Ref.write { focusedNodeId: Nothing, fullNodes: [] } globalFocusRef

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
        liftEffect $ Ref.write model.links globalLinksRef
        liftEffect $ Ref.write model.declarations globalDeclarationsRef

        -- Get existing stateRef and update with new data
        mStateRef <- liftEffect $ Ref.read globalStateRef
        case mStateRef of
          Nothing -> do
            -- Initialize fresh if no existing state
            stateRef <- liftEffect $ initWithModel model "#viz"
            liftEffect $ Ref.write (Just stateRef) globalStateRef
          Just stateRef -> do
            -- Update existing state with new model
            liftEffect $ updateWithModel model stateRef

        liftEffect $ log "[Explorer] Project switch complete"

-- | Update existing state with a new model (for project switching)
updateWithModel :: LoadedModel -> Ref Scene.SceneState -> Effect Unit
updateWithModel model stateRef = do
  log "[Explorer] Updating visualization with new model"

  -- Update model info
  Ref.write { projectName: "project", moduleCount: model.moduleCount, packageCount: model.packageCount } globalModelInfoRef

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
  currentView <- Ref.read globalViewStateRef
  _ <- runD3v2M $ renderNodesOnly currentView simNodes

  -- Update watermark for new project
  clearWatermark
  renderWatermark model.nodes

  -- Reset to TreeForm scene
  Ref.modify_ (_ { currentScene = Just Scenes.treeFormScene }) stateRef

  -- Restart simulation
  Sim.start state.simulation

-- | Go to a scene using type-safe scene ID
goToScene :: SceneId -> Ref Scene.SceneState -> Effect Unit
goToScene sceneId stateRef = do
  let scene = sceneConfigFor sceneId

  -- Handle TreeForm: render tree bezier links
  when (sceneId == TreeForm) do
    state <- Ref.read stateRef
    nodes <- Sim.getNodes state.simulation
    clearTreeLinks -- Clear any existing links
    renderTreeLinks nodes
    setTreeSceneClass true

  -- Handle TreeRun: force-directed tree with link forces
  when (sceneId == TreeRun) do
    state <- Ref.read stateRef
    nodes <- Sim.getNodes state.simulation
    links <- Ref.read globalLinksRef
    clearTreeLinks -- Remove any existing links
    addTreeForces nodes links state.simulation
    renderForceLinks nodes links -- Render straight line links
    Scene.setLinksGroupId forceLinksGroupId stateRef -- Enable link updates
    setTreeSceneClass true -- Keep packages/non-tree faded

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
  container <- select containerSelector

  svg <- appendChild SVG
    [ viewBox (show ((-ViewBox.viewBoxWidth) / 2.0) <> " " <> show ((-ViewBox.viewBoxHeight) / 2.0) <> " " <> show ViewBox.viewBoxWidth <> " " <> show ViewBox.viewBoxHeight)
    , id_ "explorer-svg"
    , class_ "ce-viz"
    ]
    container

  zoomGroup <- appendChild Group [ id_ "explorer-zoom-group" ] svg
  _ <- appendChild Group [ id_ "treemap-watermark", class_ "watermark" ] zoomGroup -- Watermark first (behind)
  _ <- appendChild Group [ id_ "explorer-links" ] zoomGroup
  nodesGroup <- appendChild Group [ id_ "explorer-nodes" ] zoomGroup

  -- ViewState now passed as parameter (no global ref read)

  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (ColorPalette.getNodeFill currentView :: SimNode -> String)
    , stroke (ColorPalette.getNodeStroke currentView :: SimNode -> String)
    , strokeWidth 1.0 -- Slightly thicker stroke for visibility
    , class_ nodeClass -- CSS class for type-based styling/transitions
    ]
    nodesGroup

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
  mStateRef <- Ref.read globalStateRef
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
        <> ", min: " <> show minScore
        <> ", max: " <> show maxScore
        <> ", avg: " <> show avgScore
        <> ", median: " <> show medianScore

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
linkOpacity score = 0.3 + (score * 0.5)  -- Range: 0.3 to 0.8

-- =============================================================================
-- Tree Link Rendering
-- =============================================================================

-- | Render tree links as vertical bezier paths (root at top)
renderTreeLinks :: Array SimNode -> Effect Unit
renderTreeLinks nodes = do
  links <- Ref.read globalLinksRef
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
  linksGroup <- select "#explorer-links"

  -- Log connectivity statistics for debugging
  let scores = Array.mapMaybe (computeLinkScore nodeMap) links
  liftEffect $ logConnectivityStats "Tree links" scores

  -- Append path elements for each tree link with dynamic attributes
  _ <- appendData Path links
    [ d (linkPathFn nodeMap)
    , fill (\(_ :: SimLink) -> "none")
    , stroke (linkStrokeFn nodeMap)
    , strokeWidth (linkWidthFn nodeMap)
    , opacity (linkOpacityFn nodeMap)
    , class_ (\(_ :: SimLink) -> "tree-link")
    ]
    linksGroup

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
        let score = linkInterconnectivity srcNode tgtNode
        in linkStrokeColor score
      _, _ -> "white"  -- Fallback

  -- Compute stroke width based on interconnectivity
  linkWidthFn :: Map.Map Int SimNode -> SimLink -> Number
  linkWidthFn nm link =
    case Map.lookup link.source nm, Map.lookup link.target nm of
      Just srcNode, Just tgtNode ->
        let score = linkInterconnectivity srcNode tgtNode
        in linkStrokeWidth score
      _, _ -> 1.5  -- Fallback

  -- Compute opacity based on interconnectivity
  linkOpacityFn :: Map.Map Int SimNode -> SimLink -> Number
  linkOpacityFn nm link =
    case Map.lookup link.source nm, Map.lookup link.target nm of
      Just srcNode, Just tgtNode ->
        let score = linkInterconnectivity srcNode tgtNode
        in linkOpacity score
      _, _ -> 0.5  -- Fallback

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
  linksGroup <- select "#explorer-links"

  -- Append line elements for each link with dynamic attributes
  _ <- appendData Line links
    [ x1 getSourceX
    , y1 getSourceY
    , x2 getTargetX
    , y2 getTargetY
    , stroke forceStrokeFn
    , strokeWidth forceWidthFn
    , opacity forceOpacityFn
    , class_ (\(_ :: SwizzledLink) -> "force-link")
    ]
    linksGroup

  pure unit
  where
  getSourceX :: SwizzledLink -> Number
  getSourceX l = l.source.x

  getSourceY :: SwizzledLink -> Number
  getSourceY l = l.source.y

  getTargetX :: SwizzledLink -> Number
  getTargetX l = l.target.x

  getTargetY :: SwizzledLink -> Number
  getTargetY l = l.target.y

  -- Compute stroke color based on interconnectivity
  forceStrokeFn :: SwizzledLink -> String
  forceStrokeFn link =
    let score = linkInterconnectivity link.source link.target
    in linkStrokeColor score

  -- Compute stroke width based on interconnectivity
  forceWidthFn :: SwizzledLink -> Number
  forceWidthFn link =
    let score = linkInterconnectivity link.source link.target
    in linkStrokeWidth score

  -- Compute opacity based on interconnectivity
  forceOpacityFn :: SwizzledLink -> Number
  forceOpacityFn link =
    let score = linkInterconnectivity link.source link.target
    in linkOpacity score

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
showViewState (Treemap UsedOnly) = "Treemap (used)"
showViewState (Treemap ProjectOnly) = "Treemap (project)"
showViewState (Treemap ProjectAndLibraries) = "Treemap (all)"
showViewState (TreeLayout UsedOnly root) = "TreeLayout (used, root: " <> root <> ")"
showViewState (TreeLayout ProjectOnly root) = "TreeLayout (project, root: " <> root <> ")"
showViewState (TreeLayout ProjectAndLibraries root) = "TreeLayout (all, root: " <> root <> ")"
showViewState (ForceLayout UsedOnly root) = "ForceLayout (used, root: " <> root <> ")"
showViewState (ForceLayout ProjectOnly root) = "ForceLayout (project, root: " <> root <> ")"
showViewState (ForceLayout ProjectAndLibraries root) = "ForceLayout (all, root: " <> root <> ")"
showViewState (Neighborhood name) = "Neighborhood (" <> name <> ")"
showViewState (FunctionCalls name) = "FunctionCalls (" <> name <> ")"

-- | Toggle focus on a node's neighborhood
-- | Click once to drill down to neighborhood, click again to restore full view
toggleFocus :: SimNode -> Effect Unit
toggleFocus clickedNode = do
  focus <- Ref.read globalFocusRef
  mStateRef <- Ref.read globalStateRef

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

          -- Build neighborhood: clicked node + its targets + its sources
          let
            neighborhoodIds = Set.fromFoldable $
              [ clickedNode.id ] <> clickedNode.targets <> clickedNode.sources

          let neighborhoodNodes = Array.filter (\n -> Set.member n.id neighborhoodIds) currentNodes

          log $ "[Explorer] Focusing on node " <> show clickedNode.id
            <> " (neighborhood: "
            <> show (Array.length neighborhoodNodes)
            <> " nodes)"

          -- Update focus state
          Ref.write { focusedNodeId: Just clickedNode.id, fullNodes: nodesToStore } globalFocusRef

          -- Push current view to navigation stack before changing
          currentView <- Ref.read globalViewStateRef
          Ref.modify_ (Array.cons currentView) globalNavigationStackRef

          -- Update ViewState for neighborhood view and notify via callback
          let neighborhoodView = Neighborhood clickedNode.name
          Ref.write neighborhoodView globalViewStateRef
          notifyViewStateChanged neighborhoodView

          -- Update simulation and DOM
          focusOnNeighborhood neighborhoodNodes state.simulation

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
    liftEffect $ Ref.write functionCalls globalFunctionCallsRef

    -- Log fetch summary
    liftEffect $ log $ "[Explorer] Batch fetched: " <> show (Object.size declarations) <> " modules with declarations, "
      <> show (Object.size functionCalls)
      <> " function entries"

    -- Render bubble packs with fetched declarations
    liftEffect do
      for_ liveNodes \node -> do
        _ <- renderModulePackWithCallbacks declarations onDeclarationClick onDeclarationHover onDeclarationLeave node
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

-- | Tick callback for bubble pack mode
-- | Updates group transforms and link positions
bubblePackTick :: Effect Unit
bubblePackTick = do
  -- Update bubble pack group positions
  updateGroupPositions nodesGroupId

  -- Update link positions
  updateLinkPositions forceLinksGroupId

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

-- | Render links between neighborhood nodes (full graph edges, not tree)
renderNeighborhoodLinks :: Array SimNode -> Effect Unit
renderNeighborhoodLinks nodes = do
  allLinks <- Ref.read globalLinksRef

  -- Build set of node IDs in neighborhood
  let nodeIdSet = Set.fromFoldable $ map _.id nodes

  -- Filter to links where BOTH source and target are in neighborhood
  let
    neighborhoodLinks = Array.filter
      (\link -> Set.member link.source nodeIdSet && Set.member link.target nodeIdSet)
      allLinks

  log $ "[Explorer] Rendering " <> show (Array.length neighborhoodLinks) <> " neighborhood links"

  -- Swizzle: convert IDs to node references (by node.id lookup, not array index)
  let
    swizzled = swizzleLinksByIndex _.id nodes neighborhoodLinks \src tgt _i link ->
      { source: src, target: tgt, linkType: link.linkType }

  -- Render as straight lines
  _ <- runD3v2M $ renderNeighborhoodLinksD3 swizzled
  pure unit

-- | Render static links for bubble pack view (no simulation updates)
-- | Uses node x/y directly to create line coordinates
renderStaticNeighborhoodLinks :: Array SimNode -> Effect Unit
renderStaticNeighborhoodLinks nodes = do
  allLinks <- Ref.read globalLinksRef

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
  linksGroup <- select "#explorer-links"

  -- Convert links to static coordinates
  let staticLinks = Array.mapMaybe (toStaticLink nodeMap) links

  _ <- appendData Line staticLinks
    [ x1 (_.x1 :: StaticLink -> Number)
    , y1 (_.y1 :: StaticLink -> Number)
    , x2 (_.x2 :: StaticLink -> Number)
    , y2 (_.y2 :: StaticLink -> Number)
    , stroke (\(_ :: StaticLink) -> "white")
    , strokeWidth (\(_ :: StaticLink) -> 1.5)
    , opacity (\(_ :: StaticLink) -> 0.6)
    , class_ (\(_ :: StaticLink) -> "neighborhood-link")
    ]
    linksGroup

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
  linksGroup <- select "#explorer-links"

  -- Log connectivity statistics for debugging
  let scores = map (\link -> linkInterconnectivity link.source link.target) links
  liftEffect $ logConnectivityStats "Neighborhood links" scores

  _ <- appendData Line links
    [ x1 getSourceX
    , y1 getSourceY
    , x2 getTargetX
    , y2 getTargetY
    , stroke neighborhoodStrokeFn
    , strokeWidth neighborhoodWidthFn
    , opacity neighborhoodOpacityFn
    , class_ (\(_ :: NeighborhoodLink) -> "neighborhood-link")
    ]
    linksGroup

  pure unit
  where
  getSourceX :: NeighborhoodLink -> Number
  getSourceX l = l.source.x

  getSourceY :: NeighborhoodLink -> Number
  getSourceY l = l.source.y

  getTargetX :: NeighborhoodLink -> Number
  getTargetX l = l.target.x

  getTargetY :: NeighborhoodLink -> Number
  getTargetY l = l.target.y

  -- Compute stroke color based on interconnectivity
  neighborhoodStrokeFn :: NeighborhoodLink -> String
  neighborhoodStrokeFn link =
    let score = linkInterconnectivity link.source link.target
    in linkStrokeColor score

  -- Compute stroke width based on interconnectivity
  neighborhoodWidthFn :: NeighborhoodLink -> Number
  neighborhoodWidthFn link =
    let score = linkInterconnectivity link.source link.target
    in linkStrokeWidth score

  -- Compute opacity based on interconnectivity
  neighborhoodOpacityFn :: NeighborhoodLink -> Number
  neighborhoodOpacityFn link =
    let score = linkInterconnectivity link.source link.target
    in linkOpacity score

-- =============================================================================
-- Navigation
-- =============================================================================

-- | Navigate back to the previous view in the navigation stack
-- | Returns true if navigation happened, false if stack was empty
navigateBack :: Effect Boolean
navigateBack = do
  stack <- Ref.read globalNavigationStackRef
  case Array.uncons stack of
    Nothing -> do
      log "[Explorer] Navigation stack empty, nothing to go back to"
      pure false
    Just { head: previousView, tail: remainingStack } -> do
      log $ "[Explorer] Navigating back to: " <> showViewState previousView
      -- Pop the stack
      Ref.write remainingStack globalNavigationStackRef
      -- Restore the previous view
      restoreToView previousView
      pure true

-- | Restore to a specific view (used by navigateBack)
restoreToView :: ViewState -> Effect Unit
restoreToView targetView = do
  focus <- Ref.read globalFocusRef
  mStateRef <- Ref.read globalStateRef

  case mStateRef of
    Nothing -> log "[Explorer] No state ref to restore"
    Just stateRef -> do
      state <- Ref.read stateRef

      case targetView of
        Treemap _ -> do
          -- Re-entering treemap view (static, non-simulation)
          Ref.write targetView globalViewStateRef
          notifyViewStateChanged targetView
        TreeLayout _ _ -> restoreFullView focus.fullNodes targetView state.simulation
        ForceLayout _ _ -> restoreFullView focus.fullNodes targetView state.simulation
        Neighborhood _ -> do
          -- Re-entering a neighborhood - find the node and focus on it
          -- For now, just update the view state (the DOM is already showing neighborhood)
          Ref.write targetView globalViewStateRef
          notifyViewStateChanged targetView
        FunctionCalls _ -> do
          -- Re-entering function calls view
          Ref.write targetView globalViewStateRef
          notifyViewStateChanged targetView

-- | Restore full view
restoreFullView :: Array SimNode -> ViewState -> Scene.CESimulation -> Effect Unit
restoreFullView fullNodes targetView sim = do
  -- Update simulation with full nodes
  Sim.setNodes fullNodes sim

  -- Restore grid forces
  restoreGridForces fullNodes sim

  -- Clear neighborhood links and disable link updates
  clearTreeLinks

  -- Color legend is handled by Halogen NarrativePanel (switches back to packages automatically)

  -- Re-render DOM with circles
  clearNodesGroup
  _ <- runD3v2M $ renderNodesOnly targetView fullNodes

  -- Restore the original Scene tick handler (uses circle positions)
  mStateRef <- Ref.read globalStateRef
  case mStateRef of
    Just stateRef -> do
      Scene.clearLinksGroupId stateRef
      Sim.onTick (Scene.onTick stateRef) sim
    Nothing -> pure unit

  -- Update ViewState and notify via callback
  Ref.write targetView globalViewStateRef
  notifyViewStateChanged targetView

  -- Clear focus state
  Ref.write { focusedNodeId: Nothing, fullNodes: [] } globalFocusRef

  Sim.reheat sim
  pure unit

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
  nodesGroup <- select "#explorer-nodes"

  -- ViewState now passed as parameter (no global ref read)

  -- Capture the selection returned by appendData (don't discard it!)
  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (ColorPalette.getNodeFill currentView :: SimNode -> String)
    , stroke (ColorPalette.getNodeStroke currentView :: SimNode -> String)
    , strokeWidth 1.0 -- Slightly thicker stroke for visibility
    , class_ nodeClass
    ]
    nodesGroup

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
  notifyShowCallGraphPopup moduleName declarationName

-- | Handle hover on a declaration circle
-- | Highlights modules that contain callers/callees
onDeclarationHover :: DeclarationHoverCallback
onDeclarationHover moduleName declarationName _kind = do
  -- Look up function calls for this declaration
  fnCalls <- Ref.read globalFunctionCallsRef
  let key = moduleName <> "." <> declarationName
  case Object.lookup key fnCalls of
    Nothing -> pure unit -- No call data, no highlighting
    Just fnInfo -> do
      -- Extract unique module names from calls and calledBy
      let calleeModules = Array.nub $ map _.targetModule fnInfo.calls
      let callerModules = Array.nub $ Array.mapMaybe extractModuleName fnInfo.calledBy
      -- Highlight the modules
      highlightCallGraph moduleName callerModules calleeModules

-- Hover hint could be added via a globalHintRef if needed

-- | Handle mouse leave from a declaration
onDeclarationLeave :: Effect Unit
onDeclarationLeave = do
  clearCallGraphHighlight

-- | Extract module name from "Module.name" string
extractModuleName :: String -> Maybe String
extractModuleName fullName =
  let
    parts = splitAtLastDot fullName
  in
    if parts.module == "" then Nothing else Just parts.module
  where
  -- Split "Foo.Bar.baz" into { module: "Foo.Bar", name: "baz" }
  splitAtLastDot :: String -> { module :: String, name :: String }
  splitAtLastDot s =
    let
      len = stringLength s
      lastDotIdx = findLastDot s (len - 1)
    in
      if lastDotIdx < 0 then { module: "", name: s }
      else { module: substring 0 lastDotIdx s, name: substring (lastDotIdx + 1) len s }

  findLastDot :: String -> Int -> Int
  findLastDot _ (-1) = -1
  findLastDot str idx =
    if charAt idx str == "." then idx
    else findLastDot str (idx - 1)

  stringLength :: String -> Int
  stringLength = go 0
    where
    go n str = case charAt n str of
      "" -> n
      _ -> go (n + 1) str

  charAt :: Int -> String -> String
  charAt = charAtFFI

  substring :: Int -> Int -> String -> String
  substring = substringFFI

foreign import charAtFFI :: Int -> String -> String
foreign import substringFFI :: Int -> Int -> String -> String
