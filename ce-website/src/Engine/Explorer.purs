-- | Code Explorer using Scene Engine
-- |
-- | Clean implementation using the compositional scene architecture.
-- | Each scene is a self-contained config; transitions are handled by the Engine.
module Engine.Explorer
  ( initExplorer
  , goToScene
  , reloadWithProject
  , globalStateRef
  , globalLinksRef
  , globalViewStateRef
  , globalModelInfoRef
  , globalNavigationStackRef
  , navigateBack
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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
import Engine.AtomicView (renderAtomicView)
import Engine.BubblePack (renderModulePackWithCallbacks, highlightCallGraph, clearCallGraphHighlight, DeclarationClickCallback, DeclarationHoverCallback)
-- NarrativePanel is now a Halogen component (Component.NarrativePanel)
-- It polls globalViewStateRef and globalModelInfoRef directly
import Engine.ViewState (ViewState(..), ScopeFilter(..))
import Engine.Treemap as Treemap
import Engine.Treemap (recalculateTreemapPositions, renderWatermark, clearWatermark)
import PSD3v2.Tooltip (hideTooltip) as Tooltip
import Engine.Scene as Scene
import Engine.Scenes as Scenes
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Links (swizzleLinks, swizzleLinksByIndex)
import PSD3.ForceEngine.Render (GroupId(..), updateGroupPositions, updateLinkPositions)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.Scale (interpolateHsl, schemeTableau10At)
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
import Viz.SpagoGridTest.GridLayout as GridLayout
import Viz.SpagoGridTest.TreeLinks (radialLinkPath)
import Web.DOM.Element (Element, classList, toNode)
import Web.DOM.Node as Node
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)

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
globalViewStateRef = unsafePerformEffect $ Ref.new (PackageGrid ProjectAndLibraries)

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
  let initialView = PackageGrid ProjectAndLibraries
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

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodesWithFix sim

  -- Add forces
  addGridForces nodesWithFix sim

  -- Get nodes from simulation for initial render
  simNodes <- Sim.getNodes sim

  -- Render SVG structure first
  _ <- runD3v2M $ renderSVG containerSelector simNodes

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

  -- Mark GridRun as current scene (we're already in Grid state with forces, just not via transition)
  Ref.modify_ (_ { currentScene = Just Scenes.gridRunScene }) stateRef

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
  _ <- runD3v2M $ renderNodesOnly simNodes

  -- Update watermark for new project
  clearWatermark
  renderWatermark model.nodes

  -- Reset to GridRun scene
  Ref.modify_ (_ { currentScene = Just Scenes.gridRunScene }) stateRef

  -- Restart simulation
  Sim.start state.simulation

-- | Go to a named scene
goToScene :: String -> Ref Scene.SceneState -> Effect Unit
goToScene sceneName stateRef = do
  let
    mScene = case sceneName of
      "GridForm" -> Just Scenes.gridFormScene
      "GridRun" -> Just Scenes.gridRunScene
      "OrbitForm" -> Just Scenes.orbitFormScene
      "OrbitRun" -> Just Scenes.orbitRunScene
      "TreeForm" -> Just Scenes.treeFormScene
      "TreeRun" -> Just Scenes.treeRunScene
      _ -> Nothing
  case mScene of
    Just scene -> do
      -- Handle TreeForm: render tree bezier links
      when (sceneName == "TreeForm") do
        state <- Ref.read stateRef
        nodes <- Sim.getNodes state.simulation
        clearTreeLinks -- Clear any existing links
        renderTreeLinks nodes
        setTreeSceneClass true

      -- Handle TreeRun: force-directed tree with link forces
      when (sceneName == "TreeRun") do
        state <- Ref.read stateRef
        nodes <- Sim.getNodes state.simulation
        links <- Ref.read globalLinksRef
        clearTreeLinks -- Remove any existing links
        addTreeForces nodes links state.simulation
        renderForceLinks nodes links -- Render straight line links
        Scene.setLinksGroupId forceLinksGroupId stateRef -- Enable link updates
        setTreeSceneClass true -- Keep packages/non-tree faded

      -- For Grid/Orbit scenes: restore grid forces, clear tree stuff, and re-render circles
      when (isGridOrOrbitScene sceneName) do
        state <- Ref.read stateRef
        nodes <- Sim.getNodes state.simulation
        restoreGridForces nodes state.simulation
        clearTreeLinks -- Clear any force links
        Scene.clearLinksGroupId stateRef -- Disable link updates
        setTreeSceneClass false

      Scene.transitionTo scene stateRef
    Nothing -> log $ "[Explorer] Unknown scene: " <> sceneName
  where
  isGridOrOrbitScene name = name == "GridForm" || name == "GridRun" || name == "OrbitForm" || name == "OrbitRun"

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

  -- Link force - tight binding (distance=0, strength=1 like Observable)
  let linkHandle = Core.createLink { distance: 0.0, strength: 1.0, iterations: 1 }
  _ <- Core.initializeLinkForce linkHandle nodes treeLinks
  Ref.modify_ (Map.insert "link" linkHandle) sim.forces

  -- Many-body (charge) force - repulsion between all nodes
  let manyBodyHandle = Core.createManyBody { strength: -50.0, theta: 0.9, distanceMin: 1.0, distanceMax: 1.0e10 }
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

renderSVG :: String -> Array SimNode -> D3v2M { nodeSel :: D3v2Selection_ SBoundOwns Element SimNode }
renderSVG containerSelector nodes = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ viewBox (show ((-GridLayout.viewBoxWidth) / 2.0) <> " " <> show ((-GridLayout.viewBoxHeight) / 2.0) <> " " <> show GridLayout.viewBoxWidth <> " " <> show GridLayout.viewBoxHeight)
    , id_ "explorer-svg"
    , class_ "ce-viz"
    ]
    container

  zoomGroup <- appendChild Group [ id_ "explorer-zoom-group" ] svg
  _ <- appendChild Group [ id_ "treemap-watermark", class_ "watermark" ] zoomGroup -- Watermark first (behind)
  _ <- appendChild Group [ id_ "explorer-links" ] zoomGroup
  nodesGroup <- appendChild Group [ id_ "explorer-nodes" ] zoomGroup

  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (nodeFill :: SimNode -> String)
    , stroke (nodeColor :: SimNode -> String) -- Use color for stroke
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

-- | Stroke color - packages use their cluster color, modules use white
nodeColor :: SimNode -> String
nodeColor n = case n.nodeType of
  PackageNode -> schemeTableau10At n.cluster  -- Package colors from palette
  ModuleNode -> "rgba(255, 255, 255, 0.9)"   -- White for modules

-- | Fill color - packages use cluster colors, modules are white (hollow if unused)
nodeFill :: SimNode -> String
nodeFill n = case n.nodeType of
  PackageNode -> schemeTableau10At n.cluster  -- Solid package color
  ModuleNode ->
    if n.isInTree
      then "rgba(255, 255, 255, 0.8)"  -- Solid white for used modules
      else "none"  -- Hollow for unused modules (blueprint style)

numMod :: Number -> Number -> Number
numMod a b = a - b * toNumber (floor (a / b))

-- | CSS class based on node type for styling and transitions
nodeClass :: SimNode -> String
nodeClass n = case n.nodeType of
  PackageNode -> "node package-node"
  ModuleNode
    | n.isInTree -> "node tree-module"
    | otherwise -> "node non-tree-module"

-- =============================================================================
-- Tree Link Rendering
-- =============================================================================

-- | Render tree links as radial bezier paths
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

  -- Append path elements for each tree link
  _ <- appendData Path links
    [ d (linkPathFn nodeMap)
    , fill (\(_ :: SimLink) -> "none")
    , stroke (\(_ :: SimLink) -> "white")
    , strokeWidth (\(_ :: SimLink) -> 1.5)
    , opacity (\(_ :: SimLink) -> 0.5)
    , class_ (\(_ :: SimLink) -> "tree-link")
    ]
    linksGroup

  pure unit

-- | Generate path string for a link
linkPathFn :: Map.Map Int SimNode -> SimLink -> String
linkPathFn nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      radialLinkPath sourceNode.treeX sourceNode.treeY targetNode.treeX targetNode.treeY
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

  -- Append line elements for each link
  _ <- appendData Line links
    [ x1 getSourceX
    , y1 getSourceY
    , x2 getTargetX
    , y2 getTargetY
    , stroke (\(_ :: SwizzledLink) -> "white")
    , strokeWidth (\(_ :: SwizzledLink) -> 1.0)
    , opacity (\(_ :: SwizzledLink) -> 0.6)
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

-- | Handle Back button click from narrative panel
-- | Now delegates to navigateBack which uses the navigation stack
handleBackButton :: Effect Unit
handleBackButton = do
  log "[Explorer] Back button clicked"
  _ <- navigateBack
  pure unit

-- | Handle control change from TangleJS-style narrative panel
-- | controlId: which control was changed (e.g., "layout", "scope")
-- | newValue: the new value selected (e.g., "orbit", "project")
handleControlChange :: String -> String -> Effect Unit
handleControlChange controlId newValue = do
  log $ "[Explorer] Control changed: " <> controlId <> " -> " <> newValue
  currentView <- Ref.read globalViewStateRef

  -- Compute the new view based on control change
  let newView = applyControlChange controlId newValue currentView

  -- Update ViewState (Halogen NarrativePanel polls this)
  Ref.write newView globalViewStateRef

  -- Trigger scene transitions using proper two-phase engine
  mStateRef <- Ref.read globalStateRef
  case mStateRef of
    Just stateRef -> do
      -- Use Scene.transitionTo for proper DumbEngine → Physics handoff
      case controlId, newView of
        "layout", PackageGrid _ -> do
          log "[Explorer] Transitioning to grid scene"
          Scene.transitionTo Scenes.gridRunScene stateRef

        "layout", ModuleOrbit _ -> do
          log "[Explorer] Transitioning to orbit scene"
          Scene.transitionTo Scenes.orbitRunScene stateRef

        "layout", DependencyTree _ -> do
          log "[Explorer] Transitioning to tree scene"
          Scene.transitionTo Scenes.treeRunScene stateRef

        _, _ -> pure unit -- Other controls don't trigger scene transitions

    Nothing -> log "[Explorer] No scene state to update"

  log $ "[Explorer] New view state: " <> showViewState newView

-- | Apply a control change to compute the new ViewState
applyControlChange :: String -> String -> ViewState -> ViewState
applyControlChange "layout" newLayout currentView =
  case newLayout, currentView of
    "grid", ModuleTreemap scope -> PackageGrid scope
    "grid", PackageGrid scope -> PackageGrid scope
    "grid", ModuleOrbit scope -> PackageGrid scope
    "grid", DependencyTree scope -> PackageGrid scope
    "orbit", ModuleTreemap scope -> ModuleOrbit scope
    "orbit", PackageGrid scope -> ModuleOrbit scope
    "orbit", ModuleOrbit scope -> ModuleOrbit scope
    "orbit", DependencyTree scope -> ModuleOrbit scope
    "tree", ModuleTreemap scope -> DependencyTree scope
    "tree", PackageGrid scope -> DependencyTree scope
    "tree", ModuleOrbit scope -> DependencyTree scope
    "tree", DependencyTree scope -> DependencyTree scope
    _, other -> other -- No change for other views

applyControlChange "scope" newScope currentView =
  let
    scope = if newScope == "project" then ProjectOnly else ProjectAndLibraries
  in
    case currentView of
      ModuleTreemap _ -> ModuleTreemap scope
      PackageGrid _ -> PackageGrid scope
      ModuleOrbit _ -> ModuleOrbit scope
      DependencyTree _ -> DependencyTree scope
      other -> other -- Neighborhood/FunctionCalls don't have scope control

applyControlChange _ _ view = view -- Unknown control, no change

-- | Show ViewState for logging
showViewState :: ViewState -> String
showViewState (ModuleTreemap ProjectOnly) = "ModuleTreemap (project only)"
showViewState (ModuleTreemap ProjectAndLibraries) = "ModuleTreemap (all)"
showViewState (PackageGrid ProjectOnly) = "PackageGrid (project only)"
showViewState (PackageGrid ProjectAndLibraries) = "PackageGrid (all)"
showViewState (ModuleOrbit ProjectOnly) = "ModuleOrbit (project only)"
showViewState (ModuleOrbit ProjectAndLibraries) = "ModuleOrbit (all)"
showViewState (DependencyTree ProjectOnly) = "DependencyTree (project only)"
showViewState (DependencyTree ProjectAndLibraries) = "DependencyTree (all)"
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

          -- Update ViewState for neighborhood view (Halogen NarrativePanel polls this)
          let neighborhoodView = Neighborhood clickedNode.name
          Ref.write neighborhoodView globalViewStateRef

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

  -- Update simulation with neighborhood nodes
  Sim.setNodes nodes sim

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

  _ <- appendData Line links
    [ x1 getSourceX
    , y1 getSourceY
    , x2 getTargetX
    , y2 getTargetY
    , stroke (\(_ :: NeighborhoodLink) -> "white")
    , strokeWidth (\(_ :: NeighborhoodLink) -> 1.5)
    , opacity (\(_ :: NeighborhoodLink) -> 0.6)
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
        ModuleTreemap _ -> do
          -- Re-entering treemap view (static, non-simulation)
          Ref.write targetView globalViewStateRef
        PackageGrid _ -> restoreFullView focus.fullNodes targetView state.simulation
        ModuleOrbit _ -> restoreFullView focus.fullNodes targetView state.simulation
        DependencyTree _ -> restoreFullView focus.fullNodes targetView state.simulation
        Neighborhood _ -> do
          -- Re-entering a neighborhood - find the node and focus on it
          -- For now, just update the view state (the DOM is already showing neighborhood)
          Ref.write targetView globalViewStateRef
        FunctionCalls _ -> do
          -- Re-entering function calls view
          Ref.write targetView globalViewStateRef

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
  _ <- runD3v2M $ renderNodesOnly fullNodes

  -- Restore the original Scene tick handler (uses circle positions)
  mStateRef <- Ref.read globalStateRef
  case mStateRef of
    Just stateRef -> do
      Scene.clearLinksGroupId stateRef
      Sim.onTick (Scene.onTick stateRef) sim
    Nothing -> pure unit

  -- Update ViewState (Halogen NarrativePanel polls globalViewStateRef)
  Ref.write targetView globalViewStateRef

  -- Clear focus state
  Ref.write { focusedNodeId: Nothing, fullNodes: [] } globalFocusRef

  Sim.reheat sim
  pure unit

-- | Set forces appropriate for neighborhood view (spread out, no grid)
setNeighborhoodForces :: Array SimNode -> Scene.CESimulation -> Effect Unit
setNeighborhoodForces nodes sim = do
  -- Clear existing forces
  Ref.write Map.empty sim.forces

  -- Many-body repulsion to spread nodes out
  let manyBodyHandle = Core.createManyBody { strength: -200.0, theta: 0.9, distanceMin: 1.0, distanceMax: 1.0e10 }
  _ <- Core.initializeForce manyBodyHandle nodes
  Ref.modify_ (Map.insert "charge" manyBodyHandle) sim.forces

  -- Collision to prevent overlap
  let collideHandle = Core.createCollideGrid 10.0 0.7 1
  _ <- Core.initializeForce collideHandle nodes
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  -- Centering force to keep neighborhood on screen
  let forceXHandle = Core.createForceX { x: 0.0, strength: 0.05 }
  _ <- Core.initializeForce forceXHandle nodes
  Ref.modify_ (Map.insert "x" forceXHandle) sim.forces

  let forceYHandle = Core.createForceY { y: 0.0, strength: 0.05 }
  _ <- Core.initializeForce forceYHandle nodes
  Ref.modify_ (Map.insert "y" forceYHandle) sim.forces

  log "[Explorer] Neighborhood forces set (charge, collide, center)"

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
renderNodesOnly :: Array SimNode -> D3v2M Unit
renderNodesOnly nodes = do
  nodesGroup <- select "#explorer-nodes"

  -- Capture the selection returned by appendData (don't discard it!)
  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (nodeFill :: SimNode -> String)
    , stroke (nodeColor :: SimNode -> String) -- Use color for stroke
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
-- | Shows the function call graph centered on the clicked declaration
onDeclarationClick :: DeclarationClickCallback
onDeclarationClick moduleName declarationName kind = do
  log $ "[Explorer] Declaration clicked: " <> moduleName <> "." <> declarationName <> " (kind: " <> kind <> ")"

  -- Only look up function calls for values (functions)
  -- Types, data, typeSynonyms etc. don't have runtime call data
  if kind /= "value" then
    log $ "[Explorer] Skipping - " <> kind <> " declarations don't have call data"
  else do
    -- Look up function calls for this declaration
    fnCalls <- Ref.read globalFunctionCallsRef
    let key = moduleName <> "." <> declarationName
    case Object.lookup key fnCalls of
      Nothing -> log $ "[Explorer] No function call data for: " <> key
      Just fnInfo -> do
        log $ "[Explorer] Found " <> show (Array.length fnInfo.calls) <> " outgoing calls, " <> show (Array.length fnInfo.calledBy) <> " callers"
        -- Show atomic force tree visualization
        renderAtomicView fnInfo

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
