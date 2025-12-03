-- | Code Explorer using Scene Engine
-- |
-- | Clean implementation using the compositional scene architecture.
-- | Each scene is a self-contained config; transitions are handled by the Engine.
module Engine.Explorer
  ( initExplorer
  , goToScene
  , globalStateRef
  , globalLinksRef
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_, forkAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Data.Loader (loadModel, LoadedModel, DeclarationsMap, FunctionCallsMap, fetchFunctionCalls)
import Data.Traversable (for_)
import Engine.AtomicView (renderAtomicView)
import Engine.BubblePack (renderModulePackWithCallbacks, renderColorLegend, clearColorLegend, highlightCallGraph, clearCallGraphHighlight, DeclarationClickCallback, DeclarationHoverCallback)
import Engine.NarrativePanel as Narrative
import PSD3v2.Tooltip (hideTooltip) as Tooltip
import Engine.Scene as Scene
import Engine.Scenes as Scenes
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Links (swizzleLinks, swizzleLinksByIndex)
import PSD3.ForceEngine.Render (GroupId(..), updateGroupPositions, updateLinkPositions)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.Scale (interpolateTurbo)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, viewBox, d, opacity, x1, x2, y1, y2)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom, onMouseEnter, onMouseLeave, onClickWithDatum)
import PSD3v2.Capabilities.Selection (select, selectAll, appendChild, appendData, on)
import PSD3v2.Interpreter.D3v2 (getElementsD3v2) as D3v2
import PSD3v2.Classify (classifyElements, clearClasses)
import Data.Set as Set
import PSD3v2.Tooltip (onTooltip, onTooltipHide)
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
initExplorer containerSelector = launchAff_ do
  log "[Explorer] BUILD: 2025-12-03 - Deferred function calls loading"
  log "[Explorer] Loading data..."
  result <- loadModel
  case result of
    Left err -> liftEffect $ log $ "[Explorer] Error: " <> err
    Right model -> do
      liftEffect $ log $ "[Explorer] Loaded: " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
      -- Store links and declarations for later use
      liftEffect $ Ref.write model.links globalLinksRef
      liftEffect $ Ref.write model.declarations globalDeclarationsRef

      -- Initialize visualization immediately (don't wait for function calls)
      stateRef <- liftEffect $ initWithModel model containerSelector
      liftEffect $ Ref.write (Just stateRef) globalStateRef
      liftEffect $ log "[Explorer] Ready - loading function calls in background..."

      -- Load function calls data in a separate fiber (truly parallel)
      -- This doesn't block the main visualization from rendering
      _ <- forkAff do
        fnCallsResult <- fetchFunctionCalls
        case fnCallsResult of
          Left err -> liftEffect $ log $ "[Explorer] Warning: Could not load function calls: " <> err
          Right fnCalls -> do
            liftEffect $ Ref.write fnCalls globalFunctionCallsRef
            liftEffect $ log $ "[Explorer] Function calls loaded: " <> show (Object.size fnCalls) <> " functions"
      pure unit

-- | Initialize with loaded model
initWithModel :: LoadedModel -> String -> Effect (Ref Scene.SceneState)
initWithModel model containerSelector = do
  log "[Explorer] Initializing with new Scene Engine"

  -- Store model info for narrative
  Ref.write { projectName: "purescript-d3-dataviz", moduleCount: model.moduleCount, packageCount: model.packageCount } globalModelInfoRef

  -- Initialize narrative panel with Back button callback
  Narrative.initNarrativePanel handleBackButton

  -- Set color palette for the color key
  let colorPalette = Array.mapWithIndex mkPackageColorEntry model.packages
  Narrative.setColorPalette colorPalette

  -- Set initial full view narrative
  Narrative.setFullViewNarrative "purescript-d3-dataviz" model.moduleCount model.packageCount

  -- Recalculate grid positions
  let gridNodes = GridLayout.recalculateGridPositions model.nodes model.packageCount

  -- Randomize module starting positions
  randomizedNodes <- randomizeModulePositions gridNodes
  let nodesWithFix = map fixPackagePosition randomizedNodes

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodesWithFix sim

  -- Add forces
  addGridForces nodesWithFix sim

  -- Get nodes from simulation for initial render
  simNodes <- Sim.getNodes sim

  -- Render SVG
  _ <- runD3v2M $ renderSVG containerSelector simNodes

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

      -- For Grid/Orbit scenes: restore grid forces and clear tree stuff
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
  _ <- appendChild Group [ id_ "explorer-links" ] zoomGroup
  nodesGroup <- appendChild Group [ id_ "explorer-nodes" ] zoomGroup

  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (nodeColor :: SimNode -> String)
    , stroke "#fff"
    , strokeWidth 0.5
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

  -- Add clear highlight on mouse leave
  _ <- on (onMouseLeave \_ -> clearClasses "#explorer-nodes" "circle" highlightClasses) nodeSel

  -- Add tooltip on hover
  _ <- on (onTooltip formatNodeTooltip) nodeSel
  _ <- on onTooltipHide nodeSel

  -- Add click to focus on neighborhood
  _ <- on (onClickWithDatum toggleFocus) nodeSel

  pure { nodeSel }

-- | Color by package cluster
nodeColor :: SimNode -> String
nodeColor n =
  let
    t = numMod (toNumber n.cluster * 0.618033988749895) 1.0
  in
    interpolateTurbo t

numMod :: Number -> Number -> Number
numMod a b = a - b * toNumber (floor (a / b))

-- | Create a color entry for a package (for the narrative color key)
mkPackageColorEntry :: Int -> { name :: String, depends :: Array String, modules :: Array String } -> Narrative.ColorEntry
mkPackageColorEntry idx pkg =
  let
    t = numMod (toNumber idx * 0.618033988749895) 1.0
    color = interpolateTurbo t
  in
    { name: pkg.name, color }

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
    , stroke (\(_ :: SimLink) -> "#888")
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
    , stroke (\(_ :: SwizzledLink) -> "#666")
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
handleBackButton :: Effect Unit
handleBackButton = do
  log "[Explorer] Back button clicked"
  focus <- Ref.read globalFocusRef
  mStateRef <- Ref.read globalStateRef

  case mStateRef, focus.focusedNodeId of
    Just stateRef, Just _ -> do
      state <- Ref.read stateRef
      restoreFullView focus.fullNodes state.simulation
      Ref.write { focusedNodeId: Nothing, fullNodes: [] } globalFocusRef
    _, _ -> log "[Explorer] No focused state to go back from"

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
        -- Currently focused on this node - restore full view
        Just fid | fid == clickedNode.id -> do
          log $ "[Explorer] Unfocusing from node " <> show clickedNode.id
          restoreFullView focus.fullNodes state.simulation
          Ref.write { focusedNodeId: Nothing, fullNodes: [] } globalFocusRef

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

          -- Update narrative for neighborhood view
          Narrative.setNeighborhoodNarrative clickedNode.name (Array.length clickedNode.targets) (Array.length clickedNode.sources)

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

  -- Render bubble packs using FFI (binds SimNode to groups for tick updates)
  declarations <- Ref.read globalDeclarationsRef
  for_ liveNodes \node -> do
    _ <- renderModulePackWithCallbacks declarations onDeclarationClick onDeclarationHover onDeclarationLeave node
    pure unit

  -- Render color legend for bubble pack view
  renderColorLegend

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
    , stroke (\(_ :: StaticLink) -> "#666")
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
    , stroke (\(_ :: NeighborhoodLink) -> "#666")
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

-- | Restore full view
restoreFullView :: Array SimNode -> Scene.CESimulation -> Effect Unit
restoreFullView fullNodes sim = do
  -- Update simulation with full nodes
  Sim.setNodes fullNodes sim

  -- Restore grid forces
  restoreGridForces fullNodes sim

  -- Clear neighborhood links and disable link updates
  clearTreeLinks

  -- Clear color legend
  clearColorLegend

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

  -- Restore full view narrative
  modelInfo <- Ref.read globalModelInfoRef
  Narrative.setFullViewNarrative modelInfo.projectName modelInfo.moduleCount modelInfo.packageCount

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

  _ <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (nodeColor :: SimNode -> String)
    , stroke "#fff"
    , strokeWidth 0.5
    , class_ nodeClass
    ]
    nodesGroup

  -- Re-attach behaviors to new circles
  circlesSel <- select "#explorer-nodes circle"

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
    circlesSel
  _ <- on (onMouseLeave \_ -> clearClasses "#explorer-nodes" "circle" highlightClasses) circlesSel

  -- Re-attach tooltip
  _ <- on (onTooltip formatNodeTooltip) circlesSel
  _ <- on onTooltipHide circlesSel

  -- Re-attach click handler
  _ <- on (onClickWithDatum toggleFocus) circlesSel

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
      -- Update narrative hint
      Narrative.setDeclarationHoverNarrative moduleName declarationName (Array.length fnInfo.calls) (Array.length fnInfo.calledBy)

-- | Handle mouse leave from a declaration
onDeclarationLeave :: Effect Unit
onDeclarationLeave = do
  clearCallGraphHighlight
  Narrative.clearDeclarationHoverNarrative

-- | Extract module name from "Module.name" string
extractModuleName :: String -> Maybe String
extractModuleName fullName =
  let parts = splitAtLastDot fullName
  in if parts.module == "" then Nothing else Just parts.module
  where
  -- Split "Foo.Bar.baz" into { module: "Foo.Bar", name: "baz" }
  splitAtLastDot :: String -> { module :: String, name :: String }
  splitAtLastDot s =
    let len = stringLength s
        lastDotIdx = findLastDot s (len - 1)
    in if lastDotIdx < 0
       then { module: "", name: s }
       else { module: substring 0 lastDotIdx s, name: substring (lastDotIdx + 1) len s }

  findLastDot :: String -> Int -> Int
  findLastDot _ (-1) = -1
  findLastDot str idx =
    if charAt idx str == "."
    then idx
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
