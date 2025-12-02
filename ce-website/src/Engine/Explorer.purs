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
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Data.Loader (loadModel, LoadedModel)
import Engine.Scene as Scene
import Engine.Scenes as Scenes
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Links (swizzleLinks)
import PSD3.ForceEngine.Render (GroupId(..))
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types as FT
import PSD3.Scale (interpolateTurbo)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, viewBox, d, opacity, x1, x2, y1, y2)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns)
import Types (SimNode, SimLink, NodeType(..), LinkType, isTreeLink)
import Viz.SpagoGridTest.GridLayout as GridLayout
import Viz.SpagoGridTest.TreeLinks (radialLinkPath)
import Web.DOM.Element (Element, classList)
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
  log "[Explorer] BUILD: 2025-12-02 15:00 - Tree4 with force links"
  log "[Explorer] Loading data..."
  result <- loadModel
  case result of
    Left err -> liftEffect $ log $ "[Explorer] Error: " <> err
    Right model -> do
      liftEffect $ log $ "[Explorer] Loaded: " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
      -- Store links for later use
      liftEffect $ Ref.write model.links globalLinksRef
      stateRef <- liftEffect $ initWithModel model containerSelector
      liftEffect $ Ref.write (Just stateRef) globalStateRef
      liftEffect $ log "[Explorer] Ready"

-- | Initialize with loaded model
initWithModel :: LoadedModel -> String -> Effect (Ref Scene.SceneState)
initWithModel model containerSelector = do
  log "[Explorer] Initializing with new Scene Engine"

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

  -- Mark Grid as current scene (we're already in Grid state, just not via transition)
  Ref.modify_ (_ { currentScene = Just Scenes.gridScene }) stateRef

  pure stateRef

-- =============================================================================
-- Public API
-- =============================================================================

-- | Go to a named scene
goToScene :: String -> Ref Scene.SceneState -> Effect Unit
goToScene sceneName stateRef = do
  let mScene = case sceneName of
        "Grid" -> Just Scenes.gridScene
        "Tree1" -> Just Scenes.tree1Scene
        "Tree2" -> Just Scenes.tree2Scene
        "Tree3" -> Just Scenes.tree3Scene
        "Tree4" -> Just Scenes.tree4Scene
        "Tree5" -> Just Scenes.tree5Scene
        _ -> Nothing
  case mScene of
    Just scene -> do
      -- Handle tree-specific setup when entering Tree2
      when (sceneName == "Tree2") do
        state <- Ref.read stateRef
        nodes <- Sim.getNodes state.simulation
        renderTreeLinks nodes
        -- Add tree-scene class to trigger CSS fade
        setTreeSceneClass true

      -- Handle Tree4: force-directed tree with link forces
      when (sceneName == "Tree4") do
        state <- Ref.read stateRef
        nodes <- Sim.getNodes state.simulation
        links <- Ref.read globalLinksRef
        clearTreeLinks  -- Remove the bezier tree links
        addTreeForces nodes links state.simulation
        renderForceLinks nodes links  -- Render straight line links
        Scene.setLinksGroupId forceLinksGroupId stateRef  -- Enable link updates
        setTreeSceneClass true  -- Keep packages/non-tree faded

      -- Remove tree-scene class and restore grid forces when leaving tree scenes
      when (sceneName == "Grid" || sceneName == "Tree1") do
        state <- Ref.read stateRef
        nodes <- Sim.getNodes state.simulation
        restoreGridForces nodes state.simulation
        clearTreeLinks  -- Clear any force links
        Scene.clearLinksGroupId stateRef  -- Disable link updates
        setTreeSceneClass false

      Scene.transitionTo scene stateRef
    Nothing -> log $ "[Explorer] Unknown scene: " <> sceneName

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
      let { x: pkgX, y: pkgY } = case Object.lookup (show n.cluster) packagePositions of
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
    , class_ nodeClass  -- CSS class for type-based styling/transitions
    ]
    nodesGroup

  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#explorer-zoom-group") svg

  pure { nodeSel }

-- | Color by package cluster
nodeColor :: SimNode -> String
nodeColor n =
  let t = numMod (toNumber n.cluster * 0.618033988749895) 1.0
  in interpolateTurbo t

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
-- Force Link Rendering (for Tree4)
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
  let swizzled = swizzleLinks nodes treeLinks \src tgt _i link ->
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

-- | FFI to clear all children from an element
foreign import clearElement :: Element -> Effect Unit

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
      if shouldAdd
        then DOMTokenList.add classes "tree-scene"
        else DOMTokenList.remove classes "tree-scene"
      log $ "[Explorer] Set tree-scene class: " <> show shouldAdd
    Nothing -> log "[Explorer] Could not find #explorer-nodes for CSS class"
