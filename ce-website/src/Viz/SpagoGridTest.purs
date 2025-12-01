-- | Code Explorer - Clean Implementation
-- |
-- | Exemplary code showcasing PSD3 library capabilities.
-- | Supports Grid and Tree scenes with smooth tick-driven transitions.
-- |
-- | KEY INSIGHT: Simulation tick drives EVERYTHING - positions AND transitions.
-- | All visual properties interpolated in PureScript using library primitives.
-- | NO custom FFI - uses only library functionality.
module Viz.SpagoGridTest
  ( initSpagoGridTest
  , transitionToScene
  , globalExplorerRef
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Int (toNumber, ceil, floor)
import Data.Loader (loadModel, LoadedModel)
import Data.Number (sqrt) as Num
import Data.Number (cos, sin) as Data.Number
import PSD3.Scale (interpolateTurbo)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Core as Core
import PSD3.Transition.Tick as Tick
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, on, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree as T
import Types (SimNode, SimLink, NodeType(..), LinkType, Scene(..))
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

type NodeRow = ( id :: Int, name :: String, nodeType :: NodeType, package :: String
               , r :: Number, cluster :: Int, targets :: Array Int, sources :: Array Int
               , gridX :: Number, gridY :: Number, orbitAngle :: Number
               , treeX :: Number, treeY :: Number
               , fx :: Nullable Number, fy :: Nullable Number )
type LinkRow = ( linkType :: LinkType )

type SpagoSimulation = Sim.Simulation NodeRow LinkRow

-- | Node ready for rendering with computed position
-- | During transition, x/y are interpolated; otherwise from simulation
type RenderNode =
  { node :: SimNode
  , renderX :: Number
  , renderY :: Number
  }

-- | Transition state during scene changes
-- | Stores both start and target positions for pure interpolation
type TransitionState =
  { targetScene :: Scene
  , progress :: Tick.Progress  -- 0.0 to 1.0
  , startPositions :: Object { x :: Number, y :: Number }   -- snapshot at transition start
  , targetPositions :: Object { x :: Number, y :: Number }  -- calculated for target scene
  }

-- | Main application state
type ExplorerState =
  { simulation :: SpagoSimulation
  , currentScene :: Scene
  , transition :: Maybe TransitionState
  }

-- | Transition delta: ~2 seconds at 60fps
transitionDelta :: Tick.TickDelta
transitionDelta = Tick.ticksForDuration 2000

-- | Global ref to store explorer state for external access (buttons, etc.)
globalExplorerRef :: Ref (Maybe (Ref ExplorerState))
globalExplorerRef = unsafePerformEffect $ Ref.new Nothing

-- =============================================================================
-- Constants
-- =============================================================================

viewBoxWidth :: Number
viewBoxWidth = 4000.0

viewBoxHeight :: Number
viewBoxHeight = 2500.0

-- =============================================================================
-- Initialization
-- =============================================================================

initSpagoGridTest :: String -> Effect Unit
initSpagoGridTest containerSelector = launchAff_ do
  log "[Explorer] Loading spago data..."
  result <- loadModel
  case result of
    Left err -> liftEffect $ log $ "[Explorer] Error: " <> err
    Right model -> do
      liftEffect $ log $ "[Explorer] Loaded: " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
      stateRef <- liftEffect $ initWithModel model containerSelector
      liftEffect $ Ref.write (Just stateRef) globalExplorerRef
      liftEffect $ log "[Explorer] State ref stored in global"

initWithModel :: LoadedModel -> String -> Effect (Ref ExplorerState)
initWithModel model containerSelector = do
  log "[Explorer] BUILD: Using library Tick module for transitions"

  -- Recalculate grid positions based on viewBox dimensions
  let gridNodes = recalculateGridPositions model.nodes model.packageCount

  -- Randomize module positions
  randomizedNodes <- randomizeModulePositions gridNodes
  let nodesWithFix = map fixPackagePosition randomizedNodes

  log $ "[SpagoGridTest] Nodes prepared: " <> show (Array.length nodesWithFix)

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodesWithFix sim

  -- Add Grid forces
  let collideHandle = Core.createCollideGrid 5.0 0.7 1
  _ <- Core.initializeForce collideHandle nodesWithFix
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  let forceXHandle = Core.createForceXGrid 0.5
  _ <- Core.initializeForce forceXHandle nodesWithFix
  Ref.modify_ (Map.insert "gridX" forceXHandle) sim.forces

  let forceYHandle = Core.createForceYGrid 0.5
  _ <- Core.initializeForce forceYHandle nodesWithFix
  Ref.modify_ (Map.insert "gridY" forceYHandle) sim.forces

  -- Render initial SVG container structure
  _ <- runD3v2M $ renderSVGContainer containerSelector

  -- Create state
  stateRef <- Ref.new
    { simulation: sim
    , currentScene: Grid
    , transition: Nothing
    }

  -- Set up tick callback - drives EVERYTHING
  Sim.onTick (onTick stateRef) sim

  -- Start simulation
  Sim.start sim
  log "[Explorer] Simulation started"

  pure stateRef

-- =============================================================================
-- Helpers
-- =============================================================================

fixPackagePosition :: SimNode -> SimNode
fixPackagePosition node = case node.nodeType of
  PackageNode -> node { fx = Nullable.notNull node.gridX, fy = Nullable.notNull node.gridY }
  ModuleNode -> node

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

-- =============================================================================
-- Tick Handler - THE HEART OF TICK-DRIVEN TRANSITIONS
-- =============================================================================

onTick :: Ref ExplorerState -> Effect Unit
onTick stateRef = do
  state <- Ref.read stateRef

  -- Get current node positions from simulation
  simNodes <- Sim.getNodes state.simulation

  -- Advance transition if active, build render nodes
  case state.transition of
    Nothing -> do
      -- Not transitioning: use simulation positions directly
      let renderNodes = map (\n -> { node: n, renderX: n.x, renderY: n.y }) simNodes
      renderVisualization renderNodes

    Just t -> do
      let newProgress = min 1.0 (t.progress + transitionDelta)

      if newProgress >= 1.0
        then do
          -- Transition complete
          log $ "[Explorer] Transition complete: " <> show t.targetScene

          -- Build render nodes at final positions
          let renderNodes = map (finalizeNodePosition t) simNodes

          -- Update simulation nodes to target positions (for next scene)
          let finalizedNodes = map (applyFinalPosition t) simNodes
          Sim.setNodes finalizedNodes state.simulation

          -- Clear transition, update scene
          Ref.write (state
            { currentScene = t.targetScene
            , transition = Nothing
            }) stateRef

          -- For Grid: unpin nodes so forces take over
          -- For Tree: keep nodes pinned at final positions
          case t.targetScene of
            Grid -> Sim.reheat state.simulation
            _ -> pure unit

          renderVisualization renderNodes

        else do
          -- Interpolate positions using library functions
          let easedProgress = Tick.easeInOutCubic newProgress
          let renderNodes = map (interpolateNodePosition t easedProgress) simNodes

          -- Update progress
          Ref.write (state { transition = Just (t { progress = newProgress }) }) stateRef

          renderVisualization renderNodes

-- | Interpolate a node's render position between start and target
interpolateNodePosition :: TransitionState -> Tick.Progress -> SimNode -> RenderNode
interpolateNodePosition t easedProgress node =
  let
    nodeKey = show node.id
    startPos = Object.lookup nodeKey t.startPositions
    targetPos = Object.lookup nodeKey t.targetPositions
  in case startPos, targetPos of
    Just s, Just tgt ->
      { node
      , renderX: Tick.lerp s.x tgt.x easedProgress
      , renderY: Tick.lerp s.y tgt.y easedProgress
      }
    _, _ ->
      -- Fallback to current position
      { node, renderX: node.x, renderY: node.y }

-- | Finalize node position at end of transition
finalizeNodePosition :: TransitionState -> SimNode -> RenderNode
finalizeNodePosition t node =
  let nodeKey = show node.id
  in case Object.lookup nodeKey t.targetPositions of
    Just tgt -> { node, renderX: tgt.x, renderY: tgt.y }
    Nothing -> { node, renderX: node.x, renderY: node.y }

-- | Apply final position to node (for updating simulation state)
applyFinalPosition :: TransitionState -> SimNode -> SimNode
applyFinalPosition t node =
  let nodeKey = show node.id
  in case Object.lookup nodeKey t.targetPositions of
    Just tgt ->
      case t.targetScene of
        Grid -> node { x = tgt.x, y = tgt.y, fx = Nullable.null, fy = Nullable.null }
        _ -> node { x = tgt.x, y = tgt.y, fx = Nullable.notNull tgt.x, fy = Nullable.notNull tgt.y }
    Nothing -> node

-- =============================================================================
-- Scene Transitions
-- =============================================================================

transitionToScene :: Scene -> Ref ExplorerState -> Effect Unit
transitionToScene targetScene stateRef = do
  state <- Ref.read stateRef

  when (state.currentScene /= targetScene && state.transition == Nothing) do
    log $ "[Explorer] Starting transition: " <> show state.currentScene <> " → " <> show targetScene

    -- Get current positions as start positions
    nodes <- Sim.getNodes state.simulation
    let startPositions = Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes

    -- Calculate target positions for new scene
    let targetPositions = calculateTargetPositions targetScene nodes

    -- Start transition
    Ref.write (state
      { transition = Just
          { targetScene
          , progress: 0.0
          , startPositions
          , targetPositions
          }
      }) stateRef

    -- Keep simulation ticking during transition
    Sim.reheat state.simulation

-- | Calculate target positions for a scene
calculateTargetPositions :: Scene -> Array SimNode -> Object { x :: Number, y :: Number }
calculateTargetPositions scene nodes = case scene of
  Grid -> calculateGridPositions nodes
  Tree -> calculateTreePositions nodes
  _ -> Object.empty

calculateGridPositions :: Array SimNode -> Object { x :: Number, y :: Number }
calculateGridPositions nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.gridX, y: n.gridY }) nodes

calculateTreePositions :: Array SimNode -> Object { x :: Number, y :: Number }
calculateTreePositions nodes =
  let
    packages = Array.filter (\n -> n.nodeType == PackageNode) nodes
    packageCount = Array.length packages

    ringRadius = 800.0
    packagePositions = Array.mapWithIndex (\i pkg ->
      let
        angle = 2.0 * pi * toNumber i / toNumber packageCount
        x = cos angle * ringRadius
        y = sin angle * ringRadius
      in Tuple (show pkg.id) { x, y }
    ) packages

    packagePosMap = Object.fromFoldable packagePositions

    modulePositions = Array.mapMaybe (\n ->
      if n.nodeType == ModuleNode
        then case Object.lookup (show n.cluster) packagePosMap of
          Just { x: px, y: py } ->
            let
              offsetAngle = toNumber n.id * 0.3
              offsetDist = 50.0 + toNumber (n.id `mod` 100) * 0.5
              mx = px + cos offsetAngle * offsetDist
              my = py + sin offsetAngle * offsetDist
            in Just (Tuple (show n.id) { x: mx, y: my })
          Nothing -> Nothing
        else Nothing
    ) nodes
  in
    Object.fromFoldable (packagePositions <> modulePositions)
  where
  pi = 3.14159265358979
  cos = Data.Number.cos
  sin = Data.Number.sin

-- =============================================================================
-- Rendering (Declarative Tree API)
-- =============================================================================

renderSVGContainer :: String -> D3v2M Unit
renderSVGContainer containerSelector = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ viewBox (show ((-viewBoxWidth) / 2.0) <> " " <> show ((-viewBoxHeight) / 2.0) <> " " <> show viewBoxWidth <> " " <> show viewBoxHeight)
    , id_ "spago-test-svg"
    , class_ "ce-viz"
    ] container

  zoomGroup <- appendChild Group [ id_ "spago-test-zoom-group" ] svg
  _ <- appendChild Group [ id_ "spago-test-links" ] zoomGroup
  _ <- appendChild Group [ id_ "spago-test-nodes" ] zoomGroup

  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#spago-test-zoom-group") svg

  pure unit

-- | Render visualization with current render nodes
renderVisualization :: Array RenderNode -> Effect Unit
renderVisualization renderNodes = do
  runD3v2M $ renderScene renderNodes

-- | Render the scene using Tree API
renderScene :: Array RenderNode -> D3v2M Unit
renderScene renderNodes = do
  nodesGroup <- select "#spago-test-nodes"
  let nodesTree = createNodesTree renderNodes
  _ <- renderTree nodesGroup nodesTree
  pure unit

-- | Create nodes tree with positions from render nodes
createNodesTree :: Array RenderNode -> T.Tree (Array RenderNode)
createNodesTree renderNodes =
  T.sceneNestedJoin "circles" "circle"
    [ renderNodes ]
    identity
    ( \rn -> T.elem Circle
        [ cx rn.renderX
        , cy rn.renderY
        , radius rn.node.r
        , fill (nodeColor rn.node)
        , stroke "#fff"
        , strokeWidth 0.5
        ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- | Color by package cluster using Turbo interpolator
nodeColor :: SimNode -> String
nodeColor n =
  let t = numMod (toNumber n.cluster * 0.618033988749895) 1.0
  in interpolateTurbo t

-- =============================================================================
-- Grid Layout
-- =============================================================================

recalculateGridPositions :: Array SimNode -> Int -> Array SimNode
recalculateGridPositions nodes packageCount =
  let
    aspect = viewBoxWidth / viewBoxHeight
    gridCols = ceil (Num.sqrt (toNumber packageCount * aspect))
    gridRows = ceil (toNumber packageCount / toNumber gridCols)

    margin = 0.1
    usableWidth = viewBoxWidth * (1.0 - 2.0 * margin)
    usableHeight = viewBoxHeight * (1.0 - 2.0 * margin)

    spacingX = usableWidth / toNumber gridCols
    spacingY = usableHeight / toNumber gridRows

    gridColsN = toNumber gridCols
    gridRowsN = toNumber gridRows

    updateNode node = case node.nodeType of
      PackageNode ->
        let
          idx = node.id
          row = idx / gridCols
          col = idx `mod` gridCols
          gx = (toNumber col - gridColsN / 2.0 + 0.5) * spacingX
          gy = (toNumber row - gridRowsN / 2.0 + 0.5) * spacingY
        in node { gridX = gx, gridY = gy, x = gx, y = gy }

      ModuleNode ->
        let
          pkgIdx = node.cluster
          pkgRow = pkgIdx / gridCols
          pkgCol = pkgIdx `mod` gridCols
          pkgX = (toNumber pkgCol - gridColsN / 2.0 + 0.5) * spacingX
          pkgY = (toNumber pkgRow - gridRowsN / 2.0 + 0.5) * spacingY
        in node { gridX = pkgX, gridY = pkgY, x = pkgX, y = pkgY }
  in
    map updateNode nodes

numMod :: Number -> Number -> Number
numMod a b = a - b * toNumber (floor (a / b))
