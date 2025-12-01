-- | Code Explorer - Clean Implementation
-- |
-- | Exemplary code showcasing PSD3 library capabilities.
-- | Supports Grid and Tree scenes with smooth tick-driven transitions.
-- |
-- | KEY INSIGHT: Simulation tick drives EVERYTHING - positions AND transitions.
-- | All visual properties interpolated using library primitives.
-- | Efficient pattern: appendData (bind once) + mutation (update in place)
module Viz.SpagoGridTest
  ( initSpagoGridTest
  , transitionToScene
  , globalExplorerRef
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Viz.SpagoGridTest.GridLayout as GridLayout
import Viz.SpagoGridTest.TreeLayout as TreeLayout
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect.Unsafe (unsafePerformEffect)
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
import Data.Int (toNumber, floor)
import Data.Loader (loadModel, LoadedModel)
import PSD3.Scale (interpolateTurbo)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Render (GroupId(..), updateCirclePositions)
import PSD3.Transition.Tick as Tick
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns)
import Types (SimNode, NodeType(..), LinkType, Scene(..))
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- | Extra fields beyond SimulationNode (id, x, y, vx, vy, fx, fy are in SimulationNode)
type NodeRow =
  ( name :: String
  , nodeType :: NodeType
  , package :: String
  , r :: Number
  , cluster :: Int
  , targets :: Array Int
  , sources :: Array Int
  , gridX :: Number
  , gridY :: Number
  , orbitAngle :: Number
  , treeX :: Number
  , treeY :: Number
  )

type LinkRow = (linkType :: LinkType)

type SpagoSimulation = Sim.Simulation NodeRow LinkRow

-- | Transition state during scene changes
-- | Stores both start and target positions for interpolation
type TransitionState =
  { targetScene :: Scene
  , progress :: Tick.Progress -- 0.0 to 1.0
  , startPositions :: Sim.PositionMap -- snapshot at transition start
  , targetPositions :: Sim.PositionMap -- calculated for target scene
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

nodesGroupId :: GroupId
nodesGroupId = GroupId "#spago-test-nodes"

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
  log "[Explorer] BUILD: Using library Sim.interpolatePositionsInPlace for fast transitions"

  -- Recalculate grid positions based on viewBox dimensions
  let gridNodes = GridLayout.recalculateGridPositions model.nodes model.packageCount

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

  -- Get nodes from simulation for initial render
  simNodes <- Sim.getNodes sim

  -- Render SVG and bind data ONCE
  _ <- runD3v2M $ renderSVG containerSelector simNodes

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
      let
        { x: pkgX, y: pkgY } = case Object.lookup (show n.cluster) packagePositions of
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

  case state.transition of
    Nothing ->
      -- Not transitioning: simulation updates positions, just render
      pure unit

    Just t -> do
      let newProgress = min 1.0 (t.progress + transitionDelta)

      if newProgress >= 1.0 then do
        -- Transition complete
        log $ "[Explorer] Transition complete: " <> show t.targetScene

        -- Finalize positions
        case t.targetScene of
          Grid -> do
            -- Set final positions and unpin (let forces take over)
            Sim.updatePositionsInPlace t.targetPositions state.simulation
            Sim.unpinNodesInPlace state.simulation
          _ -> do
            -- Pin at final positions (no forces for Tree/Orbit)
            Sim.pinNodesAtPositions t.targetPositions state.simulation

        -- Clear transition, update scene
        Ref.write
          ( state
              { currentScene = t.targetScene
              , transition = Nothing
              }
          )
          stateRef

      else do
        -- Interpolate positions in place (mutates simulation nodes)
        let easedProgress = Tick.easeInOutCubic newProgress
        Sim.interpolatePositionsInPlace t.startPositions t.targetPositions easedProgress state.simulation

        -- Update progress
        Ref.write (state { transition = Just (t { progress = newProgress }) }) stateRef

  -- Always update DOM positions from (possibly mutated) node data
  updateCirclePositions nodesGroupId

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

    -- Pin all nodes at current positions
    Sim.pinNodesInPlace state.simulation

    -- Start transition
    Ref.write
      ( state
          { transition = Just
              { targetScene
              , progress: 0.0
              , startPositions
              , targetPositions
              }
          }
      )
      stateRef

    -- Keep simulation ticking during transition
    Sim.reheat state.simulation

-- | Calculate target positions for a scene
calculateTargetPositions :: Scene -> Array SimNode -> Sim.PositionMap
calculateTargetPositions scene nodes = case scene of
  Grid -> GridLayout.calculateGridPositions nodes
  Tree -> TreeLayout.calculateTreePositions nodes
  _ -> Object.empty

-- =============================================================================
-- Rendering (bind data ONCE at initialization)
-- =============================================================================

renderSVG :: String -> Array SimNode -> D3v2M { nodeSel :: D3v2Selection_ SBoundOwns Element SimNode }
renderSVG containerSelector nodes = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ viewBox (show ((-GridLayout.viewBoxWidth) / 2.0) <> " " <> show ((-GridLayout.viewBoxHeight) / 2.0) <> " " <> show GridLayout.viewBoxWidth <> " " <> show GridLayout.viewBoxHeight)
    , id_ "spago-test-svg"
    , class_ "ce-viz"
    ]
    container

  zoomGroup <- appendChild Group [ id_ "spago-test-zoom-group" ] svg
  _ <- appendChild Group [ id_ "spago-test-links" ] zoomGroup
  nodesGroup <- appendChild Group [ id_ "spago-test-nodes" ] zoomGroup

  -- Bind node data ONCE - these same objects will be mutated during transitions
  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (nodeColor :: SimNode -> String)
    , stroke "#fff"
    , strokeWidth 0.5
    ]
    nodesGroup

  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#spago-test-zoom-group") svg

  pure { nodeSel }

-- | Color by package cluster using Turbo interpolator
nodeColor :: SimNode -> String
nodeColor n =
  let
    t = numMod (toNumber n.cluster * 0.618033988749895) 1.0
  in
    interpolateTurbo t

-- | Floating point modulo
numMod :: Number -> Number -> Number
numMod a b = a - b * toNumber (floor (a / b))
