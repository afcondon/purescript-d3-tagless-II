-- | Scene Engine
-- |
-- | Compositional scene management with swappable tick engines.
-- |
-- | Two tick engines:
-- | - DumbEngine: Interpolates positions toward predetermined targets (transitions)
-- | - PhysicsEngine: D3 force simulation (stable states with dynamics)
-- |
-- | CSS transitions can run in parallel with either engine for opacity/color changes.
module Engine.Scene
  ( -- Re-exported from library
    module SimScene
  -- App-specific types
  , SceneState
  , CESimulation
  , NodeRow
  , LinkRow
  -- App-specific functions
  , applyRulesInPlace
  , mkSceneState
  , transitionTo
  , onTick
  , setLinksGroupId
  , clearLinksGroupId
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Class.Console (log)
import Foreign.Object as Object
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Render (GroupId, updateCirclePositions, updateLinkPositions)
import PSD3.Simulation.Scene
  ( NodeRule
  , SceneConfig
  , TransitionState
  , EngineMode(..)
  , ForceConfig
  , CSSConfig
  , PositionMap
  , applyRules
  ) as SimScene
import PSD3.Transition.Tick as Tick
import Types (SimNode, NodeType, LinkType)

-- FFI imports for in-place mutation
foreign import applyTransformWhereInPlace_
  :: (SimNode -> Boolean)
  -> (SimNode -> SimNode)
  -> Ref (Array SimNode)
  -> Effect Unit

-- =============================================================================
-- App-Specific Types
-- =============================================================================

-- | Node row type (matches SimNode extra fields)
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
  , isInTree :: Boolean
  )

type LinkRow = (linkType :: LinkType)

-- | Concrete simulation type
type CESimulation = Sim.Simulation NodeRow LinkRow

-- | Main scene state
-- | Uses SimNode specifically as this is the app's concrete state type
type SceneState =
  { simulation :: CESimulation
  , currentScene :: Maybe (SimScene.SceneConfig SimNode)
  , transition :: Maybe (SimScene.TransitionState SimNode)
  , nodesGroupId :: GroupId
  , linksGroupId :: Maybe GroupId  -- Set when force links should be updated
  }

-- =============================================================================
-- App-Specific Functions
-- =============================================================================

-- | Apply rules in place (mutates simulation nodes directly)
-- | Preserves object identity for D3 data binding
-- | Note: This stays app-specific due to FFI requirements
applyRulesInPlace :: Array (SimScene.NodeRule SimNode) -> CESimulation -> Effect Unit
applyRulesInPlace rules sim =
  traverse_ applyRule rules
  where
  applyRule :: SimScene.NodeRule SimNode -> Effect Unit
  applyRule rule = applyTransformWhereInPlace_ rule.select rule.apply sim.nodes

-- | Re-initialize all forces after node data changes
-- | D3 forces may cache values from nodes, so we need to tell them to re-read
reinitializeForces :: CESimulation -> Effect Unit
reinitializeForces sim = do
  nodes <- Ref.read sim.nodes
  forces <- Ref.read sim.forces
  for_ (Map.toUnfoldable forces :: Array (Tuple String Core.ForceHandle)) \(Tuple name handle) -> do
    _ <- Core.initializeForce handle nodes
    log $ "[Scene] Re-initialized force: " <> name

-- =============================================================================
-- State Management
-- =============================================================================

-- | Create initial scene state
mkSceneState
  :: CESimulation
  -> GroupId
  -> SceneState
mkSceneState sim groupId =
  { simulation: sim
  , currentScene: Nothing
  , transition: Nothing
  , nodesGroupId: groupId
  , linksGroupId: Nothing
  }

-- | Set the links group ID to enable force link updates
setLinksGroupId :: GroupId -> Ref SceneState -> Effect Unit
setLinksGroupId gid stateRef =
  Ref.modify_ (_ { linksGroupId = Just gid }) stateRef

-- | Clear the links group ID to disable force link updates
clearLinksGroupId :: Ref SceneState -> Effect Unit
clearLinksGroupId stateRef =
  Ref.modify_ (_ { linksGroupId = Nothing }) stateRef

-- =============================================================================
-- Transitions
-- =============================================================================

-- | Transition delta: ~2 seconds at 60fps
transitionDelta :: Tick.TickDelta
transitionDelta = Tick.ticksForDuration 2000

-- | Start transition to a new scene
transitionTo
  :: SimScene.SceneConfig SimNode
  -> Ref SceneState
  -> Effect Unit
transitionTo targetScene stateRef = do
  state <- Ref.read stateRef

  -- Don't transition if already transitioning
  case state.transition of
    Just _ -> log "[Scene] Already transitioning, ignoring"
    Nothing -> do
      log $ "[Scene] Starting transition to: " <> targetScene.name

      -- Get current positions
      nodes <- Sim.getNodes state.simulation
      let startPositions = capturePositions nodes

      -- Phase 1: Apply init rules in place (e.g., pin all nodes)
      applyRulesInPlace targetScene.initRules state.simulation

      -- Calculate target positions for transition (read nodes again after mutation)
      nodesAfterInit <- Sim.getNodes state.simulation
      let targetPositions = targetScene.layout nodesAfterInit

      -- Start CSS transition if configured
      case targetScene.cssTransition of
        Just css -> startCSSTransition css
        Nothing -> pure unit

      -- Set transition state
      Ref.write (state
        { transition = Just
            { targetScene
            , startPositions
            , targetPositions
            , progress: 0.0
            }
        }) stateRef

      -- Keep simulation ticking
      Sim.reheat state.simulation

-- | Capture current positions from nodes
capturePositions :: Array SimNode -> SimScene.PositionMap
capturePositions nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes

-- | Start a CSS transition (sets CSS custom properties or classes)
startCSSTransition :: SimScene.CSSConfig -> Effect Unit
startCSSTransition css = do
  log $ "[Scene] Starting CSS transition: " <> css.property <> " -> " <> css.targetValue
  -- TODO: Implement via FFI - set transition property and target value
  pure unit

-- =============================================================================
-- Tick Handler
-- =============================================================================

-- | Main tick handler - routes to appropriate engine
onTick
  :: Ref SceneState
  -> Effect Unit
onTick stateRef = do
  state <- Ref.read stateRef

  case state.transition of
    Just t -> runDumbEngine t stateRef state
    Nothing -> runStableEngine state

  -- Always update DOM
  updateCirclePositions state.nodesGroupId

  -- Update force links if active
  case state.linksGroupId of
    Just linksGid -> updateLinkPositions linksGid
    Nothing -> pure unit

-- | DumbEngine: Interpolate positions toward target
runDumbEngine
  :: SimScene.TransitionState SimNode
  -> Ref SceneState
  -> SceneState
  -> Effect Unit
runDumbEngine t stateRef state = do
  let newProgress = min 1.0 (t.progress + transitionDelta)

  if newProgress >= 1.0
    then completeTransition t stateRef state
    else do
      -- Interpolate positions
      let easedProgress = Tick.easeInOutCubic newProgress
      Sim.interpolatePositionsInPlace t.startPositions t.targetPositions easedProgress state.simulation

      -- Update progress
      Ref.write (state { transition = Just (t { progress = newProgress }) }) stateRef

-- | Complete a transition - enter the target scene's stable mode
completeTransition
  :: SimScene.TransitionState SimNode
  -> Ref SceneState
  -> SceneState
  -> Effect Unit
completeTransition t stateRef state = do
  log $ "[Scene] Transition complete: " <> t.targetScene.name

  -- Set final positions
  Sim.updatePositionsInPlace t.targetPositions state.simulation

  -- Phase 3: Apply final rules in place (unpin, set gridXY, etc.)
  nodes <- Sim.getNodes state.simulation
  let rules = t.targetScene.finalRules nodes  -- Build rules with context
  applyRulesInPlace rules state.simulation

  -- Log rule applications for debugging
  log $ "[Scene] Applied " <> show (Array.length rules) <> " final rules"

  -- Re-initialize forces to pick up new gridX/gridY values
  reinitializeForces state.simulation

  -- Enter stable mode
  case t.targetScene.stableMode of
    SimScene.Physics _ -> do
      log "[Scene] Entering Physics mode"
      Sim.reheat state.simulation
    SimScene.Static -> do
      log "[Scene] Entering Static mode"
      pure unit

  -- Update state
  Ref.write (state
    { currentScene = Just t.targetScene
    , transition = Nothing
    }) stateRef

-- | Run stable engine based on current scene mode
runStableEngine :: SceneState -> Effect Unit
runStableEngine state = case state.currentScene of
  Nothing -> pure unit  -- No scene yet
  Just scene -> case scene.stableMode of
    SimScene.Physics _ -> pure unit  -- Simulation is already running
    SimScene.Static -> pure unit     -- Nothing to do
