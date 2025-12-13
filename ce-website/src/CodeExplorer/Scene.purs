-- | Scene Engine
-- |
-- | App-specific wrapper around the library's generic scene engine.
-- |
-- | This module:
-- | - Creates an adapter for CESimulation to use with the library engine
-- | - Adds DOM update logic (updateCirclePositions, updateLinkPositions, etc.)
-- | - Adds ViewTransition integration
-- | - Tracks GroupIds for selective link updates
-- |
-- | The core transition logic (interpolation, rules, stable modes) is handled
-- | by PSD3.Scene.Engine in the library.
module CodeExplorer.Scene
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
  , onTickWithViewTransition
  , setLinksGroupId
  , clearLinksGroupId
  , setTreeLinksGroupId
  , clearTreeLinksGroupId
  , isTransitioning
  , setCurrentScene
  -- ViewState management (stored in SceneState)
  , getViewState
  , setSceneViewState
  , getTransitionState
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Class.Console (log)
import Foreign.Object as Object
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Render (GroupId, updateCirclePositions, updateLinkPositions, updateTreeLinkPaths)
import PSD3.Scene.Engine as Engine
import PSD3.Simulation.Scene
  ( NodeRule
  , SceneConfig
  , EngineMode(..)
  , PositionMap
  , applyRulesInPlace_
  ) as SimScene
import Types (SimNode, NodeType, LinkType)
import CodeExplorer.ViewState (ViewState)
import CodeExplorer.ViewTransition as VT

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
  , radialX :: Number
  , radialY :: Number
  , isInTree :: Boolean
  , topoX :: Number
  , topoY :: Number
  , topoLayer :: Int
  )

type LinkRow = (linkType :: LinkType)

-- | Concrete simulation type
type CESimulation = Sim.Simulation NodeRow LinkRow

-- | Main scene state
-- |
-- | Wraps the library's SceneEngine with app-specific concerns:
-- | - simulation: The D3 force simulation
-- | - engine: The library's generic scene engine
-- | - GroupId tracking for selective DOM updates
-- | - viewState: Current view for rendering (owned by Scene, synced from Halogen)
-- | - transitionState: GUP-style enter/exit progress (internal to Scene)
type SceneState =
  { simulation :: CESimulation
  , engine :: Engine.SceneEngine SimNode
  , nodesGroupId :: GroupId
  , linksGroupId :: Maybe GroupId
  , treeLinksGroupId :: Maybe GroupId
  , viewState :: ViewState
  , transitionState :: VT.TransitionState
  }

-- =============================================================================
-- Adapter for Library Engine
-- =============================================================================

-- | Create an engine adapter for CESimulation.
-- |
-- | This is the bridge between the generic library engine and our specific
-- | simulation type. See PSD3.Scene.Engine for the adapter interface.
mkAdapter :: CESimulation -> Engine.EngineAdapter SimNode
mkAdapter sim =
  { getNodes: Sim.getNodes sim
  , capturePositions: capturePositions
  , interpolatePositions: \start target progress ->
      Sim.interpolatePositionsInPlace start target progress sim
  , updatePositions: \positions ->
      Sim.updatePositionsInPlace positions sim
  , applyRulesInPlace: \rules -> applyRulesInPlace rules sim
  , reinitializeForces: reinitializeForces sim
  , reheat: Sim.reheat sim
  }

-- | Capture current positions from nodes
capturePositions :: Array SimNode -> SimScene.PositionMap
capturePositions nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes

-- =============================================================================
-- App-Specific Functions
-- =============================================================================

-- | Apply rules in place (mutates simulation nodes directly)
-- | Preserves object identity for D3 data binding
-- | Uses library's first-match-wins semantics (CSS-like cascade)
applyRulesInPlace :: Array (SimScene.NodeRule SimNode) -> CESimulation -> Effect Unit
applyRulesInPlace rules sim = do
  let _ = SimScene.applyRulesInPlace_ rules sim.nodes
  pure unit

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
-- | ViewState and initial nodes needed to set up transition state
mkSceneState
  :: CESimulation
  -> GroupId
  -> ViewState
  -> Array SimNode  -- Initial nodes for transition state
  -> Effect SceneState
mkSceneState sim groupId initialView initialNodes = do
  engine <- Engine.createEngine (mkAdapter sim)
  -- Initialize transition state with all nodes entering
  let initialViewNodes = VT.getViewNodes initialView
  let initialTransition = VT.mkTransitionState initialViewNodes initialNodes
  pure
    { simulation: sim
    , engine
    , nodesGroupId: groupId
    , linksGroupId: Nothing
    , treeLinksGroupId: Nothing
    , viewState: initialView
    , transitionState: initialTransition
    }

-- | Set the links group ID to enable force link updates
setLinksGroupId :: GroupId -> Ref SceneState -> Effect Unit
setLinksGroupId gid stateRef =
  Ref.modify_ (_ { linksGroupId = Just gid }) stateRef

-- | Clear the links group ID to disable force link updates
clearLinksGroupId :: Ref SceneState -> Effect Unit
clearLinksGroupId stateRef =
  Ref.modify_ (_ { linksGroupId = Nothing }) stateRef

-- | Set the tree links group ID to enable tree link path updates during transition
setTreeLinksGroupId :: GroupId -> Ref SceneState -> Effect Unit
setTreeLinksGroupId gid stateRef =
  Ref.modify_ (_ { treeLinksGroupId = Just gid }) stateRef

-- | Clear the tree links group ID to disable tree link path updates
clearTreeLinksGroupId :: Ref SceneState -> Effect Unit
clearTreeLinksGroupId stateRef =
  Ref.modify_ (_ { treeLinksGroupId = Nothing }) stateRef

-- | Get the current view state from scene state
getViewState :: Ref SceneState -> Effect ViewState
getViewState stateRef = do
  state <- Ref.read stateRef
  pure state.viewState

-- | Set the view state and compute new transition state
-- | This updates the internal state - the tick handler will apply transitions
setSceneViewState :: ViewState -> Ref SceneState -> Effect Unit
setSceneViewState newView stateRef = do
  state <- Ref.read stateRef
  allNodes <- Sim.getNodes state.simulation
  let newTransition = VT.computeTransition newView allNodes state.transitionState
  Ref.modify_ (_ { viewState = newView, transitionState = newTransition }) stateRef

-- | Get the current transition state (mainly for debugging/testing)
getTransitionState :: Ref SceneState -> Effect VT.TransitionState
getTransitionState stateRef = do
  state <- Ref.read stateRef
  pure state.transitionState

-- =============================================================================
-- Transitions (delegated to library engine)
-- =============================================================================

-- | Start transition to a new scene
-- |
-- | Delegates to the library engine which handles:
-- | - Init rules
-- | - Position interpolation
-- | - Final rules
-- | - Stable mode entry
transitionTo
  :: SimScene.SceneConfig SimNode
  -> Ref SceneState
  -> Effect Unit
transitionTo targetScene stateRef = do
  state <- Ref.read stateRef
  Engine.transitionTo targetScene state.engine

-- | Check if a transition is currently in progress
isTransitioning :: Ref SceneState -> Effect Boolean
isTransitioning stateRef = do
  state <- Ref.read stateRef
  Engine.isTransitioning state.engine

-- | Set the current scene directly (without transition).
-- |
-- | Use this when the visualization is already in the target state
-- | (e.g., after initial rendering with pre-computed positions).
setCurrentScene :: SimScene.SceneConfig SimNode -> Ref SceneState -> Effect Unit
setCurrentScene scene stateRef = do
  state <- Ref.read stateRef
  Engine.setCurrentScene scene state.engine

-- =============================================================================
-- Tick Handler
-- =============================================================================

-- | Main tick handler
-- |
-- | Delegates interpolation to library engine, then handles app-specific DOM updates.
onTick
  :: Ref SceneState
  -> Effect Unit
onTick stateRef = do
  state <- Ref.read stateRef

  -- Delegate to library engine (handles interpolation + rules)
  _ <- Engine.tick state.engine

  -- App-specific: Update DOM
  updateCirclePositions state.nodesGroupId

  -- Update force links if active
  case state.linksGroupId of
    Just linksGid -> updateLinkPositions linksGid
    Nothing -> pure unit

  -- Update tree link paths if active (during TreeForm transition)
  case state.treeLinksGroupId of
    Just treeLinksGid -> updateTreeLinkPaths treeLinksGid
    Nothing -> pure unit

-- =============================================================================
-- View Transition Integration
-- =============================================================================

-- | Tick handler that also advances view transitions (GUP-style enter/exit)
-- | This combines Scene.onTick with VT.tickTransitionState
-- | ViewState and TransitionState are now stored in SceneState
onTickWithViewTransition
  :: Ref SceneState
  -> String -- nodesGroupSelector for VT.applyViewTransition
  -> Effect Unit
onTickWithViewTransition sceneStateRef nodesSelector = do
  -- Read state (viewState and transitionState are now in SceneState)
  state <- Ref.read sceneStateRef
  let viewState = state.viewState

  -- Advance view transition progress
  let newViewTransition = VT.tickTransitionState VT.defaultDelta state.transitionState
  Ref.modify_ (_ { transitionState = newViewTransition }) sceneStateRef

  -- Get current nodes from simulation
  allNodes <- Sim.getNodes state.simulation

  -- Apply view transition visual updates (opacity, radius, remove exited)
  VT.applyViewTransition nodesSelector viewState allNodes newViewTransition

  -- Run normal scene tick (position updates)
  onTick sceneStateRef
