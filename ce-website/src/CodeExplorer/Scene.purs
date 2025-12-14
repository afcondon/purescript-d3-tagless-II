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
  -- Callback types (for Halogen integration)
  , ModelInfo
  , FocusInfo
  , NodeClickEvent
  , ExplorerCallbacks
  -- App-specific types
  , SceneState
  , CESimulation
  , NodeRow
  , LinkRow
  , ModelData
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
  -- Model data access (stored in SceneState)
  , getLinks
  , getDeclarations
  , getFunctionCalls
  , setFunctionCalls
  , setLinks
  , setDeclarations
  -- Callback notifications (take Ref SceneState)
  , notifyViewStateChanged
  , notifyModelLoaded
  , notifyShowCallGraphPopup
  , notifyTransitionComplete
  , notifyFocusChanged
  , notifyNavigationPush
  , notifyNodeClicked
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
import Types (SimNode, SimLink, NodeType, LinkType)
import CodeExplorer.ViewState (ViewState, OverviewView)
import CodeExplorer.ViewTransition as VT
import Data.Loader (DeclarationsMap, FunctionCallsMap)

-- =============================================================================
-- Callback Types (for Halogen integration)
-- =============================================================================

-- | Model info for narrative panel
type ModelInfo =
  { projectName :: String
  , moduleCount :: Int
  , packageCount :: Int
  }

-- | Focus state for neighborhood drill-down (exported for Halogen)
type FocusInfo =
  { focusedNodeId :: Maybe Int  -- Currently focused node (Nothing = full view)
  , fullNodes :: Array SimNode  -- Original full node set for restoration
  , originView :: Maybe OverviewView  -- The view we came from (Tree or Force)
  }

-- | Node click event data - what D3 emits when a node is clicked
type NodeClickEvent =
  { nodeId :: Int
  , nodeName :: String
  , nodeType :: NodeType
  , topoLayer :: Int  -- For package neighborhood calculations
  }

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
  , onTransitionComplete :: Effect Unit
  -- ^ Called when a scene transition completes (for waypoint chaining)
  , onFocusChanged :: FocusInfo -> Effect Unit
  -- ^ Called when focus state changes (entering/exiting neighborhood)
  , onNavigationPush :: ViewState -> Effect Unit
  -- ^ Called to push current view to navigation stack before drilling down
  , onNodeClicked :: NodeClickEvent -> Effect Unit
  -- ^ Called when a node is clicked - Halogen decides what to do
  }

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
-- | - Model data (links, declarations, functionCalls) - immutable after load
-- | - callbacks: Halogen callbacks for event notifications
type SceneState =
  { simulation :: CESimulation
  , engine :: Engine.SceneEngine SimNode
  , nodesGroupId :: GroupId
  , linksGroupId :: Maybe GroupId
  , treeLinksGroupId :: Maybe GroupId
  , viewState :: ViewState
  , transitionState :: VT.TransitionState
  -- Model data (immutable after load)
  , links :: Array SimLink
  , declarations :: DeclarationsMap
  , functionCalls :: FunctionCallsMap
  -- Halogen callbacks
  , callbacks :: ExplorerCallbacks
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

-- | Model data record for creating scene state
type ModelData =
  { links :: Array SimLink
  , declarations :: DeclarationsMap
  }

-- | Create initial scene state
-- | ViewState and initial nodes needed to set up transition state
mkSceneState
  :: CESimulation
  -> GroupId
  -> ViewState
  -> Array SimNode  -- Initial nodes for transition state
  -> ModelData      -- Links and declarations from loaded model
  -> ExplorerCallbacks  -- Halogen callbacks for event notifications
  -> Effect SceneState
mkSceneState sim groupId initialView initialNodes modelData cbs = do
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
    , links: modelData.links
    , declarations: modelData.declarations
    , functionCalls: Object.empty  -- Populated lazily when entering neighborhood view
    , callbacks: cbs
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
-- Model Data Access
-- =============================================================================

-- | Get links from scene state
getLinks :: Ref SceneState -> Effect (Array SimLink)
getLinks stateRef = do
  state <- Ref.read stateRef
  pure state.links

-- | Get declarations from scene state
getDeclarations :: Ref SceneState -> Effect DeclarationsMap
getDeclarations stateRef = do
  state <- Ref.read stateRef
  pure state.declarations

-- | Get function calls from scene state
getFunctionCalls :: Ref SceneState -> Effect FunctionCallsMap
getFunctionCalls stateRef = do
  state <- Ref.read stateRef
  pure state.functionCalls

-- | Set function calls (called after lazy fetch in neighborhood view)
setFunctionCalls :: FunctionCallsMap -> Ref SceneState -> Effect Unit
setFunctionCalls fnCalls stateRef =
  Ref.modify_ (_ { functionCalls = fnCalls }) stateRef

-- | Set links (called during project reload)
setLinks :: Array SimLink -> Ref SceneState -> Effect Unit
setLinks newLinks stateRef =
  Ref.modify_ (_ { links = newLinks }) stateRef

-- | Set declarations (called during project reload)
setDeclarations :: DeclarationsMap -> Ref SceneState -> Effect Unit
setDeclarations newDeclarations stateRef =
  Ref.modify_ (_ { declarations = newDeclarations }) stateRef

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

-- =============================================================================
-- Callback Notifications
-- =============================================================================

-- | Notify ViewState change via callback
notifyViewStateChanged :: ViewState -> Ref SceneState -> Effect Unit
notifyViewStateChanged newView stateRef = do
  state <- Ref.read stateRef
  state.callbacks.onViewStateChanged newView

-- | Notify model loaded via callback
notifyModelLoaded :: ModelInfo -> Ref SceneState -> Effect Unit
notifyModelLoaded modelInfo stateRef = do
  state <- Ref.read stateRef
  state.callbacks.onModelLoaded modelInfo

-- | Notify show call graph popup via callback
notifyShowCallGraphPopup :: String -> String -> Ref SceneState -> Effect Unit
notifyShowCallGraphPopup moduleName declarationName stateRef = do
  state <- Ref.read stateRef
  state.callbacks.onShowCallGraphPopup moduleName declarationName

-- | Notify transition complete via callback
notifyTransitionComplete :: Ref SceneState -> Effect Unit
notifyTransitionComplete stateRef = do
  state <- Ref.read stateRef
  state.callbacks.onTransitionComplete

-- | Notify focus changed via callback
notifyFocusChanged :: FocusInfo -> Ref SceneState -> Effect Unit
notifyFocusChanged focusInfo stateRef = do
  state <- Ref.read stateRef
  state.callbacks.onFocusChanged focusInfo

-- | Notify navigation push via callback
notifyNavigationPush :: ViewState -> Ref SceneState -> Effect Unit
notifyNavigationPush viewState stateRef = do
  state <- Ref.read stateRef
  state.callbacks.onNavigationPush viewState

-- | Notify node clicked via callback
-- | Halogen will decide what to do (drill down, unfocus, etc.)
notifyNodeClicked :: NodeClickEvent -> Ref SceneState -> Effect Unit
notifyNodeClicked event stateRef = do
  state <- Ref.read stateRef
  state.callbacks.onNodeClicked event
