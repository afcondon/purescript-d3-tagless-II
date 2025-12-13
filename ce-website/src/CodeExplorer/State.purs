-- | Explorer State Management
-- |
-- | Global refs and callback system for the Code Explorer.
-- | This module centralizes all mutable state that needs to be shared
-- | across Explorer submodules.
module CodeExplorer.State
  ( -- * Global Refs
    globalStateRef
  , globalLinksRef
  , globalDeclarationsRef
  , globalFunctionCallsRef
  , globalModelInfoRef
  , globalViewStateRef
  , globalNavigationStackRef
  , globalFocusRef
  , globalTransitionRef
  , globalCallbacksRef

    -- * Types
  , FocusState
  , FocusInfo
  , ModelInfo
  , ExplorerCallbacks
  , NodeClickEvent

    -- * Constants
  , nodesGroupId
  , forceLinksGroupId
  , treeLinksGroupId

    -- * Callback Notifications
  , notifyViewStateChanged
  , notifyModelLoaded
  , notifyShowCallGraphPopup
  , notifyTransitionComplete
  , notifyFocusChanged
  , notifyNavigationPush
  , notifyNodeClicked
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import CodeExplorer.Scene as Scene
import CodeExplorer.ViewState (ViewState(..), OverviewView(..))
import CodeExplorer.ViewTransition as VT
import Data.Loader (DeclarationsMap, FunctionCallsMap)
import PSD3.ForceEngine.Render (GroupId(..))
import Types (SimNode, SimLink, NodeType)

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
globalViewStateRef = unsafePerformEffect $ Ref.new (Overview TreemapView)

-- | Navigation stack for zoom out (history of previous views)
-- | Stack grows from left: newest at head, oldest at tail
globalNavigationStackRef :: Ref (Array ViewState)
globalNavigationStackRef = unsafePerformEffect $ Ref.new []

-- | Focus state for neighborhood drill-down (internal)
type FocusState =
  { focusedNodeId :: Maybe Int -- Currently focused node (Nothing = full view)
  , fullNodes :: Array SimNode -- Original full node set for restoration
  , originView :: Maybe OverviewView -- The view we came from (Tree or Force)
  }

-- | Global ref for focus state
globalFocusRef :: Ref FocusState
globalFocusRef = unsafePerformEffect $ Ref.new { focusedNodeId: Nothing, fullNodes: [], originView: Nothing }

-- | Global ref for view transition state (GUP-style enter/exit/update)
globalTransitionRef :: Ref VT.TransitionState
globalTransitionRef = unsafePerformEffect $ Ref.new (VT.mkTransitionState VT.AllNodes [])

-- =============================================================================
-- Callback-Based Notification System
-- =============================================================================

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
    Nothing -> pure unit -- No callbacks registered, silent no-op

-- | Notify model loaded via callback (if set)
notifyModelLoaded :: ModelInfo -> Effect Unit
notifyModelLoaded modelInfo = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onModelLoaded modelInfo
    Nothing -> pure unit -- No callbacks registered, silent no-op

-- | Notify show call graph popup via callback (if set)
notifyShowCallGraphPopup :: String -> String -> Effect Unit
notifyShowCallGraphPopup moduleName declarationName = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onShowCallGraphPopup moduleName declarationName
    Nothing -> pure unit -- No callbacks registered, silent no-op

-- | Notify transition complete via callback (if set)
notifyTransitionComplete :: Effect Unit
notifyTransitionComplete = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onTransitionComplete
    Nothing -> pure unit -- No callbacks registered, silent no-op

-- | Notify focus changed via callback (if set)
notifyFocusChanged :: FocusInfo -> Effect Unit
notifyFocusChanged focusInfo = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onFocusChanged focusInfo
    Nothing -> pure unit -- No callbacks registered, silent no-op

-- | Notify navigation push via callback (if set)
notifyNavigationPush :: ViewState -> Effect Unit
notifyNavigationPush viewState = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onNavigationPush viewState
    Nothing -> pure unit -- No callbacks registered, silent no-op

-- | Notify node clicked via callback (if set)
-- | Halogen will decide what to do (drill down, unfocus, etc.)
notifyNodeClicked :: NodeClickEvent -> Effect Unit
notifyNodeClicked event = do
  mCallbacks <- Ref.read globalCallbacksRef
  case mCallbacks of
    Just cbs -> cbs.onNodeClicked event
    Nothing -> pure unit -- No callbacks registered, silent no-op

-- =============================================================================
-- Constants
-- =============================================================================

nodesGroupId :: GroupId
nodesGroupId = GroupId "#explorer-nodes"

forceLinksGroupId :: GroupId
forceLinksGroupId = GroupId "#explorer-links"

-- | Tree links group ID (same group, but for path elements with bezier curves)
treeLinksGroupId :: GroupId
treeLinksGroupId = GroupId "#explorer-links"
