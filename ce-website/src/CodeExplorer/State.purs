-- | Explorer State Management
-- |
-- | Global refs for the Code Explorer.
-- | This module centralizes all mutable state that needs to be shared
-- | across Explorer submodules.
-- |
-- | NOTE: Callbacks are now stored in SceneState (see Scene.purs).
-- | Types are re-exported here for backwards compatibility.
module CodeExplorer.State
  ( -- * Global Refs
    globalStateRef
    -- NOTE: Other global refs (links, declarations, functionCalls, viewState, etc.)
    -- have been moved into SceneState. Only globalStateRef remains as the single
    -- point of access to the Scene from D3 event handlers.

    -- * Types (re-exported from Scene.purs)
  , FocusState
  , FocusInfo
  , ModelInfo
  , ExplorerCallbacks
  , NodeClickEvent

    -- * Constants
  , nodesGroupId
  , forceLinksGroupId
  , treeLinksGroupId
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import CodeExplorer.Scene as Scene
import CodeExplorer.ViewState (OverviewView)
import PSD3.ForceEngine.Render (GroupId(..))
import Types (SimNode)

-- =============================================================================
-- Global State
-- =============================================================================

-- | Global ref for external access (UI buttons, etc.)
-- | This is the single point of access from D3 event handlers to the Scene.
-- | All other state (links, declarations, viewState, etc.) is accessed through SceneState.
globalStateRef :: Ref (Maybe (Ref Scene.SceneState))
globalStateRef = unsafePerformEffect $ Ref.new Nothing

-- | Focus state for neighborhood drill-down (internal)
-- | This type is used by Halogen (owns the state) and Scene (provides FocusInfo type)
type FocusState =
  { focusedNodeId :: Maybe Int -- Currently focused node (Nothing = full view)
  , fullNodes :: Array SimNode -- Original full node set for restoration
  , originView :: Maybe OverviewView -- The view we came from (Tree or Force)
  }

-- =============================================================================
-- Re-exported Types (from Scene.purs)
-- =============================================================================

-- | Focus state for neighborhood drill-down (exported for Halogen)
-- | Re-exported from Scene.purs for backwards compatibility
type FocusInfo = Scene.FocusInfo

-- | Node click event data - what D3 emits when a node is clicked
-- | Re-exported from Scene.purs for backwards compatibility
type NodeClickEvent = Scene.NodeClickEvent

-- | Callbacks for Explorer events
-- | Re-exported from Scene.purs for backwards compatibility
type ExplorerCallbacks = Scene.ExplorerCallbacks

-- | Model info for narrative panel
-- | Re-exported from Scene.purs for backwards compatibility
type ModelInfo = Scene.ModelInfo

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
