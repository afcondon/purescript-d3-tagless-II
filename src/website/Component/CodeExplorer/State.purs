module PSD3.CodeExplorer.State where

import Prelude

import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, clusterSceneAttributes)
import D3.Viz.Spago.Files (D3_Radius, SpagoNodeRow, SpagoLinkData, SpagoLink)
import PSD3.Data.Node (D3_FocusXY, NodeID)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isPackage)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import PSD3v2.Selection.Types (SEmpty)
import Web.DOM.Element (Element)
import Data.Lens (Lens', _Just, view)
import Data.Lens.Record (prop)
import Data.Map (Map, keys) as M
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Set (Set)
import Data.Set as Set
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)
import Halogen.Subscription as HS
import PSD3.CodeExplorer.Actions (Action, Scene)
import PSD3.Component.SimulationState as SimState
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (SimulationVariables, readSimulationVariables_)
import PSD3.Internal.Simulation.Types (Force, _handle)
import PSD3.Internal.Types (Datum_)
import PSD3v2.Simulation.Scene as Scene
import Type.Proxy (Proxy(..))

-- ============================================================================
-- | State Type - Specialized from Generic SimulationState
-- ============================================================================

-- | CodeExplorer state - specializes the generic SimulationComponentState
-- | with Spago-specific types
type State = SimState.SimulationComponentState Scene (Action SpagoSimNode) NodeID (SpagoNodeRow (D3_FocusXY (D3_Radius ()))) SpagoLinkData SpagoSceneAttributes SpagoModel

-- | Transition matrix type alias for convenience
type TransitionMatrix = SimState.TransitionMatrix Scene

-- | CodeExplorer's scene configuration - specialized version of library's SceneConfig
-- | Parameterized with Spago-specific node data row, ID type, link row, and attributes
type SceneConfig = Scene.SimSceneConfig (SpagoNodeRow (D3_FocusXY (D3_Radius ()))) NodeID SpagoLinkData SpagoSceneAttributes

-- ============================================================================
-- | Visualization-Specific Initialization
-- ============================================================================

initialScene :: M.Map Label (Force SpagoSimNode) -> SceneConfig
initialScene forceLibrary = {
    chooseNodes: isPackage -- chooses all nodes
  , linksShown:  const false
  , linksActive: const false
  , activeForces: Set.fromFoldable (M.keys forceLibrary)  -- Start with all forces enabled
  , cssClass: ""
  , attributes: clusterSceneAttributes
  , nodeInitializerFunctions: []
  , transitionConfig: Nothing  -- Instant transition for initial scene
}

-- ============================================================================
-- | Generic Lenses (re-exported from SimState for convenience)
-- ============================================================================

_model :: forall a r. Lens' { model :: a | r } a
_model = SimState._model

_staging :: forall a r. Lens' { staging :: a | r } a
_staging = SimState._staging

_scene :: forall a r. Lens' { scene :: a | r } a
_scene = SimState._scene

_nodes :: forall a r. Lens' { nodes :: a | r } a
_nodes = SimState._nodes

_links :: forall a r. Lens' { links :: a | r } a
_links = SimState._links

_forces :: forall a r. Lens' { forces :: a | r } a
_forces = SimState._forces

_linksWithForce :: forall a r. Lens' { linksWithForce :: a | r } a
_linksWithForce = SimState._linksWithForce

_rawdata :: forall a r. Lens' { rawdata :: a | r } a
_rawdata = SimState._rawdata

_enterselections :: forall a r. Lens' { selections :: a | r } a
_enterselections = SimState._enterselections

-- ============================================================================
-- | Spago-Specific Lenses
-- ============================================================================

-- lenses for mise-en-scene things
_activeForces :: Lens' State (Set Label)
_activeForces = _scene <<< prop (Proxy :: Proxy "activeForces")
_chooseNodes :: Lens' State (SpagoSimNode -> Boolean)
_chooseNodes = _scene <<< prop (Proxy :: Proxy "chooseNodes")
_linksShown :: Lens' State (SpagoLink -> Boolean)
_linksShown = _scene <<< prop (Proxy :: Proxy "linksShown")
_linksActive :: Lens' State (SpagoLink -> Boolean)
_linksActive = _scene <<< prop (Proxy :: Proxy "linksActive")
-- _sceneForces              = _scene <<< _forces
_cssClass :: Lens' State String
_cssClass = _scene <<< prop (Proxy :: Proxy "cssClass")
_sceneAttributes :: Lens' State SpagoSceneAttributes
_sceneAttributes = _scene <<< prop (Proxy :: Proxy "attributes")
_eventListener :: Lens' State (Maybe (HS.Listener (Action SpagoSimNode)))
_eventListener = prop (Proxy :: Proxy "eventListener")
_nodeInitializerFunctions :: forall p.
  Strong p =>
  p (Array (Array SpagoSimNode -> Array SpagoSimNode)) (Array (Array SpagoSimNode -> Array SpagoSimNode)) ->
  p State State
_nodeInitializerFunctions = _scene <<< prop (Proxy :: Proxy "nodeInitializerFunctions")

_modelNodes :: forall p.
     Strong p
  => Choice p
  => p (Array SpagoSimNode) (Array SpagoSimNode)
  -> p State State
_modelNodes = _model <<< _Just <<< _nodes

_modelLinks :: forall p.
     Strong p
  => Choice p
  => p (Array SpagoLink) (Array SpagoLink)
  -> p State State
_modelLinks = _model <<< _Just <<< _links

_stagingNodes :: forall p.
     Strong p
  => Choice p
  => p (Array SpagoSimNode) (Array SpagoSimNode)
  -> p State State
_stagingNodes = _staging <<< _rawdata <<< _nodes

_stagingLinks :: forall p.
     Strong p
  => Choice p
  => p (Array SpagoLink) (Array SpagoLink)
  -> p State State
_stagingLinks = _staging <<< _rawdata <<< _links

_stagingLinkFilter :: forall p.
  Strong p =>
  p (SpagoLink -> Boolean) (SpagoLink -> Boolean) ->
  p State State
_stagingLinkFilter = _staging <<< _linksWithForce

getSimulationVariables :: State -> SimulationVariables
getSimulationVariables state = do
  let handle = view _handle state
  readSimulationVariables_ handle

-- ============================================================================
-- | Scene Management (re-exported from SimState with Spago-specific types)
-- ============================================================================

updateScene :: (SceneConfig -> SceneConfig) -> State -> State
updateScene = SimState.updateScene

applySceneConfig :: SceneConfig -> State -> State
applySceneConfig = SimState.applySceneConfig

applySceneWithTransition :: Scene -> SceneConfig -> State -> State
applySceneWithTransition = SimState.applySceneWithTransition

-- ============================================================================
-- | Scene Field Setters (typed for Spago)
-- ============================================================================

setChooseNodes :: (SpagoSimNode -> Boolean) -> State -> State
setChooseNodes = SimState.setChooseNodes

setLinksShown :: (SpagoLink -> Boolean) -> State -> State
setLinksShown = SimState.setLinksShown

setLinksActive :: (SpagoLink -> Boolean) -> State -> State
setLinksActive = SimState.setLinksActive

setActiveForces :: Set Label -> State -> State
setActiveForces = SimState.setActiveForces

setCssClass :: String -> State -> State
setCssClass = SimState.setCssClass

setSceneAttributes :: SpagoSceneAttributes -> State -> State
setSceneAttributes = SimState.setSceneAttributes

setNodeInitializers :: Array (Array SpagoSimNode -> Array SpagoSimNode) -> State -> State
setNodeInitializers = SimState.setNodeInitializers

toggleForce :: Label -> State -> State
toggleForce = SimState.toggleForce

-- ============================================================================
-- | Model/Staging Accessors (typed for Spago)
-- ============================================================================

getModelNodes :: State -> Array SpagoSimNode
getModelNodes state = case state.model of
  Just m -> m.nodes
  Nothing -> []

getModelLinks :: State -> Array SpagoLink
getModelLinks state = case state.model of
  Just m -> m.links
  Nothing -> []

getStagingNodes :: State -> Array SpagoSimNode
getStagingNodes = SimState.getStagingNodes

getStagingLinks :: State -> Array SpagoLink
getStagingLinks = SimState.getStagingLinks

getStagingLinkFilter :: State -> (SpagoLink -> Boolean)
getStagingLinkFilter = SimState.getStagingLinkFilter

-- | Get selections from staging, unwrapping Maybe for v2 API
-- | This assumes selections have been initialized (which happens in Initialize action)
-- | SAFETY: After Initialize action runs, selections are guaranteed to be Just
-- | If called before initialization, crashes with clear error message
-- | Note: runSimulationFromState expects both selections to have SimulationNode datum type
getSelections :: State -> { nodes :: D3v2Selection_ SEmpty Element SpagoSimNode, links :: D3v2Selection_ SEmpty Element SpagoSimNode }
getSelections state = case state.staging.selections.nodes, state.staging.selections.links of
  Just nodes, Just links -> { nodes: unsafeCoerce nodes, links: unsafeCoerce links }
  _, _ -> unsafeCrashWith "getSelections called before Initialize action! Selections must be initialized first."

-- ============================================================================
-- | Staging Setters (typed for Spago)
-- ============================================================================

setStagingNodes :: Array SpagoSimNode -> State -> State
setStagingNodes = SimState.setStagingNodes

setStagingLinks :: Array SpagoLink -> State -> State
setStagingLinks = SimState.setStagingLinks

setStagingLinkFilter :: (SpagoLink -> Boolean) -> State -> State
setStagingLinkFilter = SimState.setStagingLinkFilter

-- ============================================================================
-- | Tag Management (re-exported from SimState, typed for Spago)
-- ============================================================================
-- |
-- | See PSD3.Component.SimulationState for full documentation of the tagging system.

tagNodes :: String -> (SpagoSimNode -> Boolean) -> Array SpagoSimNode -> State -> State
tagNodes label predicate nodes state = SimState.tagNodes _.id label predicate nodes state

untagNodes :: String -> Array SpagoSimNode -> State -> State
untagNodes label nodes state = SimState.untagNodes _.id label nodes state

clearTag :: String -> State -> State
clearTag = SimState.clearTag

clearAllTags :: State -> State
clearAllTags = SimState.clearAllTags

getNodeTags :: NodeID -> State -> Set String
getNodeTags = SimState.getNodeTags

nodeHasTag :: String -> NodeID -> State -> Boolean
nodeHasTag = SimState.nodeHasTag

