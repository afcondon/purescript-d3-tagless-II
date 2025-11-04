module PSD3.CodeExplorer.State where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Selection_, Datum_)
import PSD3.Simulation.Scene as Scene
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, clusterSceneAttributes)
import D3.Viz.Spago.Files (SpagoDataRow, SpagoGraphLinkID, SpagoLinkData)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isPackage)
import PSD3.Internal.FFI (SimulationVariables, readSimulationVariables_)
import PSD3.Data.Node (NodeID)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, _handle)
import PSD3.Capabilities.Simulation (Staging)
import Data.Array (filter, foldl)
import Data.Lens (Lens', _Just, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map, keys) as M
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Set (Set)
import Data.Set as Set
import Halogen.Subscription as HS
import PSD3.CodeExplorer.Actions (Action)
import PSD3.Data.Node (D3_SimulationNode(..))
import Type.Proxy (Proxy(..))
  
type State = {
  -- the simulationState manages the Nodes, Links, Forces, Selections, Ticks & simulation parameters
    simulation   :: D3SimulationState_
  -- the model should actually be a component, probably a hook so that it can be constructed by this component and not be a Maybe
  , model        :: Maybe SpagoModel
  -- we'll filter nodes/links to staging and then, if staging is valid (has selections) we will put this staging data in the simulation
  -- if there are updates to data they will be detected and handled by defensive copying in the FFI to ensure continuity of object references from links
  , staging      :: Staging D3Selection_ SpagoDataRow
-- | Contains all the settings necessary to call the Draw function
  , scene        :: MiseEnScene
-- | Event listener for D3→Halogen event flow (component infrastructure, not scene config)
  , eventListener :: Maybe (HS.Listener Action)
-- | Tags for nodes - persistent across scenes, set by ad-hoc predicates
-- | Maps NodeID to a set of tag strings. Tags automatically propagate to CSS classes.
-- | Example: Map.fromFoldable [(0, Set.fromFoldable ["package", "recent"]), (1, Set.fromFoldable ["module", "hot"])]
  , tags :: Map.Map NodeID (Set String)
-- | Show welcome overlay on first load
  , showWelcome :: Boolean
}

-- | CodeExplorer's scene configuration - specialized version of library's SceneConfig
-- | Parameterized with Spago-specific node data (SpagoDataRow) and attributes (SpagoSceneAttributes)
type SceneConfig = Scene.SceneConfig SpagoDataRow SpagoSceneAttributes

-- | DEPRECATED: Old name for SceneConfig, kept for backwards compatibility during refactor
type MiseEnScene = SceneConfig

initialScene :: M.Map Label Force -> SceneConfig
initialScene forceLibrary = {
    chooseNodes: isPackage -- chooses all nodes
  , linksShown:  const false
  , linksActive: const false
  , activeForces: Set.fromFoldable (M.keys forceLibrary)  -- Start with all forces enabled
  , cssClass: ""
  , attributes: clusterSceneAttributes
  , nodeInitializerFunctions: []
}

_model :: forall a r. Lens' { model :: a | r } a
_model = prop (Proxy :: Proxy "model")

_staging :: forall a r. Lens' { staging :: a | r } a
_staging = prop (Proxy :: Proxy "staging")

_scene :: forall a r. Lens' { scene :: a | r } a
_scene = prop (Proxy :: Proxy "scene")

_nodes :: forall a r. Lens' { nodes :: a | r } a
_nodes = prop (Proxy :: Proxy "nodes")

_links :: forall a r. Lens' { links :: a | r } a
_links = prop (Proxy :: Proxy "links")

_forces :: forall a r. Lens' { forces :: a | r } a
_forces = prop (Proxy :: Proxy "forces")

_linksWithForce :: forall a r. Lens' { linksWithForce :: a | r } a
_linksWithForce = prop (Proxy :: Proxy "linksWithForce")

_rawdata :: forall a r. Lens' { rawdata :: a | r } a
_rawdata = prop (Proxy :: Proxy "rawdata")

_enterselections :: forall a r. Lens' { selections :: a | r } a
_enterselections = prop (Proxy :: Proxy "selections")

-- lenses for mise-en-scene things
_activeForces :: Lens' State (Set Label)
_activeForces = _scene <<< prop (Proxy :: Proxy "activeForces")
_chooseNodes :: Lens' State (SpagoSimNode -> Boolean)
_chooseNodes = _scene <<< prop (Proxy :: Proxy "chooseNodes")
_linksShown :: Lens' State (SpagoGraphLinkID -> Boolean)
_linksShown = _scene <<< prop (Proxy :: Proxy "linksShown")
_linksActive :: Lens' State (Datum_ -> Boolean)
_linksActive = _scene <<< prop (Proxy :: Proxy "linksActive")
-- _sceneForces              = _scene <<< _forces
_cssClass :: Lens' State String
_cssClass = _scene <<< prop (Proxy :: Proxy "cssClass")
_sceneAttributes :: Lens' State SpagoSceneAttributes
_sceneAttributes = _scene <<< prop (Proxy :: Proxy "attributes")
_eventListener :: Lens' State (Maybe (HS.Listener Action))
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
  => p (Array SpagoGraphLinkID) (Array SpagoGraphLinkID)
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
  => p (Array SpagoGraphLinkID) (Array SpagoGraphLinkID)
  -> p State State
_stagingLinks = _staging <<< _rawdata <<< _links

_stagingLinkFilter :: forall p.
  Strong p =>
  p (Datum_ -> Boolean) (Datum_ -> Boolean) ->
  p State State
_stagingLinkFilter = _staging <<< _linksWithForce

getSimulationVariables :: State -> SimulationVariables
getSimulationVariables state = do
  let handle = view _handle state
  readSimulationVariables_ handle

-- Helper functions to replace lens-based scene configuration
-- These provide a simpler API without requiring lens knowledge

-- Update the entire scene configuration at once
updateScene :: (MiseEnScene -> MiseEnScene) -> State -> State
updateScene f state = state { scene = f state.scene }

-- Individual scene field updaters
setChooseNodes :: (SpagoSimNode -> Boolean) -> State -> State
setChooseNodes fn = updateScene (\s -> s { chooseNodes = fn })

setLinksShown :: (SpagoGraphLinkID -> Boolean) -> State -> State
setLinksShown fn = updateScene (\s -> s { linksShown = fn })

setLinksActive :: (Datum_ -> Boolean) -> State -> State
setLinksActive fn = updateScene (\s -> s { linksActive = fn })

setActiveForces :: Set Label -> State -> State
setActiveForces forces = updateScene (\s -> s { activeForces = forces })

setCssClass :: String -> State -> State
setCssClass css = updateScene (\s -> s { cssClass = css })

setSceneAttributes :: SpagoSceneAttributes -> State -> State
setSceneAttributes attrs = updateScene (\s -> s { attributes = attrs })

setNodeInitializers :: Array (Array SpagoSimNode -> Array SpagoSimNode) -> State -> State
setNodeInitializers fns = updateScene (\s -> s { nodeInitializerFunctions = fns })

toggleForce :: Label -> State -> State
toggleForce label = updateScene \s -> s {
  activeForces = if Set.member label s.activeForces
                 then Set.delete label s.activeForces
                 else Set.insert label s.activeForces
}

-- Getters for accessing nested state
getModelNodes :: State -> Array SpagoSimNode
getModelNodes state = case state.model of
  Just m -> m.nodes
  Nothing -> []

getModelLinks :: State -> Array SpagoGraphLinkID
getModelLinks state = case state.model of
  Just m -> m.links
  Nothing -> []

getStagingNodes :: State -> Array SpagoSimNode
getStagingNodes state = state.staging.rawdata.nodes

getStagingLinks :: State -> Array SpagoGraphLinkID
getStagingLinks state = state.staging.rawdata.links

getStagingLinkFilter :: State -> (Datum_ -> Boolean)
getStagingLinkFilter state = state.staging.linksWithForce

-- Update staging data
setStagingNodes :: Array SpagoSimNode -> State -> State
setStagingNodes nodes state = state { staging = state.staging { rawdata = state.staging.rawdata { nodes = nodes } } }

setStagingLinks :: Array SpagoGraphLinkID -> State -> State
setStagingLinks links state = state { staging = state.staging { rawdata = state.staging.rawdata { links = links } } }

setStagingLinkFilter :: (Datum_ -> Boolean) -> State -> State
setStagingLinkFilter fn state = state { staging = state.staging { linksWithForce = fn } }

-- | Apply a complete scene configuration in one step
-- | This is the new simplified API that eliminates the need for lens-based mutations.
-- |
-- | NOTE: This now just updates the scene configuration in state.
-- | Filtering and initialization happen inside runSimulation → SimulationM2.update.
-- | The old approach of pre-filtering in applySceneConfig was causing issues with
-- | tree scenes where initializers need access to the full dataset.
applySceneConfig :: SceneConfig -> State -> State
applySceneConfig config state = state { scene = config }

-- ============================================================================
-- Tag Management - Ad-hoc, imperative tagging for flexible visualization
-- ============================================================================
-- |
-- | The tagging system provides a flexible way to attach metadata to nodes
-- | based on arbitrary predicates. Tags are:
-- |
-- | - **Persistent**: Once set, they remain until explicitly cleared
-- | - **Accumulative**: Multiple tags can be applied to the same node
-- | - **Ad-hoc**: Applied via lambdas when needed, not predefined
-- | - **Automatic CSS**: Tags propagate to DOM as CSS classes
-- |
-- | ## Use Cases
-- |
-- | - Color coding by git activity (recent, stale, hot, cold)
-- | - Highlighting commit changesets (in-commit, modified, added)
-- | - Filtering by author, date, LOC, dependencies
-- | - Custom visual treatments (important, deprecated, experimental)
-- |
-- | ## Example Usage
-- |
-- | ```purescript
-- | -- Tag nodes based on git blame data
-- | state' <- tagNodes "recent" (\(D3SimNode n) -> n.lastEditDays < 30) allNodes state
-- | state'' <- tagNodes "hot" (\(D3SimNode n) -> n.editsLastMonth > 10) allNodes state'
-- |
-- | -- Clear all tags before applying new scheme
-- | state''' <- clearAllTags state''
-- |
-- | -- Tag specific commit changeset
-- | changeSet <- getCommitDiff "abc123"
-- | state'''' <- tagNodes "in-commit" (\(D3SimNode n) ->
-- |   case n.nodetype of
-- |     IsModule path -> elem path changeSet
-- |     _ -> false
-- | ) allNodes state'''
-- | ```
-- |
-- | Tags automatically become CSS classes on nodes, so you can style them:
-- |
-- | ```css
-- | .node.recent { fill: #00ff00; }
-- | .node.hot { stroke: #ff0000; stroke-width: 3px; }
-- | .node.in-commit { filter: brightness(1.5); }
-- | ```

-- | Apply a tag to nodes matching a predicate
-- | Tags accumulate - calling this multiple times adds more tags
tagNodes :: String -> (SpagoSimNode -> Boolean) -> Array SpagoSimNode -> State -> State
tagNodes label predicate nodes state =
  let newTags = foldl (\acc node@(D3SimNode n) ->
        if predicate node
        then Map.alter (addTag label) n.id acc
        else acc
      ) state.tags nodes
  in state { tags = newTags }
  where
    addTag :: String -> Maybe (Set String) -> Maybe (Set String)
    addTag tag Nothing = Just $ Set.singleton tag
    addTag tag (Just tags) = Just $ Set.insert tag tags

-- | Remove a specific tag from nodes
-- | If a node has no tags remaining after removal, it's removed from the map
untagNodes :: String -> Array SpagoSimNode -> State -> State
untagNodes label nodes state =
  let nodeIds = Set.fromFoldable $ (\(D3SimNode n) -> n.id) <$> nodes
      newTags = Map.mapMaybeWithKey (\id tags ->
        if Set.member id nodeIds
        then let tags' = Set.delete label tags
             in if Set.isEmpty tags' then Nothing else Just tags'
        else Just tags
      ) state.tags
  in state { tags = newTags }

-- | Clear all tags from all nodes
clearAllTags :: State -> State
clearAllTags state = state { tags = Map.empty }

-- | Get all tags for a specific node
getNodeTags :: NodeID -> State -> Set String
getNodeTags id state = fromMaybe Set.empty $ Map.lookup id state.tags

-- | Check if a node has a specific tag
nodeHasTag :: String -> NodeID -> State -> Boolean
nodeHasTag label id state =
  case Map.lookup id state.tags of
    Nothing -> false
    Just tags -> Set.member label tags

