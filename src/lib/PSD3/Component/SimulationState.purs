-- | Generic state management for complex simulation-based visualizations
-- |
-- | This module provides reusable infrastructure for visualizations that combine:
-- | - Force-directed simulation (SimulationM2)
-- | - Explicit layouts (hierarchies, trees, pack, treemap)
-- | - Scene-based transitions between views
-- | - Interactive modes with animation choreography
-- |
-- | ## Design Philosophy
-- |
-- | Complex visualizations often need multiple "scenes" - different views of the same data
-- | (e.g., package grid, package graph, module tree). This module provides:
-- |
-- | 1. **Generic State Type**: Parameterized by visualization-specific types
-- | 2. **Scene Management**: Declarative scene switching with transition control
-- | 3. **Transition Matrix**: Define which scene transitions should animate
-- | 4. **Tag System**: Ad-hoc metadata for nodes (for highlighting, filtering, coloring)
-- | 5. **Helper Functions**: Eliminate boilerplate for common state updates
-- |
-- | ## Usage
-- |
-- | Specialize the generic state with your types:
-- |
-- | ```purescript
-- | type State = SimulationComponentState
-- |   MyScene           -- Your scene ADT (e.g., data Scene = PackageView | ModuleView)
-- |   MyAction          -- Your Halogen action type
-- |   MyDataRow         -- Your simulation node row type
-- |   MyAttributes      -- Your scene attributes type
-- |   MyModel           -- Your data model type
-- | ```
-- |
-- | ## Not For Simple Visualizations
-- |
-- | If your visualization:
-- | - Has only one view/mode
-- | - Doesn't need force simulation
-- | - Doesn't need scene transitions
-- |
-- | Then you probably don't need this infrastructure. Use simpler state management.
module PSD3.Component.SimulationState
  ( -- * Core Types
    SimulationComponentState
  , TransitionMatrix

  -- * Scene Management
  , updateScene
  , applySceneConfig
  , applySceneWithTransition

  -- * Scene Field Setters
  , setChooseNodes
  , setLinksShown
  , setLinksActive
  , setActiveForces
  , setCssClass
  , setSceneAttributes
  , setNodeInitializers
  , setTransitionConfig
  , toggleForce

  -- * Model/Staging Accessors
  , getModelNodes
  , getModelLinks
  , getStagingNodes
  , getStagingLinks
  , getStagingLinkFilter

  -- * Staging Setters
  , setStagingNodes
  , setStagingLinks
  , setStagingLinkFilter

  -- * Tag Management
  , tagNodes
  , untagNodes
  , clearAllTags
  , clearTag
  , getNodeTags
  , nodeHasTag

  -- * Generic Lenses
  , _model
  , _staging
  , _scene
  , _nodes
  , _links
  , _forces
  , _linksWithForce
  , _rawdata
  , _enterselections
  , _eventListener
  ) where

import Prelude

import Data.Array (foldl)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen.Subscription as HS
import PSD3.Capabilities.Simulation (Staging)
import PSD3.Data.Node (Link, SimulationNode(..), NodeID)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Simulation.Types (D3SimulationState_)
import PSD3.Internal.Types (D3Selection_, Datum_)
import PSD3v2.Simulation.Scene as Scene
import Type.Proxy (Proxy(..))

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Generic state for complex simulation-based visualizations
-- |
-- | Type parameters:
-- | - `scene`: Your scene ADT (e.g., data Scene = PackageView | ModuleView | TreeView)
-- | - `action`: Your Halogen action type
-- | - `id`: Node ID type (must have Eq and Ord, e.g., String or Int)
-- | - `nodeData`: Your simulation node user data type (e.g., { id :: String, group :: Int })
-- | - `linkRow`: Your link user data type (e.g., { linktype :: LinkType, inSim :: Boolean })
-- | - `attrs`: Your scene-specific attributes type (e.g., SpagoSceneAttributes)
-- | - `model`: Your data model type (e.g., SpagoModel with nodes, links, metadata)
type SimulationComponentState scene action id nodeData linkRow attrs model =
  { -- | Core D3 simulation state (forces, alpha, nodes, etc.)
    simulation :: D3SimulationState_ (SimulationNode nodeData)

    -- | Data model for the visualization (often loaded from JSON)
  , model :: Maybe model

    -- | Staging area: filtered nodes/links before entering simulation
    -- | This is where scene filters are applied
  , staging :: Staging (D3Selection_ (SimulationNode nodeData)) nodeData id linkRow

    -- | Current scene configuration (filters, forces, attributes, initializers)
  , scene :: Scene.SimSceneConfig nodeData id linkRow attrs

    -- | Current scene identifier (for transition matrix lookups)
  , currentScene :: scene

    -- | Transition choreography: maps (fromScene, toScene) to animation specs
    -- | Missing entries default to instant (no animation)
  , transitionMatrix :: TransitionMatrix scene

    -- | Event listener for D3→Halogen event flow (clicks, hovers, etc.)
  , eventListener :: Maybe (HS.Listener action)

    -- | Transition listener for animation completion events
  , transitionListener :: Maybe (HS.Listener action)

    -- | Ad-hoc node tagging system (for highlighting, filtering, coloring)
    -- | Tags automatically propagate to CSS classes
  , tags :: M.Map id (Set String)

    -- | Show welcome overlay on first load
  , showWelcome :: Boolean

    -- | Tree revelation step (for progressive tree reveal animations)
    -- | 0 = initial state, increases to maxTreeDepth+1 for final relaxation
  , revelationStep :: Int

    -- | Maximum tree depth in current data (calculated from tree)
  , maxTreeDepth :: Int
  }

-- | Transition matrix: maps scene transitions to animation specifications
-- |
-- | ## Usage
-- |
-- | Define only the transitions that need animation:
-- |
-- | ```purescript
-- | transitionMatrix :: TransitionMatrix Scene
-- | transitionMatrix = M.fromFoldable
-- |   [ Tuple (Tuple PackageGrid PackageGraph) smoothTransition
-- |   , Tuple (Tuple PackageGraph ModuleTree) smoothTransition
-- |   -- All other transitions are instant (not in map)
-- |   ]
-- | ```
type TransitionMatrix scene = M.Map (Tuple scene scene) Scene.TransitionSpec

-- ============================================================================
-- Scene Management
-- ============================================================================

-- | Update the scene configuration with a function
-- | Use this when you want to modify specific scene fields
updateScene :: forall scene action id nodeData linkRow attrs model.
  (Scene.SimSceneConfig nodeData id linkRow attrs -> Scene.SimSceneConfig nodeData id linkRow attrs) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
updateScene f state = state { scene = f state.scene }

-- | Apply a complete scene configuration
-- |
-- | This replaces the entire scene configuration in one step.
-- | Filtering and initialization happen later inside runSimulation.
applySceneConfig :: forall scene action id nodeData linkRow attrs model.
  Scene.SimSceneConfig nodeData id linkRow attrs ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
applySceneConfig config state = state { scene = config }

-- | Apply a scene configuration with transition matrix lookup
-- |
-- | This implements declarative scene choreography:
-- | 1. Look up (currentScene, targetScene) in transition matrix
-- | 2. Override scene's transitionConfig with looked-up value (or Nothing)
-- | 3. Apply the configured scene
-- |
-- | This enables asymmetric transitions:
-- | - PackageGrid → PackageGraph: smooth animation
-- | - PackageGraph → PackageGrid: instant snap
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | Scene ModuleTree -> do
-- |   H.modify_ $ applySceneWithTransition ModuleTree moduleTreeScene
-- |   runSimulation
-- |   H.modify_ _ { currentScene = ModuleTree }
-- | ```
applySceneWithTransition :: forall scene action id nodeData linkRow attrs model.
  Ord scene =>
  scene ->
  Scene.SimSceneConfig nodeData id linkRow attrs ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
applySceneWithTransition targetScene baseConfig state =
  let transitionSpec = M.lookup (Tuple state.currentScene targetScene) state.transitionMatrix
      config = baseConfig { transitionConfig = transitionSpec }
  in state { scene = config }

-- ============================================================================
-- Scene Field Setters
-- ============================================================================

-- | Set which nodes to display
setChooseNodes :: forall scene action id nodeData linkRow attrs model.
  (SimulationNode nodeData -> Boolean) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setChooseNodes fn = updateScene (\s -> s { chooseNodes = fn })

-- | Set which links to render
setLinksShown :: forall scene action id nodeData linkRow attrs model.
  (Link id linkRow -> Boolean) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setLinksShown fn = updateScene (\s -> s { linksShown = fn })

-- | Set which links exert force
setLinksActive :: forall scene action id nodeData linkRow attrs model.
  (Link id linkRow -> Boolean) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setLinksActive fn = updateScene (\s -> s { linksActive = fn })

-- | Set which forces are active
setActiveForces :: forall scene action id nodeData linkRow attrs model.
  Set Label ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setActiveForces forces = updateScene (\s -> s { activeForces = forces })

-- | Set CSS class for current scene
setCssClass :: forall scene action id nodeData linkRow attrs model.
  String ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setCssClass css = updateScene (\s -> s { cssClass = css })

-- | Set scene attributes (visual styling)
setSceneAttributes :: forall scene action id nodeData linkRow attrs model.
  attrs ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setSceneAttributes attrs = updateScene (\s -> s { attributes = attrs })

-- | Set node initializer functions (positioning, pinning, etc.)
setNodeInitializers :: forall scene action id nodeData linkRow attrs model.
  Array (Array (SimulationNode nodeData) -> Array (SimulationNode nodeData)) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setNodeInitializers fns = updateScene (\s -> s { nodeInitializerFunctions = fns })

-- | Set transition configuration for scene
setTransitionConfig :: forall scene action id nodeData linkRow attrs model.
  Maybe Scene.TransitionSpec ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setTransitionConfig config = updateScene (\s -> s { transitionConfig = config })

-- | Toggle a force on/off
toggleForce :: forall scene action id nodeData linkRow attrs model.
  Label ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
toggleForce label = updateScene \s -> s {
  activeForces = if Set.member label s.activeForces
                 then Set.delete label s.activeForces
                 else Set.insert label s.activeForces
}

-- ============================================================================
-- Model/Staging Accessors
-- ============================================================================

-- | Get nodes from model
getModelNodes :: forall scene action id nodeData linkRow attrs model nodes.
  { nodes :: nodes | model } ->
  SimulationComponentState scene action id nodeData linkRow attrs { nodes :: nodes | model } ->
  nodes
getModelNodes default state = case state.model of
  Just m -> m.nodes
  Nothing -> default.nodes

-- | Get links from model
getModelLinks :: forall scene action id nodeData linkRow attrs model links.
  { links :: links | model } ->
  SimulationComponentState scene action id nodeData linkRow attrs { links :: links | model } ->
  links
getModelLinks default state = case state.model of
  Just m -> m.links
  Nothing -> default.links

-- | Get nodes from staging
getStagingNodes :: forall scene action id nodeData linkRow attrs model.
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  Array (SimulationNode nodeData)
getStagingNodes state = state.staging.rawdata.nodes

-- | Get links from staging
getStagingLinks :: forall scene action id nodeData linkRow attrs model.
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  Array (Link id linkRow)
getStagingLinks state = state.staging.rawdata.links

-- | Get link filter from staging
getStagingLinkFilter :: forall scene action id nodeData linkRow attrs model.
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  (Link id linkRow -> Boolean)
getStagingLinkFilter state = state.staging.linksWithForce

-- ============================================================================
-- Staging Setters
-- ============================================================================

-- | Set nodes in staging area
setStagingNodes :: forall scene action id nodeData linkRow attrs model.
  Array (SimulationNode nodeData) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setStagingNodes nodes state =
  state { staging = state.staging { rawdata = state.staging.rawdata { nodes = nodes } } }

-- | Set links in staging area
setStagingLinks :: forall scene action id nodeData linkRow attrs model.
  Array (Link id linkRow) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setStagingLinks links state =
  state { staging = state.staging { rawdata = state.staging.rawdata { links = links } } }

-- | Set link filter in staging area
setStagingLinkFilter :: forall scene action id nodeData linkRow attrs model.
  (Link id linkRow -> Boolean) ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
setStagingLinkFilter fn state =
  state { staging = state.staging { linksWithForce = fn } }

-- ============================================================================
-- Tag Management
-- ============================================================================
-- |
-- | The tagging system provides flexible node metadata for visualization.
-- |
-- | ## Features
-- |
-- | - **Persistent**: Tags remain until explicitly cleared
-- | - **Accumulative**: Multiple tags per node
-- | - **Ad-hoc**: Applied via lambdas when needed
-- | - **Automatic CSS**: Tags propagate to DOM as classes
-- |
-- | ## Use Cases
-- |
-- | - Highlight git activity (recent, stale, hot)
-- | - Show commit changesets (modified, added, deleted)
-- | - Filter by author, date, LOC
-- | - Custom treatments (important, deprecated, experimental)
-- |
-- | ## Example
-- |
-- | ```purescript
-- | -- Tag recently edited nodes
-- | state' <- tagNodes "recent"
-- |   (\(SimNode n) -> n.lastEditDays < 30)
-- |   allNodes
-- |   state
-- |
-- | -- Tags become CSS classes
-- | -- .node.recent { fill: #00ff00; }
-- | ```

-- | Tag nodes matching a predicate
-- | Tags accumulate - calling multiple times adds more tags
tagNodes :: forall scene action id nodeData row linkRow attrs model.
  Ord id =>
  (SimulationNode (id :: id | row) -> id) ->  -- ID extractor function
  String ->
  (SimulationNode (id :: id | row) -> Boolean) ->
  Array (SimulationNode (id :: id | row)) ->
  SimulationComponentState scene action id (id :: id | row) linkRow attrs model ->
  SimulationComponentState scene action id (id :: id | row) linkRow attrs model
tagNodes getId label predicate nodes state =
  let newTags = foldl (\acc node ->
        if predicate node
        then M.alter (addTag label) (getId node) acc
        else acc
      ) state.tags nodes
  in state { tags = newTags }
  where
    addTag :: String -> Maybe (Set String) -> Maybe (Set String)
    addTag tag Nothing = Just $ Set.singleton tag
    addTag tag (Just tags) = Just $ Set.insert tag tags

-- | Remove a specific tag from nodes
-- | If a node has no tags remaining, it's removed from the map
untagNodes :: forall scene action id nodeData row linkRow attrs model.
  Ord id =>
  (SimulationNode (id :: id | row) -> id) ->  -- ID extractor function
  String ->
  Array (SimulationNode (id :: id | row)) ->
  SimulationComponentState scene action id (id :: id | row) linkRow attrs model ->
  SimulationComponentState scene action id (id :: id | row) linkRow attrs model
untagNodes getId label nodes state =
  let nodeIds = Set.fromFoldable $ getId <$> nodes
      newTags = M.mapMaybeWithKey (\id tags ->
        if Set.member id nodeIds
        then let tags' = Set.delete label tags
             in if Set.isEmpty tags' then Nothing else Just tags'
        else Just tags
      ) state.tags
  in state { tags = newTags }

-- | Clear a specific tag from all nodes
clearTag :: forall scene action id nodeData linkRow attrs model.
  Ord id =>
  String ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
clearTag label state =
  let newTags = M.mapMaybe (\tags ->
        let tags' = Set.delete label tags
        in if Set.isEmpty tags' then Nothing else Just tags'
      ) state.tags
  in state { tags = newTags }

-- | Clear all tags from all nodes
clearAllTags :: forall scene action id nodeData linkRow attrs model.
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  SimulationComponentState scene action id nodeData linkRow attrs model
clearAllTags state = state { tags = M.empty }

-- | Get all tags for a specific node
getNodeTags :: forall scene action id nodeData linkRow attrs model.
  Ord id =>
  id ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  Set String
getNodeTags nodeId state = fromMaybe Set.empty $ M.lookup nodeId state.tags

-- | Check if a node has a specific tag
nodeHasTag :: forall scene action id nodeData linkRow attrs model.
  Ord id =>
  String ->
  id ->
  SimulationComponentState scene action id nodeData linkRow attrs model ->
  Boolean
nodeHasTag label nodeId state =
  case M.lookup nodeId state.tags of
    Nothing -> false
    Just tags -> Set.member label tags

-- ============================================================================
-- Generic Lenses
-- ============================================================================

_model :: forall scene action id nodeData attrs model r.
  Lens' { model :: model | r } model
_model = prop (Proxy :: Proxy "model")

_staging :: forall scene action id nodeData attrs model staging r.
  Lens' { staging :: staging | r } staging
_staging = prop (Proxy :: Proxy "staging")

_scene :: forall scene action id nodeData attrs model scn r.
  Lens' { scene :: scn | r } scn
_scene = prop (Proxy :: Proxy "scene")

_nodes :: forall nodes r.
  Lens' { nodes :: nodes | r } nodes
_nodes = prop (Proxy :: Proxy "nodes")

_links :: forall links r.
  Lens' { links :: links | r } links
_links = prop (Proxy :: Proxy "links")

_forces :: forall forces r.
  Lens' { forces :: forces | r } forces
_forces = prop (Proxy :: Proxy "forces")

_linksWithForce :: forall linksWithForce r.
  Lens' { linksWithForce :: linksWithForce | r } linksWithForce
_linksWithForce = prop (Proxy :: Proxy "linksWithForce")

_rawdata :: forall rawdata r.
  Lens' { rawdata :: rawdata | r } rawdata
_rawdata = prop (Proxy :: Proxy "rawdata")

_enterselections :: forall selections r.
  Lens' { selections :: selections | r } selections
_enterselections = prop (Proxy :: Proxy "selections")

_eventListener :: forall scene action id nodeData attrs model r.
  Lens' { eventListener :: Maybe (HS.Listener action) | r } (Maybe (HS.Listener action))
_eventListener = prop (Proxy :: Proxy "eventListener")
