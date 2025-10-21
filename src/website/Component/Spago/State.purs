module PSD3.Spago.State where

import Prelude

import D3.Attributes.Instances (Label)
import D3.Attributes.Sugar (x)
import D3.Data.Types (D3Selection_, Datum_)
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, clusterSceneAttributes)
import D3.Viz.Spago.Files (SpagoDataRow, SpagoGraphLinkID, SpagoLinkData)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isPackage)
import D3.FFI (SimulationVariables, readSimulationVariables)
import D3.Node (NodeID)
import D3.Selection (SelectionAttribute)
import D3.Simulation.Types (D3SimulationState_, Force, ForceStatus, _handle, getStatusMap)
import D3Tagless.Capabilities (Staging)
import Data.Lens (Lens', _Just, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map) as M
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Halogen.Subscription as HS
import PSD3.Spago.Actions (Action)
import Type.Proxy (Proxy(..))
  
type State = Record (StateRow)
type StateRow = (
  -- the simulationState manages the Nodes, Links, Forces, Selections, Ticks & simulation parameters
    simulation   :: D3SimulationState_
  -- the model should actually be a component, probably a hook so that it can be constructed by this component and not be a Maybe
  , model        :: Maybe SpagoModel
  -- we'll filter nodes/links to staging and then, if staging is valid (has selections) we will put this staging data in the simulation
  -- if there are updates to data they will be detected and handled by defensive copying in the FFI to ensure continuity of object references from links
  , staging      :: Staging D3Selection_ SpagoDataRow SpagoLinkData NodeID
-- | Contains all the settings necessary to call the Draw function
  , scene        :: MiseEnScene
-- | Event listener for D3→Halogen event flow (component infrastructure, not scene config)
  , eventListener :: Maybe (HS.Listener Action)
)

-- | Configuration for a visualization "scene" - a complete specification of:
-- | - which data to show (node/link filters)
-- | - how forces behave (force statuses)
-- | - visual appearance (CSS class, attributes)
-- | - initialization (node positioning functions)
-- |
-- | This pattern could be generalized into a library by parameterizing over
-- | the specific node and link types, but for now it's specialized to Spago.
type MiseEnScene = {
-- first: filter functions for nodes and links (both what links are shown and which ones exert force)
    chooseNodes     :: (SpagoSimNode -> Boolean)
  , linksShown      :: (SpagoGraphLinkID -> Boolean)
  , linksActive     :: (Datum_ -> Boolean) -- defined as Datum_ but it's really Link_, ugly
-- list of forces to activate
  , forceStatuses   :: M.Map Label ForceStatus
  -- governing class on the SVG means we can completely change the look of the vis (and not have to think about this at D3 level)
  , cssClass        :: String
  , attributes      :: SpagoSceneAttributes
  -- fix functions - run one after another on the data to set fixed nodes
  , nodeInitializerFunctions :: Array (Array SpagoSimNode -> Array SpagoSimNode)
  -- could add the simulation variables here too?
}
initialScene :: M.Map Label Force -> MiseEnScene
initialScene forceLibrary = {
    chooseNodes: isPackage -- chooses all nodes
  , linksShown:  const false
  , linksActive: const false
  , forceStatuses: getStatusMap forceLibrary
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

_forceStatus :: forall p.
  Strong p => Choice p => String ->
  p ForceStatus ForceStatus ->
  p State State
_forceStatus label = _forceStatuses <<< at label <<< _Just

-- lenses for mise-en-scene things 
_forceStatuses :: Lens' State (M.Map Label ForceStatus)
_forceStatuses = _scene <<< prop (Proxy :: Proxy "forceStatuses")
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

getSimulationVariables :: State -> SimulationVariables
getSimulationVariables state = do
  let handle = view _handle state
  readSimulationVariables handle

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

