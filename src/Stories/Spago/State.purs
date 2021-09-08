module Stories.Spago.State where

import Prelude

import D3.Data.Types (D3Selection_)
import D3.Examples.Spago.Files (SpagoDataRow, SpagoGraphLinkID, SpagoLinkData)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode)
import D3.Node (NodeID)
import D3.Simulation.Types (D3SimulationState_, Force)
import D3Tagless.Capabilities (Staging)
import Data.Lens (Lens', _Just)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
  
type State = Record ( SimStateRow + StateRow + ())
type StateRow row = (
  -- governing class on the SVG means we can completely change the look of the vis (and not have to think about this at D3 level)
    svgClass     :: String 
  -- just a simple list of the names of the forces that are meant to be active
  , forces       :: { active :: Array String, all :: Array Force }
  -- the model should actually be a component, probably a hook so that it can be constructed by this component and not be a Maybe
  , model        :: Maybe SpagoModel 
  -- we'll filter nodes/links to staging and then, if staging is valid (has selections) we will put this staging data in the simulation
  -- if there are updates to data they will be detected and handled by defensive copying in the FFI to ensure continuity of object references from links
  , staging      :: Staging D3Selection_ SpagoDataRow SpagoLinkData NodeID
  | row
)
type SimStateRow row = ( 
  -- the simulationState manages the Nodes, Links, Forces, Selections, Ticks & simulation parameters
  simulation   :: D3SimulationState_ 
  | row
)

_model :: forall a r. Lens' { model :: a | r } a
_model = prop (Proxy :: Proxy "model")

_staging :: forall a r. Lens' { staging :: a | r } a
_staging = prop (Proxy :: Proxy "staging")

_cssClass :: forall a r. Lens' { svgClass :: a | r } a
_cssClass = prop (Proxy :: Proxy "svgClass")

_activeForces :: forall a r r2. Lens' { forces :: { active :: a | r } | r2 } a
_activeForces = prop (Proxy :: Proxy "forces") <<< prop (Proxy :: Proxy "active")

_forces :: forall a r r2. Lens' { forces :: { all :: a | r } | r2 } a
_forces = prop (Proxy :: Proxy "forces") <<< prop (Proxy :: Proxy "all")

_d3Simulation :: forall a r. Lens' { simulation :: a | r} a
_d3Simulation = prop (Proxy :: Proxy "simulation")

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

_nodes :: forall a r. Lens' { nodes :: a | r } a
_nodes = prop (Proxy :: Proxy "nodes")

_links :: forall a r. Lens' { links :: a | r } a
_links = prop (Proxy :: Proxy "links")

_rawdata :: forall a r. Lens' { rawdata :: a | r } a
_rawdata = prop (Proxy :: Proxy "rawdata")

_enterselections :: forall a r. Lens' { selections :: a | r } a
_enterselections = prop (Proxy :: Proxy "selections")
