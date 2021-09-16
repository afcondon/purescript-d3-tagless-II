module Stories.Spago.State where

import Prelude

import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_)
import D3.Examples.Spago.Files (SpagoDataRow, SpagoGraphLinkID, SpagoLinkData)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode)
import D3.FFI (SimulationConfig_, readSimulationConfig_)
import D3.Node (D3_SimulationNode(..), NodeID)
import D3.Simulation.Types (D3SimulationState_, ForceStatus(..), _handle)
import D3Tagless.Capabilities (Staging)
import Data.Array (filter)
import Data.Lens (class Wander, Lens', _Just, filtered, over, preview, traversed, view)
import Data.Lens.Record (prop)
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (fst, snd)
import Type.Proxy (Proxy(..))
  
type State = Record (StateRow)
type StateRow = (
  -- governing class on the SVG means we can completely change the look of the vis (and not have to think about this at D3 level)
    svgClass     :: String 
  -- the model should actually be a component, probably a hook so that it can be constructed by this component and not be a Maybe
  , model        :: Maybe SpagoModel 
  -- we'll filter nodes/links to staging and then, if staging is valid (has selections) we will put this staging data in the simulation
  -- if there are updates to data they will be detected and handled by defensive copying in the FFI to ensure continuity of object references from links
  , staging      :: Staging D3Selection_ SpagoDataRow SpagoLinkData NodeID
  -- the simulationState manages the Nodes, Links, Forces, Selections, Ticks & simulation parameters
  , simulation   :: D3SimulationState_
)

_model :: forall a r. Lens' { model :: a | r } a
_model = prop (Proxy :: Proxy "model")

_staging :: forall a r. Lens' { staging :: a | r } a
_staging = prop (Proxy :: Proxy "staging")

_cssClass :: forall a r. Lens' { svgClass :: a | r } a
_cssClass = prop (Proxy :: Proxy "svgClass")

_d3Simulation :: forall a r. Lens' { simulation :: a | r} a
_d3Simulation = prop (Proxy :: Proxy "simulation")

chooseSimNodes :: (SpagoSimNode -> Boolean) -> State -> Maybe (Array SpagoSimNode)
chooseSimNodes fn state = filter fn <$> preview _modelNodes state

chooseSimLinks :: (SpagoGraphLinkID -> Boolean) -> State -> Maybe (Array SpagoGraphLinkID)
chooseSimLinks fn state = filter fn <$> preview _modelLinks state

getSimConfigRecord :: State -> SimulationConfig_
getSimConfigRecord state = do
  let handle = view (_d3Simulation <<< _handle) state
  readSimulationConfig_ handle

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

_stagingForces :: forall p. 
     Strong p
  => Choice p
  => p (Map Label ForceStatus) (Map Label ForceStatus)
  -> p State State
_stagingForces = _staging <<< _forces

-- _stagingForcesActive :: forall r2 p r1 t.
--   Strong p =>
--   Traversable t =>
--   Wander p =>
--   p Label Label -> p State State
--   -- p { staging :: { forces :: t (Tuple Label ForceStatus) | r1 } | r2 }
--   --   { staging :: { forces :: t (Tuple Label ForceStatus) | r1 } | r2 }
-- _stagingForcesActive = _staging <<< _forces <<< traversed <<< (filtered (\f -> snd f == ForceActive)) <<< _1

-- listActiveForces :: forall r1 r2 t.
--   Traversable t => { staging :: { forces :: t (Tuple Label ForceStatus) | r1 } | r2 } -> String
listActiveForces :: State -> Array String
listActiveForces state = do
  let tuples = toUnfoldable state.staging.forces 
  fst <$> (filter (\f -> (snd f) == ForceActive) $ tuples)

_nodes :: forall a r. Lens' { nodes :: a | r } a
_nodes = prop (Proxy :: Proxy "nodes")

_links :: forall a r. Lens' { links :: a | r } a
_links = prop (Proxy :: Proxy "links")

_forces :: forall a r. Lens' { forces :: a | r } a
_forces = prop (Proxy :: Proxy "forces")

_rawdata :: forall a r. Lens' { rawdata :: a | r } a
_rawdata = prop (Proxy :: Proxy "rawdata")

_enterselections :: forall a r. Lens' { selections :: a | r } a
_enterselections = prop (Proxy :: Proxy "selections")
