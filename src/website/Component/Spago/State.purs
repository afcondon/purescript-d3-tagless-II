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
import Data.Map (Map)
import Data.Map (Map, empty) as M
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
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
)

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
  -- at present just one call back which is added to the circle attributes
  , callback        :: SelectionAttribute
  -- fix functions - run one after another on the data to set fixed nodes
  , nodeInitializerFunctions :: Array (Array SpagoSimNode -> Array SpagoSimNode)
  -- could add the simulation variables here too?
}
initialScene :: Map Label Force -> MiseEnScene
initialScene forceLibrary = {
    chooseNodes: isPackage -- chooses all nodes
  , linksShown:  const false
  , linksActive: const false
  , forceStatuses: getStatusMap forceLibrary
  , cssClass: ""
  , attributes: clusterSceneAttributes
  , callback: x 0.0 -- possibly want to store the listener here rather than the callback?
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
_chooseNodes :: forall t427 t428 t432 t433.
  Strong t428 => t428 t433 t433
                 -> t428
                      { scene :: { chooseNodes :: t433
                                 | t432
                                 }
                      | t427
                      }
                      { scene :: { chooseNodes :: t433
                                 | t432
                                 }
                      | t427
                      }
_chooseNodes              = _scene <<< prop (Proxy :: Proxy "chooseNodes")
_linksShown :: forall t232 t233 t237 t238.
  Strong t233 => t233 t238 t238
                 -> t233
                      { scene :: { linksShown :: t238
                                 | t237
                                 }
                      | t232
                      }
                      { scene :: { linksShown :: t238
                                 | t237
                                 }
                      | t232
                      }
_linksShown               = _scene <<< prop (Proxy :: Proxy "linksShown")
_linksActive :: forall t252 t253 t257 t258.
  Strong t253 => t253 t258 t258
                 -> t253
                      { scene :: { linksActive :: t258
                                 | t257
                                 }
                      | t252
                      }
                      { scene :: { linksActive :: t258
                                 | t257
                                 }
                      | t252
                      }
_linksActive              = _scene <<< prop (Proxy :: Proxy "linksActive")
-- _sceneForces              = _scene <<< _forces
_cssClass :: forall t407 t408 t412 t413.
  Strong t408 => t408 t413 t413
                 -> t408
                      { scene :: { cssClass :: t413
                                 | t412
                                 }
                      | t407
                      }
                      { scene :: { cssClass :: t413
                                 | t412
                                 }
                      | t407
                      }
_cssClass                 = _scene <<< prop (Proxy :: Proxy "cssClass")
_callback :: forall t447 t448 t452 t453.
  Strong t448 => t448 t453 t453
                 -> t448
                      { scene :: { callback :: t453
                                 | t452
                                 }
                      | t447
                      }
                      { scene :: { callback :: t453
                                 | t452
                                 }
                      | t447
                      }
_callback                 = _scene <<< prop (Proxy :: Proxy "callback") 
_sceneAttributes :: forall t64 t65 t69 t70.
  Strong t65 => t65 t70 t70
                -> t65
                     { scene :: { attributes :: t70
                                | t69
                                }
                     | t64
                     }
                     { scene :: { attributes :: t70
                                | t69
                                }
                     | t64
                     }
_sceneAttributes          = _scene <<< prop (Proxy :: Proxy "attributes")
_nodeInitializerFunctions :: forall p.
  Strong p =>
  p (Array (Array SpagoSimNode -> Array SpagoSimNode)) (Array (Array SpagoSimNode -> Array SpagoSimNode)) ->
  p State State
_nodeInitializerFunctions = _scene <<< prop (Proxy :: Proxy "nodeInitializerFunctions")

-- -- REVIEW appears to be unused, why?
-- chooseSimNodes :: (SpagoSimNode -> Boolean) -> State -> Maybe (Array SpagoSimNode)
-- chooseSimNodes fn state = filter fn <$> preview _modelNodes state

-- chooseSimLinks :: (SpagoGraphLinkID -> Boolean) -> State -> Maybe (Array SpagoGraphLinkID)
-- chooseSimLinks fn state = filter fn <$> preview _modelLinks state

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

