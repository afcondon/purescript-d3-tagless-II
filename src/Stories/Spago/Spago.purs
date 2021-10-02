module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get, modify_)
import D3.Attributes.Instances (Label)
import D3.Attributes.Sugar (onMouseEvent, onMouseEventEffectful, x)
import D3.Data.Types (Datum_, MouseEvent(..))
import D3.Examples.Spago.Draw (getVizEventFromClick)
import D3.Examples.Spago.Draw as Graph
import D3.Examples.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Examples.Spago.Files (LinkType(..), NodeType(..), SpagoGraphLinkID, SpagoGraphLinkRecord, isM2M_Graph_Link, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, addGridPoints, allNodes, convertFilesToGraphModel, datum_, isModule, isPackage, isPackageOrVisibleModule, isUsedModule, link_)
import D3.Examples.Spago.Tree (treeReduction)
import D3.FFI (linksForceName)
import D3.Selection (SelectionAttribute)
import D3.Simulation.Types (SimVariable(..), _forceStatus, _forceStatuses, _onlyTheseForcesActive, initialSimulationState, toggleForceStatus)
import D3Tagless.Capabilities (actualizeForces, setConfigVariable, start, stop)
import D3Tagless.Instance.Simulation (evalEffectSimulation, runWithD3_Simulation)
import Data.Array (filter, (:))
import Data.Either (hush)
import Data.Lens (use, view, (%=))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Debug (trace)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (HalogenM, liftEffect, raise)
import Halogen as H
import Halogen.Subscription (Listener)
import Halogen.Subscription as HS
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..), VizEvent(..))
import Stories.Spago.Forces (forceLibrary)
import Stories.Spago.HTML (render)
import Stories.Spago.State (MiseEnScene, State, _callback, _chooseNodes, _cssClass, _enterselections, _links, _linksActive, _linksShown, _linksWithForce, _model, _modelLinks, _modelNodes, _nodes, _sceneAttributes, _sceneForces, _staging, _stagingLinkFilter, _stagingLinks, _stagingNodes, defaultSceneConfig)

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize }
  }
  where

  initialState :: State
  initialState = do
    { model: Nothing
    , staging: { selections: { nodes: Nothing, links: Nothing }, linksWithForce: const true, rawdata: { nodes: [], links: [] } }
    , simulation: initialSimulationState forceLibrary
    , scene: defaultSceneConfig
    }

simulationEvent :: Listener Action -> SelectionAttribute
simulationEvent l = onMouseEventEffectful MouseClick (\e d t -> liftEffect $ HS.notify l (EventFromVizualization (getVizEventFromClick e d t)))
 
handleAction :: forall t316 t317 t318.
  MonadAff t318 => 
  Action ->
  HalogenM State Action t316 t317 t318 Unit
handleAction = case _ of

  -- TODO defer reading of model data until component actually needs it
  Initialize -> do    
    -- read various JSON files and synthesize a Model
    (maybeModel :: Maybe SpagoModel) <- H.liftAff readModelData
    _model %= (const maybeModel)
    -- set things up in the DOM with SVG, groups for links and nodes, open selections for updates
    openSelections <- evalEffectSimulation Graph.initialize
    (_staging <<< _enterselections <<< _nodes) %= (const $ openSelections.nodes) 
    (_staging <<< _enterselections <<< _links) %= (const $ openSelections.links)
    -- create subscriptions for actions arising in the visualization to trigger actions in Halogen app
    { emitter, listener } <- liftEffect $ HS.create
    -- now hook up this emitter so that Halogen Actions will be triggered by notifications from that emitter
    void $ H.subscribe emitter

    _callback %= const (simulationEvent listener)
    pure unit

  Finalize -> pure unit

  -- here's where we convert low level D3 Events from the visualization into the appropriate Halogen Action
  -- this keeps the low level code concerned only with the syntax of the event, while the app (Halogen) will
  -- deal with the semantics
  EventFromVizualization ve -> do
    case ve of
      NodeClick (IsPackage _) id -> handleAction $ ToggleChildrenOfNode id
      NodeClick (IsModule _)  id -> handleAction $ SpotlightNode id
  
  -- REVIEW this isn't a good way to do this, needs list of open nodes or something
  ToggleChildrenOfNode id -> runWithD3_Simulation do -- just a copy of PackageGrid right now, need to refactor so that it's all parameterized
    _chooseNodes %= const (isPackageOrVisibleModule id)
    runSimulation
  UnToggleChildrenOfNode _ -> runWithD3_Simulation do 
    _chooseNodes %= const isPackage 
    runSimulation

  SpotlightNode _ -> runWithD3_Simulation stop

  Scene PackageGrid -> do
    _chooseNodes     %= const allNodes
    _linksShown      %= const isM2P_Link
    _linksActive     %= (const $ const true)
    _sceneForces     %= const [ "packageGrid", "clusterx", "clustery", "collide2" ]
    _cssClass        %= const "cluster"
    _sceneAttributes %= const clusterSceneAttributes
    _stagingNodes    %= addGridPoints -- additional setup of the selected data
    -- runWithD3_Simulation $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    runSimulation

  Scene PackageGraph -> do
    -- 1. set up the scene
    _chooseNodes     %= const isPackage
    _linksShown      %= const isP2P_Link
    _linksActive     %= const (sourcePackageIs "my-project")
    _sceneForces     %= const ["centerNamedNode", "center", "collide2", "charge2", "packageOrbit"]
    _cssClass        %= const "graph"
    _sceneAttributes %= const graphSceneAttributes
    -- runWithD3_Simulation $ removeNamedSelection "treelinksSelection"
    -- runWithD3_Simulation $ uniformlyDistributeNodes -- FIXME
    runSimulation

  Scene (ModuleTree _) -> do
    _chooseNodes     %= const isUsedModule
    _linksShown      %= const isM2M_Tree_Link
    _linksActive     %= (const $ const true)
    _sceneForces     %= const [ "treeNodesX", "treeNodesY", "center", "charge1", "collide2", "unusedOrbit" ]
    _cssClass        %= const "tree"
    _sceneAttributes %= const treeSceneAttributes
    -- runWithD3_Simulation $ removeNamedSelection "graphlinksSelection"
    runSimulation 
    
  ToggleForce label -> do
    _forceStatus label %= toggleForceStatus
    runSimulation -- maybe also setConfigVariable $ Alpha 0.7

  Filter (LinkShowFilter filterFn) -> do
    _linksShown %= const filterFn
    runSimulation -- maybe also setConfigVariable $ Alpha 0.7

  Filter (LinkForceFilter filterFn) -> do
    _linksActive %= const filterFn
    runSimulation -- maybe also setConfigVariable $ Alpha 0.7

  Filter (NodeFilter filterFn) -> do
    _chooseNodes %= const filterFn
    runSimulation -- maybe also setConfigVariable $ Alpha 0.7

  ChangeStyling style -> do
    _cssClass %= (const style) -- modify_ (\s -> s { svgClass = style })

  ChangeSimConfig c -> do
    runWithD3_Simulation $ setConfigVariable c 

  StartSim -> do
    runWithD3_Simulation do
      setConfigVariable $ Alpha 1.0
      start

  StopSim -> runWithD3_Simulation $ 
    do
      setConfigVariable $ Alpha 0.0
      stop


-- ======================================================================================================================
-- | manage what data from the model gets given to the visualization code and also what forces should be engaged
-- ======================================================================================================================
stageDataFromModel :: forall m.
  MonadState State m =>
  m Unit
stageDataFromModel = do
  state       <- get
  linksShown  <- use _linksShown
  linksActive <- use _linksActive
  chooseNodes <- use _chooseNodes
  _stagingLinks      %= const (filter linksShown $ view _modelLinks state)
  _stagingLinkFilter %= const linksActive
  _stagingNodes      %= const (filter chooseNodes $ view _modelNodes state)
  -- _stagingNodes   %= addGridPoints -- do this instead in Action between configure and run
  -- this next line changes the simulation, shouldnt be done here
  -- FIXME this is where the grid point can be set, once we know how many packages we have

-- ======================================================================================================================
-- | run the visualization "script" with the "scene" set-up as configured
-- ======================================================================================================================
runSimulation :: forall m.
  MonadEffect m =>
  MonadState State m =>
  m Unit
runSimulation = do
  stageDataFromModel
  staging         <- use _staging
  callback        <- use _callback
  sceneAttributes <- use _sceneAttributes
  let attributes = sceneAttributes { circles = callback : sceneAttributes.circles } -- FIXME we don't actually want to stick the default value on here, needs to be Maybe
  linksActive     <- use _linksActive
  forces          <- use _sceneForces
  runWithD3_Simulation do
    _forceStatuses %= _onlyTheseForcesActive forces
    actualizeForces
    Graph.updateSimulation staging sceneAttributes
    setConfigVariable $ Alpha 1.0

sourcePackageIs name link = (link_.source link).name == name -- TODO move to Model
-- ======================================================================================================================
-- functions to read the data from files and build the model (only lives here to prevent cycles)
-- readModelData will try to build a model from files and to derive a dependency tree from Main
-- the dependency tree will contain all nodes reachable from Main but NOT all links
-- ======================================================================================================================
readModelData :: Aff (Maybe SpagoModel)
readModelData = do
  let datadir = "http://localhost:1234/spago-data/"
  moduleJSON  <- AJAX.get ResponseFormat.string $ datadir <> "modules.json"
  packageJSON <- AJAX.get ResponseFormat.string $ datadir <> "packages.json"
  lsdepJSON   <- AJAX.get ResponseFormat.string $ datadir <> "lsdeps.jsonlines"
  locJSON     <- AJAX.get ResponseFormat.string $ datadir <> "loc.json"
  let model = hush $ convertFilesToGraphModel <$> moduleJSON <*> packageJSON <*> lsdepJSON <*> locJSON

  pure (addTreeToModel "Main" model) 

addTreeToModel :: String -> Maybe SpagoModel -> Maybe SpagoModel
addTreeToModel rootName maybeModel = do
  model  <- maybeModel
  rootID <- M.lookup rootName model.maps.name2ID
  pure $ treeReduction rootID model
