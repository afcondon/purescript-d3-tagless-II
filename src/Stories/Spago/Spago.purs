module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Sugar (onMouseEventEffectful)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (MouseEvent(..))
import D3.Examples.Spago.Draw (getVizEventFromClick)
import D3.Examples.Spago.Draw as Graph
import D3.Examples.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Examples.Spago.Files (NodeType(..), isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (SpagoModel, allNodes, convertFilesToGraphModel, fixNamedNodeTo, isPackage, isPackageOrVisibleModule, isUsedModule, link_, moduleNodesToContainerXY, modulesNodesToPhyllotaxis, packageNodesToGridXY, packagesNodesToPhyllotaxis, sourcePackageIs, treeNodesToTreeXY_H, treeNodesToTreeXY_V, unpinAllNodes)
import D3.Examples.Spago.Tree (treeReduction)
import D3.Selection (SelectionAttribute)
import D3.Simulation.Types (SimVariable(..), _forceStatus, _forceStatuses, _onlyTheseForcesActive, initialSimulationState, toggleForceStatus)
import D3Tagless.Capabilities (actualizeForces, setConfigVariable, start, stop)
import D3Tagless.Instance.Simulation (evalEffectSimulation, runWithD3_Simulation)
import Data.Array (filter, foldl, (:))
import Data.Either (hush)
import Data.Lens (use, view, (%=), (.=))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, liftEffect)
import Halogen as H
import Halogen.Subscription (Listener)
import Halogen.Subscription as HS
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..), VizEvent(..))
import Stories.Spago.Forces (forceLibrary)
import Stories.Spago.HTML (render)
import Stories.Spago.State (State, _callback, _chooseNodes, _cssClass, _enterselections, _links, _linksActive, _linksShown, _model, _modelLinks, _modelNodes, _nodeInitializerFunctions, _nodes, _sceneAttributes, _sceneForces, _staging, _stagingLinkFilter, _stagingLinks, _stagingNodes, initialScene)

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
    , scene: initialScene
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

    _callback .= (simulationEvent listener)
    pure unit

  Finalize -> pure unit

  -- here's where we convert low level D3 Events from the visualization into the appropriate Halogen Action
  -- this keeps the low level code concerned only with the DOM syntax of the event, while the app (Halogen) will
  -- deal with the app-level semantics
  EventFromVizualization ve -> do
    case ve of
      NodeClick (IsPackage _) id -> handleAction $ ToggleChildrenOfNode id
      NodeClick (IsModule _)  id -> handleAction $ SpotlightNode id
  
  -- REVIEW this isn't a good way to do this, needs list of open nodes or something
  ToggleChildrenOfNode id -> do -- just a copy of PackageGrid right now, need to refactor so that it's all parameterized
    _chooseNodes .= (isPackageOrVisibleModule id)
    runSimulation

  UnToggleChildrenOfNode _ -> do 
    _chooseNodes .= isPackage 
    runSimulation

  SpotlightNode _ -> runWithD3_Simulation stop

  Scene PackageGrid -> do
    _chooseNodes     .= allNodes
    _linksShown      .= isM2P_Link
    _linksActive     .= const true
    _sceneForces     .= [ "packageGrid", "clusterx", "clustery", "collide2" ]
    _cssClass        .= "cluster"
    _sceneAttributes .= clusterSceneAttributes
    _nodeInitializerFunctions .= [ unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY ]
    -- runWithD3_Simulation $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    runSimulation

  Scene PackageGraph -> do
    -- 1. set up the scene
    _chooseNodes     .= isPackage
    _linksShown      .= isP2P_Link
    _linksActive     .= (sourcePackageIs "my-project")
    _sceneForces     .= ["center", "collide2", "charge2", "packageOrbit"]
    _cssClass        .= "graph"
    _sceneAttributes .= graphSceneAttributes
    _nodeInitializerFunctions .= [ unpinAllNodes, packagesNodesToPhyllotaxis, fixNamedNodeTo "my-project" { x: 0.0, y: 0.0 } ]
    -- runWithD3_Simulation $ removeNamedSelection "treelinksSelection"
    -- runWithD3_Simulation $ uniformlyDistributeNodes -- FIXME
    runSimulation

  Scene (ModuleTree treetype) -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .= 
      case treetype of 
        Radial -> const true
        _      -> const false    
    _cssClass        .= "tree"
    _sceneAttributes .= treeSceneAttributes
    _sceneForces     .= 
      case treetype of
        Horizontal -> [ "htreeNodesX", "htreeNodesY", "charge1", "collide2" ]
        Vertical   -> [ "vtreeNodesX", "vtreeNodesY", "charge1", "collide2" ]
        Radial     -> [ "center", "collide2", "charge2" ]
    _nodeInitializerFunctions .=
      case treetype of
        Horizontal -> [ unpinAllNodes, treeNodesToTreeXY_H ]
        Vertical   -> [ unpinAllNodes, treeNodesToTreeXY_V ]
        Radial     -> [ unpinAllNodes, modulesNodesToPhyllotaxis, fixNamedNodeTo "Main" { x: 0.0, y: 0.0 } ]
    -- runWithD3_Simulation $ removeNamedSelection "graphlinksSelection"
    runSimulation 
    
  ToggleForce label -> do
    _forceStatus label %= toggleForceStatus
    runSimulation -- maybe also setConfigVariable $ Alpha 0.7

  Filter (LinkShowFilter filterFn) -> do
    _linksShown .= filterFn
    runSimulation -- maybe also setConfigVariable $ Alpha 0.7

  Filter (LinkForceFilter filterFn) -> do
    _linksActive .= filterFn
    runSimulation -- maybe also setConfigVariable $ Alpha 0.7

  Filter (NodeFilter filterFn) -> do
    _chooseNodes .= filterFn
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
  nodeInitializerFunctions <- use _nodeInitializerFunctions

  _stagingLinks      .= (filter linksShown $ view _modelLinks state)
  _stagingLinkFilter .= linksActive
  let rawnodes = filter chooseNodes $ view _modelNodes state
      initializedNodes = foldl (\b a -> a b) rawnodes nodeInitializerFunctions
  
  _stagingNodes      .= initializedNodes
  
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
  let attributesWithCallback = sceneAttributes { circles = callback : sceneAttributes.circles } -- FIXME we don't actually want to stick the default value on here, needs to be Maybe
  forces          <- use _sceneForces
  runWithD3_Simulation do
    stop
    _forceStatuses %= _onlyTheseForcesActive forces
    actualizeForces
    Graph.updateSimulation staging attributesWithCallback
    setConfigVariable $ Alpha 1.0
    start


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
