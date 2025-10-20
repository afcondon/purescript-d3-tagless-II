module PSD3.Spago where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Sugar (onMouseEventEffectful)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (MouseEvent(..))
import D3.Viz.Spago.Draw (getVizEventFromClick)
import D3.Viz.Spago.Draw as Graph
import D3.Viz.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Viz.Spago.Files (NodeType(..), isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Viz.Spago.Model (SpagoModel, allNodes, convertFilesToGraphModel, fixNamedNodeTo, isPackage, isPackageOrVisibleModule, isUsedModule, moduleNodesToContainerXY, modulesNodesToPhyllotaxis, packageNodesToGridXY, packagesNodesToPhyllotaxis, sourcePackageIs, treeNodesToTreeXY_R, unpinAllNodes)
import D3.Viz.Spago.Tree (treeReduction)
import D3.FFI (linksForceName)
import D3.Selection (SelectionAttribute)
import D3.Simulation.Types (SimVariable(..), initialSimulationState, onlyTheseForcesActive, toggleForceStatus)
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
import Halogen.Subscription as HS
import PSD3.Spago.Actions (Action(..), FilterData(..), Scene(..), StyleChange(..), VizEvent(..))
import PSD3.Spago.Forces (forceLibrary)
import PSD3.Spago.HTML (render)
import PSD3.Spago.State (State, _callback, _chooseNodes, _cssClass, _enterselections, _forceStatus, _forceStatuses, _links, _linksActive, _linksShown, _model, _modelLinks, _modelNodes, _nodeInitializerFunctions, _nodes, _sceneAttributes, _staging, _stagingLinkFilter, _stagingLinks, _stagingNodes, initialScene)

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
    , scene: initialScene forceLibrary
    }

simulationEvent :: HS.Listener Action -> SelectionAttribute
simulationEvent l = onMouseEventEffectful MouseClick (\e d t -> liftEffect $ HS.notify l (EventFromVizualization (getVizEventFromClick e d t)))

handleAction :: forall t316 t317 t318.
  MonadAff t318 =>
  Action ->
  HalogenM State Action t316 t317 t318 Unit
handleAction = case _ of

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

  EventFromVizualization ve -> do
    case ve of
      NodeClick (IsPackage _) id -> handleAction $ ToggleChildrenOfNode id
      NodeClick (IsModule _)  id -> handleAction $ SpotlightNode id

  ToggleChildrenOfNode id -> do
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
    _cssClass        .= "cluster"
    _sceneAttributes .= clusterSceneAttributes
    _forceStatuses   %= onlyTheseForcesActive [ "clusterx_P", "clustery_P", "clusterx_M", "clustery_M", "collide2" ]
    _nodeInitializerFunctions .= [ unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY ]
    runSimulation

  Scene PackageGraph -> do
    _chooseNodes     .= isPackage
    _linksShown      .= isP2P_Link
    _linksActive     .= (sourcePackageIs "my-project")
    _forceStatuses   %= onlyTheseForcesActive ["center", "collide2", "charge2", "packageOrbit", linksForceName ]
    _cssClass        .= "graph"
    _sceneAttributes .= graphSceneAttributes
    _nodeInitializerFunctions .= [ unpinAllNodes, packagesNodesToPhyllotaxis, fixNamedNodeTo "my-project" { x: 0.0, y: 0.0 } ]
    runSimulation

  Scene LayerSwarm -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .= const true
    _cssClass        .= "tree"
    _sceneAttributes .= treeSceneAttributes
    _forceStatuses   %= onlyTheseForcesActive [ "htreeNodesX", "collide1", "y", linksForceName ]
    _nodeInitializerFunctions .= [ unpinAllNodes ]
    runSimulation

  Scene (ModuleTree Radial) -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .= const true
    _cssClass        .= "tree radial"
    _sceneAttributes .= treeSceneAttributes
    _forceStatuses   %= onlyTheseForcesActive [ "center", "collide2", "chargetree", "charge2", linksForceName ]
    _nodeInitializerFunctions .= [ unpinAllNodes, treeNodesToTreeXY_R, fixNamedNodeTo "Main" { x: 0.0, y: 0.0 } ]
    runSimulation

  Scene (ModuleTree Horizontal) -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .= const false
    _cssClass        .= "tree horizontal"
    _sceneAttributes .= treeSceneAttributes
    _forceStatuses   %= onlyTheseForcesActive [ "htreeNodesX", "htreeNodesY", "charge1", "collide2" ]
    _nodeInitializerFunctions .= [ unpinAllNodes ]
    runSimulation

  Scene (ModuleTree Vertical) -> do
    _chooseNodes     .= isUsedModule
    _linksShown      .= isM2M_Tree_Link
    _linksActive     .=  const false
    _cssClass        .= "tree vertical"
    _sceneAttributes .= treeSceneAttributes
    _forceStatuses   %= onlyTheseForcesActive [ "vtreeNodesX", "vtreeNodesY", "charge1", "collide2" ]
    _nodeInitializerFunctions .= [ unpinAllNodes ]
    runSimulation

  ToggleForce label -> do
    _forceStatus label %= toggleForceStatus
    runSimulation

  Filter (LinkShowFilter filterFn) -> do
    _linksShown .= filterFn
    runSimulation

  Filter (LinkForceFilter filterFn) -> do
    _linksActive .= filterFn
    runSimulation

  Filter (NodeFilter filterFn) -> do
    _chooseNodes .= filterFn
    runSimulation

  ChangeStyling (TopLevelCSS style) -> do
    _cssClass .= style

  ChangeStyling (GraphStyle attributes) -> do
    _sceneAttributes .= attributes
    runSimulation

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

runSimulation :: forall m.
  MonadEffect m =>
  MonadState State m =>
  m Unit
runSimulation = do
  stageDataFromModel
  staging         <- use _staging
  callback        <- use _callback
  sceneAttributes <- use _sceneAttributes
  forceStatuses   <- use _forceStatuses
  let attributesWithCallback = sceneAttributes { circles = callback : sceneAttributes.circles }
  runWithD3_Simulation do
    stop
    actualizeForces forceStatuses
    Graph.updateSimulation staging attributesWithCallback
    setConfigVariable $ Alpha 1.0
    start


readModelData :: Aff (Maybe SpagoModel)
readModelData = do
  let datadir = "./data/spago-data/"  -- Relative to v2/ serving directory
  moduleJSON  <- AJAX.get ResponseFormat.string $ datadir <> "modules.json"
  packageJSON <- AJAX.get ResponseFormat.string $ datadir <> "packages.json"
  lsdepJSON   <- AJAX.get ResponseFormat.string $ datadir <> "lsdeps.jsonlines"
  locJSON     <- AJAX.get ResponseFormat.string $ datadir <> "LOC.json"
  let model = hush $ convertFilesToGraphModel <$> moduleJSON <*> packageJSON <*> lsdepJSON <*> locJSON

  pure (addTreeToModel "Main" model)

addTreeToModel :: String -> Maybe SpagoModel -> Maybe SpagoModel
addTreeToModel rootName maybeModel = do
  model  <- maybeModel
  rootID <- M.lookup rootName model.maps.name2ID
  pure $ treeReduction rootID model
