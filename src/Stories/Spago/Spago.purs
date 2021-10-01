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
import Data.Array (filter)
import Data.Either (hush)
import Data.Lens (use, view, (%=))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Debug (trace)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen (HalogenM, liftEffect, raise)
import Halogen as H
import Halogen.Subscription (Listener)
import Halogen.Subscription as HS
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..), VizEvent(..))
import Stories.Spago.Forces (forceLibrary)
import Stories.Spago.HTML (render)
import Stories.Spago.State (State, _callback, _cssClass, _enterselections, _links, _model, _modelLinks, _modelNodes, _nodes, _staging, _stagingLinkFilter, _stagingLinks, _stagingNodes)

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
    { 
      svgClass: ""
    , model: Nothing
    , staging: { selections: { nodes: Nothing, links: Nothing }, linksWithForce: const true, rawdata: { nodes: [], links: [] } }
    , simulation: initialSimulationState forceLibrary
    , callback: x 0.0
    , scene: PackageGrid
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

    modify_ _ { callback = simulationEvent listener }
    pure unit

  Finalize -> pure unit

  -- here's where we convert low level D3 Events from the visualization into the appropriate Halogen Action
  -- this keeps the low level code concerned only with the syntax of the event, while the app (Halogen) will
  -- deal with the semantics
  EventFromVizualization ve -> do
    case ve of
      NodeClick (IsPackage _) id -> handleAction $ ToggleChildrenOfNode id
      NodeClick (IsModule _)  id -> handleAction $ SpotlightNode id
  
  ToggleChildrenOfNode id -> runWithD3_Simulation do -- just a copy of PackageGrid right now, need to refactor so that it's all parameterized
    configureScene {
        linksShown: isM2P_Link
      , chooseNodes: isPackageOrVisibleModule id
      , linksWithForce: const true
      , activeForces: [ "packageGrid", "clusterx", "clustery", "collide2" ]
      , sceneAttributes: []
       }
    staging  <- use _staging
    callback <- use _callback
    runWithD3_Simulation do
      actualizeForces
      Graph.updateSimulation staging (clusterSceneAttributes callback)
      setConfigVariable $ Alpha 1.0

  -- FIXME just a copy of PackageGrid right now, need to refactor so that it's all parameterized
  UnToggleChildrenOfNode id -> runWithD3_Simulation do 
    configureScene {
        linksShown: isM2P_Link
      , chooseNodes: isPackage
      , linksWithForce: const true
      , activeForces: [ "packageGrid", "clusterx", "clustery", "collide2" ]
      , sceneAttributes: []
      }
    staging  <- use _staging
    callback <- use _callback
    runWithD3_Simulation do
      actualizeForces
      Graph.updateSimulation staging (clusterSceneAttributes callback)
      setConfigVariable $ Alpha 1.0

  SpotlightNode id -> runWithD3_Simulation stop

  Scene PackageGrid -> do
    _cssClass %= (const "cluster")
    -- TODO make this removeSelection part of the Halogen State of the component
    -- runWithD3_Simulation $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    configureScene {
        linksShown: isM2P_Link
      , chooseNodes: allNodes
      , linksWithForce: const true
      , activeForces: [ "packageGrid", "clusterx", "clustery", "collide2" ]
      , sceneAttributes: []
      }
    _stagingNodes %= addGridPoints
    staging <- use _staging
    callback <- use _callback
    runWithD3_Simulation do
      actualizeForces
      Graph.updateSimulation staging (clusterSceneAttributes callback)
      setConfigVariable $ Alpha 1.0

  Scene PackageGraph -> do
    _cssClass %= (const "graph")
    -- runWithD3_Simulation $ removeNamedSelection "treelinksSelection"
    -- runWithD3_Simulation $ uniformlyDistributeNodes -- FIXME
    configureScene {
        linksShown: isP2P_Link
      , chooseNodes: isPackage
      , linksWithForce: sourcePackageIs "my-project"
      , activeForces: ["centerNamedNode", "center", "collide2", "charge2", "packageOrbit"]
      , sceneAttributes: []
      }
    staging <- use _staging
    runWithD3_Simulation do
      actualizeForces
      Graph.updateSimulation staging graphSceneAttributes
      setConfigVariable $ Alpha 1.0

  Scene (ModuleTree _) -> do
    _cssClass %= (const "tree")
    -- runWithD3_Simulation $ removeNamedSelection "graphlinksSelection"
    configureScene { chooseNodes: isUsedModule       -- show all modules, 
                    , linksShown: isM2M_Tree_Link  -- show only Tree links
                    , linksWithForce: const true   -- all links shown are added to simulation
                    , activeForces: [ "treeNodesX", "treeNodesY", "center", "charge1", "collide2", "unusedOrbit" ]
                    , sceneAttributes: []
                  }
                     -- isM2M_TreeLink_ } -- show all links, the "non-tree" modules will be drawn in to fixed tree nodes
    staging <- use _staging
    runWithD3_Simulation do
      actualizeForces
      Graph.updateSimulation staging treeSceneAttributes
      setConfigVariable $ Alpha 1.0
    
  ToggleForce label -> do
    _forceStatus label %= toggleForceStatus
    runWithD3_Simulation do
      actualizeForces
      start
      setConfigVariable $ Alpha 0.7

  Filter (LinkShowFilter filterFn) -> do
    linksShown filterFn
    staging <- use _staging
    runWithD3_Simulation do
      Graph.updateSimulation staging graphSceneAttributes
      setConfigVariable $ Alpha 0.7

  Filter (LinkForceFilter filterFn) -> do
    linkSimulation filterFn
    staging <- use _staging
    runWithD3_Simulation do
      Graph.updateSimulation staging graphSceneAttributes
      setConfigVariable $ Alpha 0.7

  Filter (NodeFilter filterFn) -> do
    chooseNodes filterFn
    staging <- use _staging
    runWithD3_Simulation do
      Graph.updateSimulation staging graphSceneAttributes
      setConfigVariable $ Alpha 0.7

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
-- some utility functions to manage what data from the model gets given to the visualization code
-- (and also what forces should be engaged)
-- ======================================================================================================================
type SpagoConfigRecord = { -- convenience type to hold filter functions for nodes & links and list of forces to activate
    chooseNodes :: (SpagoSimNode -> Boolean)
  , linksShown :: (SpagoGraphLinkID -> Boolean)
  , linksWithForce  :: (Datum_ -> Boolean) -- defined as Datum_ but it's really Link_, ugly
  , activeForces :: Array Label
  , sceneAttributes :: Array SelectionAttribute
  -- could add the config variables here
}
defaultSceneConfig :: SpagoConfigRecord
defaultSceneConfig = {
    chooseNodes: const true -- chooses all nodes
  , linksShown:  const true
  , linksWithForce: const true
  , activeForces: []
  , sceneAttributes: []
}

-- filter links from Maybe Model into Staging
linksShown :: forall m. MonadState State m => (SpagoGraphLinkID -> Boolean) -> m Unit
linksShown filterFn = do
  state <- get
  _stagingLinks %= const (filter filterFn $ view _modelLinks state)

-- a further level of filtering to put subset of links into Simulation, ie exerting force
linkSimulation :: forall m. MonadState State m => (Datum_ -> Boolean) -> m Unit
linkSimulation filterFn = do
  _stagingLinkFilter %= const filterFn

-- filter nodes from Maybe Model into Staging
chooseNodes :: forall m. MonadState State m => (SpagoSimNode -> Boolean) -> m Unit
chooseNodes filterFn = do
  state <- get
  _stagingNodes %= const (filter filterFn $ view _modelNodes state)
  _stagingNodes %= addGridPoints

configureScene :: forall m.
  MonadState State m =>
  SpagoConfigRecord ->
  m Unit
configureScene config = do
  state <- get
  _stagingLinks %= const (filter config.linksShown $ view _modelLinks state)
  _stagingLinkFilter %= const config.linksWithForce
  _stagingNodes %= const (filter config.chooseNodes $ view _modelNodes state)
  _stagingNodes %= addGridPoints
  _forceStatuses %= _onlyTheseForcesActive config.activeForces

 -- FIXME this is where the grid point can be set, once we know how many packages we have

sourcePackageIs name link = (link_.source link).name == name
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
