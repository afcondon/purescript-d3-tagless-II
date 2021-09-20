module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Examples.Spago.Draw (graphSceneAttributes, treeSceneAttributes)
import D3.Examples.Spago.Draw as Graph
import D3.Examples.Spago.Files (SpagoGraphLinkID, isM2M_Graph_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, allNodes, convertFilesToGraphModel, isModule, isPackage)
import D3.Examples.Spago.Tree (treeReduction)
import D3.Simulation.Types (ForceStatus(..), SimVariable(..), _name, initialSimulationState, toggleForceStatus)
import D3Tagless.Capabilities (addForces, setConfigVariable, start)
import D3Tagless.Instance.Simulation (evalEffectSimulation, runD3SimM)
import Data.Array (elem, filter)
import Data.Either (hush)
import Data.Lens (modifying, use, view, (%=))
import Data.Lens.At (at)
import Data.Map (fromFoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..))
import Stories.Spago.Forces (forceLibrary)
import Stories.Spago.HTML (render)
import Stories.Spago.State (State, _cssClass, _enterselections, _links, _model, _modelLinks, _modelNodes, _nodes, _staging, _stagingForces, _stagingLinks, _stagingNodes)

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
  initialState = { 
      svgClass: ""
    , model: Nothing
    , staging: { selections: { nodes: Nothing, links: Nothing }, rawdata: { nodes: [], links: [] }, forces: M.empty }
    , simulation: initialSimulationState 1 -- TODO replace number with unit when all working satisfactorily 
  }

handleAction :: forall m. 
  Bind m =>
  MonadAff m =>
  MonadState State m => 
  Action -> m Unit
handleAction = case _ of

  Initialize -> do    -- TODO think we actually don't want to be doing anything here until the component is shown
    (maybeModel :: Maybe SpagoModel) <- H.liftAff readModelData
    _model %= (const maybeModel)
    runD3SimM $ addForces forceLibrary -- NB these are all disabled initially
    openSelections <- evalEffectSimulation Graph.initialize -- should result in the "enter" selections being in the simulation
    (_staging <<< _enterselections <<< _nodes) %= (const $ openSelections.nodes) 
    (_staging <<< _enterselections <<< _links) %= (const $ openSelections.links)
  
  Finalize -> pure unit

  Scene PackageGrid -> do
    _cssClass %= (const "cluster")
    -- TODO make this removeSelection part of the Halogen State of the component
    -- runD3SimM $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    setNodesLinksForces { chooseLinks: isM2P_Link
                        , chooseNodes: allNodes
                        , forces: [ "packageGrid", "clusterx", "clustery", "collide1" ] }
    staging <- use _staging
    runD3SimM $ Graph.updateSimulation staging graphSceneAttributes

  Scene PackageGraph -> do
    _cssClass %= (const "graph")
    -- runD3SimM $ removeNamedSelection "treelinksSelection"
    -- runD3SimM $ uniformlyDistributeNodes -- FIXME
    setNodesLinksForces { chooseLinks: isP2P_Link
                        , chooseNodes: isPackage
                        , forces: [ "centerNamedNode", "center", "collide2", "charge2", "links"] }
    staging <- use _staging
    runD3SimM $ Graph.updateSimulation staging graphSceneAttributes

  Scene (ModuleTree _) -> do
    _cssClass %= (const "tree")
    -- runD3SimM $ removeNamedSelection "graphlinksSelection"
    setNodesLinksForces { forces: ["treeNodesPinned", "links", "center", "charge1", "collide2", "moduleOrbit1" ]
                        , chooseNodes: isModule           -- show all modules, 
                        , chooseLinks: isM2M_Graph_Link } -- show all links, the "non-tree" modules will be drawn in to fixed tree nodes
    staging <- use _staging
    runD3SimM $ Graph.updateSimulation staging treeSceneAttributes
    
  ToggleForce label -> do
    modifying (_stagingForces <<< at label) (\maybeStatus -> toggleForceStatus <$> maybeStatus)
    staging <- use _staging
    runD3SimM $ Graph.updateForcesOnly staging

  Filter (LinkFilter filterFn) -> do
    chooseLinks filterFn
    staging <- use _staging
    runD3SimM $ Graph.updateSimulation staging graphSceneAttributes

  Filter (NodeFilter filterFn) -> do
    chooseNodes filterFn
    staging <- use _staging
    runD3SimM $ Graph.updateSimulation staging graphSceneAttributes

  ChangeStyling style -> do
    _cssClass %= (const style) -- modify_ (\s -> s { svgClass = style })

  ChangeSimConfig c -> do
    runD3SimM $ setConfigVariable c 
    runD3SimM start   

  StartSim -> do
    runD3SimM $ setConfigVariable $ Alpha 1.0
    runD3SimM start

  StopSim -> do
    runD3SimM $ setConfigVariable $ Alpha 0.0

-- ======================================================================================================================
-- some utility functions to manage what data from the model gets given to the visualization code
-- (and also what forces should be engaged)
-- ======================================================================================================================
type SpagoConfigRecord = { -- convenience type to hold filter functions for nodes & links and list of forces to activate
    chooseNodes :: (SpagoSimNode -> Boolean)
  , chooseLinks :: (SpagoGraphLinkID -> Boolean)
  , forces      :: Array String
}

chooseForces :: forall m. MonadState State m => Array String -> m Unit
chooseForces forceNames = do
  let 
    setStatus f = do
      let fName   = view _name f
          fStatus = if fName `elem` forceNames
                    then ForceActive
                    else ForceDisabled
      Tuple fName fStatus
  _stagingForces %= const (fromFoldable $ setStatus <$> forceLibrary)

-- filter links from Maybe Model into Staging
chooseLinks :: forall m. MonadState State m => (SpagoGraphLinkID -> Boolean) -> m Unit
chooseLinks filterFn = do
  state <- get
  _stagingLinks %= const (filter filterFn $ view _modelLinks state)

-- filter nodes from Maybe Model into Staging
chooseNodes :: forall m. MonadState State m => (SpagoSimNode -> Boolean) -> m Unit
chooseNodes filterFn = do
  state <- get
  _stagingNodes %= const (filter filterFn $ view _modelNodes state)

setNodesLinksForces :: forall m.
  MonadState State m =>
  SpagoConfigRecord ->
  m Unit
setNodesLinksForces config = do
  state <- get
  _stagingLinks  %= const (filter config.chooseLinks $ view _modelLinks state)
  _stagingNodes  %= const (filter config.chooseNodes $ view _modelNodes state)
  chooseForces config.forces

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
