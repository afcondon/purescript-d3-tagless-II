module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get, modify_)
import D3.Data.Tree (TreeLayout(..))
import D3.Examples.Spago.Draw (graphAttrs, removeNamedSelection, treeAttrs)
import D3.Examples.Spago.Draw as Graph
import D3.Examples.Spago.Files (SpagoGraphLinkID, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, convertFilesToGraphModel, isPackage, isUsedModule, noFilter)
import D3.Examples.Spago.Tree (treeReduction)
import D3.FFI (pinNamedNode_, pinTreeNode_, unpinNode_)
import D3.Simulation.Functions (simulationGetNodes, simulationStart, simulationStop)
import D3.Simulation.Types (D3SimulationState_, SimVariable(..))
import D3Tagless.Capabilities (addForces, enableOnlyTheseForces, setConfigVariable, toggleForceByLabel)
import D3Tagless.Instance.Simulation (runEffectSimulation)
import Data.Array (filter, length)
import Data.Either (hush)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Debug (trace)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..))
import Stories.Spago.Forces (forces, gridForceSettings, gridForceSettings2, packageForceSettings, treeForceSettings)
import Stories.Spago.HTML (render)
import Stories.Spago.State (State)

type Input = D3SimulationState_
  
component :: forall query output m. MonadAff m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize }
  }
  where

  initialState :: Input -> State
  initialState simulation = { 
      simulationState: simulation
    , svgClass: "cluster"
    , model: Nothing
    , activeLinks: []
    , activeNodes: []
    , activeForces: []
  }

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  Initialize -> do    
    (model :: Maybe SpagoModel) <- H.liftAff getModel
    let _ = trace { action: "Initialize", model: model } \_ -> unit
    H.modify_ (\s -> s { model = model })
    runEffectSimulation Graph.setup
    runEffectSimulation (addForces forces)
    -- handleAction $ Scene PackageGraph

  Finalize -> pure unit

  Scene PackageGrid -> do
    setCssEnvironment "cluster"
    runEffectSimulation $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    setActiveLinks  isM2P_Link -- only module-to-package (ie containment) links
    setActiveNodes  noFilter   -- all the nodes
    setActiveForces gridForceSettings
    unpinActiveNodes

    state <- H.get
    let _ = trace { action: "Scene PackageGrid", model: state.model } \_ -> unit
    simulationStop
    runEffectSimulation $ Graph.updateSimulation state.activeNodes state.activeLinks graphAttrs
    runEffectSimulation $ enableOnlyTheseForces state.activeForces
    simulationStart

  Scene PackageGraph -> do
    setCssEnvironment "graph"
    runEffectSimulation $ removeNamedSelection "treelinksSelection"
    setActiveLinks isP2P_Link
    setActiveNodes isPackage
    setActiveForces packageForceSettings
    unpinActiveNodes
    -- runEffectSimulation $ uniformlyDistributeNodes -- FIXME

    state <- H.get
    let _ = trace { action: "Scene PackageGraph", model: state.model } \_ -> unit
    simulationStop
    runEffectSimulation $ Graph.updateSimulation state.activeNodes state.activeLinks graphAttrs
    runEffectSimulation $ enableOnlyTheseForces state.activeForces
    simulationStart

  Scene (ModuleTree _) -> do
    setCssEnvironment "tree"
    runEffectSimulation $ removeNamedSelection "graphlinksSelection"
    setActiveForces treeForceSettings
    setActiveNodes isUsedModule
    setActiveLinks isM2M_Tree_Link
    pinTreeNodes -- side-effect, because if we make _new_ nodes the links won't be pointing to them

    state <- H.get
    let _ = trace { action: "Scene ModulerTree", model: state.model } \_ -> unit
    simulationStop
    -- runEffectSimulation $ Graph.updateSimulation state.activeNodes treeAttrs
    -- runEffectSimulation $ Graph.updateTreeLinks state.activeLinks Horizontal
    runEffectSimulation $ enableOnlyTheseForces treeForceSettings
    simulationStart

  ToggleForce label -> do
    simulationStop
    runEffectSimulation $ toggleForceByLabel label
    simulationStart

  Filter (LinkFilter x) -> do
    setActiveLinks x

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateSimulation state.activeNodes state.activeLinks graphAttrs
    simulationStart

  Filter (NodeFilter x) -> do
    setActiveNodes x

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateSimulation state.activeNodes state.activeLinks graphAttrs
    n <- simulationGetNodes
    let _ = trace { numNodes: length n }
    simulationStart



  ChangeStyling style -> modify_ (\s -> s { svgClass = style })

  ChangeSimConfig c -> runEffectSimulation $ setConfigVariable c

  StartSim -> simulationStart

  StopSim -> runEffectSimulation (setConfigVariable $ Alpha 0.0)

setCssEnvironment :: forall m. MonadState State m => String -> m Unit
setCssEnvironment string = modify_ (\s -> s { svgClass = string })

setActiveForces :: forall m. MonadState State m => Array String -> m Unit
setActiveForces forces = modify_ (\s -> s { activeForces = forces })

setActiveLinks :: forall m. MonadState State m => (SpagoGraphLinkID -> Boolean) -> m Unit
setActiveLinks fn = do
  state <- get
  case state.model of
    Nothing -> pure unit
    (Just graph) -> modify_ (\s -> s { activeLinks = filter fn graph.links })

setActiveNodes :: forall m. MonadState State m => (SpagoSimNode -> Boolean) -> m Unit
setActiveNodes fn = do
  state <- H.get
  case state.model of
    Nothing -> pure unit
    (Just graph) -> modify_ (\s -> s { activeNodes = filter fn graph.nodes })

pinTreeNodes :: forall m. MonadState State m => m Unit
pinTreeNodes = do
  state <- H.get
  let _ = pinTreeNode_ <$> state.activeNodes -- NB side-effecting on the nodes in the state so that object refs in links stay good
  pure unit

centerPinNamedNode :: forall m. MonadState State m => String -> m Unit
centerPinNamedNode name = do
  state <- H.get
  let _ = (pinNamedNode_ name 0.0 0.0) <$> state.activeNodes -- NB side-effecting on the nodes in the state so that object refs in links stay good
  pure unit

unpinActiveNodes :: forall m. MonadState State m => m Unit
unpinActiveNodes = do
  state <- H.get
  let _ = unpinNode_ <$> state.activeNodes -- NB side-effecting on the nodes in the state so that object refs in links stay good
  pure unit

-- getModel will try to build a model from files and to derive a dependency tree from Main
-- the dependency tree will contain all nodes reachable from Main but NOT all links
getModel :: Aff (Maybe SpagoModel)
getModel = do
  let datadir = "http://localhost:1234/spago-small/"
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
