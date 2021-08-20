module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, modify_)
import D3.Examples.Spago.Draw (graphAttrs)
import D3.Examples.Spago.Draw as Graph
import D3.Examples.Spago.Files (SpagoGraphLinkID, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, convertFilesToGraphModel, isPackage, isUsedModule, noFilter)
import D3.Examples.Spago.Tree (treeReduction)
import D3.FFI (pinNamedNode_, pinTreeNode_, unpinNode_)
import D3.Simulation.Functions (simulationGetNodes, simulationStart, simulationStop)
import D3.Simulation.Types (SimVariable(..), initialSimulationState)
import D3Tagless.Capabilities (addForces, enableOnlyTheseForces, setConfigVariable, toggleForceByLabel)
import D3Tagless.Instance.Simulation (evalEffectSimulation, runEffectSimulation)
import Data.Array (length)
import Data.Either (hush)
import Data.Lens (over, set)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Debug (trace)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..))
import Stories.Spago.Forces (forces, gridForceSettings, packageForceSettings, treeForceSettings)
import Stories.Spago.HTML (render)
import Stories.Spago.Lenses (_activeForces, _class, _nodesForSim)
import Stories.Spago.State (State)
  
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
      simulationState: initialSimulationState 1 -- TODO replace number with unit when all working satisfactorily 
    , svgClass: "cluster"
    , model: Nothing
    , simDataRaw: Nothing
    , simDataCooked: Nothing
    , activeForces: []
  }

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  Initialize -> do    
    (maybeModel :: Maybe SpagoModel) <- H.liftAff getModel
    let _ = trace { action: "Initialize", model: maybeModel } \_ -> unit
    H.modify_ (\s -> s { model = maybeModel })
    _ <- evalEffectSimulation Graph.setup
    runEffectSimulation (addForces forces)
    -- handleAction $ Scene PackageGraph

  Finalize -> pure unit

  Scene PackageGrid -> do
    setCssEnvironment "cluster"
    -- TODO make this removeSelection part of the Halogen State of the component
    -- runEffectSimulation $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    setActiveLinks  isM2P_Link -- only module-to-package (ie containment) links
    setActiveNodes  noFilter   -- all the nodes
    setActiveForces gridForceSettings
    unpinActiveNodes

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateSimulation state.simDataRaw graphAttrs
    runEffectSimulation $ enableOnlyTheseForces state.activeForces
    simulationStart

  Scene PackageGraph -> do
    setCssEnvironment "graph"
    -- runEffectSimulation $ removeNamedSelection "treelinksSelection"
    setActiveLinks isP2P_Link
    setActiveNodes isPackage
    setActiveForces packageForceSettings
    unpinActiveNodes
    -- runEffectSimulation $ uniformlyDistributeNodes -- FIXME

    state <- H.get
    let _ = trace { action: "Scene PackageGraph", model: state.model } \_ -> unit
    simulationStop
    runEffectSimulation $ Graph.updateSimulation state.simDataRaw graphAttrs
    runEffectSimulation $ enableOnlyTheseForces state.activeForces
    simulationStart

  Scene (ModuleTree _) -> do
    setCssEnvironment "tree"
    -- runEffectSimulation $ removeNamedSelection "graphlinksSelection"
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
    runEffectSimulation $ Graph.updateSimulation state.simDataRaw graphAttrs
    simulationStart

  Filter (NodeFilter x) -> do
    setActiveNodes x

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateSimulation state.simDataRaw graphAttrs
    n <- simulationGetNodes
    let _ = trace { numNodes: length n }
    simulationStart



  ChangeStyling style -> modify_ (\s -> s { svgClass = style })

  ChangeSimConfig c -> runEffectSimulation $ setConfigVariable c

  StartSim -> simulationStart

  StopSim -> runEffectSimulation (setConfigVariable $ Alpha 0.0)

setCssEnvironment :: forall m. MonadState State m => String -> m Unit
setCssEnvironment string = modify_ $ set _class string

setActiveForces :: forall m. MonadState State m => Array String -> m Unit
setActiveForces forces = modify_ $ set _activeForces forces

setActiveLinks :: forall m. MonadState State m => (SpagoGraphLinkID -> Boolean) -> m Unit
setActiveLinks fn = pure unit -- modify_ $ chooseSimLinks fn

setActiveNodes :: forall m. MonadState State m => (SpagoSimNode -> Boolean) -> m Unit
setActiveNodes fn = pure unit -- modify_ $ chooseSimNodes fn

pinTreeNodes :: forall m. MonadState State m => m Unit
pinTreeNodes = modify_ $ over _nodesForSim (pinTreeNode_ <$> _)

centerPinNamedNode :: forall m. MonadState State m => String -> m Unit
centerPinNamedNode name = modify_ $ over _nodesForSim ((pinNamedNode_ name 0.0 0.0) <$> _)

unpinActiveNodes :: forall m. MonadState State m => m Unit
unpinActiveNodes = modify_ $ over _nodesForSim (unpinNode_ <$> _)

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
