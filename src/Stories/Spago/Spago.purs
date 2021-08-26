module Stories.Spago where

import Prelude
import Stories.Spago.State (State, _activeForces, _class, _d3Simulation, _model, _staging)

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, modify_)
import D3.Examples.Spago.Draw (graphAttrs, treeAttrs)
import D3.Examples.Spago.Draw as Graph
import D3.Examples.Spago.Files (SpagoGraphLinkID, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, convertFilesToGraphModel, isPackage, isUsedModule, noFilter)
import D3.Examples.Spago.Tree (treeReduction)
import D3.FFI (pinNamedNode_, pinTreeNode_, unpinNode_)
import D3.Simulation.Functions (simulationStart, simulationStop)
import D3.Simulation.Types (SimVariable(..), _nodedata, initialSimulationState)
import D3Tagless.Capabilities (addForces, enableOnlyTheseForces, setConfigVariable, toggleForceByLabel)
import D3Tagless.Instance.Simulation (runEffectSimulation)
import Data.Either (hush)
import Data.Lens (modifying, over, set, use)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..))
import Stories.Spago.Forces (forces, gridForceSettings, packageForceSettings, treeForceSettings)
import Stories.Spago.HTML (render)

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
      svgClass: "cluster"
    , activeForces: []
    , model: Nothing
    , staging: { selections: { nodes: Nothing, links: Nothing }, rawdata: { nodes: [], links: [] }}
    , simulation: initialSimulationState 1 -- TODO replace number with unit when all working satisfactorily 
  }

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of

  Initialize -> do    
    (maybeModel :: Maybe SpagoModel) <- H.liftAff getModel
    modifying _model (const maybeModel)
    runEffectSimulation Graph.setup -- should result in the "enter" selections being in the simulation
    runEffectSimulation (addForces forces)
    handleAction $ Scene PackageGraph

  Finalize -> pure unit

  Scene PackageGrid -> do
    setCssEnvironment "cluster"
    -- TODO make this removeSelection part of the Halogen State of the component
    -- runEffectSimulation $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    setActiveLinks  isM2P_Link -- only module-to-package (ie containment) links
    setActiveNodes  noFilter   -- all the nodes
    setActiveForces gridForceSettings
    unpinActiveNodes
    -- everything from here on down should be factorable out???
    staging <- use _staging
    forces  <- use _activeForces
    simulationStop
    runEffectSimulation $ Graph.updateSimulation staging graphAttrs
    runEffectSimulation $ enableOnlyTheseForces forces
    simulationStart

  Scene PackageGraph -> do
    setCssEnvironment "graph"
    -- runEffectSimulation $ removeNamedSelection "treelinksSelection"
    setActiveLinks isP2P_Link
    setActiveNodes isPackage
    setActiveForces packageForceSettings
    unpinActiveNodes
    -- runEffectSimulation $ uniformlyDistributeNodes -- FIXME

    staging <- use _staging
    forces  <- use _activeForces
    simulationStop
    runEffectSimulation $ Graph.updateSimulation staging graphAttrs
    runEffectSimulation $ enableOnlyTheseForces forces
    simulationStart

  Scene (ModuleTree _) -> do
    setCssEnvironment "tree"
    -- runEffectSimulation $ removeNamedSelection "graphlinksSelection"
    setActiveForces treeForceSettings
    setActiveNodes isUsedModule
    setActiveLinks isM2M_Tree_Link
    pinTreeNodes -- side-effect, because if we make _new_ nodes the links won't be pointing to them

    staging <- use _staging
    forces  <- use _activeForces
    simulationStop
    runEffectSimulation $ Graph.updateSimulation staging treeAttrs
    runEffectSimulation $ enableOnlyTheseForces forces
    simulationStart
    
  ToggleForce label -> do
    simulationStop
    runEffectSimulation $ toggleForceByLabel label
    simulationStart

  Filter (LinkFilter x) -> do
    setActiveLinks x

    staging <- use _staging
    forces  <- use _activeForces
    simulationStop
    runEffectSimulation $ Graph.updateSimulation staging graphAttrs
    runEffectSimulation $ enableOnlyTheseForces forces
    simulationStart

  Filter (NodeFilter x) -> do
    setActiveNodes x

    staging <- use _staging
    forces  <- use _activeForces
    simulationStop
    runEffectSimulation $ Graph.updateSimulation staging graphAttrs
    runEffectSimulation $ enableOnlyTheseForces forces
    simulationStart

  ChangeStyling style -> modifying _class (const style) -- modify_ (\s -> s { svgClass = style })

  ChangeSimConfig c -> runEffectSimulation $ setConfigVariable c

  StartSim -> simulationStart

  StopSim -> runEffectSimulation (setConfigVariable $ Alpha 0.0)

setCssEnvironment :: forall m. MonadState State m => String -> m Unit
setCssEnvironment string = modifying _class (const string)

setActiveForces :: forall m. MonadState State m => Array String -> m Unit
setActiveForces forces = modify_ $ set _activeForces forces

-- TODO modifying _linksInSim (filtered _linksInModel)
setActiveLinks :: forall m. MonadState State m => (SpagoGraphLinkID -> Boolean) -> m Unit
setActiveLinks fn = pure unit

-- TODO modifying _nodesInSim (filtered _nodesInModel)
setActiveNodes :: forall m. MonadState State m => (SpagoSimNode -> Boolean) -> m Unit
setActiveNodes fn = pure unit

pinTreeNodes :: forall m. MonadState State m => m Unit
pinTreeNodes = do
  let _ = over (_d3Simulation <<< _nodedata) (map pinTreeNode_)
  pure unit

centerPinNamedNode :: forall m. MonadState State m => String -> m Unit
centerPinNamedNode name = do
  let _ = over (_d3Simulation <<< _nodedata) (map $ pinNamedNode_ name 0.0 0.0)
  pure unit

unpinActiveNodes :: forall m. MonadState State m => m Unit
-- unpinActiveNodes = modify_ $ over _nodesForSim (unpinNode_ <$> _)
unpinActiveNodes = do
  let _ = over (_d3Simulation <<< _nodedata) (map unpinNode_)
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
