module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get, modify_, runStateT)
import D3.Data.Types (D3Selection_)
import D3.Examples.Spago.Draw (graphSceneAttributes, treeSceneAttributes)
import D3.Examples.Spago.Draw as Graph
import D3.Examples.Spago.Files (SpagoDataRow, SpagoGraphLinkID, SpagoLinkData, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, convertFilesToGraphModel, isPackage, isUsedModule, noFilter)
import D3.Examples.Spago.Tree (treeReduction)
import D3.FFI (dummySelection_, pinNamedNode_, pinTreeNode_, unpinNode_)
import D3.Node (NodeID)
import D3.Selection (SelectionAttribute)
import D3.Simulation.Functions (simulationStart, simulationStop)
import D3.Simulation.Types (D3SimulationState_, _nodedata, initialSimulationState)
import D3Tagless.Capabilities (class SelectionM, class SimulationM, Staging, addForces, enableOnlyTheseForces, setConfigVariable, toggleForceByLabel)
import D3Tagless.Instance.Simulation (D3SimM(..), exec_D3M_Simulation, runEffectSimulation)
import Data.Array (filter)
import Data.Either (hush)
import Data.Lens (modifying, over, set, use, (%=))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Debug (trace)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..))
import Stories.Spago.Forces as Force
import Stories.Spago.HTML (render)
import Stories.Spago.State (State, StateRow, _activeForces, _cssClass, _d3Simulation, _model, _staging, _stagingLinks, _stagingNodes)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

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
    , forces: { all: Force.forces, active: [] }
    , model: Nothing
    , staging: { selections: { nodes: Nothing, links: Nothing }, rawdata: { nodes: [], links: [] }}
    , simulation: initialSimulationState 1 -- TODO replace number with unit when all working satisfactorily 
  }

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of

  Initialize -> do    -- TODO think we actually don't want to be doing anything here until the component is shown
    (maybeModel :: Maybe SpagoModel) <- H.liftAff getModel
    let _ = trace { spago: "initialize" } \_ -> unit
    modifying _model (const maybeModel)
    -- realizeSimulation (\_ _ -> Graph.initialize) { circle: [], labels: [] }

  Finalize -> pure unit

  Scene PackageGrid -> do
    setCssEnvironment "cluster"
    -- runEffectSimulation $ removeNamedSelection "treelinksSelection" -- make sure the links-as-SVG-paths are gone before we put in links-as-SVG-lines
    stageLinks  isM2P_Link -- only module-to-package (ie containment) links
    stageNodes  noFilter   -- all the nodes
    stageForces Force.gridForces
    unpinActiveNodes
    realizeSimulation dummySelection_ Graph.updateSimulation graphSceneAttributes

  Scene PackageGraph -> do
    setCssEnvironment "graph"
    -- runEffectSimulation $ removeNamedSelection "treelinksSelection"
    stageLinks isP2P_Link
    stageNodes isPackage
    stageForces Force.packageGraphForces
    unpinActiveNodes
    -- runEffectSimulation $ uniformlyDistributeNodes -- FIXME
    -- realizeSimulation Graph.updateSimulation graphSceneAttributes

  Scene (ModuleTree _) -> do
    setCssEnvironment "tree"
    -- runEffectSimulation $ removeNamedSelection "graphlinksSelection"
    stageForces Force.treeForces
    stageNodes isUsedModule
    stageLinks isM2M_Tree_Link
    pinTreeNodes -- side-effect, because if we make _new_ nodes the links won't be pointing to them
    -- realizeSimulation Graph.updateSimulation treeSceneAttributes
    
  ToggleForce label -> do
    simulationStop
    runEffectSimulation $ toggleForceByLabel label
    simulationStart

  Filter (LinkFilter x) -> do
    stageLinks x
    -- realizeSimulation Graph.updateSimulation graphSceneAttributes -- TODO this could change from Tree to Graph, surely not what's wanted?

  Filter (NodeFilter x) -> do
    stageNodes x
    -- realizeSimulation Graph.updateSimulation graphSceneAttributes -- TODO this could change from Tree to Graph, surely not what's wanted?

  ChangeStyling style -> _cssClass %= (const style) -- modify_ (\s -> s { svgClass = style })

  ChangeSimConfig c -> do
    runEffectSimulation $ setConfigVariable c
    simulationStart

  StartSim -> do
    simulationStart

  StopSim -> do
    simulationStop

setCssEnvironment :: forall m. MonadState State m => String -> m Unit
setCssEnvironment string = _cssClass %= (const string)

stageForces :: forall m. MonadState State m => Array String -> m Unit
stageForces forces = modify_ $ set _activeForces forces

-- TODO modifying _linksInSim (filtered _linksInModel)
stageLinks :: forall m. MonadState State m => (SpagoGraphLinkID -> Boolean) -> m Unit
stageLinks filterFn = do
  state <- get
  case state.model of
    Nothing -> pure unit
    Just m  -> do
      let filteredLinks = filter filterFn m.links
      _stagingLinks %= (const filteredLinks)

-- TODO modifying _nodesInSim (filtered _nodesInModel)
stageNodes :: forall m. MonadState State m => (SpagoSimNode -> Boolean) -> m Unit
stageNodes filterFn = do
  state <- get
  case state.model of
    Nothing -> pure unit
    Just m  -> do
      let filteredNodes = filter filterFn m.nodes
      _stagingNodes %= (const filteredNodes)

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

type SceneAttributes = { circle :: Array SelectionAttribute , labels :: Array SelectionAttribute }
type StagingType = Staging D3Selection_ SpagoDataRow SpagoLinkData NodeID


-- realizeSimulation :: forall t106 t108 t111 t113 t132 t139 t154.
--   Bind t108 => MonadState
--                  { simulation :: D3SimulationState_
--                  | t111
--                  }
--                  t108
--                 => MonadEffect t108 => (t113
--                                         -> t106
--                                            -> D3SimM
--                                                 ( forces :: { active :: Array String
--                                                             | t132
--                                                             }
--                                                 , staging :: t113
--                                                 | t154
--                                                 )
--                                                 D3Selection_
--                                                 t139
--                                        )
--                                        -> t106 -> t108 Unit
-- realizeSimulation :: forall m a.
--   Bind m => 
--   MonadState State m =>
--   MonadEffect m =>
--   SelectionM D3Selection_ m =>
--   SimulationM D3Selection_ m =>
--   (StagingType -> SceneAttributes -> m StateRow D3Selection_ a ) -> SceneAttributes -> m Unit
realizeSimulation selection script sceneAttributes = do
    simulationStop
    staging <- use _staging
    forces  <- use _activeForces
    state   <- get -- the scripts run in a StateT that only requires { simulation :: D3Simulation_ }
    state'  <- liftEffect $ liftA1 snd $ runStateT (script staging sceneAttributes) state
    -- runEffectSimulation (enableOnlyTheseForces forces)
    simulationStart
