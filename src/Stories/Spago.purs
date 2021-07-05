module Stories.Spago where

import D3.Interpreter.D3
import D3.Simulation.Forces
import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, execState, runState)
import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_, D3Simulation_, Selector)
import D3.Examples.Spago (treeReduction)
import D3.Examples.Spago.Clusters as Cluster
import D3.Examples.Spago.Graph as Graph
import D3.Examples.Spago.Model (SpagoModel, convertFilesToGraphModel, datum_, numberToGridPoint, offsetXY, scalePoint)
import D3.FFI (initSimulation_)
import D3.Interpreter (class SelectionM, class SimulationM)
import D3.Simulation.Config (SimulationConfig_, defaultConfigSimulation)
import D3.Simulation.Config as F
import D3Tagless.Block.Card as Card
import Data.Array ((:))
import Data.Const (Const)
import Data.Either (Either(..), hush)
import Data.Map (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Number (infinity)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, Fiber, Milliseconds(..), attempt, delay, forkAff, joinFiber, killFiber, throwError)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import SimulationBusListener (startSimulationFiber)
import Stories.Tailwind.Styles as Tailwind
import UIGuide.Block.Backdrop as Backdrop
import Utility (getWindowWidthHeight)

type Query :: forall k. k -> Type
type Query = Const Void

data PackageForce = PackageRing | PackageGrid | PackageFree
data ModuleForce = ClusterPackage | ForceTree
data Action
  = Initialize
  | Finalize
  | SetPackageForce PackageForce
  | SetModuleForce ModuleForce
  | ChangeSimConfig SimVariable
  | Stop
  | Start
  
type State = {
    fiber :: Maybe (Fiber Unit)
  , bus   :: Maybe (Bus.BusRW SimCommand)
}

component :: forall m. MonadAff m => H.Component Query Unit Void m
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
  initialState = { fiber: Nothing, bus: Nothing }

  renderSimControls _ =
    HH.div
      [ HP.classes [ HH.ClassName "m-6" ]]
      [ HH.h3_
          [ HH.text "Simulation controls" ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const Stop ]
              [ HH.text "Stop" ]
          ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const Start ]
              [ HH.text "Start" ]
          ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const (SetPackageForce PackageGrid) ]
              [ HH.text "PackageGrid" ]
          ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const (SetPackageForce PackageRing) ]
              [ HH.text "PackageRing" ]
          ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
        [ Tailwind.apply "story-container spago" ]
        [ HH.div
            [ Tailwind.apply "story-panel-about" ]
            -- [ renderSimControls state.simulation
            -- , renderTableForces state.simulation
            -- , renderTableElements state.simulation
            -- , Card.card_ [ blurbtext ]
            [ Card.card_ [ blurbtext ]
            ]
        , HH.div
            [ Tailwind.apply "svg-container" ]
            [ ]
        ]

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  Initialize -> do
    (detached :: D3Selection_)  <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"
    (model :: Maybe SpagoModel) <- H.liftAff getModel
    simulationBus               <- Bus.make
    -- simulation                  <- H.gets _.simulation
    -- fiber                       <- H.liftAff $ forkAff $ drawGraph simulation graph
    -- fiber                       <- H.liftAff $ forkAff $ startSimulationFiber simulationBus

    -- case model of
    --       Nothing -> pure unit
    --       (Just graph) -> do
    --         let simulation' = execState (simulationLoadForces initialForces) simulation 
    --         -- TODO properly think out / design relationship between fiber and simulation
    --         (Tuple svg simulation'' :: Tuple D3Selection_ SimulationState_) 
    --               <- H.liftEffect $ run_D3M_Simulation simulation' (Graph.script graph)
    --         H.modify_ (\s -> s { fiber = Nothing, simulation = simulation'' })
    --         pure unit
    H.modify_ (\s -> s { fiber = Nothing })
    pure unit


  Finalize -> do
      fiber <- H.gets _.fiber
      _ <- case fiber of
              Nothing      -> pure unit
              (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
      H.modify_ (\state -> state { fiber = Nothing })
  
  SetPackageForce packageForce -> do
    -- simulation <- H.gets _.simulation
    -- let updatedSimulation = 
    --       case packageForce of
    --         PackageRing -> execState (simulationEnableForcesByLabel ["packageGrid"]) simulation
    --         PackageGrid -> execState (simulationAddForce (enableForce packageOnlyFixToGridForce)) simulation
    --         PackageFree -> execState (simulationDisableForcesByLabel ["packageOrbit", "packageGrid"]) simulation
    -- H.modify_ (\state -> state { simulation = updatedSimulation })
    pure unit

  SetModuleForce _ -> do
    pure unit

  ChangeSimConfig c -> do
    -- simulation <- H.gets _.simulation
    -- let updatedSimulation = execState (simulationSetVariable c) simulation
    -- H.modify_ (\state -> state { simulation = updatedSimulation })
    pure unit

  Start -> do
    -- simulation <- H.gets _.simulation
    -- let updatedSimulation = execState simulationStart simulation
    -- H.modify_ (\state -> state { simulation = updatedSimulation })
    pure unit

  Stop -> do
    -- simulation <- H.gets _.simulation
    -- let updatedSimulation = execState simulationStop simulation
    -- H.modify_ (\state -> state { simulation = updatedSimulation })
    pure unit

-- drawGraph :: SimulationState_ -> SpagoModel -> Aff Unit
-- drawGraph simulation graph = do
--   (svg :: Tuple D3Selection_ SimulationState_) <- liftEffect $ runD3M_Simulation simulation (Cluster.script graph)
--   pure unit

-- getModel will try to build a model from files and to derive a dependency tree from Main
-- the dependency tree will contain all nodes reachable from Main but NOT all links
getModel :: Aff (Maybe SpagoModel)
getModel = do
  moduleJSON  <- AJAX.get ResponseFormat.string "http://localhost:1234/modules.json"
  packageJSON <- AJAX.get ResponseFormat.string "http://localhost:1234/packages.json"
  lsdepJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/lsdeps.jsonlines"
  locJSON     <- AJAX.get ResponseFormat.string "http://localhost:1234/loc.json"
  let model = hush $ convertFilesToGraphModel <$> moduleJSON <*> packageJSON <*> lsdepJSON <*> locJSON

  pure (addTreeToModel "Main" model) 

addTreeToModel :: String -> Maybe SpagoModel -> Maybe SpagoModel
addTreeToModel rootName maybeModel = do
  model  <- maybeModel
  rootID <- M.lookup rootName model.maps.name2ID
  pure $ treeReduction model rootID


-- | ============================================
-- | FORCES
-- | ============================================

initialForces :: Array Force
initialForces = [
    enableForce collideForce
  , enableForce manyBodyForce
  , enableForce centeringForceX
  , enableForce centeringForceY
  , enableForce centeringForceCenter
  , clusterForceX
  , clusterForceY
  , packageOnlyRadialForce
  , packageOnlyFixToGridForce
  , unusedModuleOnlyRadialForce
]

clusterForceX :: Force
clusterForceX = createForce "x" ForceX [ F.strength 0.2, F.x datum_.clusterPointX ]

clusterForceY :: Force
clusterForceY = createForce "y" ForceY [ F.strength 0.2, F.y datum_.clusterPointY ]

collideForce :: Force
collideForce = createForce "collide" ForceCollide  [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ]

manyBodyForce :: Force
manyBodyForce = createForce "charge" ForceManyBody [ F.strength (-60.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]

treeForceX :: Force
treeForceX = createForce "x" ForceX [ F.strength 0.2, F.x datum_.treePointX ]

treeForceY :: Force
treeForceY = createForce "y" ForceY [ F.strength 0.2, F.y datum_.treePointY ]
      
centeringForceX :: Force
centeringForceX = createForce "x" ForceX [ F.strength 0.1, F.x 0.0 ]

centeringForceY :: Force
centeringForceY = createForce "y" ForceY [ F.strength 0.1, F.y 0.0 ]

centeringForceCenter :: Force
centeringForceCenter = createForce "center" ForceCenter   [ F.strength 0.5, F.x 0.0, F.y 0.0 ]

packageOnlyRadialForce :: Force
packageOnlyRadialForce = createForce "packageOrbit"  ForceRadial   [ strengthFunction, F.x 0.0, F.y 0.0, F.radius 1000.0 ]
  where
    strengthFunction =
      F.strength (\d -> if datum_.isPackage d then 0.8 else 0.0)

packageOnlyFixToGridForce :: Force
packageOnlyFixToGridForce = do
  let gridXY d = offsetXY { x: (-1000.0), y: (-500.0) } $
                 scalePoint 100.0 20.0 $
                 numberToGridPoint 10 (datum_.id d)
  createForce "packageGrid" (ForceFixPositionXY gridXY) [ ]

unusedModuleOnlyRadialForce :: Force
unusedModuleOnlyRadialForce = createForce "unusedModuleOrbit" ForceRadial   [ strengthFunction, F.x 0.0, F.y 0.0, F.radius 600.0 ]
  where
    strengthFunction =
      F.strength (\d -> if datum_.isUnusedModule d then 0.8 else 0.0)
      
-- | ============================================
-- | HTML
-- | ============================================

blurbtext :: forall p i. HH.HTML p i
blurbtext = HH.div_ (title : paras)
  where
    title        = HH.h2 [ HP.classes titleClasses ] [ HH.text "About this Example"]
    titleClasses = HH.ClassName <$> [ "font-bold text-2xl" ]

    paras       = (HH.p [ HP.classes paraClasses ]) <$> paraTexts
    paraClasses = HH.ClassName <$> [ "m-4 " ]
    paraTexts   = map (\s -> [ HH.text s ] ) [

        """This example synthesizes a complex dependency graph from the optional JSON
        graph outputs of the PureScript compiler, together with the package
        dependencies from Spago and adds simple line-count per module to give an
        idea of the size of each one."""

      , """With this dataset, operated on by the physics simulation engine, we can
      explore different aspects of the project dependencies. The layout can be
      entirely driven by forces and relationships or partially or totally laid-out
      using algorithms."""

      , """For example, a dependency tree starting at the Main module can be laid-out as
      a radial tree and either fixed in that position or allowed to move under the
      influences of other forces.""" 

      , """Un-connected modules (which are only present because something in their
      package has been required) can be hidden or clustered separately."""

      , """Modules can be clustered on their packages and the packages can be positioned
      on a simple grid or arranged in a circle by a radial force that applies only
      to them."""

      , """Clicking on a module highlights it and its immediate dependents and
      dependencies. Clicking outside the highlighted module undoes the
      highlighting."""
    ]

renderTableForces :: forall m. SimulationState_ -> H.ComponentHTML Action () m
renderTableForces (SS_ simulation)  =
  HH.div_
  [ HH.div_
    [ Backdrop.backdrop_
      [ HH.div_
        [ HH.h2_ [ HH.text "Control which forces are acting"]
        , renderTable
        ]
      ]
    ]
  ]
  where
  renderTable =
    Table.table_ $
      [ renderHeader
      ]
      <> renderBody

  renderHeader =
    Table.row_
      [ Table.header  [ css "w-10" ] [ HH.text "Active" ]
      , Table.header  [ css "w-2/3 text-left" ] [ HH.text "Details" ]
      , Table.header  [ css "w-2/3 text-left" ] [ HH.text "Acting on..." ]
      ]
  
  tableData = snd <$> 
              (toUnfoldable $ simulation.forces)

  renderBody =
    Table.row_ <$> ( renderData <$> tableData )

  renderData :: ∀ p i. Force -> Array (HH.HTML p i)
  renderData (Force l s t cs h_) =
    [ Table.cell_ [ Checkbox.checkbox_ [] [] ]
    , Table.cell  [ css "text-left" ]
      [ HH.div_ [
          HH.text l
        , HH.text $ show t -- use forceDescription t for more detailed explanation
        ]
      ]
    , Table.cell  [ css "text-left" ] [ HH.text "modules" ]
    ]

renderTableElements :: forall m. SimulationState_ -> H.ComponentHTML Action () m
renderTableElements (SS_ simulation)  =
  HH.div_
  [ HH.div_
    [ Backdrop.backdrop_
      [ HH.div_
        [ HH.h2_ [ HH.text "Control which data groupings are shown"]
        , renderTable
        ]
      ]
    ]
  ]
  where
  renderTable =
    Table.table_ $
      [ renderHeader
      ]
      <> renderBody

  renderHeader =
    Table.row_
      [ Table.header  [ css "w-10" ] [ HH.text "Active" ]
      , Table.header  [ css "w-2/3 text-left" ] [ HH.text "Details" ]
      , Table.header  [ css "w-2/3 text-left" ] [ HH.text "Acting on..." ]
      ]
  
  tableData =
    snd <$> 
    (toUnfoldable $
    simulation.forces)

  renderBody =
    Table.row_ <$> ( renderData <$> tableData )

  renderData :: ∀ p i. Force -> Array (HH.HTML p i)
  renderData (Force l s t cs h_) =
    [ Table.cell_ [ Checkbox.checkbox_ [] [] ]
    , Table.cell  [ css "text-left" ]
      [ HH.div_ [
          HH.text l
        , HH.text $ show t -- use forceDescription t for more detailed explanation
        ]
      ]
    , Table.cell  [ css "text-left" ] [ HH.text "modules" ]
    ]
