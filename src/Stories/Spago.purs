module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, modify_, put)
import D3.Data.Types (D3Selection_)
import D3.Examples.Spago (treeReduction)
import D3.Examples.Spago.Graph as Graph
import D3.Examples.Spago.Model (SpagoModel, convertFilesToGraphModel, datum_, numberToGridPoint, offsetXY, scalePoint)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, enableForce)
import D3.Simulation.Functions (simulationStart)
import D3.Simulation.Types (Force(..), ForceType(..), SimVariable(..), SimulationState_(..))
import D3Tagless.Block.Card as Card
import D3Tagless.Capabilities (addForce, loadForces, removeAllForces, setConfigVariable, setForcesByLabel)
import D3Tagless.Instance.Simulation (D3SimM, exec_D3M_Simulation, runEffectSimulation)
import Data.Array ((:))
import Data.Either (hush)
import Data.Map (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Tuple (snd)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import Stories.Tailwind.Styles as Tailwind
import UIGuide.Block.Backdrop as Backdrop
import Unsafe.Coerce (unsafeCoerce)

data PackageForce = PackageRing | PackageGrid | PackageFree
data ModuleForce = ClusterPackage | ForceTree
data Action
  = Initialize
  | Finalize
  | SetPackageForce PackageForce
  | SetModuleForce ModuleForce
  | ChangeSimConfig SimVariable
  | StopSim
  | StartSim

type Input = SimulationState_
  
type State = {
    simulationState :: SimulationState_
}

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
  initialState simulation = { simulationState: simulation }

  renderSimControls =
    HH.div
      [ HP.classes [ HH.ClassName "m-6" ]]
      [ HH.h3_
          [ HH.text "Simulation controls" ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const StopSim ]
              [ HH.text "Stop" ]
          ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const StartSim ]
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
      , HH.div_
          [ Button.button
              [ HE.onClick $ const (SetPackageForce PackageFree) ]
              [ HH.text "PackageFree" ]
          ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
        [ Tailwind.apply "story-container spago" ]
        [ HH.div
            [ Tailwind.apply "story-panel-about" ]
            -- [ 
            -- , renderTableForces state.simulation
            -- , renderTableElements state.simulation
            [ renderSimControls
            , Card.card_ [ blurbtext ]
            ]
        , HH.div
            [ Tailwind.apply "svg-container" ]
            [ ]
        ]

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  Initialize -> do    
    state <- H.get
    -- (detached :: D3Selection_)  <- eval_D3MB_Simulation simulationBus $ removeExistingSVG "div.svg-container"
    (model :: Maybe SpagoModel) <- H.liftAff getModel
    case model of
      Nothing      -> pure $ unsafeCoerce unit -- TODO just temporary, pull out the script runner to fn that returns unit
      (Just graph) -> runEffectSimulation (Graph.script graph)
    runEffectSimulation (loadForces initialForces)

  Finalize -> pure unit
  
  SetPackageForce PackageGrid -> do
    runEffectSimulation $ setForcesByLabel  { enable: [ "packageGrid" ], disable: ["packageOrbit"] }
    runEffectSimulation $ setConfigVariable (Alpha 0.8)
  SetPackageForce PackageRing -> do
    runEffectSimulation $ setForcesByLabel  { enable: [ "packageOrbit" ], disable: ["packageGrid"] }
    runEffectSimulation $ setConfigVariable (Alpha 0.8)
  SetPackageForce PackageFree -> do
    runEffectSimulation $ setForcesByLabel  { enable: [], disable: ["packageOrbit", "packageGrid"] }
    runEffectSimulation $ setConfigVariable (Alpha 0.8)

  SetModuleForce moduleForce -> pure unit

  ChangeSimConfig c -> pure unit -- SetConfigVariable c

  StartSim -> simulationStart

  StopSim -> runEffectSimulation (setConfigVariable $ Alpha 0.0)


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
    enableForce $ createForce "collide" ForceCollide  [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ]
  , enableForce $ createForce "charge"  ForceManyBody [ F.strength (-60.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]
  , enableForce $ createForce "center"  ForceCenter   [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
  , enableForce $ createForce "x"       ForceX [ F.strength 0.1, F.x 0.0 ]
  , enableForce $ createForce "y"       ForceY [ F.strength 0.1, F.y 0.0 ]
  ,               createForce "clusterx"       ForceX [ F.strength 0.2, F.x datum_.clusterPointX ]
  ,               createForce "clustery"       ForceY [ F.strength 0.2, F.y datum_.clusterPointY ]
  ,               createForce "packageOrbit"   ForceRadial [ strengthFunction1, F.x 0.0, F.y 0.0, F.radius 1000.0 ]
  ,               createForce "packageGrid"    (ForceFixPositionXY gridXY) [ ]
  ,               createForce "unusedModuleOrbit" ForceRadial [ strengthFunction2, F.x 0.0, F.y 0.0, F.radius 600.0 ]
]
  where
    strengthFunction1 = F.strength (\d -> if datum_.isPackage d      then 0.8 else 0.0)
    strengthFunction2 = F.strength (\d -> if datum_.isUnusedModule d then 0.8 else 0.0)

    gridXY d = offsetXY { x: (-1000.0), y: (-500.0) } $
               scalePoint 100.0 20.0 $
               numberToGridPoint 10 (datum_.id d)

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
