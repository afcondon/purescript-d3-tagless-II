module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, gets, modify_)
import D3.Attributes.Instances (Label)
import D3.Data.Tree (TreeLayout(..))
import D3.Examples.Spago (treeReduction)
import D3.Examples.Spago.Graph as Graph
import D3.Examples.Spago.Model (SpagoModel, convertFilesToGraphModel, datum_, numberToGridPoint, offsetXY, scalePoint)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, enableForce)
import D3.Simulation.Functions (simulationStart)
import D3.Simulation.Types (Force(..), ForceStatus(..), ForceType(..), SimVariable(..), SimulationState_(..))
import D3Tagless.Block.Card as Card
import D3Tagless.Capabilities (addForces, setConfigVariable, setForcesByLabel, toggleForceByLabel)
import D3Tagless.Instance.Simulation (runEffectSimulation)
import Data.Array ((:))
import Data.Either (hush)
import Data.Map (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Tuple (snd)
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import Stories.Utilities as Utils
import UIGuide.Block.Backdrop as Backdrop
import Unsafe.Coerce (unsafeCoerce)

data Scene = PackageGrid | PackageGraph | ModuleTree TreeLayout
data Action
  = Initialize
  | Finalize
  | Scene Scene
  | ToggleForce Label
  | ChangeSimConfig SimVariable
  | StopSim
  | StartSim

type Input = SimulationState_
  
type State = {
    simulationState :: SimulationState_
  , svgClass :: String -- by controlling the class that is on the svg we can completely change the look of the vis (and not have to think about this at D3 level)
  , showLinks :: Boolean
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
  initialState simulation = { simulationState: simulation, svgClass: "cluster", showLinks: true }

  renderSimControls state =
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
              [ HE.onClick $ const (Scene PackageGrid) ]
              [ HH.text "Package Grid" ]
          ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const (Scene PackageGraph) ]
              [ HH.text "Package Graph" ]
          ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const (Scene $ ModuleTree Vertical) ]
              [ HH.text "Module Tree" ]
          ]
      , HH.div_
          [ Button.button
              [ HE.onClick $ const (Scene $ ModuleTree Radial) ]
              [ HH.text "Module Tree" ]
          ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
        [ Utils.tailwindClass "story-container spago" ]
        [ HH.div
            [ Utils.tailwindClass "story-panel-about" ]
            [ renderSimControls state
            , renderTableForces state.simulationState
            -- , renderTableElements state.simulation
            , Card.card_ [ blurbtext ]
            ]
        , HH.div
            [ Utils.tailwindClass $ "svg-container " <> state.svgClass ]
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
    runEffectSimulation (addForces initialForces)

  Finalize -> pure unit

  Scene PackageGrid -> do
    runEffectSimulation $ setForcesByLabel  { enable: [ "packageGrid", "clusterx", "clustery" ], disable: ["links", "packageOrbit"] }
    simulationStart
  Scene PackageGraph -> do
    runEffectSimulation $ setForcesByLabel  { enable: [ "packageOrbit", "links" ], disable: ["packageGrid", "clusterx", "clustery" ] }
    simulationStart
  Scene (ModuleTree _) -> do
    runEffectSimulation $ setForcesByLabel  { enable: [], disable: ["packageOrbit", "packageGrid", "clusterx", "clustery" ] }
    simulationStart

  ToggleForce label -> runEffectSimulation $ toggleForceByLabel label

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
initialForces = enabledForces <> disabledForces
  where
    enabledForces = 
      enableForce <$> [
        createForce "collide"           ForceCollide  [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ]
      , createForce "charge"            ForceManyBody [ F.strength (-60.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]
      , createForce "center"            ForceCenter   [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
      , createForce "x"                 ForceX        [ F.strength 0.1, F.x 0.0 ]
      , createForce "y"                 ForceY        [ F.strength 0.1, F.y 0.0 ]
      , createForce "packageOrbit"      ForceRadial   [ strengthFunction1, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      , createForce "unusedModuleOrbit" ForceRadial   [ strengthFunction2, F.x 0.0, F.y 0.0, F.radius 700.0 ]
      ]

    disabledForces = [
        createForce "clusterx"       ForceX [ F.strength 0.2, F.x datum_.clusterPointX ]
      , createForce "clustery"       ForceY [ F.strength 0.2, F.y datum_.clusterPointY ]
      , createForce "packageGrid"    (ForceFixPositionXY gridXY gridFilter) [ ]
      ]

    strengthFunction1 = F.strength (\d -> if datum_.isPackage d      then 0.8 else 0.0)
    strengthFunction2 = F.strength (\d -> if datum_.isUnusedModule d then 0.8 else 0.0)

    gridXY d = offsetXY { x: (-800.0), y: (-5000.0) } $
               scalePoint 180.0 100.0 $
               numberToGridPoint 10 (datum_.id d)
    gridFilter d = datum_.isPackage d
      

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
  [ HH.div
    [ Utils.tailwindClass "text-sm" ]
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

  -- renderData :: ∀ p i. Force -> Array (HH.HTML p i)
  renderData (Force label s t cs h_) =
    [ Table.cell_ [ Checkbox.checkbox_ 
                  [ HP.checked (s == ForceActive)
                  , HE.onChecked $ const (ToggleForce label)
                  ] [] ]
    , Table.cell  [ css "text-left" ]
      [ HH.div_ [
          HH.text label
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
