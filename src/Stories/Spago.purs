module Stories.Spago where

import D3.Examples.Spago.Model
import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState)
import D3.Data.Types (D3Selection_, D3Simulation_)
import D3.Examples.Spago (treeReduction)
import D3.Examples.Spago as Spago
import D3.Examples.Spago.Clusters as Cluster
import D3.Examples.Spago.Graph as Graph
import D3.FFI (initSimulation_, setAlpha_, stopSimulation_)
import D3.Interpreter.D3 (d3Run, removeExistingSVG, runD3M)
import D3.Layouts.Simulation (Force(..), ForceStatus(..), ForceType(..), SimulationManager, addForce, addForces, createForce, createSimulationManager, disableByLabelMany, disableByLabels, disableForce, forceDescription, showForces, showSimulationRunning)
import D3.Simulation.Config as F
import D3Tagless.Block.Card as Card
import D3Tagless.Block.FormField as FormField
import Data.Array (intercalate, (:))
import Data.Const (Const)
import Data.Either (hush)
import Data.Map (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number (infinity)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff, Fiber, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Radio as Radio
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import Stories.Tailwind.Styles as Tailwind
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
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
  
type State = { 
    fiber  :: Maybe (Fiber Unit)
  , simulation :: SimulationManager
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
  initialState = { fiber: Nothing, simulation: createSimulationManager }

  renderSimControls _ =
    HH.div
      [ css "flex-1" ]
      [ FormField.fieldset_
        { label: HH.text "Packages"
        , inputId: "radio-vertical"
        , helpText: []
        , error: []
        }
        [ HH.div
          [ css "flex-1" ]
          [ Radio.radio
            [ css "pr-6" ]
            [ HP.name "package-force"
            , HP.checked true
            , HE.onClick $ const (SetPackageForce PackageRing)
            ]
            [ HH.text "Vertical" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "package-force"
            , HE.onClick $ const (SetPackageForce PackageGrid) ]
            [ HH.text "Horizontal" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "package-force"
            , HE.onClick $ const (SetPackageForce PackageFree) ]
            [ HH.text "Radial" ]
          ]
        ]
      , FormField.fieldset_
        { label: HH.text "Module"
        , inputId: "radio-vertical"
        , helpText: []
        , error: []
        }
        [ HH.div
          [ css "flex-1" ]
          [ Radio.radio
            [ css "pr-6" ]
            [ HP.name "module-force"
            , HP.checked true
            , HE.onClick $ const (SetModuleForce ClusterPackage)
            ]
            [ HH.text "Package layout" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "module-force"
            , HE.onClick $ const (SetModuleForce ForceTree) ]
            [ HH.text "Tree layout" ]
          ]
        ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
        [ Tailwind.apply "story-container spago" ]
        [ HH.div
            [ Tailwind.apply "story-panel-about" ]
            [ renderSimControls state.simulation
            , renderTableForces state.simulation
            , renderTableElements state.simulation
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
    (detached :: D3Selection_) <- H.liftEffect $ d3Run $ removeExistingSVG "div.svg-container"

    (model :: Maybe SpagoModel) <- H.liftAff getModel

    -- pure unit
    case model of
          Nothing -> pure unit
          (Just graph) -> do
            -- TODO properly think out / design relationship between fiber and simulation
            simulation  <- H.gets _.simulation
            let updatedSimulation = addForces (clusterForce <> collideForce) simulation 
            fiber <- H.liftAff $ forkAff $ drawGraph simulation graph
            H.modify_ (\s -> s { fiber = Just fiber, simulation = updatedSimulation })
            pure unit

  Finalize -> do
      fiber <- H.gets _.fiber
      _ <- case fiber of
              Nothing      -> pure unit
              (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
      H.modify_ (\state -> state { fiber = Nothing })
  
  SetPackageForce packageForce -> do
    simulation <- H.gets _.simulation
    let _ = case packageForce of
              PackageRing -> addForce simulation packageOnlyRadialForce 
              PackageGrid -> addForce simulation unusedModuleOnlyRadialForce
              PackageFree -> disableByLabelMany ["packageOrbit"] simulation
    H.modify_ (\state -> state { simulation = simulation })
    pure unit

  SetModuleForce _ -> do
    pure unit


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

drawGraph :: SimulationManager -> SpagoModel -> Aff Unit
drawGraph simulation graph = do
  widthHeight <- liftEffect getWindowWidthHeight
  (svg :: D3Selection_) <- liftEffect $ liftA1 fst $ runD3M (Cluster.script widthHeight simulation graph)
  pure unit


clusterForce :: Array Force
clusterForce =  
  [ createForce "x" ForceX [ F.strength 0.2, F.x datum_.clusterPointX ]
  , createForce "y" ForceY [ F.strength 0.2, F.y datum_.clusterPointY ]
  ]

collideForce :: Array Force
collideForce = [ createForce "collide" ForceCollide  [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ] ]

manyBodyForce :: Array Force
manyBodyForce = [ createForce "charge" ForceManyBody [ F.strength (-60.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ] ]

treeForces :: Array Force
treeForces =  
  [ createForce "x" ForceX [ F.strength 0.2, F.x datum_.treePointX ]
  , createForce "y" ForceY [ F.strength 0.2, F.y datum_.treePointY ]
  ]
      
centeringForces :: Array Force
centeringForces =  
  [ createForce "x"      ForceX        [ F.strength 0.1, F.x 0.0 ]
  , createForce "y"      ForceY        [ F.strength 0.1, F.y 0.0 ]
  , createForce "center" ForceCenter   [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
  ]

packageOnlyRadialForce :: Force
packageOnlyRadialForce = createForce "packageOrbit"  ForceRadial   [ F.strength datum_.onlyPackages, F.x 0.0, F.y 0.0, F.radius 1000.0 ]

unusedModuleOnlyRadialForce :: Force
unusedModuleOnlyRadialForce = createForce "unusedModuleOrbit" ForceRadial   [ F.strength datum_.onlyUnused, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      

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

renderTableForces :: forall m. SimulationManager -> H.ComponentHTML Action () m
renderTableForces simulation  =
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
  
  tableData =
    snd <$> 
    (toUnfoldable $
    (unwrap simulation).forces)

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

renderTableElements :: forall m. SimulationManager -> H.ComponentHTML Action () m
renderTableElements simulation  =
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
    (unwrap simulation).forces)

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
