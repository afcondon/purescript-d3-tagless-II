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
import D3.Layouts.Simulation (Force(..), ForceStatus(..), ForceType(..), SimulationManager, addForce, addForces, createForce, createSimulationManager, disableByLabelMany, disableByLabels, disableForce, showForces)
import D3.Simulation.Config as F
import D3Tagless.Block.Card as Card
import D3Tagless.Block.FormField as FormField
import Data.Array (intercalate)
import Data.Const (Const)
import Data.Either (hush)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number (infinity)
import Data.Tuple (fst)
import Effect.Aff (Aff, Fiber, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Radio as Radio
import Ocelot.Block.Table as Table
import Ocelot.Block.Button as Button
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Icon as Icon
import UIGuide.Block.Documentation as Documentation
import UIGuide.Block.Backdrop as Backdrop
import Ocelot.HTML.Properties (css)
import Stories.Tailwind.Styles as Tailwind
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

  controls =
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
            [ HH.text "Spago"
            , Card.card_ [ controls ]
            , Card.card_ [ HH.text $ showForces state.simulation ]
            , Card.card_ [ HH.text $ show $ M.size (unwrap state.simulation).forces ]
            , Card.card_ [ HH.text $ show $ (unwrap state.simulation).running ]
            , renderTable state
            , Card.card_ [ HH.text blurbtext ]
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
      

blurbtext :: String
blurbtext = 
  """Id sint laboris reprehenderit officia anim nisi consectetur voluptate enim.
  Commodo cillum minim nisi laborum eiusmod veniam ullamco id ex fugiat eu anim.
  Irure est aute laborum duis. Lorem dolore id sunt incididunt ut ea. Nostrud
  enim officia nisi anim consequat cupidatat consectetur consequat ex excepteur.
  Lorem nisi in reprehenderit ex adipisicing magna elit aute sunt. Cillum non
  Lorem minim duis culpa ullamco aute ex minim. Mollit anim in nisi tempor enim
  exercitation dolore. Veniam consequat minim nostrud amet duis dolore tempor
  voluptate quis culpa. Laborum dolor pariatur ut est cupidatat elit deserunt
  occaecat tempor aliquip anim. 
  
  Velit irure ea voluptate ipsum ex exercitation
  dolore voluptate reprehenderit sit anim sunt. Anim fugiat ad ut qui cillum
  tempor occaecat et deserunt nostrud non ipsum. Id non qui mollit culpa elit
  cillum ipsum excepteur adipisicing qui. Incididunt adipisicing sit incididunt
  consequat minim id do exercitation cupidatat est sunt mollit. Anim ut ullamco
  enim culpa. Adipisicing ad non esse laboris anim consequat ut velit esse
  consequat tempor. Commodo magna esse ullamco ipsum et ipsum minim dolore esse
  veniam ea commodo labore. Nulla deserunt id ad anim anim proident labore
  occaecat sint esse nostrud. Duis velit nostrud ullamco cillum cillum Lorem
  cupidatat irure."""

renderTable :: forall m. State -> H.ComponentHTML Action () m
renderTable state  =
  HH.div_
  [ Documentation.block_
    { header: "Table"
    , subheader: "Tabular Data"
    }
    [ Backdrop.backdrop_
      [ HH.div_
        [ renderTable
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
      [ Table.header  [ css "w-10" ] [ HH.text "" ]
      , Table.header_ [ HH.text "Icon" ]
      , Table.header  [ css "w-2/3 text-left" ] [ HH.text "Description" ]
      , Table.header_ [ HH.text "" ]
      ]

  renderBody =
    Table.row_ <$> ( renderData <$> tableData )

  renderData :: ∀ p i. TestData p i -> Array (HH.HTML p i)
  renderData { name, icon } =
    [ Table.cell_ [ Checkbox.checkbox_ [] [] ]
    , Table.cell  [ css "text-2xl" ] [ icon ]
    , Table.cell  [ css "text-left" ] [ HH.text name ]
    , Table.cell  [ css "text-right" ] [ Button.button_ [ HH.text "Do Nothing" ] ]
    ]

type TestData p i = { name :: String, icon :: HH.HTML p i }

tableData :: ∀ p i. Array (TestData p i)
tableData =
  [ { name: "This is what a back arrow looks like"
    , icon: Icon.back_
    }
  , { name: "This is what a refresh arrow looks like"
    , icon: Icon.refresh_
    }
  , { name: "This is what a settings cog looks like"
    , icon: Icon.settings_
    }
  , { name: "This is what a share button looks like"
    , icon: Icon.share_
    }
  , { name: "This is what an error badge looks like"
    , icon: Icon.error [ css "text-red" ]
    }
  , { name: "This is what a tip bulb looks like"
    , icon: Icon.tip [ css "text-yellow" ]
    }
  , { name: "This is what an info badge looks like"
    , icon: Icon.info [ css "text-blue" ]
    }
  , { name: "This is what a success badge looks like"
    , icon: Icon.success [ css "text-green" ]
    }
  , { name: "This is what the Facebook icon looks like"
    , icon: Icon.facebook [ css "text-fb-blue" ]
    }
  , { name: "This is what the Instagram icon looks like"
    , icon: Icon.instagram [ css "text-ig-brown" ]
    }
  , { name: "This is what the Twitter icon looks like"
    , icon: Icon.twitter [ css "text-tw-blue" ]
    }
  , { name: "This is what a progress bar with a top caption looks like"
    , icon: HH.div_
        [ HH.p [ css "text-sm pb-2" ] [ HH.text "60% of campaign spent" ]
        ]
    }
  ]
