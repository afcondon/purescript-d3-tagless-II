module Stories.Spago where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get, gets, modify_)
import D3.Attributes.Instances (Label)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Selection_, index_ToInt)
import D3.Examples.Spago.Files (NodeType(..), SpagoGraphLinkID, SpagoNodeData, isM2M_Graph_Link, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Graph (graphAttrs, treeAttrs)
import D3.Examples.Spago.Graph as Graph
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, cluster2Point, convertFilesToGraphModel, datum_, isModule, isPackage, isUsedModule, numberToGridPoint, offsetXY, pinNodesInModel, pinTreeNode, scalePoint)
import D3.Examples.Spago.Tree (treeReduction)
import D3.FFI (pinNode_, pinTreeNode_, unpinNode_)
import D3.Node (D3_SimulationNode(..))
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, enableForce)
import D3.Simulation.Functions (simulationSetLinks, simulationStart, simulationStop)
import D3.Simulation.Types (Force(..), ForceFilter(..), ForceStatus(..), ForceType(..), SimVariable(..), SimulationState_(..), allNodes, showForceFilter)
import D3Tagless.Block.Card as Card
import D3Tagless.Capabilities (addForces, enableOnlyTheseForces, setConfigVariable, setForcesByLabel, setLinks, toggleForceByLabel)
import D3Tagless.Instance.Simulation (runEffectSimulation)
import Data.Array (filter, length, (:))
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
import Ocelot.Block.Format as Format
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import Stories.Utilities as Utils
import UIGuide.Block.Backdrop as Backdrop
import Unsafe.Coerce (unsafeCoerce)

data Scene = PackageGrid | PackageGraph | ModuleTree TreeLayout
data FilterData = LinkFilter (SpagoGraphLinkID -> Boolean)
                | NodeFilter (SpagoSimNode -> Boolean)
data Action
  = Initialize
  | Finalize
  | Scene Scene
  | ToggleForce Label
  | Filter FilterData
  | ChangeStyling String
  | ChangeSimConfig SimVariable
  | StopSim
  | StartSim

type Input = SimulationState_
  
type State = {
    simulationState :: SimulationState_
  , svgClass        :: String -- by controlling the class that is on the svg we can completely change the look of the vis (and not have to think about this at D3 level)
  , links           :: Array SpagoGraphLinkID
  , nodes           :: Array SpagoSimNode
  , activeForces    :: Array String
  , selections      :: Array D3Selection_
  , model           :: Maybe SpagoModel -- the model should actually be a component, probably a hook so that it can be constructed by this component and not be a Maybe
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
  initialState simulation = { 
      simulationState: simulation
    , svgClass: "cluster"
    , model: Nothing
    , links: []
    , nodes: []
    , selections: [] 
    , activeForces: []
  }

  renderSimState state =
    HH.div
      [ HP.classes [ HH.ClassName "m-6" ]]
      [ Format.caption_ [ HH.text "Simulation state" ]
      , HH.p_
          [ HH.text $ "class: " <> state.svgClass ] 
      , HH.p_
          [ HH.text $ "link count: " <> show (length state.links) ] 
      , HH.p_
          [ HH.text $ "node count:" <> show (length state.nodes )] 
      ]

  renderSimControls state =
    HH.div
      [ HP.classes [ HH.ClassName "m-6" ]]
      [ Format.caption_ [ HH.text "Simulation controls" ]
      , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
          [ Button.buttonGroup_
            [ Button.buttonPrimaryLeft
                [ HE.onClick $ const StopSim ]
                [ HH.text "Stop" ]
            , Button.buttonPrimaryRight
                [ HE.onClick $ const StartSim ]
                [ HH.text "Start" ]
            ]
          ]
      , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
          [ Format.caption_ [ HH.text "Scenes" ]
          , Button.buttonGroup_
            [ Button.buttonPrimaryLeft
                [ HE.onClick $ const (Scene PackageGrid) ]
                [ HH.text "Package Grid" ]
            , Button.buttonPrimaryCenter
                [ HE.onClick $ const (Scene PackageGraph) ]
                [ HH.text "Package Graph" ]
            , Button.buttonPrimaryCenter
                [ HE.onClick $ const (Scene $ ModuleTree Horizontal) ]
                [ HH.text "Horiz. Tree" ]
            , Button.buttonPrimaryRight
                [ HE.onClick $ const (Scene $ ModuleTree Radial) ]
                [ HH.text "Radial Tree" ]
            ]
          ]
      , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
          [ Format.caption_ [ HH.text "Which nodes should be displayed?" ]
          , Button.buttonGroup_
            [ Button.buttonLeft
                [ HE.onClick $ const (Filter $ NodeFilter isPackage) ]
                [ HH.text "Packages" ]
            , Button.buttonCenter
                [ HE.onClick $ const (Filter $ NodeFilter (const true)) ]
                [ HH.text "Both" ]
            , Button.buttonRight
                [ HE.onClick $ const (Filter $ NodeFilter isUsedModule) ]
                [ HH.text "Modules" ]
            ]
          ]
      , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
          [ Format.caption_ [ HH.text "Which links should be displayed?" ]
          , Button.buttonGroup_
            [ Button.buttonLeft
                [ HE.onClick $ const (Filter $ LinkFilter isM2M_Tree_Link) ]
                [ HH.text "Treelink" ]
            , Button.buttonCenter
                [ HE.onClick $ const (Filter $ LinkFilter isM2M_Graph_Link) ]
                [ HH.text "Graphlink" ]
            , Button.buttonCenter
                [ HE.onClick $ const (Filter $ LinkFilter isM2P_Link) ]
                [ HH.text "M2P" ]
            , Button.buttonCenter
                [ HE.onClick $ const (Filter $ LinkFilter isP2P_Link) ]
                [ HH.text "P2P" ]
            , Button.buttonRight
                [ HE.onClick $ const (Filter $ LinkFilter (const false)) ]
                [ HH.text "none" ]
            ]
          ]
      , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
          [ Format.caption_ [ HH.text "Which links should be exert force?" ]
          , Button.buttonGroup_
            [ Button.buttonLeft
                [ HE.onClick $ const (Filter $ LinkFilter isM2M_Tree_Link) ]
                [ HH.text "Treelink" ]
            , Button.buttonCenter
                [ HE.onClick $ const (Filter $ LinkFilter isM2M_Graph_Link) ]
                [ HH.text "Graphlink" ]
            , Button.buttonCenter
                [ HE.onClick $ const (Filter $ LinkFilter isM2P_Link) ]
                [ HH.text "M2P" ]
            , Button.buttonRight
                [ HE.onClick $ const (Filter $ LinkFilter isP2P_Link) ]
                [ HH.text "P2P" ]
            ]
          ]
      , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
          [ Format.caption_ [ HH.text "Stylesheet" ]
          , Button.buttonGroup_
            [ Button.buttonLeft
                [ HE.onClick $ const (ChangeStyling "cluster") ]
                [ HH.text "Clusters" ]
            , Button.buttonCenter
                [ HE.onClick $ const (ChangeStyling "graph") ]
                [ HH.text "Graph" ]
            , Button.buttonCenter
                [ HE.onClick $ const (ChangeStyling "tree") ]
                [ HH.text "Tree" ]
            , Button.buttonRight
                [ HE.onClick $ const (ChangeStyling "none") ]
                [ HH.text "None" ]
            ]
          ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
        [ Utils.tailwindClass "story-container spago" ]
        [ HH.div
            [ Utils.tailwindClass "story-panel-about" ]
            [ renderSimState state
            , renderSimControls state
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
    (model :: Maybe SpagoModel) <- H.liftAff getModel
    H.modify_ (\s -> s { model = model })
    runEffectSimulation Graph.setup
    runEffectSimulation (addForces forces)
    handleAction $ Scene PackageGrid

  Finalize -> pure unit

  Scene PackageGrid -> do
    setCssEnvironment "cluster"
    filterLinks isM2P_Link
    filterNodes (const true)
    setActiveForces gridForceSettings
    unpinNodes

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateNodes state.nodes graphAttrs -- all nodes
    runEffectSimulation $ Graph.updateLinks state.links -- filtered links
    runEffectSimulation $ enableOnlyTheseForces gridForceSettings
    simulationStart

  Scene PackageGraph -> do
    setCssEnvironment "graph"
    filterLinks isP2P_Link
    filterNodes isPackage
    setActiveForces packageForceSettings

    state <- H.get
    simulationStop
    -- runEffectSimulation $ uniformlyDistributeNodes -- TODO
    runEffectSimulation $ Graph.updateNodes state.nodes graphAttrs -- filtered to packages only
    runEffectSimulation $ Graph.updateLinks state.links -- filtered to only P2P
    runEffectSimulation $ enableOnlyTheseForces state.activeForces
    simulationStart

  Scene (ModuleTree _) -> do
    setCssEnvironment "tree"
    setActiveForces treeForceSettings
    filterNodes isUsedModule
    filterLinks isM2M_Tree_Link
    pinTreeNodes -- side-effect, because if we make _new_ nodes the links won't be pointing to them

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateNodes state.nodes treeAttrs
    runEffectSimulation $ Graph.updateLinksTree state.links Horizontal
    runEffectSimulation $ enableOnlyTheseForces treeForceSettings
    simulationStart

  ToggleForce label -> do
    simulationStop
    runEffectSimulation $ toggleForceByLabel label
    simulationStart

  Filter (LinkFilter x) -> do
    filterLinks x

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateLinks state.links
    simulationStart

  Filter (NodeFilter x) -> do
    filterNodes x

    state <- H.get
    simulationStop
    runEffectSimulation $ Graph.updateNodes state.nodes graphAttrs -- TODO clearly now this has to be maintained in the state, cos toggling force will change attrs!
    simulationStart



  ChangeStyling style -> modify_ (\s -> s { svgClass = style })

  ChangeSimConfig c -> runEffectSimulation $ setConfigVariable c

  StartSim -> simulationStart

  StopSim -> runEffectSimulation (setConfigVariable $ Alpha 0.0)

setCssEnvironment :: forall m. MonadState State m => String -> m Unit
setCssEnvironment string = modify_ (\s -> s { svgClass = string })

setActiveForces :: forall m. MonadState State m => Array String -> m Unit
setActiveForces forces = modify_ (\s -> s { activeForces = forces })

filterLinks :: forall m. MonadState State m => (SpagoGraphLinkID -> Boolean) -> m Unit
filterLinks fn = do
  state <- get
  case state.model of
    Nothing -> pure unit
    (Just graph) -> modify_ (\s -> s { links = filter fn graph.links })

filterNodes :: forall m. MonadState State m => (SpagoSimNode -> Boolean) -> m Unit
filterNodes fn = do
  state <- H.get
  case state.model of
    Nothing -> pure unit
    (Just graph) -> modify_ (\s -> s { nodes = filter fn graph.nodes })

pinTreeNodes :: forall m. MonadState State m => m Unit
pinTreeNodes = do
  state <- H.get
  let _ = pinTreeNode_ <$> state.nodes -- NB side-effecting on the nodes in the state so that object refs in links stay good
  pure unit

unpinNodes :: forall m. MonadState State m => m Unit
unpinNodes = do
  state <- H.get
  let _ = unpinNode_ <$> state.nodes -- NB side-effecting on the nodes in the state so that object refs in links stay good
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
  pure $ treeReduction rootID model


-- | ============================================
-- | FORCES
-- | ============================================
gridForceSettings = [ "packageGrid", "clusterx", "clustery", "collide1" ]
treeForceSettings = ["links", "center", "charge1", "collide1" ]
packageForceSettings = [ "packageOrbit", "collide2", "charge2", "collide2", "x", "y" ]
forces :: Array Force
forces = [
        createForce "collide1"     ForceCollide  allNodes [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ]
      , createForce "collide2"     ForceCollide  allNodes [ F.strength 1.0, F.radius datum_.collideRadiusBig, F.iterations 1.0 ]
      , createForce "charge1"      ForceManyBody allNodes [ F.strength (-30.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]
      , createForce "center"       ForceCenter   allNodes [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
      , createForce "x"            ForceX        allNodes [ F.strength 0.05, F.x 0.0 ]
      , createForce "y"            ForceY        allNodes [ F.strength 0.07, F.y 0.0 ]
      , createForce "charge2"      ForceManyBody allNodes [ F.strength (-100.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax 100.0 ]
      , createForce "clusterx"     ForceX        allNodes [ F.strength 0.2, F.x datum_.clusterPointX ]
      , createForce "clustery"     ForceY        allNodes [ F.strength 0.2, F.y datum_.clusterPointY ]

      , createForce "packageGrid"  (ForceFixPositionXY gridXY) (Just $ FilterNodes "packages only" datum_.isPackage) [ ] 
      , createForce "packageOrbit" ForceRadial   (selectivelyApplyForce datum_.isPackage "packages only") 
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 300.0 ]
      , createForce "moduleOrbit1" ForceRadial   (selectivelyApplyForce datum_.isUnusedModule "unused modules only") 
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 700.0 ]
      , createForce "moduleOrbit2" ForceRadial   (selectivelyApplyForce datum_.isUsedModule "used modules only")
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      , createForce "moduleOrbit3" ForceRadial   (selectivelyApplyForce datum_.isUsedModule "direct deps of Main")
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      ]
  where
    -- onlyPackages = (\d -> if datum_.isPackage d then 0.8 else 0.0)
    -- onlyUsedModules = (\d -> if datum_.isUsedModule d then 0.8 else 0.0)
    -- onlyUnusedModules = (\d -> if datum_.isUnusedModule d then 0.8 else 0.0)
    -- gridFilter d = datum_.isPackage d

    gridXY _ i = cluster2Point i
    selectivelyApplyForce filterFn description = Just $ FilterNodes description filterFn

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
  renderData (Force label s t f cs h_) =
    [ Table.cell_ [ Checkbox.checkbox_ 
                  [ HP.checked (s == ForceActive)
                  , HE.onChecked $ const (ToggleForce label)
                  ] [] ]
    , Table.cell  [ css "text-left" ]
      [ HH.div_ [
          HH.text $ label <> "\n" <> show t -- use forceDescription t for more detailed explanation
        ]
      ]
    , Table.cell  [ css "text-left" ] [ HH.text $ showForceFilter f ]
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
      [ renderHeader ]
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
  renderData (Force l s t f cs h_) =
    [ Table.cell_ [ Checkbox.checkbox_ [] [] ]
    , Table.cell  [ css "text-left" ]
      [ HH.div_ [
          HH.text $ show t <> ": " <> l
        ]
      ]
    , Table.cell  [ css "text-left" ] [ HH.text "elements" ]
    ]
