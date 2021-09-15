module Stories.Spago.HTML where

import Prelude

import D3.Attributes.Instances (Label)
import D3.Data.Tree (TreeLayout(..))
import D3.Examples.Spago.Files (isM2M_Graph_Link, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (isPackage, isUsedModule)
import D3.Simulation.Forces (showType)
import D3.Simulation.Types (D3SimulationState_(..), Force(..), ForceStatus(..), SimVariable(..), showForceFilter)
import D3Tagless.Block.Card as Card
import Data.Array (length, (:))
import Data.Lens (over, view)
import Data.Map (toUnfoldable)
import Data.Tuple (snd)
import Halogen (ComponentSlot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Format as Format
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import Stories.Spago.Actions (Action(..), FilterData(..), Scene(..))
import Stories.Spago.State (State, _stagingForces, _stagingLinks, _stagingNodes, listActiveForces)
import Stories.Utilities as Utils
import UIGuide.Block.Backdrop as Backdrop

renderSimState :: forall p. State -> HH.HTML p Action
renderSimState state =
  HH.div
    [ HP.classes [ HH.ClassName "m-6" ]]
    [ Format.caption_ [ HH.text "Simulation state" ]
    , HH.p_
        [ HH.text $ "class: " <> state.svgClass ] 
    , HH.p_
        [ HH.text $ "link count: " <> show (length $ view _stagingLinks state) ] 
    , HH.p_
        [ HH.text $ "node count:" <> show (length $ view _stagingNodes state)] 
    , HH.p_
        [ HH.text $ "initial forces:" <> show (listActiveForces state) ] 
    ]

renderSimControls :: forall p. State -> HH.HTML p Action
renderSimControls _ =
  HH.div
    [ HP.classes [ HH.ClassName "m-6" ]]
    [ Format.caption_ [ HH.text "Simulation controls" ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const StopSim ]
              [ HH.text "Stop" ]
          , Button.buttonPrimaryCenter
              [ HE.onClick $ const (ChangeSimConfig $ VelocityDecay 0.7) ]
              [ HH.text "Slow" ]
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
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ NodeFilter (const false)) ]
              [ HH.text "Neither" ]
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

render :: forall t633.
  State -> HH.HTML (ComponentSlot () t633 Action) Action
render state =
  HH.div
      [ Utils.tailwindClass "story-container spago" ]
      [ HH.div
          [ Utils.tailwindClass "story-panel-about" ]
          [ Card.card_ [ blurbtext ]
          , renderSimControls state
          , renderSimState state
          , renderTableForces state.simulation
          -- , renderTableElements state.simulation
          ]
      , HH.div
          [ Utils.tailwindClass $ "svg-container " <> state.svgClass ]
          [ ]
      ]


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

renderTableForces :: forall m. D3SimulationState_ -> H.ComponentHTML Action () m
renderTableForces (SimState_ simulation)  =
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
  renderData (Force force) = do
    [ Table.cell_ [ Checkbox.checkbox_ 
                  [ HP.checked (force.status == ForceActive)
                  , HE.onChecked $ const (ToggleForce force.name)
                  ] [] ]
    , Table.cell  [ css "text-left" ]
      [ HH.div_ [
          HH.text $ force.name <> "\n" <> showType force.type -- use forceDescription t for more detailed explanation
        ]
      ]
    , Table.cell  [ css "text-left" ] [ HH.text $ showForceFilter force.filter ]
    ]

renderTableElements :: forall m. D3SimulationState_ -> H.ComponentHTML Action () m
renderTableElements (SimState_ simulation)  =
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
  renderData force =
    [ Table.cell_ [ Checkbox.checkbox_ [] [] ]
    , Table.cell  [ css "text-left" ]
      [ HH.div_ [
          HH.text "this is a placeholder"
        ]
      ]
    , Table.cell  [ css "text-left" ] [ HH.text "elements" ]
    ]
