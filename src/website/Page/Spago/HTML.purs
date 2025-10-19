module PSD3.Pages.Spago.HTML where

import Prelude

import D3.Data.Tree (TreeLayout(..))
import D3.Examples.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Examples.Spago.Files (isM2M_Graph_Link, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Examples.Spago.Model (isM2M_Graph_Link_, isM2M_Tree_Link_, isM2P_Link_, isP2P_Link_, isPackage, isUsedModule)
import D3.Simulation.Forces (showType)
import D3.Simulation.Types (D3SimulationState_(..), Force(..), ForceStatus(..), SimVariable(..), _forceLibrary, showForceFilter)
import PSD3.Component.Card as Card
import Data.Array (length, (:))
import Data.Int (fromString, toNumber)
import Data.Lens (view)
import Data.Map (toUnfoldable)
import Data.Maybe (fromMaybe)
import Data.Tuple (snd)
import Halogen (ComponentSlot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (StepValue(..))
import Halogen.HTML.Properties as HP
import PSD3.Component.Button as Button
import PSD3.Component.Checkbox as Checkbox
import PSD3.Component.Format as Format
import PSD3.Component.Table as Table
import PSD3.Component.Properties (css)
import PSD3.Pages.Spago.Actions (Action(..), FilterData(..), Scene(..), StyleChange(..))
import PSD3.Pages.Spago.State (State, _cssClass, _stagingLinkFilter, _stagingLinks, _stagingNodes, getSimulationVariables)
import PSD3.Pages.Utilities as Utils
import PSD3.Component.Backdrop as Backdrop

render :: forall m.
  State -> HH.HTML (ComponentSlot () m Action) Action
render state =
  HH.div
      [ Utils.tailwindClass "story-container spago" ]
      [ HH.div
          [ Utils.tailwindClass "story-panel-about" ]
          [ Card.card_ [ blurbtext ]
          , renderSimControls state
          , renderSimState state
          , renderTableForces state
          -- , renderTableElements state.simulation
          ]
      , HH.div
          [ Utils.tailwindClass $ "svg-container " <> (view _cssClass state) ]
          [ ]
      ]

renderSimState :: forall p. State -> HH.HTML p Action
renderSimState state =
  HH.div
    [ HP.classes [ HH.ClassName "m-6" ]]
    [ Format.caption_ [ HH.text "Simulation state" ]
    , HH.p_
        [ HH.text $ "class: " <> (view _cssClass state) ] 
    , HH.p_
        [ HH.text $ "link count: " <> show (length $ view _stagingLinks state) ] 
    , HH.p_
        [ HH.text $ "node count:" <> show (length $ view _stagingNodes state)] 
    -- , HH.p_
    --     [ HH.text $ "initial forces:" <> show (listActiveForces state) ] 
    -- , HH.p_
    --     [ HH.text $ "sim vars: " <> show (getSimulationVariables state)]
    ]

renderSimControls :: forall p. State -> HH.HTML p Action
renderSimControls state = do
  let
    params = getSimulationVariables state
  HH.div
    [ HP.classes [ HH.ClassName "m-6" ]]
    [ Format.subHeading_ [ HH.text "Simulation controls" ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Format.contentHeading_ [ HH.text "Scenes" ]
        , Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const (Scene PackageGrid) ]
              [ HH.text "Package Grid" ]
          , Button.buttonPrimaryRight
              [ HE.onClick $ const (Scene PackageGraph) ]
              [ HH.text "Package Graph" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const (Scene $ ModuleTree Horizontal) ]
              [ HH.text "Horiz. Tree" ]
          , Button.buttonPrimaryCenter
              [ HE.onClick $ const (Scene $ ModuleTree Vertical) ]
              [ HH.text "Vert. Tree" ]
          , Button.buttonPrimaryRight
              [ HE.onClick $ const (Scene $ ModuleTree Radial) ]
              [ HH.text "Radial Tree" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const (Scene LayerSwarm) ]
              [ HH.text "LayerSwarm" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Format.contentHeading_ [ HH.text "Params" ]
        , HH.input $ slider { var: Alpha, id: "alpha-slider", min: 0.0, max: 100.0, step: 10.0, value: params.alpha * 100.0 }
        , Format.caption_ [ HH.text ("Alpha: " <> show params.alpha) ]
        -- , HH.input $ slider { var: AlphaDecay, id: "alphadecay-slider", min: 0.0, max: 100.0, step: 10.0, value: params.alphaDecay * 100.0 }
        -- , Format.caption_ [ HH.text ("AlphaDecay: " <> show params.alphaDecay) ]
        -- , HH.input $ slider { var: AlphaMin, id: "alphamin-slider", min: 0.0, max: 100.0, step: 10.0, value: params.alphaMin * 100.0 }
        -- , Format.caption_ [ HH.text ("AlphaMin: " <> show params.alphaMin) ]
        , HH.input $ slider { var: AlphaTarget, id: "alphatarget-slider", min: 0.0, max: 100.0, step: 10.0, value: params.alphaTarget * 100.0 }
        , Format.caption_ [ HH.text ("AlphaTarget: " <> show params.alphaTarget) ]
        -- , HH.input $ slider { var: VelocityDecay, id: "velocitydecay-slider", min: 0.0, max: 100.0, step: 10.0, value: params.velocityDecay * 100.0 }
        -- , Format.caption_ [ HH.text ("VelocityDecay: " <> show params.velocityDecay) ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const StopSim ]
              [ HH.text "Stop" ]
          , Button.buttonPrimaryCenter
              [ HE.onClick $ const (ChangeSimConfig $ AlphaTarget 0.3) ]
              [ HH.text "Heat" ]
          , Button.buttonPrimaryCenter
              [ HE.onClick $ const (ChangeSimConfig $ AlphaTarget 0.0) ]
              [ HH.text "Cool" ]
          , Button.buttonPrimaryRight
              [ HE.onClick $ const StartSim ]
              [ HH.text "Start" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Format.contentHeading_ [ HH.text "Which nodes should be displayed?" ]
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
        [ Format.contentHeading_ [ HH.text "Put links into simulation" ]
        , Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (Filter $ LinkShowFilter isM2M_Tree_Link) ]
              [ HH.text "Treelink" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkShowFilter isM2M_Graph_Link) ]
              [ HH.text "Graphlink" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkShowFilter isM2P_Link) ]
              [ HH.text "M2P" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkShowFilter isP2P_Link) ]
              [ HH.text "P2P" ]
          , Button.buttonRight
              [ HE.onClick $ const (Filter $ LinkShowFilter (const false)) ]
              [ HH.text "none" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Format.contentHeading_ [ HH.text "Limit only these links to exert force?" ]
        , Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (Filter $ LinkForceFilter isM2M_Tree_Link_) ]
              [ HH.text "Treelink" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkForceFilter isM2M_Graph_Link_) ]
              [ HH.text "Graphlink" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkForceFilter isM2P_Link_) ]
              [ HH.text "M2P" ]
          , Button.buttonRight
              [ HE.onClick $ const (Filter $ LinkForceFilter isP2P_Link_) ]
              [ HH.text "P2P" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Format.contentHeading_ [ HH.text "D3 attributes chosen" ]
        , Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (ChangeStyling $ GraphStyle clusterSceneAttributes) ]
              [ HH.text "Clusters" ]
          , Button.buttonCenter
              [ HE.onClick $ const (ChangeStyling $ GraphStyle graphSceneAttributes) ]
              [ HH.text "Graph" ]
          , Button.buttonRight
              [ HE.onClick $ const (ChangeStyling $ GraphStyle treeSceneAttributes) ]
              [ HH.text "Tree" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "mb-6"]]
        [ Format.contentHeading_ [ HH.text "Stylesheet" ]
        , Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (ChangeStyling $ TopLevelCSS "cluster") ]
              [ HH.text "Clusters" ]
          , Button.buttonCenter
              [ HE.onClick $ const (ChangeStyling $ TopLevelCSS "graph") ]
              [ HH.text "Graph" ]
          , Button.buttonCenter
              [ HE.onClick $ const (ChangeStyling $ TopLevelCSS "tree") ]
              [ HH.text "Tree" ]
          , Button.buttonRight
              [ HE.onClick $ const (ChangeStyling $ TopLevelCSS "none") ]
              [ HH.text "None" ]
          ]
        ]
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

renderTableForces :: forall m. State -> H.ComponentHTML Action () m
renderTableForces state  =
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
              (toUnfoldable $ view _forceLibrary state)

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

renderTableElements :: forall m. State -> H.ComponentHTML Action () m
renderTableElements state  =
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
    (toUnfoldable $ view _forceLibrary state)

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

slider config = do
  let
    toScale :: String -> Number
    toScale s =
      (toNumber $ fromMaybe 0 $ fromString s) / 100.0

  [ HE.onValueInput (ChangeSimConfig <<< config.var <<< toScale)
  , HP.type_ HP.InputRange
  , HP.id config.id
  , HP.class_ (H.ClassName "scaling-slider")
  , HP.min config.min
  , HP.max config.max
  , HP.step (Step config.step)
  , HP.value (show config.value)
  ]
