module PSD3.CodeExplorer.HTML where

import Prelude

import D3.Data.Tree (TreeLayout(..))
import D3.Viz.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Viz.Spago.Files (isM2M_Graph_Link, isM2M_Tree_Link, isM2P_Link, isP2P_Link, NodeType(..))
import D3.Viz.Spago.Model (SpagoSimNode, isM2M_Graph_Link_, isM2M_Tree_Link_, isM2P_Link_, isP2P_Link_, isPackage, isUsedModule)
import D3.Node (D3_SimulationNode(..))
import Data.String.CodeUnits (take)
import D3.Simulation.Forces (showType)
import D3.Simulation.Types (D3SimulationState_(..), Force(..), ForceStatus(..), SimVariable(..), _forceLibrary, showForceFilter)
import PSD3.Button as Button
import PSD3.Checkbox as Checkbox
import PSD3.Format as Format
import PSD3.Table as Table
import PSD3.Properties (css)
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
import PSD3.CodeExplorer.Actions (Action(..), FilterData(..), Scene(..), StyleChange(..))
import PSD3.CodeExplorer.State (State, _cssClass, _stagingLinkFilter, _stagingLinks, _stagingNodes, getSimulationVariables)

-- | Filter for project modules only (from src/ directory), but keep all packages
isProjectModule :: SpagoSimNode -> Boolean
isProjectModule (D3SimNode node) = case node.nodetype of
  IsModule path -> take 4 path == "src/"
  IsPackage _ -> true  -- Keep all packages visible

-- | Full-screen render for Spago page
render :: forall m.
  State -> HH.HTML (ComponentSlot () m Action) Action
render state =
  HH.div
      [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "spago-fullscreen" ] ]
      [ -- Floating control panel (top-left) - with editorial styling
        HH.div
          [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "floating-panel--small", HH.ClassName "spago-controls-panel", HH.ClassName "editorial" ] ]
          [ HH.h2
              [ HP.classes [ HH.ClassName "floating-panel__title", HH.ClassName "spago-controls__title" ] ]
              [ HH.text "Controls" ]
          , HH.div
              [ HP.classes [ HH.ClassName "control-group", HH.ClassName "control-group--home" ] ]
              [ HH.a
                  [ HP.href "#/"
                  , HP.classes [ HH.ClassName "button", HH.ClassName "button--primary" ]
                  ]
                  [ HH.text "← Home" ]
              ]
          , renderSimControls state
          , renderSimState state
          ]

      , -- Floating force table panel (top-right) - with editorial styling
        HH.div
          [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "floating-panel--large", HH.ClassName "spago-forces-panel", HH.ClassName "editorial" ] ]
          [ renderTableForces state ]

      , -- Main visualization area (full screen)
        HH.div
          [ HP.classes [ HH.ClassName "svg-container", HH.ClassName "fullscreen-viz", HH.ClassName "spago-viz-container", HH.ClassName (view _cssClass state) ] ]
          [ ]
      ]

renderSimState :: forall p. State -> HH.HTML p Action
renderSimState state =
  HH.div
    [ HP.classes [ HH.ClassName "spago-sim-state" ]]
    [ Format.caption_ [ HH.text "Simulation state" ]
    , HH.p_
        [ HH.text $ "CSS Class: " <> (view _cssClass state) ]
    , HH.p_
        [ HH.text $ "Links: " <> show (length $ view _stagingLinks state) ]
    , HH.p_
        [ HH.text $ "Nodes: " <> show (length $ view _stagingNodes state)]
    ]

renderSimControls :: forall p. State -> HH.HTML p Action
renderSimControls state = do
  let
    params = getSimulationVariables state
  HH.div
    [ HP.classes [ HH.ClassName "spago-sim-controls" ]]
    [ Format.subHeading_ [ HH.text "Scene Selection" ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const (Scene PackageGrid) ]
              [ HH.text "Package Grid" ]
          , Button.buttonPrimaryRight
              [ HE.onClick $ const (Scene PackageGraph) ]
              [ HH.text "Package Graph" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
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
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const (Scene LayerSwarm) ]
              [ HH.text "Layer Swarm" ]
          ]
        ]

    , Format.subHeading_ [ HH.text "Simulation Control" ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ HH.input $ slider { var: Alpha, id: "alpha-slider", min: 0.0, max: 100.0, step: 10.0, value: params.alpha * 100.0 }
        , Format.caption_ [ HH.text ("Alpha: " <> show params.alpha) ]
        , HH.input $ slider { var: AlphaTarget, id: "alphatarget-slider", min: 0.0, max: 100.0, step: 10.0, value: params.alphaTarget * 100.0 }
        , Format.caption_ [ HH.text ("AlphaTarget: " <> show params.alphaTarget) ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
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

    , Format.subHeading_ [ HH.text "Filters" ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Format.contentHeading_ [ HH.text "Node visibility" ]
        , Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (Filter $ NodeFilter isPackage) ]
              [ HH.text "Packages" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ NodeFilter (const true)) ]
              [ HH.text "All" ]
          , Button.buttonRight
              [ HE.onClick $ const (Filter $ NodeFilter isUsedModule) ]
              [ HH.text "Modules" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.button
              [ HE.onClick $ const (Filter $ NodeFilter isProjectModule) ]
              [ HH.text "Only project files" ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Format.contentHeading_ [ HH.text "Link Visibility" ]
        , Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (Filter $ LinkShowFilter isM2M_Tree_Link) ]
              [ HH.text "Tree" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkShowFilter isM2M_Graph_Link) ]
              [ HH.text "Graph" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkShowFilter isM2P_Link) ]
              [ HH.text "M2P" ]
          , Button.buttonCenter
              [ HE.onClick $ const (Filter $ LinkShowFilter isP2P_Link) ]
              [ HH.text "P2P" ]
          , Button.buttonRight
              [ HE.onClick $ const (Filter $ LinkShowFilter (const false)) ]
              [ HH.text "None" ]
          ]
        ]

    , Format.subHeading_ [ HH.text "Styling" ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (ChangeStyling $ GraphStyle clusterSceneAttributes) ]
              [ HH.text "Cluster" ]
          , Button.buttonCenter
              [ HE.onClick $ const (ChangeStyling $ GraphStyle graphSceneAttributes) ]
              [ HH.text "Graph" ]
          , Button.buttonRight
              [ HE.onClick $ const (ChangeStyling $ GraphStyle treeSceneAttributes) ]
              [ HH.text "Tree" ]
          ]
        ]
    ]

renderTableForces :: forall m. State -> H.ComponentHTML Action () m
renderTableForces state  =
  HH.div_
  [ HH.h3_ [ HH.text "Forces"]
  , HH.div
      [ HP.classes [ HH.ClassName "force-grid" ] ]
      (renderForceItem <$> forceData)
  ]
  where
  forceData = snd <$> (toUnfoldable $ view _forceLibrary state)

  renderForceItem (Force force) =
    HH.div
      [ HP.classes
          [ HH.ClassName "force-item"
          , HH.ClassName if force.status == ForceActive then "force-active" else "force-inactive"
          ]
      , HE.onClick $ const (ToggleForce force.name)
      ]
      [ HH.div
          [ HP.classes [ HH.ClassName "force-name" ] ]
          [ HH.text force.name ]
      , HH.div
          [ HP.classes [ HH.ClassName "force-type" ] ]
          [ HH.text $ showType force.type ]
      , HH.div
          [ HP.classes [ HH.ClassName "force-filter" ] ]
          [ HH.text $ showForceFilter force.filter ]
      ]

slider :: { var :: Number -> SimVariable, id :: String, min :: Number, max :: Number, step :: Number, value :: Number } -> Array (HP.IProp _ Action)
slider config = do
  let
    toScale :: String -> Number
    toScale s =
      (toNumber $ fromMaybe 0 $ fromString s) / 100.0

  [ HE.onValueInput (ChangeSimConfig <<< config.var <<< toScale)
  , HP.type_ HP.InputRange
  , HP.id config.id
  , HP.class_ (H.ClassName "range-slider scaling-slider")
  , HP.min config.min
  , HP.max config.max
  , HP.step (Step config.step)
  , HP.value (show config.value)
  ]
