module PSD3.CodeExplorer.HTML where

import Prelude

import PSD3.Data.Tree (TreeLayout(..))
import D3.Viz.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Viz.Spago.Files (NodeType(..), isM2M_Graph_Link, isM2M_Tree_Link, isM2P_Link, isP2P_Link)
import D3.Viz.Spago.GitMetrics (ColorByOption(..))
import D3.Viz.Spago.Model (SpagoSimNode, isPackage, isUsedModule)
import Data.String.CodeUnits (take)
import PSD3.Internal.Simulation.Forces (showType)
import PSD3.Internal.Simulation.Types (D3SimulationState_(..), Force(..), ForceStatus(..), SimVariable(..), _forceLibrary, showForceFilter)
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
import Data.Set as Set
import Halogen (ComponentSlot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (StepValue(..))
import Halogen.HTML.Properties as HP
import PSD3.CodeExplorer.Actions (Action(..), FilterData(..), Scene(..), StyleChange(..))
import D3.Viz.Spago.GitMetrics (ColorByOption(..))
import PSD3.CodeExplorer.State (State, getStagingLinks, getStagingNodes, getSimulationVariables)
import PSD3.Shared.ZoomSticker as ZoomSticker
import PSD3.CodeExplorer.WelcomeOverlay as WelcomeOverlay
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Filter for project modules only (from src/ directory), but keep all packages
isProjectModule :: SpagoSimNode -> Boolean
isProjectModule node = case node.nodetype of
  IsModule path -> take 4 path == "src/"
  IsPackage _ -> true  -- Keep all packages visible

-- | Full-screen render for Spago page
render :: forall m.
  State -> HH.HTML (ComponentSlot () m (Action SpagoSimNode)) (Action SpagoSimNode)
render state =
  HH.div
      [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "spago-fullscreen", HH.ClassName "page-with-watermark" ] ]
      [ -- Navigation Header
        TutorialNav.renderHeader CodeExplorer

      -- Floating control panel (top-left) - with editorial styling
      , HH.div
          [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "floating-panel--small", HH.ClassName "spago-controls-panel", HH.ClassName "editorial" ] ]
          [ HH.h2
              [ HP.classes [ HH.ClassName "floating-panel__title", HH.ClassName "spago-controls__title" ] ]
              [ HH.text "Controls" ]
          , renderSimControls state
          , renderSimState state
          ]

      , -- Floating force table panel (top-right) - with editorial styling
        HH.div
          [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "floating-panel--large", HH.ClassName "spago-forces-panel", HH.ClassName "editorial" ] ]
          [ renderTableForces state ]

      , -- Main visualization area (full screen)
        HH.div
          [ HP.classes [ HH.ClassName "svg-container", HH.ClassName "fullscreen-viz", HH.ClassName "spago-viz-container", HH.ClassName state.scene.cssClass ] ]
          [ ZoomSticker.render ]

      , -- Welcome overlay (conditional)
        if state.showWelcome
           then WelcomeOverlay.render DismissWelcome
           else HH.text ""
      ]

renderSimState :: forall p. State -> HH.HTML p (Action SpagoSimNode)
renderSimState state =
  HH.div
    [ HP.classes [ HH.ClassName "spago-sim-state" ]]
    [ Format.caption_ [ HH.text "Simulation state" ]
    , HH.p_
        [ HH.text $ "CSS Class: " <> state.scene.cssClass ]
    , HH.p_
        [ HH.text $ "Links: " <> show (length $ getStagingLinks state) ]
    , HH.p_
        [ HH.text $ "Nodes: " <> show (length $ getStagingNodes state)]
    ]

renderSimControls :: forall p. State -> HH.HTML p (Action SpagoSimNode)
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

    , Format.subHeading_ [ HH.text "Tagging (Demo)" ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.buttonGroup_
          [ Button.buttonPrimaryLeft
              [ HE.onClick $ const TagHalogen ]
              [ HH.text "Tag Halogen" ]
          , Button.buttonPrimaryRight
              [ HE.onClick $ const ClearTags ]
              [ HH.text "Clear Tags" ]
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

    , Format.subHeading_ [ HH.text "Color By" ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (ChangeColorBy ColorByGroup) ]
              [ HH.text "Package" ]
          , Button.buttonCenter
              [ HE.onClick $ const (ChangeColorBy ColorByDepth) ]
              [ HH.text "Depth" ]
          , Button.buttonRight
              [ HE.onClick $ const (ChangeColorBy ColorBySize) ]
              [ HH.text "Size" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Format.contentHeading_ [ HH.text "Git Metrics" ]
        , Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (ChangeColorBy ColorByCommits) ]
              [ HH.text "Commits" ]
          , Button.buttonCenter
              [ HE.onClick $ const (ChangeColorBy ColorByRecency) ]
              [ HH.text "Recency" ]
          , Button.buttonRight
              [ HE.onClick $ const (ChangeColorBy ColorByAge) ]
              [ HH.text "Age" ]
          ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const (ChangeColorBy ColorByAuthors) ]
              [ HH.text "Authors" ]
          , Button.buttonRight
              [ HE.onClick $ const (ChangeColorBy ColorByChurn) ]
              [ HH.text "Churn" ]
          ]
        ]

    , Format.subHeading_ [ HH.text "Git Replay" ]
    , HH.div [ HP.classes [ HH.ClassName "control-group"]]
        [ Button.buttonGroup_
          [ Button.buttonLeft
              [ HE.onClick $ const StartReplay ]
              [ HH.text "Play" ]
          , Button.buttonCenter
              [ HE.onClick $ const StopReplay ]
              [ HH.text "Stop" ]
          , Button.buttonRight
              [ HE.onClick $ const ResetReplay ]
              [ HH.text "Reset" ]
          ]
        ]
    ]

renderTableForces :: forall m. State -> H.ComponentHTML (Action SpagoSimNode) () m
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
    let isActive = Set.member force.name state.scene.activeForces
    in HH.div
      [ HP.classes
          [ HH.ClassName "force-item"
          , HH.ClassName if isActive then "force-active" else "force-inactive"
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

slider :: { var :: Number -> SimVariable, id :: String, min :: Number, max :: Number, step :: Number, value :: Number } -> Array (HP.IProp _ (Action SpagoSimNode))
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
