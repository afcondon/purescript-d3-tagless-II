-- | Orchestration.purs - Scene transitions and timing
-- |
-- | Handles the "between" - transitions, syncing DOM to data, delays.
module Component.CodeExplorerV2.Orchestration where

import Prelude

import Component.CodeExplorerV2.Forces (allForces)
import Component.CodeExplorerV2.Scenes.ForceGraph as ForceGraph
import Component.CodeExplorerV2.Scenes.Orbit as Orbit
import Component.CodeExplorerV2.Scenes.TreeReveal as TreeReveal
import Component.CodeExplorerV2.Scenes.Types (Scene(..), SceneConfig)
import D3.Viz.Spago.Draw.Attributes (graphSceneAttributes, svgAttrs)
import D3.Viz.Spago.Files (LinkType(..), SpagoLink, SpagoLinkData, SpagoNodeRow, D3_Radius)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isUsedModule, treeDepthMultiplier)
import D3.Viz.Spago.Render (spagoRenderCallbacks)
import Data.Array (filter, length) as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Number (cos, sin)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import PSD3.Data.Node (D3_FocusXY)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3v2.Attribute.Types (d, fill, opacity, stroke, strokeWidth, transform)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (class SelectionM, append, appendChild, clear, on, joinDataWithKey, select, selectAllWithData)
import PSD3v2.Capabilities.Simulation (class SimulationM2, init, setForces, start, stop)
import PSD3v2.Capabilities.Transition (class TransitionM, withTransitionStaggered)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import PSD3v2.Selection.Operations (syncDOMToData)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBoundOwns)
import PSD3v2.Simulation.Update (DeclarativeUpdateConfig, genericUpdateSimulation, selectSimulationGroups, setupSimulationGroups)
import PSD3v2.Transition.Types (transitionWith)
import Utility (getWindowWidthHeight)
import Web.DOM.Element (Element)

-- | Type alias for node row
type NodeRow = SpagoNodeRow (D3_FocusXY (D3_Radius ()))

-- | Get config for a scene
configFor :: Scene -> SceneConfig
configFor Orbit = Orbit.config
configFor TreeReveal = TreeReveal.config
configFor ForceGraph = ForceGraph.config

-- | Initialize the visualization (first render)
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  String ->
  m Unit
initialize model selector = do
  log "CodeExplorerV2: Initializing in Orbit scene"

  -- Get viewport dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Create SVG structure
  root <- select selector
  svg <- appendChild SVG (svgAttrs w h) root
  inner <- appendChild Group [] svg
  _ <- on (Drag defaultDrag) inner
  _ <- on (Zoom (defaultZoom (ScaleExtent 0.1 4.0) "g")) svg

  -- Create simulation container groups
  groups <- setupSimulationGroups inner

  -- Initialize simulation with all forces
  _ <- init
    { nodes: model.nodes
    , links: model.links
    , forces: allForces
    , activeForces: Set.fromFoldable ["collision", "clusterX_M", "clusterY_M"]
    , config:
        { alpha: 1.0
        , alphaTarget: 0.0
        , alphaMin: 0.001
        , alphaDecay: 0.0228
        , velocityDecay: 0.4
        }
    , keyFn: _.id
    , ticks: Map.empty
    }

  -- Apply Orbit scene config
  let orbitConfig = configFor Orbit
  let updateConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      updateConfig =
        { allNodes: model.nodes
        , allLinks: []
        , nodeFilter: orbitConfig.nodeFilter
        , linkFilter: orbitConfig.linkFilter
        , nodeInitializers: orbitConfig.nodeInitializers
        , activeForces: Nothing  -- Will use setForces
        , config: Nothing
        }

  setForces orbitConfig.forces

  genericUpdateSimulation
    groups
    Group
    Path
    updateConfig
    _.id
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  start
  log "CodeExplorerV2: Orbit scene initialized"

-- | Transition to tree reveal (staggered animation)
transitionToTreeReveal :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  TransitionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  m Unit
transitionToTreeReveal model = do
  log "CodeExplorerV2: Starting staggered tree reveal"

  -- Stop simulation
  stop

  -- Filter to tree nodes and links
  let treeNodes = Array.filter isUsedModule model.nodes
      treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) model.links
      nodeMap = Map.fromFoldable $ map (\n -> Tuple n.id n) model.nodes
  log $ "Tree nodes to animate: " <> show (Array.length treeNodes)
  log $ "Tree links to add: " <> show (Array.length treeLinks)

  -- Re-select groups and join to get SBoundOwns selection
  groups <- selectSimulationGroups
  JoinResult nodeJoin <- joinDataWithKey treeNodes _.id "g" groups.nodes

  -- Transition config
  let transConfig = transitionWith
        { duration: Milliseconds 2400.0
        , delay: Nothing
        , easing: Nothing
        }

  -- Apply staggered transition to groups (position)
  withTransitionStaggered transConfig (TreeReveal.staggerByDepth 900.0) nodeJoin.update
    [ transform TreeReveal.nodeToTreeTransform
    ]

  -- Also transition circles (fill) and text (opacity)
  circlesSel <- selectAllWithData "circle" nodeJoin.update
  withTransitionStaggered transConfig (TreeReveal.staggerByDepth 900.0) circlesSel
    [ fill TreeReveal.depthToColor
    ]

  textSel <- selectAllWithData "text" nodeJoin.update
  withTransitionStaggered transConfig (TreeReveal.staggerByDepth 900.0) textSel
    [ opacity 0.0
    ]

  -- Add tree links with staggered fade-in
  JoinResult linkJoin <- joinDataWithKey treeLinks (\l -> Tuple l.source l.target) "path" groups.links

  linkElements <- append Path
    [ d (radialLinkPath nodeMap)
    , stroke (\(_ :: SpagoLink) -> "#888")
    , strokeWidth (\(_ :: SpagoLink) -> 1.0)
    , fill (\(_ :: SpagoLink) -> "none")
    , opacity (\(_ :: SpagoLink) -> 0.0)
    ]
    linkJoin.enter

  withTransitionStaggered transConfig (staggerLinkByTargetDepth 900.0 nodeMap) linkElements
    [ opacity (\(_ :: SpagoLink) -> 0.3)
    ]

  log "CodeExplorerV2: Staggered tree reveal started"

-- | Transition to force graph
transitionToForceGraph :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  m Unit
transitionToForceGraph model = do
  log "CodeExplorerV2: Activating force graph"

  -- Get scene config
  let forceGraphConfig = configFor ForceGraph

  -- Sync DOM positions to data if configured
  traverse_ syncDOMToData forceGraphConfig.domSync

  -- Clear existing tree links
  clear ".links"

  -- Set forces for ForceGraph scene
  setForces forceGraphConfig.forces

  groups <- selectSimulationGroups

  -- Use graph links
  let graphLinks = Array.filter (\l -> l.linktype == M2M_Graph) model.links
  log $ "Graph links: " <> show (Array.length graphLinks)

  let updateConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      updateConfig =
        { allNodes: model.nodes
        , allLinks: graphLinks
        , nodeFilter: forceGraphConfig.nodeFilter
        , linkFilter: forceGraphConfig.linkFilter
        , nodeInitializers: forceGraphConfig.nodeInitializers
        , activeForces: Nothing  -- Already set via setForces
        , config: Nothing
        }

  genericUpdateSimulation
    groups
    Group
    Path
    updateConfig
    _.id
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  log "CodeExplorerV2: Force graph configured (call start after delay)"

-- | Start the simulation
startSimulation :: forall m.
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  m Unit
startSimulation = start

-- | Helper: Calculate SVG path for a radial tree link
radialLinkPath :: Map.Map Int SpagoSimNode -> SpagoLink -> String
radialLinkPath nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      let srcXY = fromMaybe { x: 0.0, y: 0.0 } $ toMaybe sourceNode.treeXY
          srcAngle = srcXY.x
          srcRadius = srcXY.y * treeDepthMultiplier
          tgtXY = fromMaybe { x: 0.0, y: 0.0 } $ toMaybe targetNode.treeXY
          tgtAngle = tgtXY.x
          tgtRadius = tgtXY.y * treeDepthMultiplier
          sx = srcRadius * cos srcAngle
          sy = srcRadius * sin srcAngle
          tx = tgtRadius * cos tgtAngle
          ty = tgtRadius * sin tgtAngle
          cr = (srcRadius + tgtRadius) / 2.0
          cx1 = cr * cos srcAngle
          cy1 = cr * sin srcAngle
          cx2 = cr * cos tgtAngle
          cy2 = cr * sin tgtAngle
      in "M" <> show sx <> "," <> show sy <>
         "C" <> show cx1 <> "," <> show cy1 <>
         " " <> show cx2 <> "," <> show cy2 <>
         " " <> show tx <> "," <> show ty
    _, _ -> ""

-- | Helper: Get target node depth for staggering links
linkTargetDepth :: Map.Map Int SpagoSimNode -> SpagoLink -> Int
linkTargetDepth nodeMap link =
  case Map.lookup link.target nodeMap of
    Just targetNode -> fromMaybe 0 $ toMaybe targetNode.treeDepth
    Nothing -> 0

-- | Helper: Stagger delay for links based on target depth
staggerLinkByTargetDepth :: Number -> Map.Map Int SpagoSimNode -> SpagoLink -> Int -> Milliseconds
staggerLinkByTargetDepth msPerDepth nodeMap link _ =
  Milliseconds (toNumber (linkTargetDepth nodeMap link) * msPerDepth)
