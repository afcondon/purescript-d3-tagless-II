-- | Orchestration.purs - Scene transitions and timing
-- |
-- | Handles the "between" - transitions, syncing DOM to data, delays.
module Component.CodeExplorerV2.Orchestration where

import Prelude

import Component.CodeExplorerV2.BubblePackData (PackedModule, RenderCircle, getModuleCircles, getPackedRadius)
import Component.CodeExplorerV2.Forces (allForces)
import Component.CodeExplorerV2.InitPositions (initPositionsFromGridXY_)
import Component.CodeExplorerV2.Scenes.BubblePack as BubblePack
import Component.CodeExplorerV2.Scenes.ForceGraph as ForceGraph
import Component.CodeExplorerV2.Scenes.ForceGraphV2 as ForceGraphV2
import Component.CodeExplorerV2.Scenes.Orbit as Orbit
import Component.CodeExplorerV2.Scenes.Tree as Tree
import Component.CodeExplorerV2.Scenes.TreeReveal as TreeReveal
import Component.CodeExplorerV2.Scenes.Types (Scene(..), SceneConfig)
import Effect (Effect)
import PSD3.Config.Apply (applySceneConfig)
import PSD3.Config.Scene as CFG
import D3.Viz.Spago.Draw.Attributes (graphSceneAttributes, svgAttrs)
import D3.Viz.Spago.Files (LinkType(..), SpagoLink, SpagoLinkData, SpagoNodeRow, D3_Radius)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isUsedModule, treeDepthMultiplier)
import D3.Viz.Spago.Render (spagoRenderCallbacks)
import Data.Array (filter, length) as Array
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Number (cos, sin)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import PSD3.Data.Node (D3_FocusXY)
import PSD3.Internal.FFI (keyIsID_, startSimulation_)
import PSD3.Internal.Simulation.Types (Force(..))
import Data.String (joinWith)
import PSD3v2.Attribute.Types (cx, cy, d, fill, opacity, radius, stroke, strokeWidth, transform)
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

-- | Get config for a scene (OLD SYSTEM)
configFor :: Scene -> SceneConfig
configFor Orbit = Orbit.config
configFor TreeReveal = TreeReveal.config
configFor Tree = Tree.config
configFor ForceGraph = ForceGraph.config
configFor BubblePack = BubblePack.config

-- | Get NEW config for a scene (immutable force configuration system)
configForV2 :: Scene -> CFG.SceneConfig
configForV2 Orbit = Orbit.sceneConfig
configForV2 TreeReveal = TreeReveal.sceneConfig
configForV2 Tree = Tree.sceneConfig
configForV2 ForceGraph = ForceGraphV2.sceneConfig  -- Using V2 module which has the new config
configForV2 BubblePack = BubblePack.sceneConfig

-- | FFI: Get the simulation object from window for direct access
foreign import getSimulationFromWindow_ :: forall a. Effect a

-- | FFI: Get nodes from the simulation (returns the actual node objects D3 is using)
foreign import getNodesFromSimulation_ :: forall a node. a -> Effect (Array node)

-- | Extract force name from Force
forceName :: forall d. Force d -> String
forceName (Force f) = f.name

-- | Log active forces for a scene
logActiveForces :: forall m. MonadEffect m => String -> Array (Force SpagoSimNode) -> m Unit
logActiveForces sceneName forces = do
  let names = map forceName forces
  log $ "Scene " <> sceneName <> " active forces: [" <> joinWith ", " names <> "]"

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

  -- Initialize simulation with all forces (but no nodes yet - they'll be added by genericUpdateSimulation with initializers applied)
  _ <- init
    { nodes: []  -- Empty - nodes will be added by genericUpdateSimulation with nodeInitializers applied
    , links: []  -- Empty - links will be added by scene config
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
        { allNodes: model.nodes  -- All nodes now that initialization order is fixed
        , allLinks: []
        , nodeFilter: orbitConfig.nodeFilter
        , linkFilter: orbitConfig.linkFilter
        , nodeInitializers: orbitConfig.nodeInitializers
        , activeForces: Nothing  -- Will use setForces
        , config: Nothing
        }

  -- OLD SYSTEM (disabled - using new config system only)
  -- setForces orbitConfig.forces
  -- logActiveForces "Orbit" orbitConfig.forces

  -- Get simulation first
  simulation <- liftEffect getSimulationFromWindow_

  -- Apply V2 config BEFORE starting simulation to avoid force conflicts
  liftEffect $ log "[V2] Applying Orbit scene config BEFORE genericUpdateSimulation..."
  liftEffect $ applySceneConfig (configForV2 Orbit) simulation
  liftEffect $ log "[V2] Orbit scene config applied (forces ready)"

  -- Now run genericUpdateSimulation which will start the simulation
  -- This applies nodeInitializers which set up gridXY and pin positions
  genericUpdateSimulation
    groups
    Group
    Path
    updateConfig
    _.id
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  liftEffect $ log "CodeExplorerV2: Orbit scene initialized with V2 forces"

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

  -- NEW: Apply V2 config (empty scene, alpha=0)
  simulation <- liftEffect getSimulationFromWindow_
  liftEffect $ log "[V2] Applying TreeReveal scene config (simulation stopped)..."
  liftEffect $ applySceneConfig (configForV2 TreeReveal) simulation
  liftEffect $ log "[V2] TreeReveal scene config applied (alpha=0, no restart needed)"

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

  -- Transition text (opacity) - but keep colors as-is (package colors)
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

-- | Transition to tree (after reveal completes)
transitionToTree :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  m Unit
transitionToTree model = do
  log "CodeExplorerV2: Transitioning to Tree scene"

  -- Get scene config
  let treeConfig = configFor Tree

  -- Sync DOM positions to data before applying initializers
  traverse_ syncDOMToData treeConfig.domSync

  -- OLD SYSTEM (disabled - using new config system only)
  -- setForces treeConfig.forces
  -- logActiveForces "Tree" treeConfig.forces

  groups <- selectSimulationGroups

  -- Use tree links
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) model.links
  log $ "Tree links: " <> show (Array.length treeLinks)

  let updateConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      updateConfig =
        { allNodes: model.nodes
        , allLinks: treeLinks
        , nodeFilter: treeConfig.nodeFilter
        , linkFilter: treeConfig.linkFilter
        , nodeInitializers: treeConfig.nodeInitializers  -- Will clear gridXY and pin at tree
        , activeForces: Nothing  -- Already set via setForces
        , config: Nothing
        }

  -- IMPORTANT: Run genericUpdateSimulation FIRST to initialize node positions
  genericUpdateSimulation
    groups
    Group
    Path
    updateConfig
    _.id
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  -- Now apply V2 immutable config system (nodes have positions now)
  simulation <- liftEffect getSimulationFromWindow_
  liftEffect $ log "[V2] Applying Tree scene config..."
  liftEffect $ applySceneConfig (configForV2 Tree) simulation
  liftEffect $ let _ = startSimulation_ simulation in pure unit
  liftEffect $ log "[V2] Tree scene config applied and simulation restarted"

  log "CodeExplorerV2: Tree scene configured and started"

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

  -- OLD SYSTEM (disabled - using new config system only)
  -- setForces forceGraphConfig.forces
  -- logActiveForces "ForceGraph" forceGraphConfig.forces

  groups <- selectSimulationGroups

  -- Use tree links (spanning tree, not all dependencies)
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) model.links
  log $ "Tree links for force layout: " <> show (Array.length treeLinks)

  -- IMPORTANT: Apply scene config BEFORE genericUpdateSimulation so link force exists
  -- when simulationSetLinks is called
  simulation <- liftEffect getSimulationFromWindow_
  liftEffect $ log "[V2] Applying ForceGraph scene config BEFORE genericUpdateSimulation..."
  liftEffect $ applySceneConfig (configForV2 ForceGraph) simulation
  liftEffect $ log "[V2] ForceGraph scene config applied (link force ready)"

  let updateConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      updateConfig =
        { allNodes: model.nodes
        , allLinks: treeLinks
        , nodeFilter: forceGraphConfig.nodeFilter
        , linkFilter: forceGraphConfig.linkFilter
        , nodeInitializers: forceGraphConfig.nodeInitializers
        , activeForces: Nothing  -- Already set via applySceneConfig
        , config: Nothing
        }

  -- Now run genericUpdateSimulation which will set links (force is ready)
  genericUpdateSimulation
    groups
    Group
    Path
    updateConfig
    _.id
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  -- Restart simulation
  liftEffect $ let _ = startSimulation_ simulation in pure unit
  liftEffect $ log "[V2] ForceGraph simulation restarted"

  log "CodeExplorerV2: Force graph configured"

-- | Transition to bubble pack (my-project modules with packed circles)
transitionToBubblePack :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  m Unit
transitionToBubblePack model = do
  log "CodeExplorerV2: Transitioning to BubblePack"

  -- Get scene config
  let bubblePackConfig = configFor BubblePack

  -- Sync DOM positions to data if configured
  traverse_ syncDOMToData bubblePackConfig.domSync

  -- Clear existing links
  clear ".links"

  -- OLD SYSTEM (disabled - using new config system only)
  -- setForces bubblePackConfig.forces
  -- logActiveForces "BubblePack" bubblePackConfig.forces

  groups <- selectSimulationGroups

  -- Filter to tree links between my-project modules
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) model.links
  log $ "BubblePack links: " <> show (Array.length treeLinks)

  -- IMPORTANT: Apply scene config BEFORE genericUpdateSimulation so link force exists
  simulation <- liftEffect getSimulationFromWindow_
  liftEffect $ log "[V2] Applying BubblePack scene config BEFORE genericUpdateSimulation..."
  liftEffect $ applySceneConfig (configForV2 BubblePack) simulation
  liftEffect $ log "[V2] BubblePack scene config applied (link force ready)"

  let updateConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      updateConfig =
        { allNodes: model.nodes
        , allLinks: treeLinks
        , nodeFilter: bubblePackConfig.nodeFilter
        , linkFilter: bubblePackConfig.linkFilter
        , nodeInitializers: bubblePackConfig.nodeInitializers
        , activeForces: Nothing  -- Already set via applySceneConfig
        , config: Nothing
        }

  -- Now run genericUpdateSimulation which will set links (force is ready)
  genericUpdateSimulation
    groups
    Group
    Path
    updateConfig
    _.id
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  -- Restart simulation
  liftEffect $ let _ = startSimulation_ simulation in pure unit
  liftEffect $ log "[V2] BubblePack simulation restarted"

  log "CodeExplorerV2: BubblePack configured"

-- | Transition to hybrid bubble pack (all nodes visible, only my-project packed)
transitionToBubblePackHybrid :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  m Unit
transitionToBubblePackHybrid model = do
  log "CodeExplorerV2: Transitioning to Hybrid BubblePack"

  -- Use hybrid config (keeps all nodes, packs only my-project)
  let bubblePackConfig = BubblePack.configHybrid

  -- Sync DOM positions to data if configured
  traverse_ syncDOMToData bubblePackConfig.domSync

  -- Clear existing links
  clear ".links"

  -- OLD SYSTEM (disabled - using new config system only)
  -- setForces bubblePackConfig.forces
  -- logActiveForces "BubblePackHybrid" bubblePackConfig.forces

  groups <- selectSimulationGroups

  -- Use all tree links (not just my-project)
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) model.links
  log $ "BubblePack Hybrid links: " <> show (Array.length treeLinks)

  -- IMPORTANT: Apply scene config BEFORE genericUpdateSimulation so link force exists
  simulation <- liftEffect getSimulationFromWindow_
  liftEffect $ log "[V2] Applying BubblePack (Hybrid) scene config BEFORE genericUpdateSimulation..."
  liftEffect $ applySceneConfig (configForV2 BubblePack) simulation
  liftEffect $ log "[V2] BubblePack (Hybrid) scene config applied (link force ready)"

  let updateConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      updateConfig =
        { allNodes: model.nodes
        , allLinks: treeLinks
        , nodeFilter: bubblePackConfig.nodeFilter  -- Keep all nodes (\_ -> true)
        , linkFilter: bubblePackConfig.linkFilter
        , nodeInitializers: bubblePackConfig.nodeInitializers
        , activeForces: Nothing  -- Already set via applySceneConfig
        , config: Nothing
        }

  -- Now run genericUpdateSimulation which will set links (force is ready)
  genericUpdateSimulation
    groups
    Group
    Path
    updateConfig
    _.id
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  -- Restart simulation
  liftEffect $ let _ = startSimulation_ simulation in pure unit
  liftEffect $ log "[V2] BubblePack (Hybrid) simulation restarted"

  log "CodeExplorerV2: BubblePack Hybrid configured"

-- | Render bubble pack circles inside each module node
-- | Takes pre-loaded pack data and replaces simple circles with nested packs
renderBubblePackCircles :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  Array PackedModule ->
  m Unit
renderBubblePackCircles packedModules = do
  log $ "Rendering bubble pack circles for " <> show (Array.length packedModules) <> " modules"

  -- Build a map from module name to packed circles
  let packMap = Map.fromFoldable $ map (\pm -> Tuple pm.name (getModuleCircles pm)) packedModules

  -- For each module, select its node group and replace the circle with nested circles
  for_ packedModules \pm -> do
    let circles = getModuleCircles pm
        outerRadius = getPackedRadius pm

    log $ "  " <> pm.name <> ": " <> show (Array.length circles) <> " circles, r=" <> show outerRadius

    -- Select the node group by module name
    -- D3 stores data in __data__, we need to select by the node's name property
    -- For now, use selectAll and filter - this is a simplified approach
    nodesGroup <- select "g.nodes"

    -- Join circle data to create circles for this module
    -- We need a unique key for each circle - use module name + index
    let circlesWithKey = Array.mapWithIndex (\i c -> { circle: c, key: pm.name <> "-" <> show i }) circles

    JoinResult circleJoin <- joinDataWithKey circlesWithKey _.key "circle.pack-circle" nodesGroup

    -- Append new circles
    let circleData :: { circle :: RenderCircle, key :: String } -> RenderCircle
        circleData d = d.circle

    _ <- append Circle
      [ cx (\d -> (circleData d).x)
      , cy (\d -> (circleData d).y)
      , radius (\d -> (circleData d).r)
      , fill (\d -> depthToFill (circleData d).depth)
      , stroke (\d -> depthToStroke (circleData d).depth)
      , strokeWidth (\d -> if (circleData d).depth == 0 then 1.5 else 0.5)
      , opacity (\d -> if (circleData d).depth == 0 then 0.3 else 0.7)
      ]
      circleJoin.enter

    pure unit

  log "CodeExplorerV2: Bubble pack circles rendered"

  where
    -- Colors by depth level
    depthToFill :: Int -> String
    depthToFill depth = case depth of
      0 -> "#e0e0e0"  -- Module (outer) - light gray
      1 -> "#4a90d9"  -- Category - blue
      _ -> "#2d5a87"  -- Declaration - dark blue

    depthToStroke :: Int -> String
    depthToStroke depth = case depth of
      0 -> "#999"     -- Module outline
      1 -> "#3a7bc8"  -- Category
      _ -> "#1d4a77"  -- Declaration

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
