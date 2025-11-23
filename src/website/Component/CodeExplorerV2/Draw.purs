-- | Draw module for CodeExplorerV2
-- |
-- | Handles D3 visualization for tree reveal animation.
-- | Uses imperative choreography driven by Halogen timers.
module Component.CodeExplorerV2.Draw where

import Prelude

import Component.CodeExplorerV2.Types (Scene(..))
import D3.Viz.Spago.Draw.Attributes (graphSceneAttributes, svgAttrs)
import D3.Viz.Spago.Files (LinkType(..), SpagoLinkData, SpagoNodeRow, SpagoLink, D3_Radius)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, SpagoSwizzledLink, isModule, isPackage, isUsedModule, moduleNodesToContainerXY, modulesToCircle, nodesToCircle, nodesToOrbitStart, nodesToRadialTree, packagesToCircle, pinMainAtXY, treeDepthMultiplier, unpinAllNodes)
import D3.Viz.Spago.Render (spagoRenderCallbacks)
import Data.Array (filter, length) as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Number (cos, sin)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import PSD3.Data.Node (D3_FocusXY)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3v2.Attribute.Types (d, fill, opacity, stroke, strokeWidth, transform)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (class SelectionM, append, appendChild, clear, select, on, joinDataWithKey, selectAllWithData)
import PSD3v2.Capabilities.Simulation (class SimulationM2, init, start, stop)
import PSD3v2.Capabilities.Transition (class TransitionM, withTransitionStaggered)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, JoinResult(..))
import PSD3v2.Simulation.Update (DeclarativeUpdateConfig, genericUpdateSimulation, setupSimulationGroups, selectSimulationGroups)
import PSD3v2.Transition.Types (transitionWith)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)
import Web.DOM.Element (Element)

-- | Type alias for node row (matching Spago)
type NodeRow = SpagoNodeRow (D3_FocusXY (D3_Radius ()))

-- | Stagger delay based on tree depth
-- |
-- | Each depth layer animates after the previous one.
-- | delay = depth * msPerDepth
staggerByTreeDepth :: Number -> SpagoSimNode -> Int -> Milliseconds
staggerByTreeDepth msPerDepth node _ =
  let depth = fromMaybe 0 $ toMaybe node.treeDepth
  in Milliseconds (toNumber depth * msPerDepth)

-- | Calculate transform string for radial tree position
-- |
-- | Converts treeXY (angle, radius) to translate(x, y)
nodeToTreeTransform :: SpagoSimNode -> String
nodeToTreeTransform node =
  case toMaybe node.treeXY of
    Just treeXY ->
      let angle = treeXY.x
          radius = treeXY.y * treeDepthMultiplier
          x = radius * cos angle
          y = radius * sin angle
      in "translate(" <> show x <> "," <> show y <> ")"
    Nothing ->
      -- Fallback to current position
      "translate(" <> show node.x <> "," <> show node.y <> ")"

-- | Calculate fill color based on tree depth
-- |
-- | Uses HSL interpolation from warm (depth 0) to cool (max depth)
-- | Creates a rainbow effect across the tree layers
depthToColor :: SpagoSimNode -> String
depthToColor node =
  let depth = fromMaybe 0 $ toMaybe node.treeDepth
      -- Map depth to hue: 0 (red) -> 240 (blue) over ~20 levels
      -- Adjust hue range for pleasing colors
      hue = 30.0 + (toNumber depth * 10.0)  -- Orange to purple
      saturation = 70.0
      lightness = 50.0
  in "hsl(" <> show hue <> ", " <> show saturation <> "%, " <> show lightness <> "%)"

-- | Calculate SVG path for a radial tree link
-- |
-- | Creates a curved bezier path from source to target using their treeXY positions.
-- | Takes a node lookup map since links are not swizzled (source/target are IDs).
-- | Uses the same coordinate system as nodeToTreeTransform (raw radians, no rotation).
radialLinkPath :: Map.Map Int SpagoSimNode -> SpagoLink -> String
radialLinkPath nodeMap link =
  case Map.lookup link.source nodeMap, Map.lookup link.target nodeMap of
    Just sourceNode, Just targetNode ->
      let -- Get source position from treeXY (x = angle in radians, y = depth)
          srcXY = fromMaybe { x: 0.0, y: 0.0 } $ toMaybe sourceNode.treeXY
          srcAngle = srcXY.x
          srcRadius = srcXY.y * treeDepthMultiplier

          -- Get target position from treeXY
          tgtXY = fromMaybe { x: 0.0, y: 0.0 } $ toMaybe targetNode.treeXY
          tgtAngle = tgtXY.x
          tgtRadius = tgtXY.y * treeDepthMultiplier

          -- Convert to Cartesian (matches nodeToTreeTransform)
          sx = srcRadius * cos srcAngle
          sy = srcRadius * sin srcAngle
          tx = tgtRadius * cos tgtAngle
          ty = tgtRadius * sin tgtAngle

          -- Control points for smooth bezier curve
          -- Use midpoint radius for control points
          cr = (srcRadius + tgtRadius) / 2.0
          cx1 = cr * cos srcAngle
          cy1 = cr * sin srcAngle
          cx2 = cr * cos tgtAngle
          cy2 = cr * sin tgtAngle

      -- Bezier curve from source to target
      in "M" <> show sx <> "," <> show sy <>
         "C" <> show cx1 <> "," <> show cy1 <>
         " " <> show cx2 <> "," <> show cy2 <>
         " " <> show tx <> "," <> show ty
    _, _ -> "" -- Skip links with missing nodes

-- | Get target node depth for staggering link transitions
-- |
-- | Links should appear when their target node appears
linkTargetDepth :: Map.Map Int SpagoSimNode -> SpagoLink -> Int
linkTargetDepth nodeMap link =
  case Map.lookup link.target nodeMap of
    Just targetNode -> fromMaybe 0 $ toMaybe targetNode.treeDepth
    Nothing -> 0

-- | Stagger delay for links based on target node depth
staggerLinkByTargetDepth :: Number -> Map.Map Int SpagoSimNode -> SpagoLink -> Int -> Milliseconds
staggerLinkByTargetDepth msPerDepth nodeMap link _ =
  Milliseconds (toNumber (linkTargetDepth nodeMap link) * msPerDepth)

-- | Staggered tree reveal animation
-- |
-- | Stops simulation and animates nodes to their tree positions with
-- | staggered delays based on depth. Nodes at depth 1 animate first,
-- | then depth 2, etc.
staggeredTreeReveal :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  TransitionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  Array SpagoLink ->
  m Unit
staggeredTreeReveal model _unswizzledLinks = do
  log "CodeExplorerV2.Draw: Starting staggered tree reveal"

  -- Stop simulation
  stop

  -- Filter to tree nodes and links (from model)
  -- Build node map for looking up nodes by ID (since links are not swizzled)
  let treeNodes = Array.filter isUsedModule model.nodes
      treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) model.links
      nodeMap = Map.fromFoldable $ map (\n -> Tuple n.id n) model.nodes
  log $ "Tree nodes to animate: " <> show (Array.length treeNodes)
  log $ "Tree links to add: " <> show (Array.length treeLinks)

  -- Re-select groups and join to get SBoundOwns selection
  -- Must use same key function as genericUpdateSimulation to match existing elements
  groups <- selectSimulationGroups
  JoinResult nodeJoin <- joinDataWithKey treeNodes (\n -> n.id) "g" groups.nodes

  -- The update selection has our tree nodes with data bound
  -- (enter and exit should be empty since we're re-binding same data)
  let config = transitionWith
        { duration: Milliseconds 2400.0  -- Slower, more dramatic
        , delay: Nothing  -- Ignored when using staggered
        , easing: Nothing
        }

  -- Apply staggered transition to groups (position)
  -- 900ms delay between depth layers for ~18 second total with 20 depth levels
  withTransitionStaggered config (staggerByTreeDepth 900.0) nodeJoin.update
    [ transform nodeToTreeTransform
    ]

  -- Also transition the circles (fill) and text (opacity) inside the groups
  -- Use selectAllWithData to get child elements with their inherited data
  circlesSel <- selectAllWithData "circle" nodeJoin.update
  withTransitionStaggered config (staggerByTreeDepth 900.0) circlesSel
    [ fill depthToColor
    ]

  textSel <- selectAllWithData "text" nodeJoin.update
  withTransitionStaggered config (staggerByTreeDepth 900.0) textSel
    [ opacity 0.0  -- Hide labels during reveal
    ]

  -- Add tree links
  -- Join tree links data and create path elements
  -- Use source-target as unique key since SpagoLink doesn't have an id field
  JoinResult linkJoin <- joinDataWithKey treeLinks (\l -> Tuple l.source l.target) "path" groups.links

  -- Create new link paths with enter selection (start invisible)
  -- Use nodeMap to look up source/target nodes by ID
  linkElements <- append Path
    [ d (radialLinkPath nodeMap)
    , stroke (\(_ :: SpagoLink) -> "#888")  -- Mid-gray
    , strokeWidth (\(_ :: SpagoLink) -> 1.0)
    , fill (\(_ :: SpagoLink) -> "none")
    , opacity (\(_ :: SpagoLink) -> 0.0)  -- Start invisible
    ]
    linkJoin.enter

  -- Transition links to fade in, staggered by target node depth
  -- Links appear when their target nodes animate into place
  withTransitionStaggered config (staggerLinkByTargetDepth 900.0 nodeMap) linkElements
    [ opacity (\(_ :: SpagoLink) -> 0.3)  -- Fade to subtle visibility
    ]

  log "CodeExplorerV2.Draw: Staggered tree reveal started"

-- | Initialize the visualization in Orbit scene
-- |
-- | Sets up SVG structure and positions nodes in concentric rings:
-- | - Main module pinned at center
-- | - Modules in inner ring
-- | - Packages in outer ring
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  Array (Force SpagoSimNode) ->
  Set Label ->
  SpagoModel ->
  Array SpagoLink ->
  String ->
  Scene ->
  m Unit
initialize forcesArray activeForces model unswizzledLinks selector _initialScene = do
  log "CodeExplorerV2.Draw: Initializing in Orbit scene"

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

  -- Initialize simulation with forces
  _ <- init
    { nodes: model.nodes
    , links: model.links
    , forces: forcesArray
    , activeForces: activeForces
    , config:
        { alpha: 1.0
        , alphaTarget: 0.0
        , alphaMin: 0.001
        , alphaDecay: 0.0228
        , velocityDecay: 0.4
        }
    , keyFn: (\n -> n.id)
    , ticks: Map.empty
    }

  let onlyMainAndPackages n = ((n.name /= "my-project") && (isPackage n)) || (n.name == "main")
      excludeMyProject n = n.name /= "my-project"
      onlyMain n = n.name == "PSD3.Main"

  -- Configure for Orbit scene: all nodes visible (except my-project), no links
  -- nodesToOrbitStart gives good starting positions, then radial forces pull into orbits
  let orbitConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      orbitConfig =
        { allNodes: model.nodes
        , allLinks: []  -- No links in orbit view
        , nodeFilter: \_ -> true
        , linkFilter: Nothing
        , nodeInitializers: [ (nodesToCircle isPackage 1600.0), pinMainAtXY 0.0 0.0 ]  -- Packages spread out beyond tree extent
        , activeForces: Just $ Set.fromFoldable ["collision", "clusterX_M", "clusterY_M"]
        , config: Nothing
        }

  genericUpdateSimulation
    groups
    Group
    Path
    orbitConfig
    (\n -> n.id)
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  start
  log "CodeExplorerV2.Draw: Orbit scene initialized"

-- | Phase 2: Transition to tree reveal
-- |
-- | - Stops simulation
-- | - Transitions nodes to their radial tree positions
-- | - Adds bezier tree links
transitionToTreeReveal :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  Array SpagoLink ->
  m Unit
transitionToTreeReveal model unswizzledLinks = do
  log "CodeExplorerV2.Draw: Transitioning to tree reveal"

  -- Stop simulation
  stop

  -- Re-select groups
  groups <- selectSimulationGroups

  -- Only tree modules and tree links
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) unswizzledLinks

  log $ "Tree links: " <> show (Array.length treeLinks)

  let treeConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      treeConfig =
        { allNodes: model.nodes
        , allLinks: treeLinks
        , nodeFilter: isUsedModule  -- Only modules in tree
        , linkFilter: Just (\l -> l.linktype == M2M_Tree)
        , nodeInitializers: [ nodesToRadialTree ]  -- Position and pin at tree positions
        , activeForces: Nothing  -- No forces during transition
        , config: Nothing
        }

  -- This will animate nodes to tree positions and add links
  genericUpdateSimulation
    groups
    Group
    Path
    treeConfig
    (\n -> n.id)
    keyIsID_
    graphSceneAttributes  -- TODO: Use bezier link style
    spagoRenderCallbacks

  log "CodeExplorerV2.Draw: Tree reveal transition started"

-- | Phase 3: Activate force-driven graph
-- |
-- | - Removes tree links, adds graph links
-- | - Unpins nodes
-- | - Sets up simulation forces (caller should start after delay)
activateForceTree :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  Array SpagoLink ->
  m Unit
activateForceTree model unswizzledLinks = do
  log "CodeExplorerV2.Draw: Activating force graph"

  -- Clear existing tree links (they were added with different key function)
  clear ".links"

  groups <- selectSimulationGroups

  -- Use graph links instead of tree links for simulation
  let graphLinks = Array.filter (\l -> l.linktype == M2M_Graph) unswizzledLinks

  log $ "Graph links: " <> show (Array.length graphLinks)

  -- Unpin nodes and use only graph simulation forces (no cluster forces)
  let forceGraphConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
      forceGraphConfig =
        { allNodes: model.nodes
        , allLinks: graphLinks
        , nodeFilter: isUsedModule
        , linkFilter: Just (\l -> l.linktype == M2M_Graph)
        , nodeInitializers: [ unpinAllNodes ]  -- Remove fx/fy to let forces work
        , activeForces: Just $ Set.fromFoldable ["charge", "collision", "links", "center"]
        , config: Nothing
        }

  genericUpdateSimulation
    groups
    Group
    Path
    forceGraphConfig
    (\n -> n.id)
    keyIsID_
    graphSceneAttributes
    spagoRenderCallbacks

  log "CodeExplorerV2.Draw: Force graph configured (simulation not started yet)"

-- | Start the simulation
-- | Call this after a delay to let links appear first
startSimulation :: forall m.
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  m Unit
startSimulation = start

-- | Update scene (for manual scene switching)
updateScene :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  Array SpagoLink ->
  Scene ->
  Set Label ->
  m Unit
updateScene model links targetScene _activeForces = do
  log $ "CodeExplorerV2.Draw: Updating to " <> show targetScene

  case targetScene of
    Orbit -> do
      -- Reset to orbit view
      groups <- selectSimulationGroups
      let orbitConfig :: DeclarativeUpdateConfig NodeRow Int SpagoLinkData
          orbitConfig =
            { allNodes: model.nodes
            , allLinks: []
            , nodeFilter: \n -> n.name /= "my-project"
            , linkFilter: Nothing
            , nodeInitializers: [ nodesToOrbitStart ]
            , activeForces: Just $ Set.fromFoldable ["collision", "clusterX_M", "clusterY_M"]
            , config: Nothing
            }
      genericUpdateSimulation groups Group Path orbitConfig (\n -> n.id) keyIsID_ graphSceneAttributes spagoRenderCallbacks
      start

    TreeReveal ->
      transitionToTreeReveal model links

    ForceTree ->
      activateForceTree model links
