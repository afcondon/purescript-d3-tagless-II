-- | Force-Directed Graph View
-- |
-- | Shows modules in a force-directed layout with three animation phases:
-- | 1. Grow radial tree from center (nodes move from center to tree positions)
-- | 2. Morph bezier links to straight lines
-- | 3. Engage force simulation (unpin nodes, let forces settle)
-- |
-- | Architecture:
-- | - Uses tick-based animation like TreeView for phases 1-2
-- | - Phase 3 hands off to D3 force simulation
-- | - Cleanup properly stops simulation when switching views
module Viz.ForceGraph
  ( render
  , renderAnimated
  , resetToTreemap
  , cleanup
  , Config
  , SimulationHandle
  , AnimationState
  , ForcePhase(..)
  , initAnimation
  , tickAnimation
  , isAnimating
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Types (SimNode, SimLink, NodeType(..))
import Data.TreeLayout as TreeLayout
import DataViz.Layout.Hierarchy.TreeStyle as TreeStyle
import PSD3.ForceEngine as FE
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Links (swizzleLinksByIndex, filterLinksToSubset)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import Effect.Class (liftEffect)
import PSD3v2.Transform (transformCircles, transformPaths, removeElement)
import PSD3v2.Classify (classifyElements)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree as T
import PSD3v2.Behavior.Types (Behavior(..), DragConfig(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Behavior.FFI as BehaviorFFI
import PSD3v3.Integration (v3AttrFn, v3AttrFnStr, v3AttrStr)
import PSD3v3.Expr (str)
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Types
-- =============================================================================

-- | Rendering configuration
type Config =
  { containerSelector :: String
  , packageCount :: Int
  , rootId :: Int           -- Which node to use as tree root (for radial tree phase)
  }

-- | Handle to the simulation for cleanup
type SimulationHandle =
  { cleanup :: Effect Unit
  }

-- | The three phases of force view animation
data ForcePhase
  = TreeGrowth       -- Phase 1: Grow radial tree from center
  | LinkMorph        -- Phase 2: Morph bezier links to straight lines
  | ForceActive      -- Phase 3: Force simulation running

derive instance eqForcePhase :: Eq ForcePhase

instance showForcePhase :: Show ForcePhase where
  show TreeGrowth = "TreeGrowth"
  show LinkMorph = "LinkMorph"
  show ForceActive = "ForceActive"

-- | Animation state for force view
type AnimationState =
  { phase :: ForcePhase
  , progress :: Tick.Progress    -- 0.0 to 1.0 within current phase
  , isComplete :: Boolean
  , isFirstFrame :: Boolean
  , treeLayout :: Maybe TreeLayout.TreeLayoutResult
  }

-- | Swizzled link with source/target as full nodes
type SwizzledLink =
  { source :: SimNode
  , target :: SimNode
  , index :: Int
  }

-- | Link element for visualization
type LinkElement =
  { sourceTreeX :: Number
  , sourceTreeY :: Number
  , targetTreeX :: Number
  , targetTreeY :: Number
  , rootX :: Number
  , rootY :: Number
  }

-- =============================================================================
-- Constants
-- =============================================================================

simulationId :: String
simulationId = "code-explorer-force"

-- =============================================================================
-- Animation State Management
-- =============================================================================

-- | Initialize animation state
initAnimation :: AnimationState
initAnimation =
  { phase: TreeGrowth
  , progress: 0.0
  , isComplete: false
  , isFirstFrame: true
  , treeLayout: Nothing
  }

-- | Advance animation by one tick
-- | Returns updated state, potentially transitioning between phases
tickAnimation :: Tick.TickDelta -> AnimationState -> AnimationState
tickAnimation delta state =
  if state.isComplete then state
  else
    let newProgress = min 1.0 (state.progress + delta)
    in
      if newProgress >= 1.0 then
        -- Phase complete - advance to next phase
        case state.phase of
          TreeGrowth ->
            state
              { phase = LinkMorph
              , progress = 0.0
              , isFirstFrame = false
              }
          LinkMorph ->
            -- Transition to ForceActive but don't mark complete yet
            -- Need one more render call to actually start the simulation
            state
              { phase = ForceActive
              , progress = 0.0
              , isComplete = false  -- Will be set after simulation starts
              , isFirstFrame = true -- Signal that we need to start simulation
              }
          ForceActive ->
            -- After simulation is started, mark complete
            state { isComplete = true, isFirstFrame = false }
      else
        state
          { progress = newProgress
          , isFirstFrame = false
          }

-- | Check if animation is still running (phases 1 or 2)
isAnimating :: AnimationState -> Boolean
isAnimating state = not state.isComplete

-- =============================================================================
-- Rendering (Simple - jumps straight to force simulation)
-- =============================================================================

-- | Simple render that jumps straight to force simulation
-- | Use renderAnimated for the full phased animation
render :: Config -> Array SimNode -> Array SimLink -> Effect SimulationHandle
render config nodes links = do
  log $ "[ForceGraph] Simple render - " <> show (Array.length nodes) <> " nodes"

  -- Filter to modules only
  let moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes
  let moduleLinks = Array.filter (\l -> isModuleLink nodes l) links

  -- Swizzle links (use ById since link.source/target are node IDs, not array indices)
  let rawLinks = moduleLinks <#> \l -> { source: l.source, target: l.target }
  let swizzled = swizzleLinksByIndex _.id moduleNodes rawLinks \src tgt i _ ->
        { source: src, target: tgt, index: i }

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes moduleNodes sim
  Sim.setLinks rawLinks sim

  -- Add forces
  Sim.addForce (FE.ManyBody "charge" FE.defaultManyBody { strength = -100.0 }) sim
  Sim.addForce (FE.Collide "collide" FE.defaultCollide { radius = 15.0 }) sim
  Sim.addForce (FE.Center "center" FE.defaultCenter) sim
  Sim.addForce (FE.Link "links" FE.defaultLink { distance = 40.0 }) sim

  -- Register for drag
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- State ref
  stateRef <- Ref.new { nodes: moduleNodes, swizzled }

  -- Render DOM
  _ <- runD3v2M do
    container <- select config.containerSelector
    renderTree container (buildForceSvgTree config)

  -- Set up tick handler
  Sim.onTick (tickForce stateRef config) sim
  Sim.start sim

  pure
    { cleanup: do
        log "[ForceGraph] Cleaning up"
        Sim.stop sim
        BehaviorFFI.unregisterSimulation_ simulationId
    }

-- =============================================================================
-- Animated Rendering (Three Phases)
-- =============================================================================

-- | Render with phased animation
-- | Phase 1: Grow radial tree from center
-- | Phase 2: Morph bezier links to straight lines
-- | Phase 3: Engage force simulation (returns SimulationHandle)
renderAnimated :: Config
               -> AnimationState
               -> Array SimNode
               -> Array SimLink
               -> Effect { animState :: AnimationState, simHandle :: Maybe SimulationHandle }
renderAnimated config animState nodes links = do
  log $ "[ForceGraph] renderAnimated phase=" <> show animState.phase
      <> " progress=" <> show animState.progress

  -- Get or compute tree layout (same as TreeView, but with radial style)
  layout <- case animState.treeLayout of
    Just l -> pure l
    Nothing -> do
      log "[ForceGraph] Computing radial tree layout..."
      let result = TreeLayout.computeTreeLayout TreeStyle.radialTree config.rootId nodes links
      log $ "[ForceGraph] Layout computed: " <> show (Array.length result.treeNodes) <> " nodes"
      pure result

  let rootX = 0.0  -- Radial tree is centered at origin
  let rootY = 0.0

  -- First frame: set up DOM structure
  if animState.isFirstFrame then do
    log "[ForceGraph] First frame - adding tree links"
    addTreeLinks config layout rootX rootY
  else
    pure unit

  -- Handle current phase
  case animState.phase of
    TreeGrowth -> do
      -- Animate circles from center to radial tree positions
      updateTreeGrowthPositions config animState layout rootX rootY
      pure { animState: animState { treeLayout = Just layout }, simHandle: Nothing }

    LinkMorph -> do
      -- Keep nodes at tree positions, morph links from bezier to straight
      updateLinkMorphPositions config animState layout
      pure { animState: animState { treeLayout = Just layout }, simHandle: Nothing }

    ForceActive -> do
      -- Phase 3: Start force simulation
      log "[ForceGraph] Entering ForceActive phase - starting simulation"
      handle <- startForceSimulation config layout nodes links
      pure { animState: animState { treeLayout = Just layout }, simHandle: Just handle }

-- | Reset to treemap positions
resetToTreemap :: String -> Effect Unit
resetToTreemap containerSelector = do
  log "[ForceGraph] Resetting to treemap positions"
  removeElement (containerSelector <> " g.force-links-group")
  removeElement (containerSelector <> " svg.force-svg")
  transformCircles containerSelector \rect ->
    { cx: rect.x + rect.width / 2.0
    , cy: rect.y + rect.height / 2.0
    }

-- =============================================================================
-- Phase 1: Tree Growth Animation
-- =============================================================================

-- | Update positions during tree growth phase
updateTreeGrowthPositions :: Config -> AnimationState -> TreeLayout.TreeLayoutResult -> Number -> Number -> Effect Unit
updateTreeGrowthPositions config animState layout rootX rootY = do
  let easedProgress = Tick.easeOutCubic animState.progress
  let treeNodeMap = Map.fromFoldable $ layout.treeNodes <#> \tn -> Tuple tn.id tn

  -- Move circles from center to radial tree positions
  transformCircles config.containerSelector \rect ->
    case rect.simNode of
      Just sn ->
        case Map.lookup sn.id treeNodeMap of
          Just tn | tn.isInTree ->
            -- Interpolate from center to radial position
            { cx: Tick.lerp rootX tn.radialX easedProgress
            , cy: Tick.lerp rootY tn.radialY easedProgress
            }
          _ ->
            -- Not in tree - stay at treemap position
            { cx: rect.x + rect.width / 2.0
            , cy: rect.y + rect.height / 2.0
            }
      Nothing ->
        { cx: rect.x + rect.width / 2.0
        , cy: rect.y + rect.height / 2.0
        }

  -- Update bezier links (from root outward)
  transformPaths (config.containerSelector <> " g.force-links-group") \el ->
    let
      srcX = Tick.lerp rootX el.sourceTreeX easedProgress
      srcY = Tick.lerp rootY el.sourceTreeY easedProgress
      tgtX = Tick.lerp rootX el.targetTreeX easedProgress
      tgtY = Tick.lerp rootY el.targetTreeY easedProgress
    in
      -- Use radial bezier during tree growth
      TreeStyle.radialTree.linkPath srcX srcY tgtX tgtY

  -- Classify circles
  let inTreeIds = Set.fromFoldable $ Array.mapMaybe
        (\tn -> if tn.isInTree then Just tn.id else Nothing)
        layout.treeNodes

  classifyElements config.containerSelector "circle" \rect ->
    case rect.simNode of
      Just sn | Set.member sn.id inTreeIds -> "in-tree"
      _ -> "dimmed"

-- =============================================================================
-- Phase 2: Link Morphing
-- =============================================================================

-- | Update during link morph phase (bezier -> straight)
updateLinkMorphPositions :: Config -> AnimationState -> TreeLayout.TreeLayoutResult -> Effect Unit
updateLinkMorphPositions config animState _layout = do
  let easedProgress = Tick.easeInOutCubic animState.progress

  -- Nodes stay at their tree positions (no movement this phase)
  -- Links morph from bezier to straight line
  transformPaths (config.containerSelector <> " g.force-links-group") \el ->
    let
      -- Start: bezier path
      bezierPath = TreeStyle.radialTree.linkPath el.sourceTreeX el.sourceTreeY el.targetTreeX el.targetTreeY
      -- End: straight line
      straightPath = "M" <> show el.sourceTreeX <> "," <> show el.sourceTreeY
                  <> "L" <> show el.targetTreeX <> "," <> show el.targetTreeY
    in
      -- For now, just switch at 50% progress (true morphing would need path interpolation)
      if easedProgress < 0.5 then bezierPath else straightPath

-- =============================================================================
-- Phase 3: Force Simulation
-- =============================================================================

-- | Start force simulation after animation phases complete
startForceSimulation :: Config -> TreeLayout.TreeLayoutResult -> Array SimNode -> Array SimLink -> Effect SimulationHandle
startForceSimulation config layout nodes links = do
  log "[ForceGraph] Starting force simulation"

  let moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes

  -- Initialize node positions from tree layout
  let treeNodeMap = Map.fromFoldable $ layout.treeNodes <#> \tn -> Tuple tn.id tn
  let positionedNodes = moduleNodes <#> \n ->
        case Map.lookup n.id treeNodeMap of
          Just tn | tn.isInTree -> n { x = tn.radialX, y = tn.radialY }
          _ -> n

  -- IMPORTANT: Filter links to ONLY those where both endpoints are in positionedNodes
  -- This prevents "node not found" errors when D3's link force tries to swizzle
  let rawLinks = links <#> \l -> { source: l.source, target: l.target }
  let filteredLinks = filterLinksToSubset _.id positionedNodes rawLinks

  log $ "[ForceGraph] Positioned " <> show (Array.length positionedNodes) <> " nodes, "
      <> show (Array.length filteredLinks) <> " links (filtered from " <> show (Array.length rawLinks) <> ")"

  -- Create simulation with positioned nodes
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes positionedNodes sim
  Sim.setLinks filteredLinks sim

  -- Add forces (gentler than simple render since nodes are already positioned)
  Sim.addForce (FE.ManyBody "charge" FE.defaultManyBody { strength = -50.0 }) sim
  Sim.addForce (FE.Collide "collide" FE.defaultCollide { radius = 10.0 }) sim
  Sim.addForce (FE.Link "links" FE.defaultLink { distance = 30.0 }) sim
  -- No Center force - let nodes drift from tree positions

  -- Register for drag
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- Swizzle for tick rendering (use ById since link.source/target are node IDs)
  let swizzled = swizzleLinksByIndex _.id positionedNodes filteredLinks \src tgt i _ ->
        { source: src, target: tgt, index: i }
  stateRef <- Ref.new { nodes: positionedNodes, swizzled }

  -- Set up tick handler (updates both nodes AND links)
  Sim.onTick (tickForceWithStraightLinks stateRef config) sim
  Sim.start sim

  pure
    { cleanup: do
        log "[ForceGraph] Cleanup"
        Sim.stop sim
        BehaviorFFI.unregisterSimulation_ simulationId
    }

-- =============================================================================
-- Tick Handlers
-- =============================================================================

-- | Tick handler for simple force render
tickForce :: Ref.Ref { nodes :: Array SimNode, swizzled :: Array SwizzledLink }
          -> Config
          -> Effect Unit
tickForce stateRef _config = runD3v2M do
  state <- liftEffect $ Ref.read stateRef

  linksGroup <- select "#force-zoom-group .force-links-group"
  nodesGroup <- select "#force-zoom-group .force-nodes-group"

  _ <- renderTree linksGroup (T.joinData "force-links" "line" state.swizzled linkLineTemplate)
  _ <- renderTree nodesGroup (T.joinData "force-nodes" "circle" state.nodes nodeTemplate)

  pure unit

-- | Tick handler for force with straight lines (phase 3)
tickForceWithStraightLinks :: Ref.Ref { nodes :: Array SimNode, swizzled :: Array SwizzledLink }
                           -> Config
                           -> Effect Unit
tickForceWithStraightLinks stateRef config = do
  state <- Ref.read stateRef

  -- Update circle positions
  transformCircles config.containerSelector \rect ->
    case rect.simNode of
      Just sn ->
        case Array.find (\n -> n.id == sn.id) state.nodes of
          Just n -> { cx: n.x, cy: n.y }
          Nothing -> { cx: rect.x + rect.width / 2.0, cy: rect.y + rect.height / 2.0 }
      Nothing -> { cx: rect.x + rect.width / 2.0, cy: rect.y + rect.height / 2.0 }

  -- Update straight line links
  -- For now, just keep them at last positions (would need link data binding for proper update)
  pure unit

-- =============================================================================
-- DOM Structure Builders
-- =============================================================================

-- | Build SVG structure for force view
buildForceSvgTree :: Config -> T.Tree Unit
buildForceSvgTree _config =
  T.named SVG "svg"
    [ v3AttrStr "viewBox" (str "-950 -570 1900 1140")
    , v3AttrStr "class" (str "force-svg")
    ]
    `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#force-zoom-group" ]
    `T.withChildren`
      [ T.named Group "zoom-group"
          [ v3AttrStr "id" (str "force-zoom-group") ]
          `T.withChildren`
            [ T.named Group "links-group"
                [ v3AttrStr "class" (str "force-links-group") ]
            , T.named Group "nodes-group"
                [ v3AttrStr "class" (str "force-nodes-group") ]
            ]
      ]

-- | Add tree links (bezier curves) to the SVG
addTreeLinks :: Config -> TreeLayout.TreeLayoutResult -> Number -> Number -> Effect Unit
addTreeLinks config layout rootX rootY = do
  let nodeMap = buildTreeNodeMap layout.treeNodes
  let linkElements = Array.mapMaybe (buildLinkElement nodeMap rootX rootY) (Set.toUnfoldable layout.treeEdges)

  log $ "[ForceGraph] Adding " <> show (Array.length linkElements) <> " tree links"

  -- Remove any existing links group
  removeElement (config.containerSelector <> " g.force-links-group")

  -- Add links to SVG
  _ <- runD3v2M do
    svg <- select (config.containerSelector <> " svg")
    renderTree svg (buildLinksVizTree linkElements rootX rootY)

  pure unit

-- | Build VizTree for tree links
buildLinksVizTree :: Array LinkElement -> Number -> Number -> T.Tree LinkElement
buildLinksVizTree linkElements rootX rootY =
  T.named Group "force-links-group"
    [ v3AttrFnStr "class" (const "force-links-group")
    ]
  `T.withChild`
    T.joinData "force-links" "path" linkElements (linkTemplate rootX rootY)

-- | Template for link path (starts at root position)
linkTemplate :: Number -> Number -> LinkElement -> T.Tree LinkElement
linkTemplate rootX rootY _el =
  T.elem Path
    [ v3AttrFnStr "d" (\_ -> TreeStyle.radialTree.linkPath rootX rootY rootX rootY)
    , v3AttrStr "fill" (str "none")
    , v3AttrStr "stroke" (str "rgba(255, 255, 255, 0.5)")
    , v3AttrFn "stroke-width" (\_ -> 1.5)
    , v3AttrFnStr "class" (\_ -> "force-link")
    ]

-- | Template for link as a line (for simple force render)
linkLineTemplate :: SwizzledLink -> T.Tree SwizzledLink
linkLineTemplate _link =
  T.elem Line
    [ v3AttrFn "x1" (_.source.x :: SwizzledLink -> Number)
    , v3AttrFn "y1" (_.source.y :: SwizzledLink -> Number)
    , v3AttrFn "x2" (_.target.x :: SwizzledLink -> Number)
    , v3AttrFn "y2" (_.target.y :: SwizzledLink -> Number)
    , v3AttrStr "stroke" (str "rgba(255, 255, 255, 0.3)")
    , v3AttrFn "stroke-width" (\_ -> 1.0)
    , v3AttrStr "class" (str "force-link")
    ]

-- | Template for node circle
nodeTemplate :: SimNode -> T.Tree SimNode
nodeTemplate _node =
  T.elem Circle
    [ v3AttrFn "cx" (_.x :: SimNode -> Number)
    , v3AttrFn "cy" (_.y :: SimNode -> Number)
    , v3AttrFn "r" (\n -> max 5.0 (n.r * 0.5))
    , v3AttrStr "fill" (str "rgba(255, 255, 255, 0.5)")
    , v3AttrStr "stroke" (str "rgba(255, 255, 255, 0.8)")
    , v3AttrFn "stroke-width" (\_ -> 1.5)
    , v3AttrStr "class" (str "force-node module-node")
    ]
    `T.withBehaviors` [ Drag (SimulationDrag simulationId) ]

-- | Cleanup function
cleanup :: Effect Unit
cleanup = do
  log "[ForceGraph] Cleanup called"
  pure unit

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Check if both ends of a link are modules
isModuleLink :: Array SimNode -> SimLink -> Boolean
isModuleLink nodes link =
  let
    srcNode = Array.find (\n -> n.id == link.source) nodes
    tgtNode = Array.find (\n -> n.id == link.target) nodes
  in
    case srcNode, tgtNode of
      Just s, Just t -> s.nodeType == ModuleNode && t.nodeType == ModuleNode
      _, _ -> false

-- | Build map from node ID to TreeNode
buildTreeNodeMap :: Array TreeLayout.TreeNode -> Map Int TreeLayout.TreeNode
buildTreeNodeMap nodes = Map.fromFoldable $ nodes <#> \n -> Tuple n.id n

-- | Build link element from tree edge
buildLinkElement :: Map Int TreeLayout.TreeNode -> Number -> Number -> Tuple Int Int -> Maybe LinkElement
buildLinkElement nodeMap rootX rootY (Tuple srcId tgtId) = do
  sourceNode <- Map.lookup srcId nodeMap
  targetNode <- Map.lookup tgtId nodeMap

  pure
    { sourceTreeX: sourceNode.radialX  -- Use radial positions
    , sourceTreeY: sourceNode.radialY
    , targetTreeX: targetNode.radialX
    , targetTreeY: targetNode.radialY
    , rootX
    , rootY
    }
