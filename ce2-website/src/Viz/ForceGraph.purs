-- | Force-Directed Graph View - STATELESS ARCHITECTURE
-- |
-- | Shows modules in a force-directed layout with three animation phases:
-- | 1. Grow radial tree from center (nodes move from center to tree positions)
-- | 2. Morph bezier links to straight lines
-- | 3. Engage force simulation (unpin nodes, let forces settle)
-- |
-- | ALL STATE LIVES IN HALOGEN (App.purs):
-- | - forcePhase :: ForcePhase (TreeGrowth | LinkMorph | ForceActive)
-- | - forceProgress :: Progress (0.0 to 1.0 within current phase)
-- | - forceLayout :: Maybe TreeLayoutResult
-- | - simulation :: Maybe ForceSimulation
-- |
-- | This module provides STATELESS functions:
-- | - createSimulationWithCallbacks: Creates simulation, returns handle
-- | - computeLayout: Pure layout computation
-- | - renderTreeGrowthPhase: Renders phase 1
-- | - renderLinkMorphPhase: Renders phase 2
-- | - renderForcePhase: Renders phase 3 (simulation active)
-- | - buildSceneData: Pure function to build render data
-- | - renderScene: Renders scene to DOM
module Viz.ForceGraph
  ( -- Simulation creation
    createSimulationWithCallbacks
  , ForceSimulation
  , SimNodeRow
  , SimulationConfig
  , defaultSimulationConfig
    -- Layout computation
  , computeLayout
    -- Force setup (declarative)
  , forceSetup
    -- Phase rendering
  , renderTreeGrowthPhase
  , renderLinkMorphPhase
  , renderForcePhase
    -- Scene data
  , SceneData
  , RenderNode
  , SwizzledLink
  , buildSceneData
  , renderScene
    -- DOM setup
  , addTreeLinks
    -- Cleanup
  , resetToTreemap
  , cleanupSimulation
    -- Constants
  , simulationId
  ) where

import Prelude

import Control.Monad (when)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Types (SimNode, SimLink, NodeType(..))
import Data.TreeLayout as TreeLayout
import DataViz.Layout.Hierarchy.TreeStyle as TreeStyle
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Setup as Setup
import PSD3.ForceEngine.Events (SimulationCallbacks)
import PSD3.ForceEngine.Links (swizzleLinksByIndex, filterLinksToSubset)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M)
import PSD3.Transform (transformCircles, transformPaths, removeElement)
import PSD3.Classify (classifyElements)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST as T
import PSD3.Internal.Behavior.Types (Behavior(..), DragConfig(..), ScaleExtent(..), defaultZoom)
import PSD3.Internal.Behavior.FFI as BehaviorFFI
import PSD3.Expr.Integration (v3Attr, v3AttrFn, v3AttrFnStr, v3AttrStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Types
-- =============================================================================

-- | Row types for the simulation
type SimNodeRow =
  ( name :: String
  , nodeType :: NodeType
  , package :: String
  , r :: Number
  , cluster :: Int
  , targets :: Array Int
  , sources :: Array Int
  , gridX :: Number
  , gridY :: Number
  , orbitAngle :: Number
  , treeX :: Number
  , treeY :: Number
  , radialX :: Number
  , radialY :: Number
  , isInTree :: Boolean
  , topoX :: Number
  , topoY :: Number
  , topoLayer :: Int
  )

-- | Concrete simulation type (links only have source/target for the force engine)
type ForceSimulation = Sim.Simulation SimNodeRow ()

-- | Configuration for simulation creation
type SimulationConfig =
  { containerSelector :: String
  , rootId :: Int
  }

-- | Default configuration
defaultSimulationConfig :: SimulationConfig
defaultSimulationConfig =
  { containerSelector: "#viz"
  , rootId: 0
  }

-- | Node ready for rendering with position
type RenderNode =
  { node :: SimNode
  , cx :: Number
  , cy :: Number
  }

-- | Swizzled link with source/target as full nodes
type SwizzledLink =
  { source :: SimNode
  , target :: SimNode
  , index :: Int
  }

-- | Link element for tree visualization (during phases 1-2)
-- | Includes node IDs so we can look up live positions during phase 3
type LinkElement =
  { sourceId :: Int
  , targetId :: Int
  , sourceTreeX :: Number
  , sourceTreeY :: Number
  , targetTreeX :: Number
  , targetTreeY :: Number
  , rootX :: Number
  , rootY :: Number
  }

-- | Scene data for force phase rendering - pure data structure
type SceneData =
  { nodes :: Array RenderNode
  , links :: Array SwizzledLink
  }

-- =============================================================================
-- Constants
-- =============================================================================

-- | Simulation ID for drag registry
simulationId :: String
simulationId = "code-explorer-force"

-- =============================================================================
-- Force Setup (Declarative Configuration)
-- =============================================================================

-- | Force setup for the code explorer
-- | Based on Observable's force layout tree:
-- | https://observablehq.com/@d3/force-directed-tree
-- | Plus collide force to handle our larger nodes
forceSetup :: Setup.Setup SimNode
forceSetup = Setup.setup "code-explorer"
  [ Setup.link "links"
      # Setup.withDistance (Setup.static 0.0)   -- Zero distance - nodes want to overlap
      # Setup.withStrength (Setup.static 1.0)   -- Full strength links
  , Setup.manyBody "charge"
      # Setup.withStrength (Setup.static (-50.0))  -- Weaker repulsion than before
  , Setup.collide "collide"
      # Setup.withRadius (Setup.dynamic _.r)   -- Use node's actual radius
  , Setup.positionX "forceX"
      # Setup.withStrength (Setup.static 0.1)   -- Default d3.forceX() strength
  , Setup.positionY "forceY"
      # Setup.withStrength (Setup.static 0.1)   -- Default d3.forceY() strength
  ]

-- =============================================================================
-- Simulation Creation (with Halogen Callbacks)
-- =============================================================================

-- | Create a simulation with callbacks for Halogen subscription pattern
-- | The simulation is created but NOT started - Halogen controls lifecycle
createSimulationWithCallbacks
  :: SimulationCallbacks
  -> TreeLayout.TreeLayoutResult  -- Pre-computed layout for initial positions
  -> Array SimNode
  -> Array SimLink
  -> Effect ForceSimulation
createSimulationWithCallbacks callbacks layout nodes links = do
  log "[ForceGraph] Creating simulation with callbacks"

  -- Filter to modules only
  let moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes

  -- Build map from tree layout
  let treeNodeMap = Map.fromFoldable $ layout.treeNodes <#> \tn -> Tuple tn.id tn

  -- Only include nodes that are IN THE TREE - non-tree nodes would have
  -- grid positions that cause chaos when mixed with radial positions
  let positionedNodes = Array.mapMaybe (\n ->
        case Map.lookup n.id treeNodeMap of
          Just tn | tn.isInTree -> Just (n { x = tn.radialX, y = tn.radialY })
          _ -> Nothing  -- Exclude non-tree nodes from simulation
        ) moduleNodes

  -- Use tree edges from layout (not linkType which isn't set properly)
  -- Convert Set of edge tuples to Array of link records
  let treeEdgeArray = Set.toUnfoldable layout.treeEdges :: Array (Tuple Int Int)
  let treeLinks = treeEdgeArray <#> \(Tuple src tgt) -> { source: src, target: tgt }

  log $ "[ForceGraph] Tree edges from layout: " <> show (Array.length treeLinks)

  -- Build set of positioned node IDs for efficient lookup
  let positionedNodeIds = Set.fromFoldable $ positionedNodes <#> _.id

  -- Check each tree edge to see if both endpoints are in positioned nodes
  let checkEdge (Tuple src tgt) =
        let srcIn = Set.member src positionedNodeIds
            tgtIn = Set.member tgt positionedNodeIds
        in { src, tgt, srcIn, tgtIn, bothIn: srcIn && tgtIn }
  let edgeStatus = treeEdgeArray <#> checkEdge
  let missingEdges = Array.filter (not <<< _.bothIn) edgeStatus

  log $ "[ForceGraph] Edges with missing endpoints: " <> show (Array.length missingEdges)
  when (Array.length missingEdges > 0) do
    log $ "[ForceGraph] First 5 missing edges:"
    _ <- Array.foldM (\_ e -> log $ "  " <> show e.src <> " -> " <> show e.tgt <> " (srcIn=" <> show e.srcIn <> ", tgtIn=" <> show e.tgtIn <> ")") unit (Array.take 5 missingEdges)
    pure unit

  -- Filter to only links between nodes in the simulation
  let filteredLinks = filterLinksToSubset _.id positionedNodes treeLinks

  log $ "[ForceGraph] After filterLinksToSubset: " <> show (Array.length filteredLinks) <> " links"
  log $ "[ForceGraph] Simulation includes " <> show (Array.length positionedNodes) <> " in-tree nodes, "
      <> show (Array.length filteredLinks) <> " links"

  -- Log a sample of positions
  case Array.take 3 positionedNodes of
    sample -> do
      log $ "[ForceGraph] Sample positions:"
      _ <- Array.foldM (\_ n -> log $ "  Node " <> show n.id <> ": x=" <> show n.x <> ", y=" <> show n.y) unit sample
      pure unit

  -- Create simulation with callbacks
  sim <- Sim.createWithCallbacks Sim.defaultConfig callbacks
  Sim.setNodes positionedNodes sim
  Sim.setLinks filteredLinks sim

  log $ "[ForceGraph] Set " <> show (Array.length filteredLinks) <> " links in simulation"

  -- Apply declarative force setup
  Setup.applySetup forceSetup sim

  -- Verify links were set
  simLinks <- Sim.getLinks sim
  log $ "[ForceGraph] After applySetup, sim has " <> show (Array.length simLinks) <> " links"

  -- Register for drag
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- Do NOT start yet - Halogen will start when ready (after phases 1-2)
  pure sim

-- =============================================================================
-- Layout Computation (Pure)
-- =============================================================================

-- | Compute tree layout for the radial tree animation
-- | This is a pure computation that can be called once and reused
computeLayout :: Int -> Array SimNode -> Array SimLink -> TreeLayout.TreeLayoutResult
computeLayout rootId nodes links =
  TreeLayout.computeTreeLayout TreeStyle.radialTree rootId nodes links

-- =============================================================================
-- Phase 1: Tree Growth Rendering
-- =============================================================================

-- | Render tree growth phase (nodes move from center to radial positions)
-- | This is called on each animation tick during phase 1
renderTreeGrowthPhase
  :: String                          -- Container selector
  -> TreeLayout.TreeLayoutResult     -- Pre-computed layout
  -> Tick.Progress                   -- 0.0 to 1.0
  -> Effect Unit
renderTreeGrowthPhase containerSelector layout progress = do
  let easedProgress = Tick.easeOutCubic progress
  let treeNodeMap = Map.fromFoldable $ layout.treeNodes <#> \tn -> Tuple tn.id tn
  let rootX = 0.0
  let rootY = 0.0

  -- Move circles from center to radial tree positions
  transformCircles containerSelector \rect ->
    case rect.simNode of
      Just sn ->
        case Map.lookup sn.id treeNodeMap of
          Just tn | tn.isInTree ->
            { cx: Tick.lerp rootX tn.radialX easedProgress
            , cy: Tick.lerp rootY tn.radialY easedProgress
            }
          _ ->
            { cx: rect.x + rect.width / 2.0
            , cy: rect.y + rect.height / 2.0
            }
      Nothing ->
        { cx: rect.x + rect.width / 2.0
        , cy: rect.y + rect.height / 2.0
        }

  -- Update bezier links (from root outward)
  transformPaths (containerSelector <> " g.force-links-group") \el ->
    let
      srcX = Tick.lerp rootX el.sourceTreeX easedProgress
      srcY = Tick.lerp rootY el.sourceTreeY easedProgress
      tgtX = Tick.lerp rootX el.targetTreeX easedProgress
      tgtY = Tick.lerp rootY el.targetTreeY easedProgress
    in
      TreeStyle.radialTree.linkPath srcX srcY tgtX tgtY

  -- Classify circles (in-tree vs dimmed)
  let inTreeIds = Set.fromFoldable $ Array.mapMaybe
        (\tn -> if tn.isInTree then Just tn.id else Nothing)
        layout.treeNodes

  classifyElements containerSelector "circle" \rect ->
    case rect.simNode of
      Just sn | Set.member sn.id inTreeIds -> "in-tree"
      _ -> "dimmed"

-- =============================================================================
-- Phase 2: Link Morph Rendering
-- =============================================================================

-- | Render link morph phase (bezier curves become straight lines)
-- | Nodes stay at their radial tree positions
renderLinkMorphPhase
  :: String                          -- Container selector
  -> TreeLayout.TreeLayoutResult     -- Pre-computed layout
  -> Tick.Progress                   -- 0.0 to 1.0
  -> Effect Unit
renderLinkMorphPhase containerSelector _layout progress = do
  let easedProgress = Tick.easeInOutCubic progress

  -- Morph links from bezier to straight line
  transformPaths (containerSelector <> " g.force-links-group") \el ->
    let
      bezierPath = TreeStyle.radialTree.linkPath el.sourceTreeX el.sourceTreeY el.targetTreeX el.targetTreeY
      straightPath = "M" <> show el.sourceTreeX <> "," <> show el.sourceTreeY
                  <> "L" <> show el.targetTreeX <> "," <> show el.targetTreeY
    in
      -- Switch at 50% progress (true morphing would need path interpolation)
      if easedProgress < 0.5 then bezierPath else straightPath

-- =============================================================================
-- Phase 3: Force Simulation Rendering
-- =============================================================================

-- | Render force phase using live node positions from simulation
-- | Called on each tick from the D3 simulation
renderForcePhase
  :: String              -- Container selector
  -> Array SimNode       -- Current nodes with live positions
  -> Effect Unit
renderForcePhase containerSelector currentNodes = do
  let nodeMap = Map.fromFoldable $ currentNodes <#> \n -> Tuple n.id n

  -- Update circle positions using live simulation data
  transformCircles containerSelector \rect ->
    case rect.simNode of
      Just sn ->
        case Map.lookup sn.id nodeMap of
          Just n -> { cx: n.x, cy: n.y }
          Nothing -> { cx: rect.x + rect.width / 2.0, cy: rect.y + rect.height / 2.0 }
      Nothing -> { cx: rect.x + rect.width / 2.0, cy: rect.y + rect.height / 2.0 }

  -- Update links to follow node positions
  -- LinkElements have sourceId/targetId, use nodeMap to look up current positions
  transformPaths (containerSelector <> " g.force-links-group") \el ->
    let
      -- Look up current positions from simulation nodes
      srcPos = case Map.lookup el.sourceId nodeMap of
        Just n -> { x: n.x, y: n.y }
        Nothing -> { x: el.sourceTreeX, y: el.sourceTreeY }
      tgtPos = case Map.lookup el.targetId nodeMap of
        Just n -> { x: n.x, y: n.y }
        Nothing -> { x: el.targetTreeX, y: el.targetTreeY }
    in
      "M" <> show srcPos.x <> "," <> show srcPos.y
        <> "L" <> show tgtPos.x <> "," <> show tgtPos.y

-- =============================================================================
-- Scene Data Construction (Pure)
-- =============================================================================

-- | Build scene data from current simulation state
-- | This is a PURE function - all state comes from parameters
buildSceneData
  :: Array SimNode       -- Current nodes from simulation (with positions)
  -> Array SimLink       -- Links from model
  -> SceneData
buildSceneData currentNodes links =
  let
    -- Filter to modules only
    moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) currentNodes

    -- Build render nodes with current positions
    renderNodes = moduleNodes <#> \n ->
      { node: n
      , cx: n.x
      , cy: n.y
      }

    -- Filter links to module-to-module only
    rawLinks = links <#> \l -> { source: l.source, target: l.target }
    filteredLinks = filterLinksToSubset _.id moduleNodes rawLinks
    swizzledLinks = swizzleLinksByIndex _.id moduleNodes filteredLinks \src tgt i _ ->
      { source: src, target: tgt, index: i }
  in
    { nodes: renderNodes, links: swizzledLinks }

-- | Render scene to DOM (stateless, side-effecting)
renderScene :: String -> SceneData -> Effect Unit
renderScene containerSelector scene = runD3v2M do
  nodesGroup <- select (containerSelector <> " #force-zoom-group .force-nodes-group")
  let nodesTree = createNodesTree scene
  _ <- renderTree nodesGroup nodesTree

  linksGroup <- select (containerSelector <> " #force-zoom-group .force-links-group")
  let linksTree = createLinksTree scene
  _ <- renderTree linksGroup linksTree

  pure unit

-- =============================================================================
-- DOM Setup
-- =============================================================================

-- | Add tree links (bezier curves) to the SVG for phases 1-2
addTreeLinks
  :: String                          -- Container selector
  -> TreeLayout.TreeLayoutResult     -- Pre-computed layout
  -> Effect Unit
addTreeLinks containerSelector layout = do
  let nodeMap = buildTreeNodeMap layout.treeNodes
  let rootX = 0.0
  let rootY = 0.0
  let linkElements = Array.mapMaybe (buildLinkElement nodeMap rootX rootY) (Set.toUnfoldable layout.treeEdges)

  log $ "[ForceGraph] Adding " <> show (Array.length linkElements) <> " tree links"

  -- Remove any existing links group
  removeElement (containerSelector <> " g.force-links-group")

  -- Add links to SVG
  _ <- runD3v2M do
    svg <- select (containerSelector <> " svg")
    renderTree svg (buildLinksVizTree linkElements rootX rootY)

  pure unit

-- =============================================================================
-- Cleanup
-- =============================================================================

-- | Reset circles to treemap positions
resetToTreemap :: String -> Effect Unit
resetToTreemap containerSelector = do
  log "[ForceGraph] Resetting to treemap positions"
  removeElement (containerSelector <> " g.force-links-group")
  removeElement (containerSelector <> " svg.force-svg")
  transformCircles containerSelector \rect ->
    { cx: rect.x + rect.width / 2.0
    , cy: rect.y + rect.height / 2.0
    }

-- | Cleanup simulation when switching views
cleanupSimulation :: ForceSimulation -> Effect Unit
cleanupSimulation sim = do
  log "[ForceGraph] Cleaning up simulation"
  Sim.stop sim
  BehaviorFFI.unregisterSimulation_ simulationId

-- =============================================================================
-- Tree Construction for Rendering
-- =============================================================================

-- | Create nodes tree for scene rendering
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree scene =
  T.sceneNestedJoin "nodes" "circle"
    [ scene ]
    (_.nodes)
    ( \rn -> T.elem Circle
        [ v3Attr "cx" (lit rn.cx)
        , v3Attr "cy" (lit rn.cy)
        , v3Attr "r" (lit (max 5.0 (rn.node.r * 0.5)))
        , v3AttrStr "fill" (str "rgba(255, 255, 255, 0.5)")
        , v3AttrStr "stroke" (str "rgba(255, 255, 255, 0.8)")
        , v3Attr "stroke-width" (lit 1.5)
        , v3AttrStr "class" (str "force-node module-node")
        ]
        `T.withBehaviors` [ Drag (SimulationDrag simulationId) ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- | Create links tree for scene rendering
createLinksTree :: SceneData -> T.Tree SceneData
createLinksTree scene =
  T.sceneNestedJoin "links" "line"
    [ scene ]
    (_.links)
    ( \link -> T.elem Line
        [ v3Attr "x1" (lit link.source.x)
        , v3Attr "y1" (lit link.source.y)
        , v3Attr "x2" (lit link.target.x)
        , v3Attr "y2" (lit link.target.y)
        , v3AttrStr "stroke" (str "rgba(255, 255, 255, 0.3)")
        , v3Attr "stroke-width" (lit 1.0)
        , v3AttrStr "class" (str "force-link")
        ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- | Build map from node ID to TreeNode
buildTreeNodeMap :: Array TreeLayout.TreeNode -> Map Int TreeLayout.TreeNode
buildTreeNodeMap nodes = Map.fromFoldable $ nodes <#> \n -> Tuple n.id n

-- | Build link element from tree edge
buildLinkElement :: Map Int TreeLayout.TreeNode -> Number -> Number -> Tuple Int Int -> Maybe LinkElement
buildLinkElement nodeMap rootX rootY (Tuple srcId tgtId) = do
  sourceNode <- Map.lookup srcId nodeMap
  targetNode <- Map.lookup tgtId nodeMap

  pure
    { sourceId: srcId
    , targetId: tgtId
    , sourceTreeX: sourceNode.radialX
    , sourceTreeY: sourceNode.radialY
    , targetTreeX: targetNode.radialX
    , targetTreeY: targetNode.radialY
    , rootX
    , rootY
    }

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
