-- | Les Misérables Force Graph with GUP (General Update Pattern)
-- |
-- | HALOGEN-FIRST ARCHITECTURE:
-- | - All state lives in Halogen (enteringProgress, exitingNodes)
-- | - This module provides STATELESS functions for visualization
-- | - The simulation is an opaque handle that Halogen manages
-- |
-- | Visualization functions:
-- | - createSimulation: Creates a D3 force simulation
-- | - subscribeToTick: Sets up tick callback
-- | - buildSceneData: Pure function to compute scene from nodes + transitions
-- | - renderSVGContainer: Creates SVG structure (one-time)
-- | - renderScene: Renders scene data to DOM
-- |
-- | NO Effect.Ref - uses only library functionality.
module D3.Viz.LesMisV3.GUPDemo
  ( -- Types
    LesMisSimulation
  , LesMisNodeRow
  , LesMisLinkRow
  , RenderNode
  , SceneData
  , SwizzledLink
  , ExitingNode
  -- Simulation lifecycle
  , createSimulation
  , createSimulationWithCallbacks
  -- Rendering (stateless)
  , renderSVGContainer
  , renderScene
  , buildSceneData
  -- Configuration
  , lesMisSetup
  , transitionDelta
  , simulationId
  -- Helpers (re-exported for Halogen to use)
  , pickRandom
  , cloneNode
  ) where

import Prelude

import Data.Array as Array
import Data.Array (filter, length, (!!))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Random (randomInt)
import D3.Viz.LesMisV3.Model (LesMisModel, LesMisNode, LesMisLink)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Setup as Setup
import PSD3.ForceEngine.Events (SimulationCallbacks)
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)
import PSD3.ForceEngine.Links (filterLinksToSubset, swizzleLinksByIndex)
import PSD3.Transition.Tick as Tick
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Behavior.FFI as BehaviorFFI
import PSD3v2.Behavior.Types (Behavior(..), DragConfig(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Selection.Types (ElementType(..)) as ET
import PSD3v2.VizTree.Tree as T
import Type.Proxy (Proxy(..))

-- v3 DSL imports
import PSD3v3.Expr (class NumExpr, class BoolExpr, class CompareExpr, class StringExpr, ifThenElse)
import PSD3v3.Datum (class DatumExpr, field)
import PSD3v3.Sugar ((*:), (+.), (-.), (>.), s)
import PSD3v3.Interpreter.Eval (EvalD, runEvalD)

-- =============================================================================
-- Types
-- =============================================================================

-- | Row types for the simulation (extra fields beyond SimulationNode's id, x, y, vx, vy, fx, fy)
type LesMisNodeRow = (name :: String, group :: Int)
type LesMisLinkRow = (value :: Number)

-- | Concrete simulation type for Les Misérables
type LesMisSimulation = Sim.Simulation LesMisNodeRow LesMisLinkRow

-- | Exiting node uses library's Transitioning type
type ExitingNode = Tick.Transitioning LesMisNode

-- | Node ready for rendering with computed visual state
type RenderNode =
  { node :: LesMisNode
  , enterProgress :: Maybe Number -- Just 0.0-1.0 if entering, Nothing if not
  , exitProgress :: Maybe Number -- Just 0.0-1.0 if exiting, Nothing if not
  }

-- | Flattened node datum for v3 expressions
-- | We flatten the nested structure so v3 can access fields directly
type NodeDatum =
  { x :: Number
  , y :: Number
  , enterProgress :: Number  -- -1.0 means not entering, 0.0-1.0 means entering
  , exitProgress :: Number   -- -1.0 means not exiting, 0.0-1.0 means exiting
  }

-- | Row type for v3 DatumExpr
type NodeDatumRow = (x :: Number, y :: Number, enterProgress :: Number, exitProgress :: Number)

-- | Convert RenderNode to NodeDatum for v3 expressions
toNodeDatum :: RenderNode -> NodeDatum
toNodeDatum rn =
  { x: rn.node.x
  , y: rn.node.y
  , enterProgress: case rn.enterProgress of
      Just p -> p
      Nothing -> -1.0  -- Sentinel: not entering
  , exitProgress: case rn.exitProgress of
      Just p -> p
      Nothing -> -1.0  -- Sentinel: not exiting
  }

-- | Flattened link datum for v3 expressions
type LinkDatum =
  { sourceX :: Number
  , sourceY :: Number
  , targetX :: Number
  , targetY :: Number
  , value :: Number
  }

-- | Row type for v3 DatumExpr (links)
type LinkDatumRow = (sourceX :: Number, sourceY :: Number, targetX :: Number, targetY :: Number, value :: Number)

-- | Convert SwizzledLink to LinkDatum for v3 expressions
toLinkDatum :: SwizzledLink -> LinkDatum
toLinkDatum link =
  { sourceX: link.source.x
  , sourceY: link.source.y
  , targetX: link.target.x
  , targetY: link.target.y
  , value: link.value
  }

-- | Swizzled link for rendering (node references instead of indices)
type SwizzledLink =
  { source :: LesMisNode
  , target :: LesMisNode
  , value :: Number
  , index :: Int
  , isExiting :: Boolean
  }

-- | Scene data for the Tree API
type SceneData =
  { nodes :: Array RenderNode
  , links :: Array SwizzledLink
  }

-- =============================================================================
-- v3 Field Accessors
-- =============================================================================

-- Node fields
nodeX :: forall repr. DatumExpr repr NodeDatumRow => repr Number
nodeX = field (Proxy :: Proxy "x")

nodeY :: forall repr. DatumExpr repr NodeDatumRow => repr Number
nodeY = field (Proxy :: Proxy "y")

nodeEnterProgress :: forall repr. DatumExpr repr NodeDatumRow => repr Number
nodeEnterProgress = field (Proxy :: Proxy "enterProgress")

nodeExitProgress :: forall repr. DatumExpr repr NodeDatumRow => repr Number
nodeExitProgress = field (Proxy :: Proxy "exitProgress")

-- Link fields
linkSourceX :: forall repr. DatumExpr repr LinkDatumRow => repr Number
linkSourceX = field (Proxy :: Proxy "sourceX")

linkSourceY :: forall repr. DatumExpr repr LinkDatumRow => repr Number
linkSourceY = field (Proxy :: Proxy "sourceY")

linkTargetX :: forall repr. DatumExpr repr LinkDatumRow => repr Number
linkTargetX = field (Proxy :: Proxy "targetX")

linkTargetY :: forall repr. DatumExpr repr LinkDatumRow => repr Number
linkTargetY = field (Proxy :: Proxy "targetY")

linkValue :: forall repr. DatumExpr repr LinkDatumRow => repr Number
linkValue = field (Proxy :: Proxy "value")

-- =============================================================================
-- v3 Expressions for Node Visual Properties
-- =============================================================================

-- | v3 expression: Node radius
-- | if entering (progress >= 0): lerp 20.0 5.0 progress
-- | else if exiting (progress >= 0): lerp 5.0 20.0 progress
-- | else: 5.0
nodeRadiusExpr :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => DatumExpr repr NodeDatumRow => repr Number
nodeRadiusExpr = ifThenElse
  (nodeEnterProgress >. lit (-0.5))  -- Is entering? (progress >= 0)
  -- Entering: 20 - 15 * progress = start large, shrink to 5
  (lit 20.0 -. (nodeEnterProgress *: 15.0))  -- Use -. for expr-expr subtraction
  (ifThenElse
    (nodeExitProgress >. lit (-0.5))  -- Is exiting?
    -- Exiting: 5 + 15 * progress = start small, grow to 20
    (lit 5.0 +. (nodeExitProgress *: 15.0))  -- Use +. for expr-expr addition
    -- Normal: 5
    (lit 5.0)
  )

-- | v3 expression: Node fill color
-- | Entering: green, Exiting: brown, Normal: gray
nodeFillExpr :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => StringExpr repr => DatumExpr repr NodeDatumRow => repr String
nodeFillExpr = ifThenElse
  (nodeEnterProgress >. lit (-0.5))
  (s "#2ca02c")  -- Green for entering
  (ifThenElse
    (nodeExitProgress >. lit (-0.5))
    (s "#8c564b")  -- Brown for exiting
    (s "#7f7f7f")  -- Gray for normal
  )

-- | v3 expression: Node opacity
-- | Exiting: fades from 1 → 0
-- | Otherwise: 1.0
nodeOpacityExpr :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => DatumExpr repr NodeDatumRow => repr Number
nodeOpacityExpr = ifThenElse
  (nodeExitProgress >. lit (-0.5))
  -- Exiting: 1.0 - progress (fade out)
  (lit 1.0 -. nodeExitProgress)  -- Use -. for expr-expr subtraction
  (lit 1.0)

-- =============================================================================
-- v3 Expression Evaluators
-- =============================================================================

-- | Evaluate node expressions
evalNodeNum :: EvalD NodeDatum Number -> NodeDatum -> Number
evalNodeNum expr datum = runEvalD expr datum 0

evalNodeStr :: EvalD NodeDatum String -> NodeDatum -> String
evalNodeStr expr datum = runEvalD expr datum 0

-- | Evaluate link expressions
evalLinkNum :: EvalD LinkDatum Number -> LinkDatum -> Number
evalLinkNum expr datum = runEvalD expr datum 0

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 900.0

svgHeight :: Number
svgHeight = 600.0

-- | Transition speed: progress increment per tick
-- | At 60fps, 0.025 ≈ 40 ticks ≈ 0.67 seconds
transitionDelta :: Tick.TickDelta
transitionDelta = 0.025

-- | Shared force setup for Les Misérables demo
-- | Used by all GUP operations to ensure forces are preserved
lesMisSetup :: Setup.Setup LesMisNode
lesMisSetup = Setup.setup "lesmis"
  [ Setup.manyBody "charge"
      # Setup.withStrength (Setup.static (-100.0))
      # Setup.withDistanceMax 500.0
  , Setup.collide "collision"
      # Setup.withRadius (Setup.static 5.0)
      # Setup.withStrength (Setup.static 1.0)
  , Setup.center "center"
      # Setup.withX (Setup.static 0.0)
      # Setup.withY (Setup.static 0.0)
      # Setup.withStrength (Setup.static 0.1)
  , Setup.link "links"
      # Setup.withDistance (Setup.static 30.0)
      # Setup.withStrength (Setup.static 0.5)
  ]

-- | Simulation ID for the registry
-- | This enables declarative SimulationDrag to find and reheat this simulation
simulationId :: String
simulationId = "lesmis-gup"

-- =============================================================================
-- Simulation Lifecycle (Stateless)
-- =============================================================================

-- | Clone a node to create an independent copy
-- | This prevents mutations (like fx/fy from drag) from affecting other simulations
cloneNode :: LesMisNode -> LesMisNode
cloneNode n = { id: n.id, name: n.name, group: n.group, x: n.x, y: n.y, vx: n.vx, vy: n.vy, fx: n.fx, fy: n.fy }

-- | Create a simulation for the Les Misérables force graph
-- | Returns the simulation handle (no state ref)
-- | The simulation is started but paused - call Sim.start to run
createSimulation :: LesMisModel -> Effect LesMisSimulation
createSimulation model = do
  -- Clone nodes to have independent data from other visualizations
  let clonedNodes = map cloneNode model.nodes

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes clonedNodes sim
  Sim.setLinks model.links sim

  -- Register simulation for declarative drag behaviors
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- Add forces
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Start simulation
  Sim.start sim

  pure sim

-- | Create a simulation with callbacks (for Halogen subscription pattern)
-- | This enables using subscribeToSimulation from PSD3.ForceEngine.Halogen
createSimulationWithCallbacks :: SimulationCallbacks -> LesMisModel -> Effect LesMisSimulation
createSimulationWithCallbacks callbacks model = do
  -- Clone nodes to have independent data from other visualizations
  let clonedNodes = map cloneNode model.nodes

  -- Create simulation with callbacks
  sim <- Sim.createWithCallbacks Sim.defaultConfig callbacks
  Sim.setNodes clonedNodes sim
  Sim.setLinks model.links sim

  -- Register simulation for declarative drag behaviors
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- Add forces
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Start simulation
  Sim.start sim

  pure sim

-- =============================================================================
-- Scene Building (Pure except for reading simulation positions)
-- =============================================================================

-- | Build scene data from current simulation state + transition progress
-- | This is the main function that combines:
-- | - Current node positions (from simulation)
-- | - Entering progress (from Halogen state)
-- | - Exiting nodes (from Halogen state)
-- | - Links (filtered to visible nodes)
buildSceneData
  :: Array LesMisNode       -- Current nodes from simulation
  -> Map String Tick.Progress -- Entering progress map
  -> Array ExitingNode      -- Exiting nodes with frozen positions
  -> Array LesMisLink       -- All links (will be filtered)
  -> SceneData
buildSceneData currentNodes enteringProgress exitingNodes allLinks =
  let
    -- Build render nodes for current (visible) nodes
    renderNodes = map
      ( \n ->
          { node: n
          , enterProgress: Map.lookup n.name enteringProgress
          , exitProgress: Nothing
          }
      )
      currentNodes

    -- Build render nodes for exiting nodes (with frozen positions and exit progress)
    exitingRenderNodes = map
      ( \e ->
          { node: e.item
          , enterProgress: Nothing
          , exitProgress: Just e.progress
          }
      )
      exitingNodes

    allRenderNodes = renderNodes <> exitingRenderNodes

    -- Create links only between current nodes (not exiting)
    visibleLinks = filterLinksToSubset _.id currentNodes allLinks
    swizzledLinks = swizzleLinksByIndex _.id currentNodes visibleLinks \src tgt i link ->
      { source: src, target: tgt, value: link.value, index: i, isExiting: false }
  in
    { nodes: allRenderNodes, links: swizzledLinks }

-- =============================================================================
-- Rendering (Stateless)
-- =============================================================================

-- | Create the SVG container structure using TreeAPI
-- | Zoom behavior is now declarative via withBehaviors
-- | This should be called once at initialization
renderSVGContainer :: String -> Effect Unit
renderSVGContainer containerSelector = runD3v2M do
  container <- select containerSelector

  -- Declarative structure tree with zoom behavior attached to SVG
  let
    containerTree :: T.Tree Unit
    containerTree =
      T.named ET.SVG "svg"
        [ v3Attr "width" (lit svgWidth)
        , v3Attr "height" (lit svgHeight)
        , v3AttrStr "viewBox" (str (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight))
        , v3AttrStr "id" (str "lesmis-gup-svg")
        , v3AttrStr "class" (str "lesmis-gup")
        ]
        `T.withBehaviors`
          [ Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#lesmis-gup-zoom-group" ]
        `T.withChildren`
          [ T.named ET.Group "zoomGroup"
              [ v3AttrStr "id" (str "lesmis-gup-zoom-group"), v3AttrStr "class" (str "zoom-group") ]
              `T.withChildren`
                [ T.named ET.Group "linksGroup" [ v3AttrStr "id" (str "lesmis-gup-links"), v3AttrStr "class" (str "links") ]
                , T.named ET.Group "nodesGroup" [ v3AttrStr "id" (str "lesmis-gup-nodes"), v3AttrStr "class" (str "nodes") ]
                ]
          ]

  -- Render structure (behaviors are attached automatically)
  _ <- renderTree container containerTree

  pure unit

-- | Render scene data to DOM
-- | This is a stateless render function - it just renders what it's given
renderScene :: SceneData -> Effect Unit
renderScene scene = runD3v2M do
  -- Render nodes
  nodesGroup <- select "#lesmis-gup-nodes"
  let nodesTree = createNodesTree scene
  _ <- renderTree nodesGroup nodesTree

  -- Render links
  linksGroup <- select "#lesmis-gup-links"
  let linksTree = createLinksTree scene
  _ <- renderTree linksGroup linksTree

  pure unit

-- | Create nodes tree with v3 expressions for visual properties
-- | Binds RenderNode (which contains node :: LesMisNode) so drag can access the simulation node
-- | Uses SimulationDragNested which sets event.subject.node.fx/fy
createNodesTree :: SceneData -> T.Tree RenderNode
createNodesTree scene =
    T.joinData "nodes" "circle" scene.nodes $ \renderNode ->
      -- Convert RenderNode to NodeDatum for v3 expression evaluation
      let datum = toNodeDatum renderNode in
      -- v3 expressions compute visual properties from datum fields
      -- withBehaviors adds simulation-aware drag (nested variant for RenderNode.node)
      T.elem ET.Circle
        [ v3Attr "cx" (lit (evalNodeNum nodeX datum))           -- v3: d.x
        , v3Attr "cy" (lit (evalNodeNum nodeY datum))           -- v3: d.y
        , v3Attr "r" (lit (evalNodeNum nodeRadiusExpr datum))  -- v3: conditional lerp based on progress
        , v3AttrStr "fill" (str (evalNodeStr nodeFillExpr datum))      -- v3: conditional color
        , v3Attr "opacity" (lit (evalNodeNum nodeOpacityExpr datum)) -- v3: 1.0 or fade out
        , v3AttrStr "stroke" (str "#fff")
        , v3Attr "stroke-width" (lit 1.5)
        ]
        `T.withBehaviors`
          [ Drag (SimulationDragNested simulationId) ]

-- | Create links tree with v3 expressions
-- | Converts SwizzledLinks to LinkDatum for v3 expression evaluation
createLinksTree :: SceneData -> T.Tree LinkDatum
createLinksTree scene =
  let
    -- Convert SwizzledLinks to flattened LinkDatum for v3 expressions
    linkDatums = map toLinkDatum scene.links
  in
    T.joinData "links" "line" linkDatums $ \datum ->
      -- v3 expressions compute link endpoints from datum fields
      T.elem ET.Line
        [ v3Attr "x1" (lit (evalLinkNum linkSourceX datum))  -- v3: d.sourceX
        , v3Attr "y1" (lit (evalLinkNum linkSourceY datum))  -- v3: d.sourceY
        , v3Attr "x2" (lit (evalLinkNum linkTargetX datum))  -- v3: d.targetX
        , v3Attr "y2" (lit (evalLinkNum linkTargetY datum))  -- v3: d.targetY
        , v3Attr "stroke-width" (lit (sqrt (evalLinkNum linkValue datum)))  -- v3: sqrt(d.value)
        , v3AttrStr "stroke" (str "#999")
        , v3Attr "opacity" (lit 0.6)
        ]

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Pick N random elements from an array
pickRandom :: forall a. Int -> Array a -> Effect (Array a)
pickRandom n arr = do
  if n <= 0 || length arr == 0 then pure []
  else do
    indices <- pickRandomIndices n (length arr) []
    pure $ Array.mapMaybe (\i -> arr !! i) indices

pickRandomIndices :: Int -> Int -> Array Int -> Effect (Array Int)
pickRandomIndices 0 _ acc = pure acc
pickRandomIndices _ 0 acc = pure acc
pickRandomIndices n maxIdx acc = do
  idx <- randomInt 0 (maxIdx - 1)
  if Array.elem idx acc then pickRandomIndices n maxIdx acc -- Try again
  else pickRandomIndices (n - 1) maxIdx (acc <> [ idx ])
