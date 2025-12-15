-- | Les Misérables Force Graph with GUP (General Update Pattern)
-- |
-- | Combines:
-- | - Force simulation for continuous position updates
-- | - Tick-driven transitions for enter/exit animations
-- | - v3 Finally Tagless expressions for computed attributes
-- |
-- | KEY INSIGHT: Simulation tick drives EVERYTHING - positions AND transitions.
-- | No CSS transitions needed. All visual properties interpolated in PureScript.
-- |
-- | v3 EXPRESSIONS: Polymorphic attribute expressions that can be:
-- | - Evaluated at runtime (Eval interpreter)
-- | - Generated as source code (CodeGen interpreter)
-- | - Rendered as SVG strings (SVG interpreter)
-- |
-- | NO FFI - uses only library functionality.
module D3.Viz.LesMisV3.GUPDemo
  ( LesMisGUPState
  , ExitingNode
  , LesMisSimulation
  , LesMisNodeRow
  , LesMisLinkRow
  , initGUPDemo
  , addRandomNodes
  , removeRandomNodes
  , resetToFull
  -- New API demo
  , testGUPAPI
  ) where

import Prelude

import Data.Array as Array
import Data.Array (filter, length, (!!))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import D3.Viz.LesMisV3.Model (LesMisModel, LesMisNode)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Setup as Setup
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)
import PSD3.ForceEngine.Links (filterLinksToSubset, swizzleLinksByIndex)
import PSD3.Transition.Tick as Tick
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Behavior.FFI as BehaviorFFI
import PSD3v2.Behavior.Types (Behavior(..), DragConfig(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Selection.Types (ElementType(..)) as ET
import PSD3v2.VizTree.Tree as T
import Type.Proxy (Proxy(..))

-- v3 DSL imports
import PSD3v3.Expr (class NumExpr, class BoolExpr, class CompareExpr, class StringExpr, ifThenElse, lit)
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

-- | State for the GUP demo
-- | Tracks transitions with progress, driven by simulation tick
type LesMisGUPState =
  { fullModel :: LesMisModel -- Original full dataset
  , visibleNodeIds :: Array String -- Currently visible node IDs
  , enteringProgress :: Map String Tick.Progress -- nodeId → progress (0→1)
  , exitingNodes :: Array ExitingNode -- Nodes being animated out with progress
  , simulation :: LesMisSimulation -- Force simulation handle
  , containerSelector :: String -- DOM container
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

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Simulation ID for the registry
-- | This enables declarative SimulationDrag to find and reheat this simulation
simulationId :: String
simulationId = "lesmis-gup"

-- | Clone a node to create an independent copy
-- | This prevents mutations (like fx/fy from drag) from affecting other simulations
cloneNode :: LesMisNode -> LesMisNode
cloneNode n = { id: n.id, name: n.name, group: n.group, x: n.x, y: n.y, vx: n.vx, vy: n.vy, fx: n.fx, fy: n.fy }

-- | Initialize the GUP demo
-- | Returns a state ref for controlling the visualization
initGUPDemo :: LesMisModel -> String -> Effect (Ref LesMisGUPState)
initGUPDemo model containerSelector = do
  -- Clone nodes to have independent data from other visualizations
  let clonedNodes = map cloneNode model.nodes
  let clonedModel = model { nodes = clonedNodes }

  -- Start with all nodes visible, all entering
  let allNodeIds = map _.name clonedNodes
  let initialEntering = Tick.startProgress allNodeIds Map.empty

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

  -- Create state ref
  stateRef <- Ref.new
    { fullModel: clonedModel
    , visibleNodeIds: allNodeIds
    , enteringProgress: initialEntering
    , exitingNodes: []
    , simulation: sim
    , containerSelector
    }

  -- Render initial SVG structure
  runD3v2M $ renderSVGContainer containerSelector

  -- Set up tick callback - this drives EVERYTHING
  Sim.onTick (onSimulationTick stateRef) sim

  -- Start simulation
  Sim.start sim

  pure stateRef

-- =============================================================================
-- GUP Operations
-- =============================================================================

-- | Add N random nodes that aren't currently visible
-- | Uses applySetupWithData for true GUP semantics
addRandomNodes :: Int -> Ref LesMisGUPState -> Effect Unit
addRandomNodes count stateRef = do
  state <- Ref.read stateRef

  -- Get current nodes from simulation (these have positions)
  currentNodes <- Sim.getNodes state.simulation

  -- Find nodes from fullModel that aren't in the simulation
  let currentIds = map _.name currentNodes
  let hiddenNodes = filter (\n -> not (Array.elem n.name currentIds)) state.fullModel.nodes

  -- Pick random hidden nodes to add
  nodesToAdd <- pickRandom count hiddenNodes

  -- Compute desired state: current nodes + new nodes
  let desiredNodes = currentNodes <> nodesToAdd
  let desiredLinks = filterLinksToSubset _.id desiredNodes state.fullModel.links

  -- Apply with GUP semantics
  result <- Setup.applySetupWithData lesMisSetup desiredNodes desiredLinks state.simulation

  -- Use GUP result to drive entering transitions
  let enteredNames = map _.name result.nodes.entered
  let newEntering = Tick.startProgress enteredNames state.enteringProgress

  -- Update visibleNodeIds to match new reality
  let newVisible = map _.name desiredNodes

  -- Update state
  Ref.write
    ( state
        { visibleNodeIds = newVisible
        , enteringProgress = newEntering
        }
    )
    stateRef

  -- Reheat simulation so nodes settle into new positions
  Sim.reheat state.simulation

-- | Remove N random visible nodes
-- | Uses applySetupWithData for true GUP semantics
removeRandomNodes :: Int -> Ref LesMisGUPState -> Effect Unit
removeRandomNodes count stateRef = do
  state <- Ref.read stateRef

  -- Get current nodes from simulation
  currentNodes <- Sim.getNodes state.simulation

  -- Pick random nodes to remove
  nodesToRemove <- pickRandom count currentNodes

  -- Compute desired state: current - removed
  let removeNames = map _.name nodesToRemove
  let desiredNodes = filter (\n -> not (Array.elem n.name removeNames)) currentNodes
  let desiredLinks = filterLinksToSubset _.id desiredNodes state.fullModel.links

  -- Apply with GUP semantics
  result <- Setup.applySetupWithData lesMisSetup desiredNodes desiredLinks state.simulation

  -- Use GUP result to drive exiting transitions
  -- The exited nodes have their positions frozen for the exit animation
  let newExiting = Tick.startTransitions result.nodes.exited

  -- Remove exiting nodes from entering if they were still entering
  let exitedNames = map _.name result.nodes.exited
  let newEntering = Array.foldl (\m id -> Map.delete id m) state.enteringProgress exitedNames

  -- Update visibleNodeIds to match new reality
  let newVisible = map _.name desiredNodes

  -- Update state
  Ref.write
    ( state
        { visibleNodeIds = newVisible
        , enteringProgress = newEntering
        , exitingNodes = state.exitingNodes <> newExiting
        }
    )
    stateRef

  -- Reheat simulation so remaining nodes settle
  Sim.reheat state.simulation

-- | Reset to full dataset
-- | Uses applySetupWithData for true GUP semantics
resetToFull :: Ref LesMisGUPState -> Effect Unit
resetToFull stateRef = do
  state <- Ref.read stateRef

  -- Full dataset is the desired state
  let allNodes = state.fullModel.nodes
  let allLinks = state.fullModel.links

  -- Apply with GUP semantics
  result <- Setup.applySetupWithData lesMisSetup allNodes allLinks state.simulation

  -- Use GUP result to drive entering transitions for newly added nodes
  let enteredNames = map _.name result.nodes.entered
  let newEntering = Tick.startProgress enteredNames state.enteringProgress

  -- Update visibleNodeIds to match new reality
  let allIds = map _.name allNodes

  -- Clear exiting nodes and set all as visible
  Ref.write
    ( state
        { visibleNodeIds = allIds
        , enteringProgress = newEntering
        , exitingNodes = []  -- Clear any pending exits
        }
    )
    stateRef

  Sim.reheat state.simulation

-- =============================================================================
-- Simulation Tick Handler - THE HEART OF TICK-DRIVEN TRANSITIONS
-- =============================================================================

-- | Called on each simulation tick
-- | Advances transition progress AND re-renders
onSimulationTick :: Ref LesMisGUPState -> Effect Unit
onSimulationTick stateRef = do
  state <- Ref.read stateRef

  -- Advance and filter transitions using library functions
  let { active: stillEntering } = Tick.tickProgressMap transitionDelta state.enteringProgress
  let { active: stillExiting } = Tick.tickTransitions transitionDelta state.exitingNodes

  -- Update state with advanced transitions
  Ref.write
    ( state
        { enteringProgress = stillEntering
        , exitingNodes = stillExiting
        }
    )
    stateRef

  -- Render with current state
  renderVisualization stateRef

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Create the SVG container structure using TreeAPI
-- | Zoom behavior is now declarative via withBehaviors
renderSVGContainer :: String -> D3v2M Unit
renderSVGContainer containerSelector = do
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

-- | Render visualization with current state
renderVisualization :: Ref LesMisGUPState -> Effect Unit
renderVisualization stateRef = do
  state <- Ref.read stateRef

  -- Get current node positions from simulation
  currentNodes <- Sim.getNodes state.simulation

  -- Build render nodes with transition state
  let visibleNodes = filter (\n -> Array.elem n.name state.visibleNodeIds) currentNodes

  let
    renderNodes = map
      ( \n ->
          { node: n
          , enterProgress: Map.lookup n.name state.enteringProgress
          , exitProgress: Nothing
          }
      )
      visibleNodes

  -- Add exiting nodes (with frozen positions and exit progress)
  let
    exitingRenderNodes = map
      ( \e ->
          { node: e.item
          , enterProgress: Nothing
          , exitProgress: Just e.progress
          }
      )
      state.exitingNodes

  let allRenderNodes = renderNodes <> exitingRenderNodes

  -- Create links only between visible nodes (not exiting)
  let visibleLinks = filterLinksToSubset _.id visibleNodes state.fullModel.links
  let
    swizzledLinks = swizzleLinksByIndex _.id visibleNodes visibleLinks \src tgt i link ->
      { source: src, target: tgt, value: link.value, index: i, isExiting: false }

  -- Create scene data
  let scene = { nodes: allRenderNodes, links: swizzledLinks }

  -- Render
  runD3v2M $ renderGUPScene scene

-- | Render the scene
renderGUPScene :: SceneData -> D3v2M Unit
renderGUPScene scene = do
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

-- =============================================================================
-- New GUP API Demo
-- =============================================================================

-- | Test the new Setup.applySetupWithData API
-- |
-- | This demonstrates the "data-change" approach where we actually modify
-- | what's in the simulation (vs the filter-render approach used above).
-- |
-- | The test:
-- | 1. Creates a simulation with all nodes
-- | 2. Applies applySetupWithData with a subset (remove some nodes)
-- | 3. Logs the enter/update/exit categorization
-- | 4. Applies again with a different subset (add some back, remove others)
-- | 5. Logs the categorization again to show position preservation
testGUPAPI :: Ref LesMisGUPState -> Effect Unit
testGUPAPI stateRef = do
  state <- Ref.read stateRef
  let allNodes = state.fullModel.nodes
  let allLinks = state.fullModel.links

  log "=== Testing Setup.applySetupWithData API ==="
  log $ "Total nodes: " <> show (length allNodes)
  log $ "Total links: " <> show (length allLinks)

  -- Get current nodes from simulation (they have x,y positions from simulation)
  currentNodes <- Sim.getNodes state.simulation

  log "\n--- Step 1: Current simulation state ---"
  log $ "Current nodes in simulation: " <> show (length currentNodes)

  -- Create a subset: keep only nodes with id < 30
  let subset1Nodes = filter (\n -> n.id < 30) currentNodes
  let subset1Links = filterLinksToSubset _.id subset1Nodes allLinks

  log "\n--- Step 2: Apply subset (nodes with id < 30) ---"
  log $ "Desired nodes: " <> show (length subset1Nodes)
  log $ "Desired links: " <> show (length subset1Links)

  -- Build a complete setup matching initGUPDemo forces
  -- IMPORTANT: applySetup removes any forces not in the setup, so we must include ALL forces
  let setup1 :: Setup.Setup LesMisNode
      setup1 = Setup.setup "test"
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

  -- Apply with GUP semantics
  result1 <- Setup.applySetupWithData setup1 subset1Nodes subset1Links state.simulation

  log "\n--- GUP Result (step 2) ---"
  log $ "Entered nodes: " <> show (length result1.nodes.entered) <> " (should be 0 - filtering down)"
  log $ "Updated nodes: " <> show (length result1.nodes.updated) <> " (should be ~30)"
  log $ "Exited nodes: " <> show (length result1.nodes.exited) <> " (nodes removed)"
  log $ "Entered links: " <> show (length result1.links.entered)
  log $ "Updated links: " <> show (length result1.links.updated)
  log $ "Exited links: " <> show (length result1.links.exited)

  -- Show sample exited node names
  let exitedNames = map _.name (Array.take 5 result1.nodes.exited)
  log $ "Sample exited node names: " <> show exitedNames

  -- Now apply a second subset: nodes 10-50 (overlapping, some new, some removed)
  let subset2Nodes = filter (\n -> n.id >= 10 && n.id < 50) allNodes
  let subset2Links = filterLinksToSubset _.id subset2Nodes allLinks

  log "\n--- Step 3: Apply subset (nodes 10-50) ---"
  log $ "Desired nodes: " <> show (length subset2Nodes)

  result2 <- Setup.applySetupWithData setup1 subset2Nodes subset2Links state.simulation

  log "\n--- GUP Result (step 3) ---"
  log $ "Entered nodes: " <> show (length result2.nodes.entered) <> " (nodes 30-49, newly added)"
  log $ "Updated nodes: " <> show (length result2.nodes.updated) <> " (nodes 10-29, positions preserved)"
  log $ "Exited nodes: " <> show (length result2.nodes.exited) <> " (nodes 0-9, removed)"

  -- Verify position preservation: sample an updated node
  case Array.head result2.nodes.updated of
    Nothing -> log "No updated nodes to check"
    Just n -> do
      log $ "\nSample updated node: " <> n.name <> " (id=" <> show n.id <> ")"
      log $ "  Position: (" <> show n.x <> ", " <> show n.y <> ")"
      log $ "  Velocity: (" <> show n.vx <> ", " <> show n.vy <> ")"
      log "  (These should be preserved from simulation, not reset)"

  log "\n=== GUP API Test Complete ==="

  -- Restore full dataset
  _ <- Setup.applySetupWithData setup1 allNodes allLinks state.simulation
  Sim.reheat state.simulation
