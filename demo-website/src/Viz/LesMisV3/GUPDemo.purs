-- | Les Misérables Force Graph with GUP (General Update Pattern)
-- |
-- | Combines:
-- | - Force simulation for continuous position updates
-- | - Enter/Update/Exit with CSS transitions for visual feedback
-- |
-- | KEY INSIGHT: Simulation controls position (cx, cy) continuously.
-- | CSS transitions animate non-positional properties based on class:
-- | - Enter: .gup-node--entering (green, radius 0, opacity 0) → .gup-node (normal)
-- | - Update: .gup-node → .gup-node--updating (gray)
-- | - Exit: .gup-node → .gup-node--exiting (brown, radius 0, opacity 0) → removed
-- |
-- | NO FFI - uses only library functionality.
module D3.Viz.LesMisV3.GUPDemo
  ( LesMisGUPState
  , LesMisSimulation
  , LesMisNodeRow
  , LesMisLinkRow
  , initGUPDemo
  , addRandomNodes
  , removeRandomNodes
  , resetToFull
  ) where

import Prelude

import Data.Array as Array
import Data.Array (filter, length, (!!))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import D3.Viz.LesMisV3.Model (LesMisModel, LesMisNode, LesMisLink)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)
import PSD3v2.Attribute.Types (cx, cy, stroke, strokeWidth, x1, x2, y1, y2, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, on, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Selection.Types (ElementType(..)) as ET
import PSD3v2.VizTree.Tree as T

-- =============================================================================
-- Types
-- =============================================================================

-- | Row types for the simulation (extra fields beyond x, y, vx, vy / source, target)
type LesMisNodeRow = ( id :: String, group :: Int, fx :: Nullable Number, fy :: Nullable Number, index :: Int )
type LesMisLinkRow = ( value :: Number )

-- | Concrete simulation type for Les Misérables
type LesMisSimulation = Sim.Simulation LesMisNodeRow LesMisLinkRow

-- | GUP state for a node (which class to apply)
data NodeGUPState = Entering | Updating | Exiting | Normal

derive instance eqNodeGUPState :: Eq NodeGUPState
derive instance ordNodeGUPState :: Ord NodeGUPState

-- | Node with its GUP state
type GUPNodeWithState =
  { node :: LesMisNode
  , gupState :: NodeGUPState
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
  { nodes :: Array GUPNodeWithState
  , links :: Array SwizzledLink
  }

-- | State for the GUP demo
-- | Tracks previous visible nodes to compute enter/update/exit
type LesMisGUPState =
  { fullModel :: LesMisModel           -- Original full dataset
  , visibleNodeIds :: Array String     -- Currently visible node IDs
  , previousNodeIds :: Array String    -- Previously visible node IDs (for GUP)
  , exitingNodes :: Array LesMisNode   -- Nodes being animated out
  , simulation :: LesMisSimulation     -- Force simulation handle
  , containerSelector :: String        -- DOM container
  }

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 900.0

svgHeight :: Number
svgHeight = 600.0

-- | Transition duration
transitionDuration :: Milliseconds
transitionDuration = Milliseconds 500.0

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the GUP demo
-- | Returns a state ref for controlling the visualization
initGUPDemo :: LesMisModel -> String -> Effect (Ref LesMisGUPState)
initGUPDemo model containerSelector = do
  -- Start with all nodes visible
  let allNodeIds = map _.id model.nodes

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes model.nodes sim
  Sim.setLinks model.links sim

  -- Add forces
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Create state ref
  stateRef <- Ref.new
    { fullModel: model
    , visibleNodeIds: allNodeIds
    , previousNodeIds: []  -- No previous state yet
    , exitingNodes: []
    , simulation: sim
    , containerSelector
    }

  -- Render initial SVG structure
  runD3v2M $ renderSVGContainer containerSelector

  -- Set up tick callback
  Sim.onTick (onSimulationTick stateRef) sim

  -- Start simulation
  Sim.start sim

  -- Do initial render (all nodes will be "entering")
  updateVisualization stateRef

  -- After enter transition completes, switch entering nodes to normal
  launchAff_ do
    delay transitionDuration
    liftEffect do
      state <- Ref.read stateRef
      Ref.write (state { previousNodeIds = state.visibleNodeIds }) stateRef
      updateVisualization stateRef

  pure stateRef

-- =============================================================================
-- GUP Operations
-- =============================================================================

-- | Add N random nodes that aren't currently visible
addRandomNodes :: Int -> Ref LesMisGUPState -> Effect Unit
addRandomNodes count stateRef = do
  state <- Ref.read stateRef

  let hiddenIds = filter (\id -> not (Array.elem id state.visibleNodeIds))
                         (map _.id state.fullModel.nodes)

  -- Pick random hidden nodes to add
  nodesToAdd <- pickRandom count hiddenIds
  let newVisible = state.visibleNodeIds <> nodesToAdd

  -- Update state: new nodes will show as "entering"
  Ref.write (state { visibleNodeIds = newVisible }) stateRef

  -- Reheat simulation so nodes settle into new positions
  Sim.reheat state.simulation

  updateVisualization stateRef

  -- After transition, update previousNodeIds
  launchAff_ do
    delay transitionDuration
    liftEffect do
      s <- Ref.read stateRef
      Ref.write (s { previousNodeIds = s.visibleNodeIds }) stateRef

-- | Remove N random visible nodes
removeRandomNodes :: Int -> Ref LesMisGUPState -> Effect Unit
removeRandomNodes count stateRef = do
  state <- Ref.read stateRef

  -- Pick random visible nodes to remove
  nodesToRemove <- pickRandom count state.visibleNodeIds
  let newVisible = filter (\id -> not (Array.elem id nodesToRemove)) state.visibleNodeIds

  -- Get the actual node data for exiting nodes (freeze their positions)
  currentNodes <- Sim.getNodes state.simulation
  let exitingNodeData = filter (\n -> Array.elem n.id nodesToRemove) currentNodes

  -- Update state: removed nodes will show as "exiting"
  Ref.write (state { visibleNodeIds = newVisible
                   , exitingNodes = state.exitingNodes <> exitingNodeData
                   }) stateRef

  -- Reheat simulation so remaining nodes settle
  Sim.reheat state.simulation

  updateVisualization stateRef

  -- After transition, clean up exiting nodes and update previousNodeIds
  launchAff_ do
    delay transitionDuration
    liftEffect do
      s <- Ref.read stateRef
      -- Remove the exiting nodes that were added in this batch
      let remainingExiting = filter (\n -> not (Array.elem n.id nodesToRemove)) s.exitingNodes
      Ref.write (s { previousNodeIds = s.visibleNodeIds
                   , exitingNodes = remainingExiting
                   }) stateRef
      updateVisualization stateRef

-- | Reset to full dataset
resetToFull :: Ref LesMisGUPState -> Effect Unit
resetToFull stateRef = do
  state <- Ref.read stateRef
  let allIds = map _.id state.fullModel.nodes

  -- Clear exiting nodes and set all as visible
  Ref.write (state { visibleNodeIds = allIds, exitingNodes = [] }) stateRef
  updateVisualization stateRef

  -- After transition, update previousNodeIds
  launchAff_ do
    delay transitionDuration
    liftEffect do
      s <- Ref.read stateRef
      Ref.write (s { previousNodeIds = s.visibleNodeIds }) stateRef

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Create the SVG container structure
renderSVGContainer :: String -> D3v2M Unit
renderSVGContainer containerSelector = do
  container <- select containerSelector

  svg <- appendChild ET.SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "lesmis-gup-svg"
    , class_ "lesmis-gup"
    ] container

  zoomGroup <- appendChild ET.Group [ id_ "lesmis-gup-zoom-group", class_ "zoom-group" ] svg
  _ <- appendChild ET.Group [ id_ "lesmis-gup-links", class_ "links" ] zoomGroup
  _ <- appendChild ET.Group [ id_ "lesmis-gup-nodes", class_ "nodes" ] zoomGroup

  -- Attach zoom behavior
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#lesmis-gup-zoom-group") svg

  pure unit

-- | Update visualization with current visible nodes
-- | Computes enter/update/exit based on previous state
updateVisualization :: Ref LesMisGUPState -> Effect Unit
updateVisualization stateRef = do
  state <- Ref.read stateRef

  -- Get current node positions from simulation
  currentNodes <- Sim.getNodes state.simulation

  -- Compute GUP states
  let visibleNodes = filter (\n -> Array.elem n.id state.visibleNodeIds) currentNodes

  let nodesWithState = map (\n ->
        let gupState = computeGUPState n.id state.previousNodeIds state.visibleNodeIds
        in { node: n, gupState }
      ) visibleNodes

  -- Add exiting nodes (with frozen positions from when they were removed)
  let exitingWithState = map (\n -> { node: n, gupState: Exiting }) state.exitingNodes

  let allNodesWithState = nodesWithState <> exitingWithState

  -- Create links only between visible nodes (not exiting)
  let visibleLinks = filterLinksForNodes state.fullModel.links visibleNodes
  let swizzledLinks = swizzleLinksForGUP visibleNodes visibleLinks false

  -- Create scene data
  let scene = { nodes: allNodesWithState, links: swizzledLinks }

  -- Render with CSS-based GUP
  runD3v2M $ renderGUPScene scene

-- | Compute the GUP state for a node
computeGUPState :: String -> Array String -> Array String -> NodeGUPState
computeGUPState nodeId previousIds currentIds =
  let wasVisible = Array.elem nodeId previousIds
      isVisible = Array.elem nodeId currentIds
  in case wasVisible, isVisible of
    false, true  -> Entering   -- New node
    true,  true  -> Normal     -- Existing node (could be Updating if we want to flash)
    true,  false -> Exiting    -- Being removed
    false, false -> Normal     -- Shouldn't happen

-- | Render scene with CSS-based transitions
renderGUPScene :: SceneData -> D3v2M Unit
renderGUPScene scene = do
  -- Render nodes with GUP classes
  nodesGroup <- select "#lesmis-gup-nodes"
  let nodesTree = createNodesTree scene
  _ <- renderTree nodesGroup nodesTree

  -- Render links
  linksGroup <- select "#lesmis-gup-links"
  let linksTree = createLinksTree scene
  _ <- renderTree linksGroup linksTree

  pure unit

-- | Create nodes tree with CSS-based GUP classes
-- | All colors controlled by CSS classes - no inline fill
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree scene =
  T.sceneNestedJoin "nodes" "circle"
    [scene]
    (_.nodes)
    (\{ node, gupState } -> T.elem ET.Circle
      [ cx node.x
      , cy node.y
      , radius 5.0
      -- No fill here - CSS controls all colors via classes
      , stroke "#fff"
      , strokeWidth 1.5
      , class_ (nodeClassForState gupState)
      ])
    { enterBehavior: Nothing  -- CSS handles transitions
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- | Get the CSS class for a node based on its GUP state
nodeClassForState :: NodeGUPState -> String
nodeClassForState = case _ of
  Entering -> "gup-node gup-node--entering"
  Updating -> "gup-node gup-node--updating"
  Exiting  -> "gup-node gup-node--exiting"
  Normal   -> "gup-node"

-- | Create links tree (simple join, no fancy transitions)
createLinksTree :: SceneData -> T.Tree SceneData
createLinksTree scene =
  T.sceneNestedJoin "links" "line"
    [scene]
    (_.links)
    (\link -> T.elem ET.Line
      [ x1 link.source.x
      , y1 link.source.y
      , x2 link.target.x
      , y2 link.target.y
      , strokeWidth (sqrt link.value)
      , stroke "#999"
      , opacity 0.6
      , class_ (if link.isExiting then "gup-link gup-link--exiting" else "gup-link")
      ])
    { enterBehavior: Nothing  -- CSS handles transitions
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- =============================================================================
-- Simulation Tick Handler
-- =============================================================================

-- | Called on each simulation tick - update positions
-- | Re-renders the entire scene with updated positions from simulation
onSimulationTick :: Ref LesMisGUPState -> Effect Unit
onSimulationTick stateRef = updateVisualization stateRef

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Pick N random elements from an array
pickRandom :: forall a. Int -> Array a -> Effect (Array a)
pickRandom n arr = do
  if n <= 0 || length arr == 0
    then pure []
    else do
      indices <- pickRandomIndices n (length arr) []
      pure $ Array.mapMaybe (\i -> arr !! i) indices

pickRandomIndices :: Int -> Int -> Array Int -> Effect (Array Int)
pickRandomIndices 0 _ acc = pure acc
pickRandomIndices _ 0 acc = pure acc
pickRandomIndices n maxIdx acc = do
  idx <- randomInt 0 (maxIdx - 1)
  if Array.elem idx acc
    then pickRandomIndices n maxIdx acc  -- Try again
    else pickRandomIndices (n - 1) maxIdx (acc <> [idx])

-- | Filter links to only those between visible nodes
filterLinksForNodes :: Array LesMisLink -> Array LesMisNode -> Array LesMisLink
filterLinksForNodes links nodes =
  let nodeIndices = map _.index nodes
  in filter (\l -> Array.elem l.source nodeIndices && Array.elem l.target nodeIndices) links

-- | Swizzle links for GUP (convert indices to node refs)
-- | Uses mapMaybe to filter out any links where endpoints aren't found
swizzleLinksForGUP :: Array LesMisNode -> Array LesMisLink -> Boolean -> Array SwizzledLink
swizzleLinksForGUP nodes links isExiting =
  Array.mapMaybe swizzleLink links
    # Array.mapWithIndex (\i l -> l { index = i })
  where
  swizzleLink :: LesMisLink -> Maybe SwizzledLink
  swizzleLink link = do
    sourceNode <- Array.find (\n -> n.index == link.source) nodes
    targetNode <- Array.find (\n -> n.index == link.target) nodes
    pure { source: sourceNode
         , target: targetNode
         , value: link.value
         , index: 0  -- Will be set by mapWithIndex
         , isExiting
         }
