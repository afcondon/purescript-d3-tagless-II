-- | Les Misérables with Grid-style Forces (ForceX/Y instead of ManyBody+Link)
-- |
-- | Test to isolate whether Code Explorer's slowdown is due to force type.
-- | Same node count as scale test (1001 nodes), but uses grid forces.
module Viz.LesMis.GUPGridTest
  ( initGridTest
  ) where

import Prelude

import Data.Array as Array
import Data.Traversable (traverse)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null)
import Effect (Effect)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Class.Console (log)
import Data.Map as Map
import Viz.LesMis.Model (LesMisModel, LesMisNode)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Render (GroupId(..), updateCirclePositions)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, width, height, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns)
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- | Extended node type with grid positions
type GridNode =
  { id :: String
  , group :: Int
  , index :: Int
  , x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , fx :: Nullable Number
  , fy :: Nullable Number
  , gridX :: Number  -- Target X position
  , gridY :: Number  -- Target Y position
  , r :: Number      -- Node radius
  }

-- | Row types for the simulation
type GridNodeRow = ( id :: String, group :: Int, fx :: Nullable Number, fy :: Nullable Number, index :: Int, gridX :: Number, gridY :: Number, r :: Number )
type GridLinkRow = ()

type GridSimulation = Sim.Simulation GridNodeRow GridLinkRow

-- | State
type GridTestState =
  { simulation :: GridSimulation
  }

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 900.0

svgHeight :: Number
svgHeight = 600.0

nodesGroupId :: GroupId
nodesGroupId = GroupId "#grid-test-nodes"

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the grid test
-- | Takes the scaled LesMis model and uses ONLY grid forces (no ManyBody, no Link)
initGridTest :: LesMisModel -> String -> Effect (Ref GridTestState)
initGridTest model containerSelector = do
  log "[GridTest] Starting with grid-style forces (no ManyBody, no Link)"
  log $ "[GridTest] Nodes: " <> show (Array.length model.nodes)

  -- Convert LesMis nodes to grid nodes
  -- Assign grid positions based on group (cluster)
  gridNodes <- assignGridPositions model.nodes

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes gridNodes sim
  -- NO LINKS!

  -- Add Grid forces (same as Code Explorer)
  log "[GridTest] Adding grid forces..."

  -- Collision: reads node.r directly, adds 5px padding
  let collideHandle = Core.createCollideGrid 5.0 0.7 1
  _ <- Core.initializeForce collideHandle gridNodes
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  -- ForceX: reads node.gridX directly
  let forceXHandle = Core.createForceXGrid 0.5
  _ <- Core.initializeForce forceXHandle gridNodes
  Ref.modify_ (Map.insert "gridX" forceXHandle) sim.forces

  -- ForceY: reads node.gridY directly
  let forceYHandle = Core.createForceYGrid 0.5
  _ <- Core.initializeForce forceYHandle gridNodes
  Ref.modify_ (Map.insert "gridY" forceYHandle) sim.forces

  -- Get nodes from simulation
  simNodes <- Sim.getNodes sim
  log $ "[GridTest] Sim nodes ready: " <> show (Array.length simNodes)

  -- Render SVG
  _ <- runD3v2M $ renderSVG containerSelector simNodes

  -- Create state
  stateRef <- Ref.new { simulation: sim }

  -- Set up tick callback
  Sim.onTick (onTick stateRef) sim

  -- Start simulation
  Sim.start sim
  log "[GridTest] Simulation started"

  pure stateRef

-- | Assign grid positions to nodes based on their group
-- | Creates a grid layout where groups cluster together
-- | IMPORTANT: Randomize initial positions so nodes animate toward grid!
assignGridPositions :: Array LesMisNode -> Effect (Array GridNode)
assignGridPositions nodes = do
  -- Group nodes by their group field
  -- Place each group in a different region
  let numGroups = 10  -- Les Mis has groups 0-9
  let gridSpacing = 150.0

  traverse (assignNode numGroups gridSpacing) nodes

assignNode :: Int -> Number -> LesMisNode -> Effect GridNode
assignNode numGroups spacing node = do
  -- Calculate grid position based on group
  let groupIdx = node.group `mod` numGroups
  let row = groupIdx / 4
  let col = groupIdx `mod` 4
  let gridX = (toNumber col - 1.5) * spacing
  let gridY = (toNumber row - 1.0) * spacing

  -- IMPORTANT: Start at random position (not grid position!)
  -- This is what creates the animation
  rx <- random
  ry <- random
  let startX = (rx - 0.5) * 600.0  -- Random in [-300, 300]
  let startY = (ry - 0.5) * 400.0  -- Random in [-200, 200]

  pure
    { id: node.id
    , group: node.group
    , index: node.index
    , x: startX      -- Start at random position
    , y: startY
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , gridX: gridX   -- Target grid position
    , gridY: gridY
    , r: 5.0
    }

-- =============================================================================
-- Tick Handler
-- =============================================================================

onTick :: Ref GridTestState -> Effect Unit
onTick _ = do
  -- Just update positions via FFI
  updateCirclePositions nodesGroupId

-- =============================================================================
-- Rendering
-- =============================================================================

renderSVG
  :: String
  -> Array GridNode
  -> D3v2M { nodeSel :: D3v2Selection_ SBoundOwns Element GridNode }
renderSVG containerSelector nodes = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "grid-test-svg"
    , class_ "grid-test"
    ] container

  zoomGroup <- appendChild Group [ id_ "grid-test-zoom-group" ] svg
  nodesGroup <- appendChild Group [ id_ "grid-test-nodes" ] zoomGroup

  -- Bind node data
  nodeSel <- appendData Circle nodes
    [ cx (_.x :: GridNode -> Number)
    , cy (_.y :: GridNode -> Number)
    , radius (_.r :: GridNode -> Number)
    , fill (groupColor :: GridNode -> String)
    , stroke "#fff"
    , strokeWidth 1.0
    ] nodesGroup

  -- Attach zoom
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#grid-test-zoom-group") svg

  pure { nodeSel }

-- | Color by group
groupColor :: GridNode -> String
groupColor n = case n.group `mod` 10 of
  0 -> "#1f77b4"
  1 -> "#ff7f0e"
  2 -> "#2ca02c"
  3 -> "#d62728"
  4 -> "#9467bd"
  5 -> "#8c564b"
  6 -> "#e377c2"
  7 -> "#7f7f7f"
  8 -> "#bcbd22"
  _ -> "#17becf"
