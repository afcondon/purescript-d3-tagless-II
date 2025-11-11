module D3.Viz.LesMiserablesGUP where

-- | Force-directed graph visualization with General Update Pattern
-- | Based on Mike Bostock's Les Misérables character network
-- | This version demonstrates SimulationM2 update pattern with dynamic filtering
-- | Original: https://observablehq.com/@d3/force-directed-graph
-- | See Acknowledgements page for full credits

import Control.Monad.State (class MonadState)
import Data.Int as Int
import Data.Nullable (Nullable, notNull, null)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, radius, strokeColor, strokeOpacity, strokeWidth, x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.LesMiserablesGUP.Model (LesMisRawModel)
import D3.Viz.LesMiserablesGUP.Render (datum_, defaultLesMisAttributes, lesMisRenderCallbacks, link_)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import PSD3.Internal.Selection.Types (DragBehavior(..))
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Selection.Types (Behavior(..))
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force)
import PSD3.Internal.Zoom (ScaleExtent(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, on, setAttributes, simpleJoin)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2, addTickFunction, init, start)
import PSD3.Simulation.Update (genericUpdateSimulation)
import PSD3.Data.Node (D3Link_Swizzled, D3Link_Unswizzled, D3_SimulationNode(..))
import PSD3.Shared.ZoomableViewbox (zoomableSVG)
import Data.Array (length)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt, ceil, floor, (%))
import Data.Number as Number
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.Internal.Simulation.Types (Step(..))
import Prelude (class Bind, Unit, bind, discard, map, negate, pure, unit, void, ($), (/), (*), (+), (<<<))
import Utility (getWindowWidthHeight)

-- | NOTE: Accessors (link_, datum_) are now in D3.Viz.LesMiserablesGUP.Render
-- | This keeps the visualization code clean and focused on structure, not data access.

-- Snippet_Start
-- Name: LesMisScript
-- | Legacy draw function - kept for reference
-- | This shows the old SimulationM2 API style (now replaced by drawSimplified)
-- | Commented out as the old methods no longer exist
{-
draw :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SimulationM2 D3Selection_ m =>
  LesMisRawModel -> Selector D3Selection_ -> m Unit
draw model selector = do
  -- Old API: setNodes, setLinks, setConfigVariable are no longer available
  -- Use drawSimplified below instead
  pure unit
-}
-- Snippet_End

-- Snippet_Start
-- Name: LesMisSimplified
-- | Simplified version using SimulationM with record-based init
-- | This is the initial draw - sets up the visualization structure
drawSimplified :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SimulationM2 D3Selection_ m =>
  Array Force -> Set.Set String -> LesMisRawModel -> Selector D3Selection_ -> m { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
drawSimplified forceLibrary activeForces model selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  rootSel <- attach selector

  -- Use zoomableSVG helper for consistent zoom/pan behavior
  { svg, zoomGroup } <- zoomableSVG rootSel
    { minX: -w / 2.0
    , minY: -h / 2.0
    , width: w
    , height: h
    , svgClass: "lesmis"
    , innerClass: "zoom-group"
    , innerWidth: w
    , innerHeight: h
    , scaleMin: 1.0  -- No zoom out (100% minimum)
    , scaleMax: 4.0  -- 400% maximum zoom
    }

  linksGroup <- appendTo zoomGroup Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ]
  nodesGroup <- appendTo zoomGroup Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ]

  -- Initialize simulation to get enhanced data back
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: model.nodes
    , links: model.links
    , forces: forceLibrary
    , activeForces: activeForces
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: keyIsID_
    , ticks: Map.fromFoldable []  -- Will add ticks after we have selections
    }

  -- NOW join the simulation-enhanced data to DOM
  nodesSelection <- simpleJoin nodesGroup Circle nodesInSim keyIsID_
  setAttributes nodesSelection [ radius 5.0, fill datum_.colorByGroup ]
  linksSelection <- simpleJoin linksGroup Line linksInSim keyIsID_
  setAttributes linksSelection [ strokeWidth (sqrt <<< link_.value), strokeColor link_.color ]

  -- Add tick functions with the selections (using SimulationM2 for now)
  addTickFunction "nodes" $ Step nodesSelection [ cx datum_.x, cy datum_.y ]
  addTickFunction "links" $ Step linksSelection
      [ x1 (_.x <<< link_.source)
      , y1 (_.y <<< link_.source)
      , x2 (_.x <<< link_.target)
      , y2 (_.y <<< link_.target)
      ]

  -- Add drag interaction for nodes
  -- Note: zoom is already configured by zoomableSVG helper
  _ <- nodesSelection `on` Drag (CustomDrag "lesmis" simdrag_)

  -- Start the simulation!
  start

  -- Return the selections for later updates
  pure { nodes: Just nodesGroup, links: Just linksGroup }
-- Snippet_End

-- Snippet_Start
-- Name: LesMisUpdatePattern
-- | Update the visualization using the generic library updateSimulation
-- |
-- | This demonstrates how clean the DECLARATIVE API makes updates:
-- | - Provide FULL datasets (allNodes, allLinks)
-- | - Provide single node filter predicate
-- | - Library automatically filters links to match
-- | - Impossible to mess up (no way to provide inconsistent data)
updateSimulation :: forall row m r.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ (D3_SimulationNode r) | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodes :: Maybe (D3Selection_ (D3_SimulationNode r))
  , links :: Maybe (D3Selection_ D3Link_Swizzled)
  } ->
  { allNodes :: Array (D3_SimulationNode r)          -- FULL dataset
  , allLinks :: Array D3Link_Unswizzled              -- FULL dataset
  , nodeFilter :: D3_SimulationNode r -> Boolean     -- Which nodes to show
  , activeForces :: Set.Set Label
  } ->
  m Unit
updateSimulation selections dataConfig =
  genericUpdateSimulation
    selections
    Circle  -- Node element type (simple circles)
    Line    -- Link element type
    { allNodes: dataConfig.allNodes           -- Full dataset
    , allLinks: dataConfig.allLinks           -- Full dataset
    , nodeFilter: dataConfig.nodeFilter       -- Single predicate (library filters links automatically!)
    , linkFilter: Nothing                     -- No visual filtering needed (small price for safety)
    , nodeInitializers: []                    -- No initializers needed for simple circles
    , activeForces: Just dataConfig.activeForces
    , config: Nothing
    }
    keyIsID_
    defaultLesMisAttributes
    lesMisRenderCallbacks
-- Snippet_End

-- Snippet_Start
-- Name: LesMisGridLayout
-- | Grid layout helpers for gentle transition to grid positions
-- |
-- | These functions calculate grid positions for nodes and prepare them
-- | for smooth D3 transitions followed by pinning.

type PointXY = { x :: Number, y :: Number }

-- | Calculate grid position from index
numberToGridPoint :: Int -> Int -> PointXY
numberToGridPoint columns i = do
  let
    c = Int.toNumber columns
    d = Int.toNumber i
    x = (d % c)
    y = floor (d / c)
  { x, y }

-- | Scale and offset a point
scaleAndOffset :: Number -> Number -> Number -> Number -> PointXY -> PointXY
scaleAndOffset scaleX scaleY offsetX offsetY { x, y } =
  { x: x * scaleX + offsetX, y: y * scaleY + offsetY }

-- | Calculate grid positions for all nodes with fx/fy set
-- | Returns array of nodes with fx/fy set to grid positions
nodesToGridLayout :: forall r. Array (D3_SimulationNode (fx :: Nullable Number, fy :: Nullable Number | r)) -> Number -> Number -> Array (D3_SimulationNode (fx :: Nullable Number, fy :: Nullable Number | r))
nodesToGridLayout nodes gridSpacing _windowSize =
  Array.mapWithIndex setGridPosition nodes
  where
    nodeCount = length nodes
    -- Calculate square grid dimensions
    columns = Int.floor $ ceil $ sqrt $ Int.toNumber nodeCount
    -- Center the grid
    offset = -(Int.toNumber columns * gridSpacing) / 2.0

    setGridPosition i (D3SimNode node) =
      let gridPt = numberToGridPoint columns i
          finalPt = scaleAndOffset gridSpacing gridSpacing offset offset gridPt
      in D3SimNode (node { fx = notNull finalPt.x, fy = notNull finalPt.y })

-- | Unpin all nodes by setting fx/fy to null
-- | This allows them to be controlled by forces again
unpinAllNodes :: forall r. Array (D3_SimulationNode (fx :: Nullable Number, fy :: Nullable Number | r)) -> Array (D3_SimulationNode (fx :: Nullable Number, fy :: Nullable Number | r))
unpinAllNodes nodes = map unpin nodes
  where
    unpin (D3SimNode node) = D3SimNode (node { fx = null, fy = null })

-- | FFI: Transition nodes to grid positions with smooth D3 animation
-- | Selects elements by class, transitions their transform attribute
-- | Calls completion callback when done
foreign import transitionNodesToGridPositions_
  :: forall d.
     String                        -- SVG class selector
  -> String                        -- Node class selector
  -> String                        -- Link class selector
  -> Array (D3_SimulationNode d)   -- Nodes with target fx/fy set
  -> Effect Unit                   -- Completion callback
  -> Effect Unit

-- Snippet_End