module D3.Viz.LesMiserablesGUP where

-- | Force-directed graph visualization with General Update Pattern
-- | Based on Mike Bostock's Les Misérables character network
-- | This version demonstrates SimulationM2 update pattern with dynamic filtering
-- | Original: https://observablehq.com/@d3/force-directed-graph
-- | See Acknowledgements page for full credits

import Control.Monad.State (class MonadState)
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
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode)
import PSD3.Shared.ZoomableViewbox (zoomableSVG)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.Internal.Simulation.Types (Step(..))
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, void, ($), (/), (<<<))
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
-- | This demonstrates how clean the library makes updates:
-- | - Just call genericUpdateSimulation with callbacks
-- | - Library handles all the complexity
-- | - Impossible to mess up
updateSimulation :: forall row m d.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodes :: Maybe D3Selection_
  , links :: Maybe D3Selection_
  } ->
  { nodes :: Array (D3_SimulationNode d)
  , links :: Array D3Link_Unswizzled
  , nodeFilter :: Maybe (D3_SimulationNode d -> Boolean)
  , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)
  , activeForces :: Set.Set Label
  } ->
  m Unit
updateSimulation selections dataConfig =
  genericUpdateSimulation
    selections
    Circle  -- Node element type (simple circles)
    Line    -- Link element type
    { nodes: Just dataConfig.nodes
    , links: Just dataConfig.links
    , nodeFilter: dataConfig.nodeFilter
    , linkFilter: dataConfig.linkFilter
    , activeForces: Just dataConfig.activeForces
    , config: Nothing
    , keyFn: keyIsID_
    }
    keyIsID_
    defaultLesMisAttributes
    lesMisRenderCallbacks
-- Snippet_End