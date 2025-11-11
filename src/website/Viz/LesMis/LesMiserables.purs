module D3.Viz.LesMiserables where

-- | Force-directed graph visualization
-- | Based on Mike Bostock's Les Misérables character network
-- | Original: https://observablehq.com/@d3/force-directed-graph
-- | See Acknowledgements page for full credits

import Control.Monad.State (class MonadState)
import PSD3.Attributes (DatumFn(..))
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, radius, strokeColor, strokeOpacity, strokeWidth, x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.LesMis.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..))
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, SimVariable(..), Step(..))
import PSD3.Internal.Zoom (ScaleExtent(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, on, setAttributes, simpleJoin)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2, addTickFunction, init, start, update)
import PSD3.Data.Node (D3Link_Swizzled, D3_SimulationNode(..))
import PSD3.Shared.ZoomableViewbox (zoomableSVG)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Number (sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, ($), (/), (<<<))
import Utility (getWindowWidthHeight)

-- PHANTOM TYPE SUCCESS: No more datum_/link_ boilerplate needed!
-- With phantom types, the compiler infers datum types from selections.
-- After simpleJoin, lambdas get the correct types automatically.
-- Snippet_Start
-- Name: LesMisAccessors
-- OLD APPROACH (with boilerplate):
{-
link_ = {
    source: _.source <<< unboxD3SimLink
  , target: _.target <<< unboxD3SimLink
  , value:  _.value <<< unboxD3SimLink
  , color:  d3SchemeCategory10N_ <<< toNumber <<< _.target.group <<< unboxD3SimLink
}

datum_ = {
    id    : _.id <<< unboxD3SimNode
  , x     : _.x <<< unboxD3SimNode
  , y     : _.y <<< unboxD3SimNode
  , group : _.group <<< unboxD3SimNode
  , colorByGroup: d3SchemeCategory10N_ <<< toNumber <<< _.group <<< unboxD3SimNode
}
-}
-- NEW APPROACH: Use typed lambdas directly! No accessor records needed.
-- The compiler infers the datum type from the selection.
-- Snippet_End

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
drawSimplified :: forall row d m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ LesMisSimNode | row } m =>
  SimulationM2 D3Selection_ m =>
  Array (Force LesMisSimNode) -> Set.Set String -> LesMisRawModel -> Selector (D3Selection_ d) -> m Unit
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
  -- Use unboxD3SimNode to convert Datum_ -> LesMisSimRecord, wrapped in DatumFn
  setAttributes nodesSelection [ radius 5.0, fill (DatumFn (d3SchemeCategory10N_ <<< toNumber <<< _.group <<< unboxD3SimNode)) ]

  linksSelection <- simpleJoin linksGroup Line linksInSim keyIsID_
  -- Use unboxD3SimLink to convert Datum_ -> swizzled link record, wrapped in DatumFn
  setAttributes linksSelection [
      strokeWidth (DatumFn (sqrt <<< _.value <<< unboxD3SimLink)),
      strokeColor (DatumFn (d3SchemeCategory10N_ <<< toNumber <<< _.target.group <<< unboxD3SimLink))
    ]

  -- Add tick functions using unboxD3SimNode, wrapped in DatumFn
  addTickFunction "nodes" $ Step nodesSelection [
      cx (DatumFn (_.x <<< unboxD3SimNode)),
      cy (DatumFn (_.y <<< unboxD3SimNode))
    ]
  addTickFunction "links" $ Step linksSelection
      [ x1 (DatumFn (_.source.x <<< unboxD3SimLink))
      , y1 (DatumFn (_.source.y <<< unboxD3SimLink))
      , x2 (DatumFn (_.target.x <<< unboxD3SimLink))
      , y2 (DatumFn (_.target.y <<< unboxD3SimLink))
      ]

  -- Add drag interaction for nodes
  -- Note: zoom is already configured by zoomableSVG helper
  _ <- nodesSelection `on` Drag (CustomDrag "lesmis" simdrag_)

  -- Start the simulation!
  start
  pure unit
-- Snippet_End