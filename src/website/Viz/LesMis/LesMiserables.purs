module D3.Viz.LesMiserables where

-- | Force-directed graph visualization
-- | Based on Mike Bostock's Les Misérables character network
-- | Original: https://observablehq.com/@d3/force-directed-graph
-- | See Acknowledgements page for full credits

import Control.Monad.State (class MonadState)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, radius, strokeColor, strokeOpacity, strokeWidth, x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.LesMis.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.Viz.LesMiserables.Model (LesMisRawModel)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..))
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, SimVariable(..), Step(..))
import PSD3.Internal.Zoom (ScaleExtent(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, on, setAttributes, simpleJoin)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2, addTickFunction, init, start, update)
import PSD3.Data.Node (D3Link_Swizzled, D3_SimulationNode)
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
drawSimplified :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SimulationM2 D3Selection_ m =>
  Array Force -> Set.Set String -> LesMisRawModel -> Selector D3Selection_ -> m Unit
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
  -- Typed lambda! After simpleJoin, nodesSelection :: D3Selection_ LesMisSimNode
  -- So d is inferred as D3_SimulationNode (LesMisNodeData + ...) which has .group field
  setAttributes nodesSelection [ radius 5.0, fill (\(D3SimNode d) -> d3SchemeCategory10N_ (toNumber d.group)) ]

  linksSelection <- simpleJoin linksGroup Line linksInSim keyIsID_
  -- Typed lambda! linksSelection :: D3Selection_ (D3Link_Swizzled ...)
  -- So d is the swizzled link with .source, .target, .value fields
  setAttributes linksSelection [
      strokeWidth (\d -> sqrt d.value),
      strokeColor (\d -> d3SchemeCategory10N_ (toNumber d.target.group))
    ]

  -- Add tick functions with typed lambdas
  addTickFunction "nodes" $ Step nodesSelection [
      cx (\(D3SimNode d) -> d.x),
      cy (\(D3SimNode d) -> d.y)
    ]
  addTickFunction "links" $ Step linksSelection
      [ x1 (\d -> d.source.x)
      , y1 (\d -> d.source.y)
      , x2 (\d -> d.target.x)
      , y2 (\d -> d.target.y)
      ]

  -- Add drag interaction for nodes
  -- Note: zoom is already configured by zoomableSVG helper
  _ <- nodesSelection `on` Drag (CustomDrag "lesmis" simdrag_)

  -- Start the simulation!
  start
  pure unit
-- Snippet_End