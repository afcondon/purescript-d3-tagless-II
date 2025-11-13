module D3.Viz.Spago.Draw where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed)
import PSD3.Internal.Types (D3Selection_, D3This_, Datum_, Element(..))
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, svgAttrs)
import D3.Viz.Spago.Model (datum_)
import D3.Viz.Spago.Render (spagoRenderCallbacks)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, on)
import PSD3.Capabilities.Simulation (class SimulationM2, SimulationUpdate)
import PSD3.Simulation.Update (genericUpdateSimulation)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Data.Node (D3_SimulationNode, D3Link_Unswizzled, NodeID)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.CodeExplorer.Actions (VizEvent(..))
import Utility (getWindowWidthHeight)
import Web.Event.Internal.Types (Event)

getVizEventFromClick :: Event -> Datum_ -> D3This_ -> VizEvent
getVizEventFromClick e d t = NodeClick (datum_.nodetype d) (datum_.id d)

-- | Initialize the SVG structure for Spago visualization
-- | Creates the zoom/pan container and group elements for nodes and links
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SimulationM2 D3Selection_ m =>
  SelectionM D3Selection_ m =>
  m { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root  <- attach "div.svg-container"

  svg   <- appendTo root Svg (svgAttrs w h)
  inner <- appendTo svg  Group []
  _     <- inner `on` Drag DefaultDrag
  _     <- svg   `on` Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                           , scale  : ScaleExtent 0.1 4.0
                           , name   : "spago"
                           , target : inner
                           }
  -- Create groups for nodes and links (links first so they render under nodes)
  linksGroup <- appendTo inner Group [ classed "links" ]
  nodesGroup <- appendTo inner Group [ classed "nodes" ]

  pure { nodes: Just nodesGroup, links: Just linksGroup }

-- | Update simulation using the fully generic library updateSimulation
-- |
-- | This is now a thin wrapper that delegates to the library's genericUpdateSimulation
-- | with Spago-specific render callbacks. The library guarantees correct ordering and
-- | state integrity.
-- |
-- | ## What this function does:
-- | 1. Call library's genericUpdateSimulation with Spago render callbacks
-- | 2. That's it! The library handles everything else:
-- |    - SimulationM2 update API (data merging, swizzling, forces)
-- |    - General Update Pattern (enter/update/exit/merge)
-- |    - Tick function registration
-- |
-- | ## What the visualization provides (via spagoRenderCallbacks):
-- | - How to create nodes (Group → Circle + Text)
-- | - How to update nodes
-- | - How to create/update links
-- | - What attributes to apply on tick
-- |
-- | ## Impossible to mess up because:
-- | - Library controls the flow
-- | - Callbacks are declarative ("what" not "when")
-- | - DECLARATIVE API: Provide full datasets + node filter predicate
-- | - Library automatically filters links to match visible nodes
-- | - No way to provide inconsistent data
updateSimulation :: forall m d.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodes :: Maybe D3Selection_
  , links :: Maybe D3Selection_
  } ->
  { allNodes :: Array (D3_SimulationNode d)                                         -- FULL dataset
  , allLinks :: Array D3Link_Unswizzled                                            -- FULL dataset
  , nodeFilter :: D3_SimulationNode d -> Boolean                                   -- Which nodes to show
  , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)                             -- Optional visual filtering (e.g., hide dev dependencies)
  , nodeInitializers :: Array (Array (D3_SimulationNode d) -> Array (D3_SimulationNode d))  -- Tree layout, grid, pinning, etc.
  , activeForces :: Set Label
  , linksWithForce :: Datum_ -> Boolean
  } ->
  SpagoSceneAttributes ->
  m Unit
updateSimulation selections dataConfig attrs =
  genericUpdateSimulation
    selections
    Group  -- Node element type
    Line   -- Link element type
    { allNodes: dataConfig.allNodes           -- Full dataset
    , allLinks: dataConfig.allLinks           -- Full dataset
    , nodeFilter: dataConfig.nodeFilter       -- Single predicate (library filters links automatically!)
    , linkFilter: dataConfig.linkFilter       -- Optional visual filtering (restored!)
    , nodeInitializers: dataConfig.nodeInitializers  -- Passed from scene config
    , activeForces: Just dataConfig.activeForces
    , config: Nothing
    }
    keyIsID_
    attrs
    spagoRenderCallbacks

