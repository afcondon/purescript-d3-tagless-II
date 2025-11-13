module D3.Viz.Spago.Draw where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed)
import PSD3.Internal.Types (D3Selection_, D3This_, Datum_, Element(..))
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, svgAttrs)
import D3.Viz.Spago.Model (SpagoSimNode)
import D3.Viz.Spago.Render (spagoRenderCallbacks)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, on)
import PSD3.Capabilities.Simulation (class SimulationM2, SimulationUpdate)
import PSD3.Simulation.Update (genericUpdateSimulation)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Data.Node (D3Link_Unswizzled, D3Link_Swizzled, NodeID)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.CodeExplorer.Actions (VizEvent(..))
import Utility (getWindowWidthHeight)
import Web.Event.Internal.Types (Event)
import Unsafe.Coerce (unsafeCoerce)

getVizEventFromClick :: Event -> Datum_ -> D3This_ -> VizEvent
getVizEventFromClick e d t =
  let node = unsafeCoerce d :: SpagoSimNode
  in NodeClick node.nodetype node.id

-- | Initialize the SVG structure for Spago visualization
-- | Creates the zoom/pan container and group elements for nodes and links
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SimulationM2 D3Selection_ m =>
  SelectionM D3Selection_ m =>
  m { nodes :: Maybe (D3Selection_ SpagoSimNode), links :: Maybe (D3Selection_ SpagoSimNode) }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root :: D3Selection_ Unit <- attach "div.svg-container"

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

  -- SAFE: Cast untyped selections to expected types - data will be bound by genericUpdateSimulation
  pure { nodes: Just (unsafeCoerce nodesGroup), links: Just (unsafeCoerce linksGroup) }

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
updateSimulation :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodes :: Maybe (D3Selection_ SpagoSimNode)
  , links :: Maybe (D3Selection_ SpagoSimNode)  -- Unified type for library compatibility, cast internally
  } ->
  { allNodes :: Array SpagoSimNode                                         -- FULL dataset
  , allLinks :: Array D3Link_Unswizzled                                   -- FULL dataset
  , nodeFilter :: SpagoSimNode -> Boolean                                 -- Which nodes to show
  , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)                    -- Optional visual filtering (e.g., hide dev dependencies)
  , nodeInitializers :: Array (Array SpagoSimNode -> Array SpagoSimNode)  -- Tree layout, grid, pinning, etc.
  , activeForces :: Set Label
  , linksWithForce :: Datum_ -> Boolean
  } ->
  SpagoSceneAttributes ->
  m Unit
updateSimulation selections dataConfig attrs =
  genericUpdateSimulation
    { nodes: selections.nodes
    , links: unsafeCoerce selections.links  -- Cast back to D3Selection_ D3Link_Swizzled for genericUpdateSimulation
    }
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
    (\n -> n.id)  -- Node key function
    keyIsID_  -- Link key function (links also have index property)
    attrs
    spagoRenderCallbacks

