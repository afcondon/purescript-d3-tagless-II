module D3.Viz.Spago.Draw where

import Prelude

import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, svgAttrs)
import D3.Viz.Spago.Files (SpagoLink)
import D3.Viz.Spago.Model (SpagoSimNode)
import D3.Viz.Spago.Render (spagoRenderCallbacks)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Types (Datum_)
import Unsafe.Coerce (unsafeCoerce)
import PSD3v2.Attribute.Types (class_)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (class SelectionM, appendChild, select, on)
import PSD3v2.Capabilities.Simulation (class SimulationM2)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SEmpty)
import PSD3v2.Simulation.Update (genericUpdateSimulation)
import Web.DOM.Element (Element)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Utility (getWindowWidthHeight)

-- | Initialize the SVG structure for Spago visualization
-- | Creates the zoom/pan container and group elements for nodes and links
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SelectionM D3v2Selection_ m =>
  m { nodes :: D3v2Selection_ SEmpty Element SpagoSimNode, links :: D3v2Selection_ SEmpty Element SpagoSimNode }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root <- select "div.svg-container"

  svg   <- appendChild SVG (svgAttrs w h) root
  inner <- appendChild Group [] svg
  _     <- on (Drag defaultDrag) inner
  _     <- on (Zoom (defaultZoom (ScaleExtent 0.1 4.0) "g")) svg

  -- Create groups for nodes and links (links first so they render under nodes)
  linksGroup <- appendChild Group [ class_ "links" ] inner
  nodesGroup <- appendChild Group [ class_ "nodes" ] inner

  -- Return empty selections - data will be bound by genericUpdateSimulation
  pure { nodes: nodesGroup, links: linksGroup }

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
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  { nodes :: D3v2Selection_ SEmpty Element SpagoSimNode
  , links :: D3v2Selection_ SEmpty Element SpagoSimNode
  } ->
  { allNodes :: Array SpagoSimNode                                         -- FULL dataset
  , allLinks :: Array SpagoLink                                            -- FULL dataset (typed!)
  , nodeFilter :: SpagoSimNode -> Boolean                                 -- Which nodes to show
  , linkFilter :: Maybe (SpagoLink -> Boolean)                             -- Optional visual filtering (e.g., hide dev dependencies)
  , nodeInitializers :: Array (Array SpagoSimNode -> Array SpagoSimNode)  -- Tree layout, grid, pinning, etc.
  , activeForces :: Set Label
  , linksWithForce :: SpagoLink -> Boolean                                 -- Which links exert force (now typed!)
  } ->
  SpagoSceneAttributes ->
  m Unit
updateSimulation selections dataConfig attrs =
  genericUpdateSimulation
    { nodes: selections.nodes
    , links: unsafeCoerce selections.links  -- Cast SpagoSimNode datum to D3Link_Swizzled datum for links
    }
    Group  -- Node element type (groups containing circle + text children)
    Path    -- Link element type (paths for morphable bezier/diagonal links)
    { allNodes: dataConfig.allNodes           -- Full dataset
    , allLinks: dataConfig.allLinks           -- Full dataset
    , nodeFilter: dataConfig.nodeFilter       -- Single predicate (library filters links automatically!)
    , linkFilter: dataConfig.linkFilter       -- Optional visual filtering (restored!)
    , nodeInitializers: dataConfig.nodeInitializers  -- Passed from scene config
    , activeForces: Just dataConfig.activeForces
    , config: Nothing
    }
    (\n -> n.id)  -- Node key function (integer id matches link source/target)
    keyIsID_  -- Link key function (unused - library uses swizzledLinkKey_)
    attrs
    spagoRenderCallbacks

