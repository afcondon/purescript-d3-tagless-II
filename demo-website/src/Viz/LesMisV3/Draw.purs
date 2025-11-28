-- | Les Misérables Force-Directed Graph (V3 Architecture)
-- |
-- | A clean, simple force-directed graph using the ForceEngine library.
-- | Uses direct SelectionM primitives for rendering (no TreeAPI).
-- |
-- | NO FFI in this demo - all D3 interaction goes through library modules.
module D3.Viz.LesMisV3.Draw
  ( startLesMis
  ) where

import Prelude

import D3.Viz.LesMisV3.Model (LesMisModel, LesMisNode)
import Data.Int (toNumber)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter, swizzleLinks)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, setAttrs, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, getElementsD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBoundOwns)
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- | Swizzled link (source/target are node references, not indices)
type SwizzledLink =
  { source :: LesMisNode
  , target :: LesMisNode
  , value :: Number
  , index :: Int
  }

-- | State for the visualization
type VizState =
  { nodesSel :: D3v2Selection_ SBoundOwns Element LesMisNode
  , linksSel :: D3v2Selection_ SBoundOwns Element SwizzledLink
  }

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 900.0

svgHeight :: Number
svgHeight = 600.0

-- =============================================================================
-- Entry Point
-- =============================================================================

-- | Start the Les Misérables force-directed graph
-- | Returns a cleanup function to stop the simulation
startLesMis :: LesMisModel -> String -> Effect (Effect Unit)
startLesMis model containerSelector = do
  -- Swizzle links (replace indices with node references)
  let swizzledLinks = swizzleLinks model.nodes model.links \src tgt i link ->
        { source: src, target: tgt, index: i, value: link.value }

  -- Create simulation using library API
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes model.nodes sim
  Sim.setLinks model.links sim

  -- Add forces using declarative ForceSpec
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Render initial DOM and get selections for updates
  vizStateRef <- renderInitialDOM containerSelector model.nodes swizzledLinks

  -- Attach drag behavior to nodes with simulation reheat
  state <- Ref.read vizStateRef
  Core.attachDragWithReheat (getElementsD3v2 state.nodesSel) (Sim.reheat sim)

  -- Set tick callback to update DOM
  Sim.onTick (updateDOM vizStateRef) sim

  -- Start simulation
  Sim.start sim

  -- Return cleanup function
  pure (Sim.stop sim)

-- =============================================================================
-- DOM Rendering (using direct SelectionM - no TreeAPI)
-- =============================================================================

-- | Render the initial SVG structure and elements
-- | Returns a ref containing selections for later updates
renderInitialDOM :: String -> Array LesMisNode -> Array SwizzledLink -> Effect (Ref VizState)
renderInitialDOM containerSelector nodes swizzledLinks = runD3v2M do
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Build SVG structure
  svg <- appendChild SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "lesmis-v3-svg"
    , class_ "lesmis-v3"
    ] container

  zoomGroup <- appendChild Group [ id_ "lesmis-zoom-group", class_ "zoom-group" ] svg
  linksGroup <- appendChild Group [ class_ "links" ] zoomGroup
  nodesGroup <- appendChild Group [ class_ "nodes" ] zoomGroup

  -- Render links
  linksSel <- appendData Line swizzledLinks
    [ x1 (_.source.x :: SwizzledLink -> Number)
    , y1 (_.source.y :: SwizzledLink -> Number)
    , x2 (_.target.x :: SwizzledLink -> Number)
    , y2 (_.target.y :: SwizzledLink -> Number)
    , strokeWidth ((\link -> sqrt link.value) :: SwizzledLink -> Number)
    , stroke "#999"
    , opacity 0.6
    ] linksGroup

  -- Render nodes
  nodesSel <- appendData Circle nodes
    [ cx (_.x :: LesMisNode -> Number)
    , cy (_.y :: LesMisNode -> Number)
    , radius 5.0
    , fill ((\node -> d3SchemeCategory10N_ (toNumber node.group)) :: LesMisNode -> String)
    , stroke "#fff"
    , strokeWidth 1.5
    ] nodesGroup

  -- Attach zoom behavior
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#lesmis-zoom-group") svg

  -- Create state ref
  liftEffect $ Ref.new { nodesSel, linksSel }

-- =============================================================================
-- DOM Updates (on each tick)
-- =============================================================================

-- | Update DOM positions based on current node positions
-- | Called on each simulation tick
updateDOM :: Ref VizState -> Effect Unit
updateDOM vizStateRef = runD3v2M do
  state <- liftEffect $ Ref.read vizStateRef
  _ <- setAttrs [ cx (_.x :: LesMisNode -> Number), cy (_.y :: LesMisNode -> Number) ] state.nodesSel
  _ <- setAttrs
         [ x1 (_.source.x :: SwizzledLink -> Number), y1 (_.source.y :: SwizzledLink -> Number)
         , x2 (_.target.x :: SwizzledLink -> Number), y2 (_.target.y :: SwizzledLink -> Number)
         ] state.linksSel
  pure unit
