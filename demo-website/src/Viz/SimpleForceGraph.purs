-- | Simplest Force Graph Example
-- |
-- | A minimal force-directed graph with hardcoded data.
-- | Uses SelectionM primitives directly - fully typed selections, no TreeAPI.
module D3.Viz.SimpleForceGraph
  ( simpleForceGraph
  ) where

import Prelude

import Data.Nullable (null)
import Effect (Effect)
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Simulation (SimulationNode, SwizzledLink)
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultCenter, defaultLink)
import PSD3.ForceEngine.Links (swizzleLinks)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, viewBox, width, height)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, setAttrs, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, getElementsD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBoundOwns)
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- SimulationNode already includes id :: Int, so just add name for display
type Node = SimulationNode (name :: String)
type Link = { source :: Int, target :: Int }
type SLink = SwizzledLink (name :: String) (index :: Int)

-- =============================================================================
-- Data
-- =============================================================================

nodes :: Array Node
nodes =
  [ { id: 0, name: "A", x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 1, name: "B", x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 2, name: "C", x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 3, name: "D", x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 4, name: "E", x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  ]

links :: Array Link
links =
  [ { source: 0, target: 1 }, { source: 0, target: 2 }, { source: 1, target: 2 }
  , { source: 2, target: 3 }, { source: 3, target: 4 }, { source: 4, target: 0 }
  ]

-- =============================================================================
-- Visualization
-- =============================================================================

simpleForceGraph :: String -> Effect (Effect Unit)
simpleForceGraph selector = do
  let swizzled = swizzleLinks nodes links \src tgt i _ -> { source: src, target: tgt, index: i }

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodes sim
  Sim.setLinks links sim
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -200.0 }) sim
  Sim.addForce (Collide "collide" defaultCollide { radius = 20.0 }) sim
  Sim.addForce (Center "center" defaultCenter) sim
  Sim.addForce (Link "links" defaultLink { distance = 50.0 }) sim

  -- Render DOM structure using SelectionM primitives (no TreeAPI!)
  { nodeSel, linkSel } <- runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Create SVG with viewBox
    svg <- appendChild SVG [ width 400.0, height 300.0, viewBox "-200 -150 400 300" ] container
    g <- appendChild Group [] svg

    -- Create groups for links and nodes
    linksGroup <- appendChild Group [] g
    nodesGroup <- appendChild Group [] g

    -- Render links - appendData returns typed SBoundOwns selection!
    linkSel <- appendData Line swizzled
      [ x1 (_.source.x :: SLink -> Number), y1 (_.source.y :: SLink -> Number)
      , x2 (_.target.x :: SLink -> Number), y2 (_.target.y :: SLink -> Number)
      , stroke "#999", strokeWidth 2.0
      ] linksGroup

    -- Render nodes
    nodeSel <- appendData Circle nodes
      [ cx (_.x :: Node -> Number), cy (_.y :: Node -> Number), radius 10.0
      , fill "#69b3a2", stroke "#fff", strokeWidth 2.0
      ] nodesGroup

    -- Attach zoom to SVG targeting g
    _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "g") svg

    pure { nodeSel, linkSel }

  -- Attach drag
  Core.attachDragWithReheat (getElementsD3v2 nodeSel) (Sim.reheat sim)

  -- Tick handler
  Sim.onTick (tick nodeSel linkSel) sim
  Sim.start sim
  pure (Sim.stop sim)

  where
  tick :: D3v2Selection_ SBoundOwns Element Node -> D3v2Selection_ SBoundOwns Element SLink -> Effect Unit
  tick nodeSel linkSel = runD3v2M do
    _ <- setAttrs [ cx (_.x :: Node -> Number), cy (_.y :: Node -> Number) ] nodeSel
    _ <- setAttrs
           [ x1 (_.source.x :: SLink -> Number), y1 (_.source.y :: SLink -> Number)
           , x2 (_.target.x :: SLink -> Number), y2 (_.target.y :: SLink -> Number)
           ] linkSel
    pure unit
