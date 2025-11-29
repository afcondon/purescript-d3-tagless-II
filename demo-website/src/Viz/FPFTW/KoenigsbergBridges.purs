-- | Königsberg Bridges Visualization
-- |
-- | The famous problem that gave birth to graph theory: Euler proved in 1736
-- | that it's impossible to walk through the city crossing each bridge exactly once.
-- | The graph has 4 nodes (land masses) and 7 edges (bridges).
-- |
-- | A graph has an Eulerian path iff it has at most 2 nodes with odd degree.
-- | Königsberg has 4 nodes with odd degree (all of them!), so no Eulerian path exists.
module D3.Viz.FPFTW.KoenigsbergBridges where

import Prelude

import Data.Int (toNumber)
import Data.Nullable (null)
import Effect (Effect)
import PSD3.Data.Node (SimulationNode, SwizzledLink)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultCenter, defaultLink, swizzleLinks)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, viewBox, width, height, x, y, textContent, textAnchor, fontSize)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, setAttrs)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBoundOwns)
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

type Node = SimulationNode (id :: String, name :: String, degree :: Int)
type Link = { source :: Int, target :: Int }
type SLink = SwizzledLink (id :: String, name :: String, degree :: Int) (index :: Int)

-- =============================================================================
-- Data: The Königsberg Graph
-- =============================================================================

-- | The four land masses of Königsberg
-- | - North bank (degree 3: bridges to Island x2, East)
-- | - South bank (degree 3: bridges to Island x2, East)
-- | - Island (degree 5: bridges to North x2, South x2, East)
-- | - East bank (degree 3: bridges to North, South, Island)
nodes :: Array Node
nodes =
  [ { id: "N", name: "North", degree: 3, x: 0.0, y: -80.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: "S", name: "South", degree: 3, x: 0.0, y: 80.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: "I", name: "Island", degree: 5, x: -80.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: "E", name: "East", degree: 3, x: 80.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  ]

-- | The seven bridges
-- | 2 bridges from North to Island
-- | 2 bridges from South to Island
-- | 1 bridge from North to East
-- | 1 bridge from South to East
-- | 1 bridge from Island to East
links :: Array Link
links =
  [ { source: 0, target: 2 } -- North - Island (bridge 1)
  , { source: 0, target: 2 } -- North - Island (bridge 2)
  , { source: 1, target: 2 } -- South - Island (bridge 3)
  , { source: 1, target: 2 } -- South - Island (bridge 4)
  , { source: 0, target: 3 } -- North - East (bridge 5)
  , { source: 1, target: 3 } -- South - East (bridge 6)
  , { source: 2, target: 3 } -- Island - East (bridge 7)
  ]

-- =============================================================================
-- Visualization
-- =============================================================================

-- | Draw the Königsberg bridges graph
-- | Returns cleanup function to stop simulation
drawKoenigsbergBridges :: String -> Effect (Effect Unit)
drawKoenigsbergBridges selector = do
  let swizzled = swizzleLinks nodes links \src tgt i _ -> { source: src, target: tgt, index: i }

  -- Create simulation with gentle forces
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodes sim
  Sim.setLinks links sim
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -300.0 }) sim
  Sim.addForce (Collide "collide" defaultCollide { radius = 35.0 }) sim
  Sim.addForce (Center "center" defaultCenter) sim
  Sim.addForce (Link "links" defaultLink { distance = 80.0 }) sim

  -- Render DOM using SelectionM primitives
  { nodeSel, linkSel, labelSel } <- runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    svg <- appendChild SVG
      [ width 400.0
      , height 300.0
      , viewBox "-200 -150 400 300"
      ]
      container
    g <- appendChild Group [] svg

    linksGroup <- appendChild Group [] g
    nodesGroup <- appendChild Group [] g
    labelsGroup <- appendChild Group [] g

    -- Links (bridges) - offset duplicate links slightly
    linkSel <- appendData Line swizzled
      [ x1 (_.source.x :: SLink -> Number)
      , y1 (_.source.y :: SLink -> Number)
      , x2 (_.target.x :: SLink -> Number)
      , y2 (_.target.y :: SLink -> Number)
      , stroke "#8B4513" -- Brown for bridges
      , strokeWidth 4.0
      ]
      linksGroup

    -- Nodes (land masses) - size by degree
    nodeSel <- appendData Circle nodes
      [ cx (_.x :: Node -> Number)
      , cy (_.y :: Node -> Number)
      , radius ((\n -> 12.0 + toNumber n.degree * 3.0) :: Node -> Number)
      , fill "#4a7c59" -- Green for land
      , stroke "#2d4f35"
      , strokeWidth 2.0
      ]
      nodesGroup

    -- Labels
    labelSel <- appendData Text nodes
      [ x (_.x :: Node -> Number)
      , y ((_.y >>> (_ + 4.0)) :: Node -> Number)
      , textContent (_.name :: Node -> String)
      , textAnchor "middle"
      , fill "#fff"
      , fontSize 10.0
      ]
      labelsGroup

    pure { nodeSel, linkSel, labelSel }

  -- Tick handler
  Sim.onTick (tick nodeSel linkSel labelSel) sim
  Sim.start sim
  pure (Sim.stop sim)

  where

  tick
    :: D3v2Selection_ SBoundOwns Element Node
    -> D3v2Selection_ SBoundOwns Element SLink
    -> D3v2Selection_ SBoundOwns Element Node
    -> Effect Unit
  tick nodeSel linkSel labelSel = runD3v2M do
    _ <- setAttrs [ cx (_.x :: Node -> Number), cy (_.y :: Node -> Number) ] nodeSel
    _ <- setAttrs
      [ x1 (_.source.x :: SLink -> Number)
      , y1 (_.source.y :: SLink -> Number)
      , x2 (_.target.x :: SLink -> Number)
      , y2 (_.target.y :: SLink -> Number)
      ]
      linkSel
    _ <- setAttrs
      [ x (_.x :: Node -> Number)
      , y ((_.y >>> (_ + 4.0)) :: Node -> Number)
      ]
      labelSel
    pure unit
