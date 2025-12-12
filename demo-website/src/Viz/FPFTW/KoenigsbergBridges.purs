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
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Simulation (SimulationNode, SwizzledLink)
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultCenter, defaultLink)
import PSD3.ForceEngine.Links (swizzleLinks)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, viewBox, width, height, x, y, textContent, textAnchor, fontSize, id_)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- SimulationNode includes id :: Int, so we add name and degree
type Node = SimulationNode (name :: String, degree :: Int)
type Link = { source :: Int, target :: Int }
type SLink = SwizzledLink (name :: String, degree :: Int) (index :: Int)

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
  [ { id: 0, name: "North", degree: 3, x: 0.0, y: -80.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 1, name: "South", degree: 3, x: 0.0, y: 80.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 2, name: "Island", degree: 5, x: -80.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 3, name: "East", degree: 3, x: 80.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
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

  -- State ref for tick updates
  stateRef <- Ref.new { nodes, swizzled }

  -- Render initial DOM using TreeAPI
  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    let containerTree :: T.Tree Unit
        containerTree =
          T.named SVG "svg"
            [ width 400.0, height 300.0, viewBox "-200 -150 400 300" ]
            `T.withChildren`
              [ T.elem Group []
                  `T.withChildren`
                    [ T.named Group "links" [ id_ "koenigsberg-links" ]
                    , T.named Group "nodes" [ id_ "koenigsberg-nodes" ]
                    , T.named Group "labels" [ id_ "koenigsberg-labels" ]
                    ]
              ]
    _ <- renderTree container containerTree
    pure unit

  -- Tick handler
  Sim.onTick (tick stateRef) sim
  Sim.start sim
  pure (Sim.stop sim)

-- | Tick handler - renders with current positions
tick :: Ref.Ref { nodes :: Array Node, swizzled :: Array SLink } -> Effect Unit
tick stateRef = runD3v2M do
  state <- liftEffect $ Ref.read stateRef

  -- Select groups
  linksGroup <- select "#koenigsberg-links" :: _ (D3v2Selection_ SEmpty Element Unit)
  nodesGroup <- select "#koenigsberg-nodes" :: _ (D3v2Selection_ SEmpty Element Unit)
  labelsGroup <- select "#koenigsberg-labels" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Render links (bridges)
  let linksTree = T.joinData "links" "line" state.swizzled $ \_ ->
        T.elem Line
          [ x1 (_.source.x :: SLink -> Number)
          , y1 (_.source.y :: SLink -> Number)
          , x2 (_.target.x :: SLink -> Number)
          , y2 (_.target.y :: SLink -> Number)
          , stroke "#8B4513" -- Brown for bridges
          , strokeWidth 4.0
          ]
  _ <- renderTree linksGroup linksTree

  -- Render nodes (land masses) - size by degree
  let nodesTree = T.joinData "nodes" "circle" state.nodes $ \_ ->
        T.elem Circle
          [ cx (_.x :: Node -> Number)
          , cy (_.y :: Node -> Number)
          , radius ((\n -> 12.0 + toNumber n.degree * 3.0) :: Node -> Number)
          , fill "#4a7c59" -- Green for land
          , stroke "#2d4f35"
          , strokeWidth 2.0
          ]
  _ <- renderTree nodesGroup nodesTree

  -- Render labels
  let labelsTree = T.joinData "labels" "text" state.nodes $ \_ ->
        T.elem Text
          [ x (_.x :: Node -> Number)
          , y ((_.y >>> (_ + 4.0)) :: Node -> Number)
          , textContent (_.name :: Node -> String)
          , textAnchor "middle"
          , fill "#fff"
          , fontSize 10.0
          ]
  _ <- renderTree labelsGroup labelsTree

  pure unit
