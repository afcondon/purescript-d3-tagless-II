-- | Simplest Force Graph Example
-- |
-- | A minimal force-directed graph with hardcoded data.
-- | Uses TreeAPI with declarative behaviors.
module D3.Viz.SimpleForceGraph
  ( simpleForceGraph
  ) where

import Prelude

import Data.Nullable (null)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Simulation (SimulationNode, SwizzledLink)
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultCenter, defaultLink)
import PSD3.ForceEngine.Links (swizzleLinks)
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Behavior.Types (Behavior(..), DragConfig(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Behavior.FFI as BehaviorFFI
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
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
-- Constants
-- =============================================================================

simulationId :: String
simulationId = "simple-force"

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

  -- Register simulation for declarative drag
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- Create state ref for tick updates
  stateRef <- Ref.new { nodes, swizzled }

  -- Render initial DOM structure using TreeAPI
  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    let containerTree :: T.Tree Unit
        containerTree =
          T.named SVG "svg"
            [ v3Attr "width" (lit 400.0), v3Attr "height" (lit 300.0), v3AttrStr "viewBox" (str "-200 -150 400 300"), v3AttrStr "id" (str "simple-force-svg") ]
            `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#simple-force-zoom-group" ]
            `T.withChildren`
              [ T.named Group "zoom-group" [ v3AttrStr "id" (str "simple-force-zoom-group") ]
                  `T.withChildren`
                    [ T.named Group "links-group" [ v3AttrStr "id" (str "simple-force-links") ]
                    , T.named Group "nodes-group" [ v3AttrStr "id" (str "simple-force-nodes") ]
                    ]
              ]
    _ <- renderTree container containerTree
    pure unit

  -- Tick handler renders with TreeAPI
  Sim.onTick (tick stateRef selector) sim
  Sim.start sim

  -- Return cleanup
  pure do
    Sim.stop sim
    BehaviorFFI.unregisterSimulation_ simulationId

-- | Tick handler - renders nodes and links with current positions
tick :: Ref.Ref { nodes :: Array Node, swizzled :: Array SLink } -> String -> Effect Unit
tick stateRef _selector = runD3v2M do
  state <- liftEffect $ Ref.read stateRef

  -- Select the groups
  linksGroup <- select "#simple-force-links" :: _ (D3v2Selection_ SEmpty Element Unit)
  nodesGroup <- select "#simple-force-nodes" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Render links
  let linksTree = T.joinData "links" "line" state.swizzled $ \_ ->
        T.elem Line
          [ v3AttrFn "x1" (_.source.x :: SLink -> Number), v3AttrFn "y1" (_.source.y :: SLink -> Number)
          , v3AttrFn "x2" (_.target.x :: SLink -> Number), v3AttrFn "y2" (_.target.y :: SLink -> Number)
          , v3AttrStr "stroke" (str "#999"), v3Attr "stroke-width" (lit 2.0)
          ]
  _ <- renderTree linksGroup linksTree

  -- Render nodes with drag behavior
  let nodesTree = T.joinData "nodes" "circle" state.nodes $ \_ ->
        T.elem Circle
          [ v3AttrFn "cx" (_.x :: Node -> Number), v3AttrFn "cy" (_.y :: Node -> Number), v3Attr "r" (lit 10.0)
          , v3AttrStr "fill" (str "#69b3a2"), v3AttrStr "stroke" (str "#fff"), v3Attr "stroke-width" (lit 2.0)
          ]
          `T.withBehaviors` [ Drag (SimulationDrag simulationId) ]
  _ <- renderTree nodesGroup nodesTree

  pure unit
