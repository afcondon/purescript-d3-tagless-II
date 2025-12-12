-- | Les Misérables Force-Directed Graph (V3 Architecture)
-- |
-- | A clean, simple force-directed graph using the ForceEngine library.
-- | Uses TreeAPI with declarative behaviors for rendering.
-- |
-- | NO FFI in this demo - all D3 interaction goes through library modules.
module D3.Viz.LesMisV3.Draw
  ( startLesMis
  ) where

import Prelude

import D3.Viz.LesMisV3.Model (LesMisModel, LesMisNode)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)
import PSD3.ForceEngine.Links (swizzleLinks)
import PSD3.Scale (schemeCategory10At)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.FFI as BehaviorFFI
import PSD3v2.Behavior.Types (Behavior(..), DragConfig(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
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

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 900.0

svgHeight :: Number
svgHeight = 600.0

-- | Simulation ID for the registry
-- | Separate from GUPDemo to avoid interference
simulationId :: String
simulationId = "lesmis-main"

-- =============================================================================
-- Entry Point
-- =============================================================================

-- | Clone a node to create an independent copy
-- | This prevents mutations (like fx/fy from drag) from affecting other simulations
cloneNode :: LesMisNode -> LesMisNode
cloneNode n = { id: n.id, name: n.name, group: n.group, x: n.x, y: n.y, vx: n.vx, vy: n.vy, fx: n.fx, fy: n.fy }

-- | Start the Les Misérables force-directed graph
-- | Returns a cleanup function to stop the simulation
startLesMis :: LesMisModel -> String -> Effect (Effect Unit)
startLesMis model containerSelector = do
  -- Clone nodes to have independent data from other visualizations
  let clonedNodes = map cloneNode model.nodes

  -- Swizzle links using cloned nodes
  let swizzledLinks = swizzleLinks clonedNodes model.links \src tgt i link ->
        { source: src, target: tgt, index: i, value: link.value }

  -- Create simulation using library API
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes clonedNodes sim
  Sim.setLinks model.links sim

  -- Register simulation for declarative drag
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- Add forces using declarative ForceSpec
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Create state ref for tick updates
  stateRef <- Ref.new { nodes: clonedNodes, links: swizzledLinks }

  -- Render initial SVG structure using TreeAPI
  runD3v2M do
    container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)
    let containerTree :: T.Tree Unit
        containerTree =
          T.named SVG "svg"
            [ width svgWidth
            , height svgHeight
            , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
            , id_ "lesmis-v3-svg"
            , class_ "lesmis-v3"
            ]
            `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#lesmis-zoom-group" ]
            `T.withChildren`
              [ T.named Group "zoom-group" [ id_ "lesmis-zoom-group", class_ "zoom-group" ]
                  `T.withChildren`
                    [ T.named Group "links" [ id_ "lesmis-links", class_ "links" ]
                    , T.named Group "nodes" [ id_ "lesmis-nodes", class_ "nodes" ]
                    ]
              ]
    _ <- renderTree container containerTree
    pure unit

  -- Set tick callback to update DOM using TreeAPI
  Sim.onTick (updateDOM stateRef) sim

  -- Start simulation
  Sim.start sim

  -- Return cleanup function (also unregisters simulation)
  pure do
    Sim.stop sim
    BehaviorFFI.unregisterSimulation_ simulationId

-- =============================================================================
-- DOM Updates (on each tick) - using TreeAPI
-- =============================================================================

-- | Update DOM positions based on current node positions
-- | Called on each simulation tick
updateDOM :: Ref.Ref { nodes :: Array LesMisNode, links :: Array SwizzledLink } -> Effect Unit
updateDOM stateRef = runD3v2M do
  state <- liftEffect $ Ref.read stateRef

  -- Select the groups
  linksGroup <- select "#lesmis-links" :: _ (D3v2Selection_ SEmpty Element Unit)
  nodesGroup <- select "#lesmis-nodes" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Render links
  let linksTree = T.joinData "links" "line" state.links $ \_ ->
        T.elem Line
          [ x1 (_.source.x :: SwizzledLink -> Number)
          , y1 (_.source.y :: SwizzledLink -> Number)
          , x2 (_.target.x :: SwizzledLink -> Number)
          , y2 (_.target.y :: SwizzledLink -> Number)
          , strokeWidth ((\link -> sqrt link.value) :: SwizzledLink -> Number)
          , stroke "#999"
          , opacity 0.6
          ]
  _ <- renderTree linksGroup linksTree

  -- Render nodes with drag behavior
  let nodesTree = T.joinData "nodes" "circle" state.nodes $ \_ ->
        T.elem Circle
          [ cx (_.x :: LesMisNode -> Number)
          , cy (_.y :: LesMisNode -> Number)
          , radius 5.0
          , fill ((\node -> schemeCategory10At node.group) :: LesMisNode -> String)
          , stroke "#fff"
          , strokeWidth 1.5
          ]
          `T.withBehaviors` [ Drag (SimulationDrag simulationId) ]
  _ <- renderTree nodesGroup nodesTree

  pure unit
