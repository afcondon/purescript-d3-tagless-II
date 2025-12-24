-- | Conditional Chimera - First Chimeric Visualization Example
-- |
-- | Demonstrates ConditionalRender: nodes render as different shapes based on
-- | their connectivity (degree). This is the simplest chimeric pattern:
-- | - Hub nodes (degree >= 3): Rendered as large stars
-- | - Bridge nodes (degree == 2): Rendered as diamonds
-- | - Leaf nodes (degree == 1): Rendered as small circles
module D3.Viz.ConditionalChimera
  ( conditionalChimera
  ) where

import Prelude

import Data.Array as Array
import Data.Nullable (null)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Simulation (SimulationNode, SwizzledLink)
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultCenter, defaultLink)
import PSD3.ForceEngine.Links (swizzleLinks)
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, r, fill, stroke, strokeWidth, from, cx, cy)
import PSD3.Internal.Behavior.Types (Behavior(..), DragConfig(..), ScaleExtent(..), defaultZoom)
import PSD3.Internal.Behavior.FFI as BehaviorFFI
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

type Node = SimulationNode (name :: String, degree :: Int)
type Link = { source :: Int, target :: Int }
type SLink = SwizzledLink (name :: String, degree :: Int) (index :: Int)

-- =============================================================================
-- Data - A small network with varied connectivity
-- =============================================================================

nodes :: Array Node
nodes =
  [ { id: 0, name: "Hub", degree: 4, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 1, name: "Bridge1", degree: 2, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 2, name: "Bridge2", degree: 2, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 3, name: "Leaf1", degree: 1, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  , { id: 4, name: "Leaf2", degree: 1, x: 0.0, y: 0.0, vx: 0.0, vy: null, fx: null, fy: null }
  , { id: 5, name: "Leaf3", degree: 1, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
  ]

links :: Array Link
links =
  [ { source: 0, target: 1 }  -- Hub connects to bridge1
  , { source: 0, target: 2 }  -- Hub connects to bridge2
  , { source: 0, target: 3 }  -- Hub connects to leaf1
  , { source: 0, target: 4 }  -- Hub connects to leaf2
  , { source: 1, target: 5 }  -- Bridge1 connects to leaf3
  , { source: 2, target: 5 }  -- Bridge2 connects to leaf3
  ]

-- =============================================================================
-- Chimeric Node Templates
-- =============================================================================

-- Hub nodes: Large red circles
hubTemplate :: Node -> T.Tree Node
hubTemplate node =
  T.elem Circle
    [ r $ num 18.0
    , fill $ text "#ff6b6b"
    , stroke $ text "#c92a2a"
    , strokeWidth $ num 3.0
    ]
  `T.withChildren`
    [ T.elem Text
        [ attr "text-anchor" $ text "middle"
        , attr "dy" $ text "0.3em"
        , attr "font-size" $ text "12px"
        , attr "font-weight" $ text "bold"
        , fill $ text "white"
        , from node $ \n -> attr "textContent" $ text n.name
        ]
    ]

-- Bridge nodes: Medium blue circles
bridgeTemplate :: Node -> T.Tree Node
bridgeTemplate node =
  T.elem Circle
    [ r $ num 12.0
    , fill $ text "#4dabf7"
    , stroke $ text "#1971c2"
    , strokeWidth $ num 2.0
    ]
  `T.withChildren`
    [ T.elem Text
        [ attr "text-anchor" $ text "middle"
        , attr "dy" $ text "0.3em"
        , fill $ text "white"
        , from node $ \n -> attr "textContent" $ text n.name
        ]
    ]

-- Leaf nodes: Small green circles
leafTemplate :: Node -> T.Tree Node
leafTemplate node =
  T.elem Circle
    [ r $ num 8.0
    , fill $ text "#51cf66"
    , stroke $ text "#2f9e44"
    , strokeWidth $ num 1.5
    ]
  `T.withChildren`
    [ T.elem Text
        [ attr "text-anchor" $ text "middle"
        , attr "dy" $ text "0.3em"
        , attr "font-size" $ text "9px"
        , fill $ text "white"
        , from node $ \n -> attr "textContent" $ text n.name
        ]
    ]

-- =============================================================================
-- Visualization
-- =============================================================================

simulationId :: String
simulationId = "conditional-chimera"

conditionalChimera :: String -> Effect (Effect Unit)
conditionalChimera selector = do
  let swizzled = swizzleLinks nodes links \src tgt i _ -> { source: src, target: tgt, index: i }

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodes sim
  Sim.setLinks links sim
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -300.0 }) sim
  Sim.addForce (Collide "collide" defaultCollide { radius = 30.0 }) sim
  Sim.addForce (Center "center" defaultCenter) sim
  Sim.addForce (Link "links" defaultLink { distance = 100.0 }) sim

  -- Register simulation
  BehaviorFFI.registerSimulation_ simulationId (Sim.reheat sim)

  -- Create state ref
  stateRef <- Ref.new { nodes, swizzled }

  -- Render using TreeAPI with ConditionalRender
  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      containerTree :: T.Tree Unit
      containerTree =
        T.named SVG "svg"
          [ width $ num 800.0, height $ num 600.0
          , viewBox (-400.0) (-300.0) 800.0 600.0
          , attr "id" $ text "chimera-svg"
          ]
          `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#chimera-zoom-group" ]
          `T.withChildren`
            [ T.named Group "zoom-group" [ attr "id" $ text "chimera-zoom-group" ]
                `T.withChildren`
                  [ linksGroup swizzled
                  , nodesGroup nodes
                  ]
            ]

    selections <- renderTree container containerTree

    -- Tick function with General Update Pattern
    let
      tick :: Effect Unit
      tick = do
        state <- Ref.read stateRef
        runD3v2M do
          -- Update links (simple position update, no GUP needed)
          case selections."links" of
            Nothing -> pure unit
            Just linksSel -> do
              liftEffect $ Sim.tickGeometry linksSel

          -- Update nodes (GUP with chimeric rendering)
          case selections."nodes" of
            Nothing -> pure unit
            Just nodesSel -> do
              liftEffect $ Sim.tickGeometry nodesSel

    -- Start simulation
    Sim.onTick tick sim
    Sim.restart sim

    pure $ Sim.stop sim

-- =============================================================================
-- Scene Components
-- =============================================================================

linksGroup :: Array SLink -> T.Tree Unit
linksGroup swizzled =
  T.joinData "links" "line" swizzled $ \link ->
    T.elem Line
      [ stroke $ text "#999"
      , strokeWidth $ num 1.5
      , from link $ \l -> attr "x1" $ num l.source.x
      , from link $ \l -> attr "y1" $ num l.source.y
      , from link $ \l -> attr "x2" $ num l.target.x
      , from link $ \l -> attr "y2" $ num l.target.y
      ]

-- THIS IS THE CHIMERA: Nodes render differently based on degree
nodesGroup :: Array Node -> T.Tree Unit
nodesGroup nodeArray =
  T.joinData "nodes" "g" nodeArray $ \node ->
    T.elem Group
      [ from node $ \n -> cx $ num n.x
      , from node $ \n -> cy $ num n.y
      , attr "transform" $ from node (\n -> text $ "translate(" <> show n.x <> "," <> show n.y <> ")")
      ]
      `T.withBehaviors` [ Drag $ SimpleDrag simulationId ]
      `T.withChildren`
        [ -- CONDITIONAL RENDER: Choose template based on degree
          T.conditionalRender
            [ { predicate: \n -> n.degree >= 3
              , spec: hubTemplate
              }
            , { predicate: \n -> n.degree == 2
              , spec: bridgeTemplate
              }
            , { predicate: \n -> n.degree == 1
              , spec: leafTemplate
              }
            ]
        ]
