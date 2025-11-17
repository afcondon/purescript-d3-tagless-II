module D3.Viz.FPFTW.TransitiveReduction where

-- | Transitive Reduction Visualization
-- | Demonstrates graph transformation: removing redundant edges from a DAG
-- | Shows before/after side-by-side to make the transformation visually clear

import Prelude

import Data.Array (elem, length, mapWithIndex, (!!))
import Data.Array as Array
import Data.Graph.Algorithms (SimpleGraph, getAllEdges, getRemovedEdges, transitiveReduction)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PSD3v2.Attribute.Types (class_, cx, cy, fill, fillOpacity, height, radius, stroke, strokeOpacity, strokeWidth, textAnchor, textContent, transform, viewBox, width, x, x1, x2, y, y1, y2)
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_, runD3v2M)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Use library's SimpleGraph for String nodes
type Graph = SimpleGraph String

-- | A positioned node for visualization
type PositionedNode =
  { id :: String
  , x :: Number
  , y :: Number
  }

-- | Example dependency graph with transitive edges
-- | "A depends on B" means edge A -> B
exampleGraph :: Graph
exampleGraph =
  { nodes: ["A", "B", "C", "D", "E"]
  , edges: Map.fromFoldable
      [ Tuple "A" (Set.fromFoldable ["B", "C"])       -- A -> B, A -> C
      , Tuple "B" (Set.fromFoldable ["D"])             -- B -> D
      , Tuple "C" (Set.fromFoldable ["D", "E"])        -- C -> D, C -> E
      , Tuple "D" (Set.fromFoldable ["E"])             -- D -> E
      , Tuple "E" Set.empty                             -- E has no dependencies
      ]
  }

-- | Add a transitive edge A -> D (A -> B -> D already exists)
-- | Add a transitive edge A -> E (A -> C -> E already exists)
exampleGraphWithTransitive :: Graph
exampleGraphWithTransitive =
  { nodes: exampleGraph.nodes
  , edges: Map.fromFoldable
      [ Tuple "A" (Set.fromFoldable ["B", "C", "D", "E"])  -- Added transitive: D, E
      , Tuple "B" (Set.fromFoldable ["D"])
      , Tuple "C" (Set.fromFoldable ["D", "E"])
      , Tuple "D" (Set.fromFoldable ["E"])
      , Tuple "E" Set.empty
      ]
  }

-- | Simple circular layout for nodes using manual calculation
circularLayout :: Number -> Number -> Number -> Array String -> Array PositionedNode
circularLayout centerX centerY circleRadius nodes =
  let
    -- Hardcoded positions for 5 nodes in a circle (avoiding trig functions)
    positions =
      [ { dx: 0.0, dy: -1.0 }      -- Top
      , { dx: 0.95, dy: -0.31 }    -- Top right
      , { dx: 0.59, dy: 0.81 }     -- Bottom right
      , { dx: -0.59, dy: 0.81 }    -- Bottom left
      , { dx: -0.95, dy: -0.31 }   -- Top left
      ]
  in
    mapWithIndex (\i node ->
      case positions !! i of
        Just pos ->
          { id: node
          , x: centerX + circleRadius * pos.dx
          , y: centerY + circleRadius * pos.dy
          }
        Nothing ->
          { id: node
          , x: centerX
          , y: centerY
          }
    ) nodes

-- | Visualize a graph (nodes + edges)
visualizeGraph :: String -> Graph -> Number -> Number -> Number -> Boolean -> Array (Tuple String String) -> T.Tree Unit
visualizeGraph title graph centerX centerY circleRadius highlightRemoved removedEdges =
  let
    positions = circularLayout centerX centerY circleRadius graph.nodes
    posMap = Map.fromFoldable $ positions <#> \p -> Tuple p.id p

    -- Helper to get position
    getPos :: String -> Maybe PositionedNode
    getPos nodeId = Map.lookup nodeId posMap

    -- Render edges
    allEdges = getAllEdges graph
    edgeElements = Array.catMaybes $ allEdges <#> \(Tuple source target) -> do
      sourcePos <- getPos source
      targetPos <- getPos target
      let isRemoved = highlightRemoved && elem (Tuple source target) removedEdges
      pure $ T.elem Line
        [ x1 sourcePos.x
        , y1 sourcePos.y
        , x2 targetPos.x
        , y2 targetPos.y
        , stroke if isRemoved then "#E74C3C" else "#999"
        , strokeWidth if isRemoved then 3.0 else 2.0
        , strokeOpacity if isRemoved then 0.7 else 1.0
        , class_ if isRemoved then "edge removed-edge" else "edge"
        ]

    -- Render nodes
    nodeElements = positions <#> \pos ->
      T.named Group ("node-" <> pos.id)
        []
        `T.withChildren`
          [ T.elem Circle
              [ cx pos.x
              , cy pos.y
              , radius 20.0
              , fill "#4A90E2"
              , stroke "#2E5C8A"
              , strokeWidth 2.0
              , class_ "graph-node"
              ]
          , T.elem Text
              [ x pos.x
              , y (pos.y + 5.0)
              , textContent pos.id
              , textAnchor "middle"
              , fill "#fff"
              , class_ "node-label"
              ]
          ]

    -- Title
    titleElement = T.elem Text
      [ x centerX
      , y (centerY - circleRadius - 30.0)
      , textContent title
      , textAnchor "middle"
      , fill "#333"
      , class_ "graph-title"
      ]
  in
    T.named Group ("graph-" <> title)
      []
      `T.withChildren`
        ([titleElement] <> edgeElements <> nodeElements)

-- | Draw transitive reduction comparison (before and after)
drawTransitiveReduction :: String -> Effect Unit
drawTransitiveReduction containerSelector = runD3v2M do
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  let
    totalWidth = 1000.0
    totalHeight = 400.0
    graphRadius = 120.0

    original = exampleGraphWithTransitive
    reduced = transitiveReduction original
    removedEdges = getRemovedEdges original reduced

    edgeCount original' = Array.length $ getAllEdges original'

    -- Create side-by-side visualization
    vizTree =
      T.named SVG "svg"
        [ width totalWidth
        , height totalHeight
        , viewBox "0 0 1000 400"
        , class_ "transitive-reduction"
        ]
        `T.withChildren`
          [ -- Original graph (left)
            visualizeGraph
              ("Original (" <> show (edgeCount original) <> " edges)")
              original
              250.0
              200.0
              graphRadius
              true
              removedEdges
          , -- Reduced graph (right)
            visualizeGraph
              ("Reduced (" <> show (edgeCount reduced) <> " edges)")
              reduced
              750.0
              200.0
              graphRadius
              false
              []
          , -- Arrow indicating transformation
            T.elem Text
              [ x 500.0
              , y 200.0
              , textContent "→"
              , textAnchor "middle"
              , fill "#666"
              , class_ "transformation-arrow"
              ]
          ]

  _ <- renderTree container vizTree
  pure unit
