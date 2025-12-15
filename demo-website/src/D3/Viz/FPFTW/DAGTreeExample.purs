-- | DAG Tree Example Visualization
-- |
-- | Demonstrates the DAGTree library feature: tree layout with extra links.
-- | Shows a vertical tree with cross-layer links in orange.
module D3.Viz.FPFTW.DAGTreeExample
  ( drawDAGTreeExample
  ) where

import Prelude

import Data.List as List
import Data.Tree (Tree, mkTree)
import Effect (Effect)
import PSD3.Data.DAGTree (DAGLink, dagTree, addLinks, layoutDAGTree)
import PSD3.Data.Tree (TreeLayout(..))
-- v3 Integration: all attributes via v3Attr/v3AttrStr (no ToAttr typeclass)
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- | A simple node with id and label
type DAGNode =
  { id :: String
  , label :: String
  , layer :: Int  -- For display purposes
  }

-- | Positioned node after layout
type PositionedNode = { datum :: DAGNode, x :: Number, y :: Number, depth :: Int }

-- | Tree link
type TreeLink = { source :: PositionedNode, target :: PositionedNode }

-- | Extra link with type
type ExtraLink = { source :: PositionedNode, target :: PositionedNode, linkType :: String }

-- =============================================================================
-- Sample Data
-- =============================================================================

-- | Example tree structure:
-- |
-- |          A (root)
-- |         / \
-- |        B   C
-- |       / \   \
-- |      D   E   F
-- |         / \
-- |        G   H
-- |
-- | Extra links (cross-layer, shown in orange):
-- |   A -> E  (skip layer)
-- |   C -> G  (cross-branch, skip layer)
-- |   B -> F  (cross-branch)

sampleTree :: Tree DAGNode
sampleTree =
  mkTree { id: "A", label: "A", layer: 0 } $ List.fromFoldable
    [ mkTree { id: "B", label: "B", layer: 1 } $ List.fromFoldable
        [ mkTree { id: "D", label: "D", layer: 2 } List.Nil
        , mkTree { id: "E", label: "E", layer: 2 } $ List.fromFoldable
            [ mkTree { id: "G", label: "G", layer: 3 } List.Nil
            , mkTree { id: "H", label: "H", layer: 3 } List.Nil
            ]
        ]
    , mkTree { id: "C", label: "C", layer: 1 } $ List.fromFoldable
        [ mkTree { id: "F", label: "F", layer: 2 } List.Nil
        ]
    ]

-- | Extra links that aren't part of the tree hierarchy
-- | These demonstrate cross-layer and cross-branch connections
extraLinks :: Array (DAGLink String)
extraLinks =
  [ { source: "A", target: "E", linkType: "skip" }    -- Root to grandchild (skips layer)
  , { source: "C", target: "G", linkType: "cross" }   -- Cross-branch, skips layer
  , { source: "B", target: "F", linkType: "cross" }   -- Cross-branch, same depth
  ]

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Draw the DAG tree example
drawDAGTreeExample :: String -> Effect Unit
drawDAGTreeExample selector = void $ runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let
    size = { width: 500.0, height: 350.0 }
    padding = { top: 40.0, bottom: 20.0, left: 50.0, right: 50.0 }
    innerWidth = size.width - padding.left - padding.right
    innerHeight = size.height - padding.top - padding.bottom
    legendY = size.height - 15.0

    -- Build the DAG tree
    dag = dagTree sampleTree _.id # addLinks extraLinks

    -- Layout with adjusted size for padding
    positioned = layoutDAGTree Vertical { width: innerWidth, height: innerHeight } dag

    -- Offset positions by padding
    offsetNode n = n { x = n.x + padding.left, y = n.y + padding.top }
    offsetTreeLink link =
      { source: offsetNode link.source
      , target: offsetNode link.target
      }
    offsetExtraLink link =
      { source: offsetNode link.source
      , target: offsetNode link.target
      , linkType: link.linkType
      }

    nodes = map offsetNode positioned.nodes
    treeLinks = map offsetTreeLink positioned.treeLinks
    extraLinksPositioned = map offsetExtraLink positioned.extraLinks

    -- Build the static structure (groups and legend)
    staticTree :: T.Tree Unit
    staticTree =
      T.named SVG "svg"
        [ v3Attr "width" (lit size.width)
        , v3Attr "height" (lit size.height)
        , v3AttrStr "class" (str "dag-tree-example")
        ]
        `T.withChildren`
          [ -- Title
            T.elem Text
              [ v3Attr "x" (lit (size.width / 2.0))
              , v3Attr "y" (lit 20.0)
              , v3AttrStr "textContent" (str "DAG Tree: Tree + Extra Links")
              , v3Attr "font-size" (lit 14.0)
              , v3AttrStr "fill" (str "#2F4F4F")
              , v3AttrStr "text-anchor" (str "middle")
              , v3AttrStr "class" (str "diagram-title")
              ]

          -- Empty groups for data-bound elements
          , T.named Group "tree-links" [ v3AttrStr "class" (str "tree-links") ]
          , T.named Group "extra-links" [ v3AttrStr "class" (str "extra-links") ]
          , T.named Group "nodes" [ v3AttrStr "class" (str "nodes") ]
          , T.named Group "labels" [ v3AttrStr "class" (str "labels") ]

          -- Legend: Tree edge
          , T.elem Line
              [ v3Attr "x1" (lit (size.width - 200.0))
              , v3Attr "y1" (lit legendY)
              , v3Attr "x2" (lit (size.width - 170.0))
              , v3Attr "y2" (lit legendY)
              , v3AttrStr "stroke" (str "#708090")
              , v3Attr "stroke-width" (lit 2.0)
              ]
          , T.elem Text
              [ v3Attr "x" (lit (size.width - 165.0))
              , v3Attr "y" (lit (legendY + 4.0))
              , v3AttrStr "textContent" (str "Tree edge")
              , v3Attr "font-size" (lit 11.0)
              , v3AttrStr "fill" (str "#708090")
              ]

          -- Legend: Extra link
          , T.elem Line
              [ v3Attr "x1" (lit (size.width - 90.0))
              , v3Attr "y1" (lit legendY)
              , v3Attr "x2" (lit (size.width - 60.0))
              , v3Attr "y2" (lit legendY)
              , v3AttrStr "stroke" (str "#F4A460")
              , v3Attr "stroke-width" (lit 2.5)
              ]
          , T.elem Text
              [ v3Attr "x" (lit (size.width - 55.0))
              , v3Attr "y" (lit (legendY + 4.0))
              , v3AttrStr "textContent" (str "Extra link")
              , v3Attr "font-size" (lit 11.0)
              , v3AttrStr "fill" (str "#F4A460")
              ]
          ]

  -- Render the static structure
  _ <- renderTree container staticTree

  -- Select groups and render data-bound elements
  treeLinksGroup <- select ".dag-tree-example .tree-links" :: _ (D3v2Selection_ SEmpty Element Unit)
  let treeLinksTree = T.joinData "links" "line" treeLinks $ \_ ->
        T.elem Line
          [ v3AttrFn "x1" (_.source.x :: TreeLink -> Number)
          , v3AttrFn "y1" (_.source.y :: TreeLink -> Number)
          , v3AttrFn "x2" (_.target.x :: TreeLink -> Number)
          , v3AttrFn "y2" (_.target.y :: TreeLink -> Number)
          , v3AttrStr "stroke" (str "#708090")
          , v3Attr "stroke-width" (lit 2.0)
          , v3Attr "opacity" (lit 0.6)
          ]
  _ <- renderTree treeLinksGroup treeLinksTree

  extraLinksGroup <- select ".dag-tree-example .extra-links" :: _ (D3v2Selection_ SEmpty Element Unit)
  let extraLinksTree = T.joinData "extra" "line" extraLinksPositioned $ \_ ->
        T.elem Line
          [ v3AttrFn "x1" (_.source.x :: ExtraLink -> Number)
          , v3AttrFn "y1" (_.source.y :: ExtraLink -> Number)
          , v3AttrFn "x2" (_.target.x :: ExtraLink -> Number)
          , v3AttrFn "y2" (_.target.y :: ExtraLink -> Number)
          , v3AttrStr "stroke" (str "#F4A460")  -- Sandy orange
          , v3Attr "stroke-width" (lit 2.5)
          , v3AttrStr "class" (str "extra-link")
          ]
  _ <- renderTree extraLinksGroup extraLinksTree

  nodesGroup <- select ".dag-tree-example .nodes" :: _ (D3v2Selection_ SEmpty Element Unit)
  let nodesTree = T.joinData "nodes" "circle" nodes $ \_ ->
        T.elem Circle
          [ v3AttrFn "cx" (_.x :: PositionedNode -> Number)
          , v3AttrFn "cy" (_.y :: PositionedNode -> Number)
          , v3Attr "r" (lit 18.0)
          , v3AttrStr "fill" (str "#4A90A4")
          , v3AttrStr "stroke" (str "#fff")
          , v3Attr "stroke-width" (lit 2.0)
          ]
  _ <- renderTree nodesGroup nodesTree

  labelsGroup <- select ".dag-tree-example .labels" :: _ (D3v2Selection_ SEmpty Element Unit)
  let labelsTree = T.joinData "labels" "text" nodes $ \_ ->
        T.elem Text
          [ v3AttrFn "x" (_.x :: PositionedNode -> Number)
          , v3AttrFn "y" ((\n -> n.y + 5.0) :: PositionedNode -> Number)
          , v3AttrFnStr "textContent" (_.datum.label :: PositionedNode -> String)
          , v3Attr "font-size" (lit 14.0)
          , v3AttrStr "fill" (str "#fff")
          , v3AttrStr "text-anchor" (str "middle")
          , v3AttrStr "class" (str "node-label")
          ]
  _ <- renderTree labelsGroup labelsTree

  pure unit
