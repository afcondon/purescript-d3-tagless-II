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
import PSD3v2.Attribute.Types (cx, cy, radius, fill, stroke, strokeWidth, textContent, x, y, x1, x2, y1, y2, class_, fontSize, opacity, width, height) as Attr
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, clear)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(..))

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
  -- Clear any existing content
  clear selector

  container <- select selector

  let
    size = { width: 500.0, height: 350.0 }
    padding = { top: 40.0, bottom: 20.0, left: 50.0, right: 50.0 }
    innerWidth = size.width - padding.left - padding.right
    innerHeight = size.height - padding.top - padding.bottom

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

  -- Create SVG
  svg <- appendChild SVG
    [ Attr.width size.width
    , Attr.height size.height
    , Attr.class_ "dag-tree-example"
    ]
    container

  -- Title
  _ <- appendChild Text
    [ Attr.x (size.width / 2.0)
    , Attr.y 20.0
    , Attr.textContent "DAG Tree: Tree + Extra Links"
    , Attr.fontSize 14.0
    , Attr.fill "#2F4F4F"
    , Attr.class_ "diagram-title"
    ]
    svg

  -- Render tree links (gray, behind everything)
  linksGroup <- appendChild Group [ Attr.class_ "tree-links" ] svg
  _ <- appendData Line treeLinks
    [ Attr.x1 (_.source.x :: TreeLink -> Number)
    , Attr.y1 (_.source.y :: TreeLink -> Number)
    , Attr.x2 (_.target.x :: TreeLink -> Number)
    , Attr.y2 (_.target.y :: TreeLink -> Number)
    , Attr.stroke "#708090"
    , Attr.strokeWidth 2.0
    , Attr.opacity 0.6
    ]
    linksGroup

  -- Render extra links (orange, dashed effect via class)
  extraGroup <- appendChild Group [ Attr.class_ "extra-links" ] svg
  _ <- appendData Line extraLinksPositioned
    [ Attr.x1 (_.source.x :: ExtraLink -> Number)
    , Attr.y1 (_.source.y :: ExtraLink -> Number)
    , Attr.x2 (_.target.x :: ExtraLink -> Number)
    , Attr.y2 (_.target.y :: ExtraLink -> Number)
    , Attr.stroke "#F4A460"  -- Sandy orange
    , Attr.strokeWidth 2.5
    , Attr.class_ "extra-link"
    ]
    extraGroup

  -- Render nodes (circles)
  nodesGroup <- appendChild Group [ Attr.class_ "nodes" ] svg
  _ <- appendData Circle nodes
    [ Attr.cx (_.x :: PositionedNode -> Number)
    , Attr.cy (_.y :: PositionedNode -> Number)
    , Attr.radius 18.0
    , Attr.fill "#4A90A4"
    , Attr.stroke "#fff"
    , Attr.strokeWidth 2.0
    ]
    nodesGroup

  -- Render node labels
  labelsGroup <- appendChild Group [ Attr.class_ "labels" ] svg
  _ <- appendData Text nodes
    [ Attr.x (_.x :: PositionedNode -> Number)
    , Attr.y ((\n -> n.y + 5.0) :: PositionedNode -> Number)
    , Attr.textContent (_.datum.label :: PositionedNode -> String)
    , Attr.fontSize 14.0
    , Attr.fill "#fff"
    , Attr.class_ "node-label"
    ]
    labelsGroup

  -- Legend
  let legendY = size.height - 15.0

  -- Tree link legend
  _ <- appendChild Line
    [ Attr.x1 (size.width - 200.0)
    , Attr.y1 legendY
    , Attr.x2 (size.width - 170.0)
    , Attr.y2 legendY
    , Attr.stroke "#708090"
    , Attr.strokeWidth 2.0
    ]
    svg

  _ <- appendChild Text
    [ Attr.x (size.width - 165.0)
    , Attr.y (legendY + 4.0)
    , Attr.textContent "Tree edge"
    , Attr.fontSize 11.0
    , Attr.fill "#708090"
    ]
    svg

  -- Extra link legend
  _ <- appendChild Line
    [ Attr.x1 (size.width - 90.0)
    , Attr.y1 legendY
    , Attr.x2 (size.width - 60.0)
    , Attr.y2 legendY
    , Attr.stroke "#F4A460"
    , Attr.strokeWidth 2.5
    ]
    svg

  _ <- appendChild Text
    [ Attr.x (size.width - 55.0)
    , Attr.y (legendY + 4.0)
    , Attr.textContent "Extra link"
    , Attr.fontSize 11.0
    , Attr.fill "#F4A460"
    ]
    svg

  pure unit
