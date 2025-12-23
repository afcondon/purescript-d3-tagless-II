module D3.Viz.PatternTree.Isometric
  ( drawPatternForestIsometric
  , layout3D
  , isometricProject
  , projectIsometric
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Int as Int
import Data.List (List(..))
import Data.Number (pi, cos, sin, abs)
import Data.Tree (Tree, mkTree)
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, height, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, viewBox, width, x, y)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Interpreter.D3 (D3v2Selection_, reselectD3v2, runD3v2M)
import Web.DOM.Element (Element)
import D3.Viz.PatternTree.Types (PatternNode, PatternNode3D, LinkDatum)
import D3.Viz.PatternTree.Layout (makeLinks, nodeColor)

-- | Layout a pattern tree in 3D space where:
-- | - X = temporal progression (sequence children advance in X)
-- | - Y = tree depth (how deep in the hierarchy)
-- | - Z = parallel stack (parallel children stack in Z)
layout3D :: Number -> Number -> Number -> Int -> PatternTree -> Tree PatternNode3D
layout3D xPos yPos zPos depth pattern = case pattern of
  Sound s ->
    mkTree
      { label: s
      , nodeType: "sound"
      , x: xPos
      , y: yPos
      , z: zPos
      , depth
      }
      Nil

  Rest ->
    mkTree
      { label: "~"
      , nodeType: "rest"
      , x: xPos
      , y: yPos
      , z: zPos
      , depth
      }
      Nil

  Sequence children ->
    let
      -- Sequence: children advance in X (temporal progression)
      childDepth = depth + 1
      childY = yPos + 60.0  -- Move down in tree
      spacing = 80.0        -- Horizontal spacing for sequence
      layoutChild idx child =
        layout3D (xPos + (toNumber idx) * spacing) childY zPos childDepth child
      layoutedChildren = Array.mapWithIndex layoutChild children
    in
      mkTree
        { label: "seq"
        , nodeType: "sequence"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Array.toUnfoldable layoutedChildren)

  Parallel children ->
    let
      -- Parallel: children stack in Z (simultaneity)
      childDepth = depth + 1
      childY = yPos + 60.0  -- Move down in tree
      spacing = 40.0        -- Z spacing for parallel stack
      layoutChild idx child =
        layout3D xPos childY (zPos + (toNumber idx) * spacing) childDepth child
      layoutedChildren = Array.mapWithIndex layoutChild children
    in
      mkTree
        { label: "par"
        , nodeType: "parallel"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Array.toUnfoldable layoutedChildren)

  Choice children ->
    let
      -- Choice: spread in both X and Z slightly
      childDepth = depth + 1
      childY = yPos + 60.0
      xSpacing = 50.0
      zSpacing = 30.0
      layoutChild idx child =
        layout3D
          (xPos + (toNumber idx) * xSpacing)
          childY
          (zPos + (toNumber idx) * zSpacing)
          childDepth
          child
      layoutedChildren = Array.mapWithIndex layoutChild children
    in
      mkTree
        { label: "choice"
        , nodeType: "choice"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Array.toUnfoldable layoutedChildren)

  -- New extended constructors - modifiers with single child
  Fast n child ->
    let
      childDepth = depth + 1
      childY = yPos + 60.0
      childTree = layout3D xPos childY zPos childDepth child
    in
      mkTree
        { label: "*" <> show (Int.round n)
        , nodeType: "fast"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Cons childTree Nil)

  Slow n child ->
    let
      childDepth = depth + 1
      childY = yPos + 60.0
      childTree = layout3D xPos childY zPos childDepth child
    in
      mkTree
        { label: "/" <> show (Int.round n)
        , nodeType: "slow"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Cons childTree Nil)

  Euclidean n k child ->
    let
      childDepth = depth + 1
      childY = yPos + 60.0
      childTree = layout3D xPos childY zPos childDepth child
    in
      mkTree
        { label: "(" <> show n <> "," <> show k <> ")"
        , nodeType: "euclidean"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Cons childTree Nil)

  Degrade prob child ->
    let
      childDepth = depth + 1
      childY = yPos + 60.0
      childTree = layout3D xPos childY zPos childDepth child
    in
      mkTree
        { label: "?" <> show (Int.round (prob * 100.0)) <> "%"
        , nodeType: "degrade"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Cons childTree Nil)

  Repeat n child ->
    let
      childDepth = depth + 1
      childY = yPos + 60.0
      childTree = layout3D xPos childY zPos childDepth child
    in
      mkTree
        { label: "!" <> show n
        , nodeType: "repeat"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Cons childTree Nil)

  Elongate n child ->
    let
      childDepth = depth + 1
      childY = yPos + 60.0
      childTree = layout3D xPos childY zPos childDepth child
    in
      mkTree
        { label: "@" <> show (Int.round n)
        , nodeType: "elongate"
        , x: xPos
        , y: yPos
        , z: zPos
        , depth
        }
        (Cons childTree Nil)

  where
  toNumber :: Int -> Number
  toNumber = Int.toNumber

-- | Isometric projection: convert (x, y, z) to (iso_x, iso_y)
-- | Uses standard 30-degree isometric angle
isometricProject :: forall r. { x :: Number, y :: Number, z :: Number | r } -> { x :: Number, y :: Number }
isometricProject pos =
  { x: (pos.x - pos.z) * cos (pi / 6.0)  -- 30 degrees
  , y: pos.y + (pos.x + pos.z) * sin (pi / 6.0)
  }

-- | Apply isometric projection to all nodes in tree
-- | Note: path is not tracked for isometric view (experimental)
projectIsometric :: Tree PatternNode3D -> Tree PatternNode
projectIsometric tree3d =
  let
    val = head tree3d
    children = tail tree3d
    projected = isometricProject val
    projectedChildren = map projectIsometric children
  in
    mkTree
      { label: val.label
      , nodeType: val.nodeType
      , x: projected.x
      , y: projected.y
      , depth: val.depth
      , path: []  -- Isometric view doesn't use click paths
      }
      projectedChildren

-- | Isometric link path generator
-- | Creates bezier curves that follow isometric perspective
isometricLinkPath :: Number -> Number -> Number -> Number -> String
isometricLinkPath x1 y1 x2 y2 =
  let
    distance = abs (x2 - x1) + abs (y2 - y1)
    parentOffset = distance * 0.1
    childOffset = distance * 0.25
    cx1 = x1 + parentOffset
    cy1 = y1 + parentOffset
    cx2 = x2 - childOffset
    cy2 = y2 - childOffset
  in
    "M" <> show x1 <> "," <> show y1
    <> " C" <> show cx1 <> "," <> show cy1
    <> " " <> show cx2 <> "," <> show cy2
    <> " " <> show x2 <> "," <> show y2

-- | Draw a forest of pattern trees in isometric 3D layout
drawPatternForestIsometric :: String -> Array PatternTree -> Effect Unit
drawPatternForestIsometric selector patterns = do
  let chartWidth = 1200.0
  let chartHeight = 800.0
  let centerX = chartWidth / 2.0
  let centerY = 100.0  -- Start near top

  -- Layout each pattern tree in 3D, spacing them in X
  let spacing = 200.0
  let layout3DPattern idx pattern' =
        layout3D (Int.toNumber idx * spacing) 0.0 0.0 0 pattern'

  let trees3D = Array.mapWithIndex layout3DPattern patterns

  -- Project to isometric 2D
  let projectedTrees = map projectIsometric trees3D

  -- Flatten all trees to node and link arrays
  let allNodes = Array.foldl (<>) [] $ map (Array.fromFoldable) projectedTrees
  let allLinks = Array.foldl (<>) [] $ map makeLinks projectedTrees

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- SVG container with zoom and links
    let
      linksTree :: T.Tree LinkDatum
      linksTree =
        T.named SVG "pattern-forest-svg"
        [ width $ num chartWidth
        , height $ num chartHeight
        , viewBox 0.0 0.0 chartWidth chartHeight
        , attr "class" $ text "pattern-forest-viz pattern-forest-isometric"
        ]
        `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#pattern-forest-zoom-group" ]
        `T.withChild`
          ( T.named Group "zoomContainer"
              [ attr "class" $ text "forest-zoom-container" ]
              `T.withChild`
                ( T.named Group "zoom-group"
                    [ attr "id" $ text "pattern-forest-zoom-group"
                    , attr "class" $ text "zoom-group"
                    , attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")")
                    ]
                    `T.withChild`
                      ( T.named Group "chartGroup"
                          [ attr "class" $ text "forest-content" ]
                          `T.withChild`
                            ( T.named Group "linksGroup"
                                [ attr "class" $ text "links" ]
                                `T.withChild`
                                  ( T.joinData "links" "path" allLinks $ \link ->
                                      T.elem Path
                                        [ path $ text ( isometricLinkPath
                                                link.source.x
                                                link.source.y
                                                link.target.x
                                                link.target.y
                                            )
                                        , fill $ text "none"
                                        , stroke $ text "#ccc"
                                        , strokeWidth $ num 2.0
                                        , attr "class" $ text "link"
                                        ]
                                  )
                            )
                      )
                )
          )

    -- Render links first
    linksSelections <- renderTree container linksTree

    -- Second tree: Nodes on top
    chartGroupSel <- liftEffect $ reselectD3v2 "chartGroup" linksSelections

    let
      nodesTree :: T.Tree PatternNode
      nodesTree =
        T.named Group "nodesGroup"
        [ attr "class" $ text "nodes" ]
        `T.withChild`
          ( T.joinData "nodeGroups" "g" allNodes $ \node ->
              T.named Group ("node-" <> node.label)
                [ attr "class" $ text ("node node-" <> node.nodeType) ]
                `T.withChildren`
                  [ T.elem Circle
                      [ cx $ num node.x
                      , cy $ num node.y
                      , r $ num 8.0
                      , fill $ text (nodeColor node.nodeType)
                      , stroke $ text "#fff"
                      , strokeWidth $ num 2.0
                      ]
                  , T.elem Text
                      [ x $ num node.x
                      , y $ num (node.y - 14.0)
                      , textContent $ text node.label
                      , fontSize $ num 13.0
                      , textAnchor $ text "middle"
                      , fill $ text "#000"
                      , attr "font-weight" $ text "bold"
                      , attr "class" $ text "pattern-node-label"
                      ]
                  ]
          )

    -- Render nodes on top
    _ <- renderTree chartGroupSel nodesTree
    pure unit
