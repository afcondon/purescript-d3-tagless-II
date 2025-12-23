module D3.Viz.PatternTree.Radial
  ( drawPatternForestRadial
  , radialPoint
  , projectRadial
  ) where

import Prelude

import Component.PatternTree (PatternTree)
import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Number (pi, cos, sin)
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Link (linkBezierRadialCartesian)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, height, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, viewBox, width, x, y)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Interpreter.D3 (D3v2Selection_, reselectD3v2, runD3v2M)
import Web.DOM.Element (Element)
import D3.Viz.PatternTree.Types (PatternNode, LinkDatum)
import D3.Viz.PatternTree.Layout (patternForestToTree, makeForestLinks, makeForestNodes, nodeColor)

-- | Radial projection: convert (x, y) to polar coordinates
-- | x is mapped to angle, y (depth) is mapped to radius
radialPoint :: forall r. { x :: Number, y :: Number | r } -> Number -> Number -> { x :: Number, y :: Number }
radialPoint node w h =
  let
    -- Map x to angle (0 to 2π)
    angle = (node.x / w) * 2.0 * pi - (pi / 2.0) -- Start at top (-π/2)
    -- Map y (depth) to radius
    minDim = if w < h then w else h
    rad = (node.y / h) * (minDim / 2.0) * 0.85 -- Scale to 85% of radius
  in
    { x: rad * cos angle
    , y: rad * sin angle
    }

-- | Apply radial projection to a tree
projectRadial :: forall r. Number -> Number -> Tree { x :: Number, y :: Number | r } -> Tree { x :: Number, y :: Number | r }
projectRadial w h t =
  let
    val = head t
    children = tail t
    projected = radialPoint val w h
    projectedChildren = map (projectRadial w h) children
  in
    mkTree (val { x = projected.x, y = projected.y }) projectedChildren

-- | Draw a forest of pattern trees in radial layout
drawPatternForestRadial :: String -> Array PatternTree -> Effect Unit
drawPatternForestRadial selector patterns = do
  let chartSize = 1200.0
  let centerX = chartSize / 2.0
  let centerY = chartSize / 2.0

  -- Create forest with fake root
  let forestTree = patternForestToTree patterns
  let
    config = defaultTreeConfig
      { size =
          { width: chartSize
          , height: chartSize
          }
      }
  let positioned = tree config forestTree

  -- Apply radial projection
  let radialTree = projectRadial chartSize chartSize positioned

  -- Flatten to arrays, filtering out fake root
  let nodes = makeForestNodes radialTree
  let links = makeForestLinks radialTree

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- First tree: SVG container with zoom and links
    let
      linksTree :: T.Tree LinkDatum
      linksTree =
        T.named SVG "pattern-forest-svg"
        [ width $ num chartSize
        , height $ num chartSize
        , viewBox 0.0 0.0 chartSize chartSize
        , attr "class" $ text "pattern-forest-viz pattern-forest-radial"
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
                                  ( T.joinData "links" "path" links $ \link ->
                                      T.elem Path
                                        [ path $ text ( linkBezierRadialCartesian
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
          ( T.joinData "nodeGroups" "g" nodes $ \node ->
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
