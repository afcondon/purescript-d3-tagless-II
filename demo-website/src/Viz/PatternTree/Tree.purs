module D3.Viz.PatternTree.Tree
  ( drawPatternTree
  , drawPatternForest
  ) where

import Prelude

import Component.PatternTree (PatternTree)
import Data.Array as Array
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
import D3.Viz.PatternTree.Layout (patternTreeToTree, patternForestToTree, makeLinks, makeForestLinks, makeForestNodes, linkPath, nodeColor)

-- | Draw a single pattern tree
drawPatternTree :: String -> PatternTree -> Effect Unit
drawPatternTree selector patternTree = do
  let chartWidth = 600.0
  let chartHeight = 400.0
  let padding = 40.0

  -- Convert to Data.Tree and apply layout
  let dataTree = patternTreeToTree patternTree
  let
    config = defaultTreeConfig
      { size =
          { width: chartWidth - (2.0 * padding)
          , height: chartHeight - (2.0 * padding)
          }
      }
  let positioned = tree config dataTree

  -- Flatten to arrays
  let nodes = Array.fromFoldable positioned
  let links = makeLinks positioned

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- First tree: SVG container with links
    let
      linksTree :: T.Tree LinkDatum
      linksTree =
        T.named SVG "pattern-svg"
        [ width $ num chartWidth
        , height $ num chartHeight
        , viewBox 0.0 0.0 chartWidth chartHeight
        , attr "class" $ text "pattern-tree-viz"
        ]
        `T.withChild`
          ( T.named Group "chartGroup"
              [ attr "class" $ text "tree-content" ]
              `T.withChild`
                ( T.named Group "linksGroup"
                    [ attr "class" $ text "links" ]
                    `T.withChild`
                      ( T.joinData "links" "path" links $ \link ->
                          T.elem Path
                            [ path $ text ( linkPath
                                    (link.source.x + padding)
                                    (link.source.y + padding)
                                    (link.target.x + padding)
                                    (link.target.y + padding)
                                )
                            , fill $ text "none"
                            , stroke $ text "#ccc"
                            , strokeWidth $ num 2.0
                            , attr "class" $ text "link"
                            ]
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
                      [ cx $ num (node.x + padding)
                      , cy $ num (node.y + padding)
                      , r $ num 8.0
                      , fill $ text (nodeColor node.nodeType)
                      , stroke $ text "#fff"
                      , strokeWidth $ num 2.0
                      ]
                  , T.elem Text
                      [ x $ num (node.x + padding)
                      , y $ num (node.y + padding - 14.0)
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

-- | Draw a forest of pattern trees side-by-side
drawPatternForest :: String -> Array PatternTree -> Effect Unit
drawPatternForest selector patterns = do
  let chartWidth = 1200.0
  let chartHeight = 600.0
  let padding = 40.0

  -- Create forest with fake root
  let forestTree = patternForestToTree patterns
  let
    config = defaultTreeConfig
      { size =
          { width: chartWidth - (2.0 * padding)
          , height: chartHeight - (2.0 * padding)
          }
      }
  let positioned = tree config forestTree

  -- Flatten to arrays, filtering out fake root
  let nodes = makeForestNodes positioned
  let links = makeForestLinks positioned

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- First tree: SVG container with zoom and links
    let
      linksTree :: T.Tree LinkDatum
      linksTree =
        T.named SVG "pattern-forest-svg"
        [ width $ num chartWidth
        , height $ num chartHeight
        , viewBox 0.0 0.0 chartWidth chartHeight
        , attr "class" $ text "pattern-forest-viz"
        ]
        `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#pattern-forest-zoom-group" ]
        `T.withChild`
          ( T.named Group "zoomContainer"
              [ attr "class" $ text "forest-zoom-container" ]
              `T.withChild`
                ( T.named Group "zoom-group"
                    [ attr "id" $ text "pattern-forest-zoom-group"
                    , attr "class" $ text "zoom-group"
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
                                        [ path $ text ( linkPath
                                                (link.source.x + padding)
                                                (link.source.y + padding)
                                                (link.target.x + padding)
                                                (link.target.y + padding)
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
                      [ cx $ num (node.x + padding)
                      , cy $ num (node.y + padding)
                      , r $ num 8.0
                      , fill $ text (nodeColor node.nodeType)
                      , stroke $ text "#fff"
                      , strokeWidth $ num 2.0
                      ]
                  , T.elem Text
                      [ x $ num (node.x + padding)
                      , y $ num (node.y + padding - 14.0)
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
