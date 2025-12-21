module D3.Viz.PatternTreeViz
  ( drawPatternTree
  , drawPatternForest
  , drawPatternForestRadial
  , drawPatternForestIsometric
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Int as Int
import Data.List (List(..))
import Data.Number (pi, cos, sin, abs)
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Link (linkBezierRadialCartesian)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, height, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, viewBox, width, x, y)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Interpreter.D3 (D3v2Selection_, reselectD3v2, runD3v2M)
import Web.DOM.Element (Element)

-- | Node data for visualization
type PatternNode =
  { label :: String      -- Display text
  , nodeType :: String   -- "sound", "rest", "sequence", "parallel", "choice"
  , x :: Number          -- Positioned by layout
  , y :: Number          -- Positioned by layout
  , depth :: Int         -- Required by tree layout (will be computed)
  }

-- | 3D Node data for isometric visualization
type PatternNode3D =
  { label :: String      -- Display text
  , nodeType :: String   -- "sound", "rest", "sequence", "parallel", "choice"
  , x :: Number          -- Temporal position (sequence advances this)
  , y :: Number          -- Tree depth (parent-child hierarchy)
  , z :: Number          -- Parallel stack depth (parallel increments this)
  , depth :: Int         -- Tree depth as integer
  }

-- | Link data type
type LinkDatum =
  { source :: { x :: Number, y :: Number }
  , target :: { x :: Number, y :: Number }
  }

-- | Convert PatternTree to Data.Tree for layout
patternTreeToTree :: PatternTree -> Tree PatternNode
patternTreeToTree = go
  where
  go = case _ of
    Sound s ->
      mkTree { label: s, nodeType: "sound", x: 0.0, y: 0.0, depth: 0 } Nil

    Rest ->
      mkTree { label: "~", nodeType: "rest", x: 0.0, y: 0.0, depth: 0 } Nil

    Sequence children ->
      mkTree
        { label: "seq", nodeType: "sequence", x: 0.0, y: 0.0, depth: 0 }
        (Array.toUnfoldable $ map patternTreeToTree children)

    Parallel children ->
      mkTree
        { label: "par", nodeType: "parallel", x: 0.0, y: 0.0, depth: 0 }
        (Array.toUnfoldable $ map patternTreeToTree children)

    Choice children ->
      mkTree
        { label: "choice", nodeType: "choice", x: 0.0, y: 0.0, depth: 0 }
        (Array.toUnfoldable $ map patternTreeToTree children)

-- | Forest layout: "fake giant tree" approach
-- | Takes multiple pattern trees and lays them out as a forest
patternForestToTree :: Array PatternTree -> Tree PatternNode
patternForestToTree patterns =
  let
    -- Convert each pattern to a tree
    trees = map patternTreeToTree patterns
    -- Create fake root connecting all trees
    fakeRoot = mkTree
      { label: "forest", nodeType: "forest", x: 0.0, y: 0.0, depth: 0 }
      (Array.toUnfoldable trees)
  in
    fakeRoot

-- | Create links from parent to children
makeLinks :: Tree PatternNode -> Array LinkDatum
makeLinks tree' = Array.fromFoldable $ makeLinksList tree'
  where
  makeLinksList :: Tree PatternNode -> List LinkDatum
  makeLinksList t =
    let
      val = head t
      children = tail t
      childLinks = children >>= \child ->
        let childVal = head child
        in Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- | Create links for forest, filtering out fake root connections
makeForestLinks :: Tree PatternNode -> Array LinkDatum
makeForestLinks tree' =
  let
    val = head tree'
    children = tail tree'
  in
    if val.nodeType == "forest"
      then Array.fromFoldable $ children >>= (\child -> makeLinksList child)
      else makeLinks tree'
  where
  makeLinksList :: Tree PatternNode -> List LinkDatum
  makeLinksList t =
    let
      val = head t
      children = tail t
      childLinks = children >>= \child ->
        let childVal = head child
        in Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- | Flatten tree to nodes, filtering out fake root
makeForestNodes :: Tree PatternNode -> Array PatternNode
makeForestNodes tree' =
  let
    allNodes = Array.fromFoldable tree'
  in
    Array.filter (\node -> node.nodeType /= "forest") allNodes

-- | Link path generator (curved links)
linkPath :: Number -> Number -> Number -> Number -> String
linkPath x1' y1' x2' y2' =
  "M" <> show x1' <> "," <> show y1'
    <> "C"
    <> show x1'
    <> ","
    <> show ((y1' + y2') / 2.0)
    <> " "
    <> show x2'
    <> ","
    <> show ((y1' + y2') / 2.0)
    <> " "
    <> show x2'
    <> ","
    <> show y2'

-- | Get color for node type
nodeColor :: String -> String
nodeColor = case _ of
  "sound" -> "#4CAF50"      -- Green for sounds
  "rest" -> "#999999"       -- Gray for rests
  "sequence" -> "#2196F3"   -- Blue for sequences
  "parallel" -> "#FF9800"   -- Orange for parallel
  "choice" -> "#9C27B0"     -- Purple for choice
  _ -> "#000000"

-- | Draw a single pattern tree
drawPatternTree :: String -> PatternTree -> Effect Unit
drawPatternTree selector patternTree = do
  Console.log $ "=== drawPatternTree called with selector: " <> selector
  Console.log $ "Pattern tree to render: " <> show patternTree

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

  Console.log $ "Rendering pattern tree: " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    liftEffect $ Console.log "Container selected successfully"

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

    liftEffect $ Console.log "Pattern tree visualization complete!"

-- | Draw a forest of pattern trees side-by-side
drawPatternForest :: String -> Array PatternTree -> Effect Unit
drawPatternForest selector patterns = do
  Console.log $ "=== drawPatternForest called with " <> show (Array.length patterns) <> " patterns"

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

  Console.log $ "Rendering pattern forest: " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    liftEffect $ Console.log "Container selected successfully"

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

    liftEffect $ Console.log "Pattern forest visualization complete!"

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
  Console.log $ "=== drawPatternForestRadial called with " <> show (Array.length patterns) <> " patterns"

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

  Console.log $ "Rendering radial pattern forest: " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    liftEffect $ Console.log "Container selected successfully"

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

    liftEffect $ Console.log "Radial pattern forest visualization complete!"

-- ============================================================================
-- 3D Layout for Musical Pattern Trees
-- ============================================================================

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
  Console.log $ "=== drawPatternForestIsometric called with " <> show (Array.length patterns) <> " patterns"

  let chartWidth = 1200.0
  let chartHeight = 800.0
  let centerX = chartWidth / 2.0
  let centerY = 100.0  -- Start near top

  -- Layout each pattern tree in 3D, spacing them in X
  let spacing = 200.0
  let layout3DPattern idx pattern =
        layout3D (Int.toNumber idx * spacing) 0.0 0.0 0 pattern

  let trees3D = Array.mapWithIndex layout3DPattern patterns

  -- Project to isometric 2D
  let projectedTrees = map projectIsometric trees3D

  -- Flatten all trees to node and link arrays
  let allNodes = Array.foldl (<>) [] $ map (Array.fromFoldable) projectedTrees
  let allLinks = Array.foldl (<>) [] $ map makeLinks projectedTrees

  Console.log $ "Rendering isometric pattern forest: " <> show (Array.length allNodes) <> " nodes, " <> show (Array.length allLinks) <> " links"

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    liftEffect $ Console.log "Container selected successfully"

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

    liftEffect $ Console.log "Isometric pattern forest visualization complete!"
