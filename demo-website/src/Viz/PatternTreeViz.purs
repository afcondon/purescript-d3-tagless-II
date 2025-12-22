module D3.Viz.PatternTreeViz
  ( drawPatternTree
  , drawPatternForest
  , drawPatternForestRadial
  , drawPatternForestIsometric
  , drawPatternForestSunburst
  , NamedPattern
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Number (pi, cos, sin, abs, sqrt)
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Link (linkBezierRadialCartesian)
import DataViz.Layout.Hierarchy.Partition (HierarchyData(..), PartitionNode(..), defaultPartitionConfig, hierarchy, partition)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, height, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, viewBox, width, x, y)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom, onClick)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Operations as Ops
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

    -- New extended constructors - wrap child with modifier label
    Fast n child ->
      mkTree
        { label: "*" <> show (Int.round n), nodeType: "fast", x: 0.0, y: 0.0, depth: 0 }
        (Cons (patternTreeToTree child) Nil)

    Slow n child ->
      mkTree
        { label: "/" <> show (Int.round n), nodeType: "slow", x: 0.0, y: 0.0, depth: 0 }
        (Cons (patternTreeToTree child) Nil)

    Euclidean n k child ->
      mkTree
        { label: "(" <> show n <> "," <> show k <> ")", nodeType: "euclidean", x: 0.0, y: 0.0, depth: 0 }
        (Cons (patternTreeToTree child) Nil)

    Degrade prob child ->
      mkTree
        { label: "?" <> show (Int.round (prob * 100.0)) <> "%", nodeType: "degrade", x: 0.0, y: 0.0, depth: 0 }
        (Cons (patternTreeToTree child) Nil)

    Repeat n child ->
      mkTree
        { label: "!" <> show n, nodeType: "repeat", x: 0.0, y: 0.0, depth: 0 }
        (Cons (patternTreeToTree child) Nil)

    Elongate n child ->
      mkTree
        { label: "@" <> show (Int.round n), nodeType: "elongate", x: 0.0, y: 0.0, depth: 0 }
        (Cons (patternTreeToTree child) Nil)

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
  "fast" -> "#E91E63"       -- Pink for fast
  "slow" -> "#00BCD4"       -- Cyan for slow
  "euclidean" -> "#FFEB3B"  -- Yellow for euclidean
  "degrade" -> "#795548"    -- Brown for degrade/probability
  "repeat" -> "#673AB7"     -- Deep purple for repeat
  "elongate" -> "#009688"   -- Teal for elongate
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

-- ============================================================================
-- Sunburst Layout for Musical Pattern Trees
-- ============================================================================

-- | Convert PatternTree to HierarchyData for partition layout
-- | Each node gets value = 1 so siblings share angular space equally
patternToHierarchy :: PatternTree -> HierarchyData { label :: String, nodeType :: String }
patternToHierarchy = case _ of
  Sound s ->
    HierarchyData
      { data_: { label: s, nodeType: "sound" }
      , value: Just 1.0  -- Leaf nodes have explicit value
      , children: Nothing
      }

  Rest ->
    HierarchyData
      { data_: { label: "~", nodeType: "rest" }
      , value: Just 1.0
      , children: Nothing
      }

  Sequence children ->
    HierarchyData
      { data_: { label: "seq", nodeType: "sequence" }
      , value: Nothing  -- Will sum children
      , children: Just $ map patternToHierarchy children
      }

  Parallel children ->
    HierarchyData
      { data_: { label: "par", nodeType: "parallel" }
      , value: Nothing
      , children: Just $ map patternToHierarchy children
      }

  Choice children ->
    HierarchyData
      { data_: { label: "?", nodeType: "choice" }
      , value: Nothing
      , children: Just $ map patternToHierarchy children
      }

  -- New extended constructors - wrap child with modifier
  Fast n child ->
    HierarchyData
      { data_: { label: "*" <> show (Int.round n), nodeType: "fast" }
      , value: Nothing
      , children: Just [ patternToHierarchy child ]
      }

  Slow n child ->
    HierarchyData
      { data_: { label: "/" <> show (Int.round n), nodeType: "slow" }
      , value: Nothing
      , children: Just [ patternToHierarchy child ]
      }

  Euclidean n k child ->
    HierarchyData
      { data_: { label: "(" <> show n <> "," <> show k <> ")", nodeType: "euclidean" }
      , value: Nothing
      , children: Just [ patternToHierarchy child ]
      }

  Degrade prob child ->
    HierarchyData
      { data_: { label: "?" <> show (Int.round (prob * 100.0)) <> "%", nodeType: "degrade" }
      , value: Nothing
      , children: Just [ patternToHierarchy child ]
      }

  Repeat n child ->
    HierarchyData
      { data_: { label: "!" <> show n, nodeType: "repeat" }
      , value: Nothing
      , children: Just [ patternToHierarchy child ]
      }

  Elongate n child ->
    HierarchyData
      { data_: { label: "@" <> show (Int.round n), nodeType: "elongate" }
      , value: Nothing
      , children: Just [ patternToHierarchy child ]
      }

-- | Get color for node type (sunburst version - more saturated)
sunburstColor :: String -> String
sunburstColor = case _ of
  "sound" -> "#4CAF50"      -- Green for sounds
  "rest" -> "#9E9E9E"       -- Gray for rests
  "sequence" -> "#2196F3"   -- Blue for sequences
  "parallel" -> "#FF9800"   -- Orange for parallel
  "choice" -> "#9C27B0"     -- Purple for choice
  "fast" -> "#E91E63"       -- Pink for fast
  "slow" -> "#00BCD4"       -- Cyan for slow
  "euclidean" -> "#FFEB3B"  -- Yellow for euclidean
  "degrade" -> "#795548"    -- Brown for degrade/probability
  "repeat" -> "#673AB7"     -- Deep purple for repeat
  "elongate" -> "#009688"   -- Teal for elongate
  _ -> "#607D8B"

-- | Convert partition coordinates to sunburst arc path
-- | x0, x1 are normalized [0,1] representing angles around the circle
-- | y0, y1 are normalized [0,1] representing radius from center
sunburstArcPath :: Number -> Number -> Number -> Number -> Number -> String
sunburstArcPath x0_ y0_ x1_ y1_ radius =
  let
    -- Convert normalized x to angles (0 to 2π), starting at top
    startAngle = x0_ * 2.0 * pi - (pi / 2.0)
    endAngle = x1_ * 2.0 * pi - (pi / 2.0)

    -- Convert normalized y to radius
    innerRadius = y0_ * radius
    outerRadius = y1_ * radius

    -- Calculate arc points
    x00 = cos startAngle * innerRadius
    y00 = sin startAngle * innerRadius
    x01 = cos endAngle * innerRadius
    y01 = sin endAngle * innerRadius
    x10 = cos startAngle * outerRadius
    y10 = sin startAngle * outerRadius
    x11 = cos endAngle * outerRadius
    y11 = sin endAngle * outerRadius

    -- Large arc flag: 1 if angle > π, 0 otherwise
    largeArc = if (endAngle - startAngle) > pi then 1 else 0
  in
    -- SVG path for arc segment
    "M" <> show x10 <> "," <> show y10
      <> "A" <> show outerRadius <> "," <> show outerRadius
      <> " 0 " <> show largeArc <> " 1 "
      <> show x11 <> "," <> show y11
      <> "L" <> show x01 <> "," <> show y01
      <> "A" <> show innerRadius <> "," <> show innerRadius
      <> " 0 " <> show largeArc <> " 0 "
      <> show x00 <> "," <> show y00
      <> "Z"

-- | Flatten PartitionNode tree to array
flattenPartition :: forall a. PartitionNode a -> Array (PartitionNode a)
flattenPartition node@(PartNode n) =
  if Array.length n.children == 0 then [ node ]
  else [ node ] <> (n.children >>= flattenPartition)

-- | Named pattern for sunburst visualization
type NamedPattern = { name :: String, pattern :: PatternTree, trackIndex :: Int, active :: Boolean }

-- | Draw multiple pattern trees as sunbursts side by side
-- | onToggle callback is called with track index when center is clicked
drawPatternForestSunburst :: String -> Array NamedPattern -> (Int -> Effect Unit) -> Effect Unit
drawPatternForestSunburst selector namedPatterns onToggle = do
  Console.log $ "=== drawPatternForestSunburst called with " <> show (Array.length namedPatterns) <> " patterns"

  let numPatterns = Array.length namedPatterns
  let chartWidth = 1400.0
  let chartHeight = 1200.0  -- Taller for grid layout

  -- Grid layout: determine columns and rows
  let cols = max 1 (min 5 (Int.ceil (sqrt (Int.toNumber numPatterns))))
  let rows = Int.ceil (Int.toNumber numPatterns / Int.toNumber cols)

  -- Calculate size for each sunburst based on grid
  let cellWidth = (chartWidth - 100.0) / Int.toNumber cols
  let cellHeight = (chartHeight - 200.0) / Int.toNumber rows
  let sunburstSize = min 200.0 (min cellWidth cellHeight * 0.85)
  let radius = sunburstSize / 2.0

  -- Starting position (centered in available space)
  let gridWidth = Int.toNumber cols * cellWidth
  let startX = (chartWidth - gridWidth) / 2.0 + cellWidth / 2.0
  let startY = 150.0 + cellHeight / 2.0  -- Below header area

  -- Convert each pattern to partitioned hierarchy with grid position
  let processPattern idx { name, pattern, trackIndex, active } =
        let
          hierData = patternToHierarchy pattern
          partRoot = hierarchy hierData
          config = defaultPartitionConfig
            { size = { width: 1.0, height: 1.0 }
            , padding = 0.002
            }
          partitioned = partition config partRoot
          -- Flatten and filter out root (depth 0)
          allNodes = flattenPartition partitioned
          nodes = Array.filter (\(PartNode n) -> n.depth > 0) allNodes
          -- Separate leaf nodes (sounds/rests) for labeling
          leafNodes = Array.filter (\(PartNode n) -> n.data_.nodeType == "sound" || n.data_.nodeType == "rest") nodes
          -- Grid position
          col = idx `mod` cols
          row = idx / cols
          centerX = startX + Int.toNumber col * cellWidth
          centerY = startY + Int.toNumber row * cellHeight
        in
          { name, nodes, leafNodes, centerX, centerY, radius, idx, trackIndex, active }

  let sunburstData = Array.mapWithIndex processPattern namedPatterns

  runD3v2M do
    -- Clear the container first
    Ops.clear selector

    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    liftEffect $ Console.log "Container selected for sunburst"

    -- Render each sunburst separately
    -- First render the SVG container
    let
      svgTree :: T.Tree Unit
      svgTree =
        T.named SVG "pattern-sunburst-svg"
          [ width $ num chartWidth
          , height $ num chartHeight
          , viewBox 0.0 0.0 chartWidth chartHeight
          , attr "class" $ text "pattern-forest-viz pattern-forest-sunburst"
          ]
          `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#pattern-sunburst-zoom-group" ]
          `T.withChild`
            ( T.named Group "zoom-group"
                [ attr "id" $ text "pattern-sunburst-zoom-group"
                , attr "class" $ text "zoom-group"
                ]
            )

    svgSel <- renderTree container svgTree
    zoomGroupSel <- liftEffect $ reselectD3v2 "zoom-group" svgSel

    -- Render each sunburst into the zoom group
    for_ sunburstData \{ name, nodes, leafNodes, centerX, centerY, radius: r', idx, trackIndex, active } -> do
      -- Opacity based on active state
      let arcOpacity = if active then 0.85 else 0.25

      -- Render arcs
      let
        arcsTree :: T.Tree (PartitionNode { label :: String, nodeType :: String })
        arcsTree =
          T.named Group ("sunburst-" <> show idx)
            [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")") ]
            `T.withChild`
              ( T.joinData ("arcs-" <> show idx) "path" nodes $ \(PartNode node) ->
                  T.elem Path
                    [ path $ text (sunburstArcPath node.x0 node.y0 node.x1 node.y1 r')
                    , fill $ text (sunburstColor node.data_.nodeType)
                    , attr "fill-opacity" $ num arcOpacity
                    , stroke $ text "#fff"
                    , strokeWidth $ num 1.0
                    , attr "class" $ text ("arc arc-" <> node.data_.nodeType)
                    ]
              )
      _ <- renderTree zoomGroupSel arcsTree

      -- Render center circle with track name (clickable to toggle)
      let
        innerRadius = r' * 0.35  -- Inner hole radius
        centerBg = if active then "#fff" else "#f5f5f5"
        centerStroke = if active then "#ddd" else "#ccc"
        centerTextColor = if active then "#333" else "#999"
        centerTree :: T.Tree Unit
        centerTree =
          T.named Group ("center-" <> show idx)
            [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")")
            , attr "class" $ text "sunburst-center"
            , attr "style" $ text "cursor: pointer;"
            , attr "data-track-index" $ text (show trackIndex)
            ]
            `T.withBehaviors` [ onClick (onToggle trackIndex) ]
            `T.withChildren`
              [ T.elem Circle
                  [ cx $ num 0.0
                  , cy $ num 0.0
                  , r $ num innerRadius
                  , fill $ text centerBg
                  , stroke $ text centerStroke
                  , strokeWidth $ num 2.0
                  , attr "class" $ text "center-circle"
                  ]
              , T.elem Text
                  [ x $ num 0.0
                  , y $ num 4.0  -- Slight offset for vertical centering
                  , textContent $ text name
                  , fontSize $ num 12.0
                  , textAnchor $ text "middle"
                  , fill $ text centerTextColor
                  , attr "font-weight" $ text "600"
                  , attr "class" $ text "center-label"
                  ]
              ]
      _ <- renderTree zoomGroupSel centerTree

      -- Render sound labels on leaf arcs (only for sounds, not rests, and only if active)
      when active do
        let soundLeaves = Array.filter (\(PartNode n) -> n.data_.nodeType == "sound") leafNodes
        let
          soundLabelsTree :: T.Tree (PartitionNode { label :: String, nodeType :: String })
          soundLabelsTree =
            T.named Group ("sound-labels-" <> show idx)
              [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")") ]
              `T.withChild`
                ( T.joinData ("labels-" <> show idx) "text" soundLeaves $ \(PartNode node) ->
                    let
                      -- Calculate midpoint angle and radius for label positioning
                      midAngle = ((node.x0 + node.x1) / 2.0) * 2.0 * pi - (pi / 2.0)
                      midRadius = ((node.y0 + node.y1) / 2.0) * r'
                      labelX = cos midAngle * midRadius
                      labelY = sin midAngle * midRadius
                      -- Rotate text to follow arc angle
                      rotateAngle = ((node.x0 + node.x1) / 2.0) * 360.0 - 90.0
                      -- Flip text if on left side of circle
                      finalRotate = if rotateAngle > 90.0 && rotateAngle < 270.0
                        then rotateAngle + 180.0
                        else rotateAngle
                    in
                      T.elem Text
                        [ x $ num labelX
                        , y $ num labelY
                        , textContent $ text node.data_.label
                        , fontSize $ num 9.0
                        , textAnchor $ text "middle"
                        , attr "dominant-baseline" $ text "middle"
                        , fill $ text "#000"
                        , attr "transform" $ text ("rotate(" <> show finalRotate <> "," <> show labelX <> "," <> show labelY <> ")")
                        , attr "class" $ text "sound-label"
                        ]
                )
        _ <- renderTree zoomGroupSel soundLabelsTree
        pure unit

    liftEffect $ Console.log "Sunburst pattern forest complete!"
