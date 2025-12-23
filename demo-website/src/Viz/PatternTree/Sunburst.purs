module D3.Viz.PatternTree.Sunburst
  ( drawPatternForestSunburst
  , patternToHierarchy
  , sunburstColor
  , sunburstFill
  , sunburstStroke
  , sunburstArcPath
  , flattenPartition
  , combinatorBadge
  , isCombinator
  , HierarchyNodeData
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (pi, cos, sin, sqrt)
import DataViz.Layout.Hierarchy.Partition (HierarchyData(..), PartitionNode(..), defaultPartitionConfig, hierarchy, partition)
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, height, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, viewBox, width, x, y)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom, onClick)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Operations as Ops
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Interpreter.D3 (D3v2Selection_, reselectD3v2, runD3v2M)
import Web.DOM.Element (Element)
import Control.Monad (when)

-- | Hierarchy node data with path for click handling
type HierarchyNodeData =
  { label :: String
  , nodeType :: String
  , path :: Array Int  -- Path from root (for click handling)
  }

-- | Convert PatternTree to HierarchyData for partition layout
patternToHierarchy :: PatternTree -> HierarchyData HierarchyNodeData
patternToHierarchy = go []
  where
  go :: Array Int -> PatternTree -> HierarchyData HierarchyNodeData
  go currentPath = case _ of
    Sound s ->
      HierarchyData
        { data_: { label: s, nodeType: "sound", path: currentPath }
        , value: Just 1.0  -- Leaf nodes have explicit value
        , children: Nothing
        }

    Rest ->
      HierarchyData
        { data_: { label: "~", nodeType: "rest", path: currentPath }
        , value: Just 1.0
        , children: Nothing
        }

    Sequence children ->
      HierarchyData
        { data_: { label: "seq", nodeType: "sequence", path: currentPath }
        , value: Nothing  -- Will sum children
        , children: Just $ Array.mapWithIndex (\i c -> go (currentPath <> [i]) c) children
        }

    Parallel children ->
      HierarchyData
        { data_: { label: "par", nodeType: "parallel", path: currentPath }
        , value: Nothing
        , children: Just $ Array.mapWithIndex (\i c -> go (currentPath <> [i]) c) children
        }

    Choice children ->
      HierarchyData
        { data_: { label: "?", nodeType: "choice", path: currentPath }
        , value: Nothing
        , children: Just $ Array.mapWithIndex (\i c -> go (currentPath <> [i]) c) children
        }

    -- New extended constructors - wrap child with modifier
    Fast n child ->
      HierarchyData
        { data_: { label: "*" <> show (Int.round n), nodeType: "fast", path: currentPath }
        , value: Nothing
        , children: Just [ go (currentPath <> [0]) child ]
        }

    Slow n child ->
      HierarchyData
        { data_: { label: "/" <> show (Int.round n), nodeType: "slow", path: currentPath }
        , value: Nothing
        , children: Just [ go (currentPath <> [0]) child ]
        }

    Euclidean n k child ->
      HierarchyData
        { data_: { label: "(" <> show n <> "," <> show k <> ")", nodeType: "euclidean", path: currentPath }
        , value: Nothing
        , children: Just [ go (currentPath <> [0]) child ]
        }

    Degrade prob child ->
      HierarchyData
        { data_: { label: "?" <> show (Int.round (prob * 100.0)) <> "%", nodeType: "degrade", path: currentPath }
        , value: Nothing
        , children: Just [ go (currentPath <> [0]) child ]
        }

    Repeat n child ->
      HierarchyData
        { data_: { label: "!" <> show n, nodeType: "repeat", path: currentPath }
        , value: Nothing
        , children: Just [ go (currentPath <> [0]) child ]
        }

    Elongate n child ->
      HierarchyData
        { data_: { label: "@" <> show (Int.round n), nodeType: "elongate", path: currentPath }
        , value: Nothing
        , children: Just [ go (currentPath <> [0]) child ]
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

-- | Get fill for node type - uses patterns for combinators
-- | Returns either a color or a pattern URL
sunburstFill :: String -> String
sunburstFill = case _ of
  "fast" -> "url(#fastPattern)"       -- Diagonal stripes for compression
  "slow" -> "url(#slowPattern)"       -- Horizontal bands for expansion
  "euclidean" -> "url(#euclidPattern)" -- Dots for rhythmic distribution
  "degrade" -> "url(#degradePattern)" -- Checkerboard for probability
  "repeat" -> "url(#repeatPattern)"   -- Vertical stripes for stutter
  "elongate" -> "url(#elongatePattern)" -- Gradient for stretch
  other -> sunburstColor other         -- Regular color for other types

-- | Get stroke style for node type
-- | Different combinators get distinctive strokes
sunburstStroke :: String -> { color :: String, width :: Number, dashArray :: String }
sunburstStroke = case _ of
  "fast" -> { color: "#C2185B", width: 2.0, dashArray: "3,2" }    -- Dashed pink
  "slow" -> { color: "#0097A7", width: 3.0, dashArray: "" }       -- Thick cyan
  "euclidean" -> { color: "#F57F17", width: 2.0, dashArray: "1,3" } -- Dotted yellow
  "degrade" -> { color: "#5D4037", width: 1.5, dashArray: "4,2" }  -- Dashed brown
  "repeat" -> { color: "#512DA8", width: 2.5, dashArray: "" }     -- Thick purple
  "elongate" -> { color: "#00796B", width: 2.0, dashArray: "6,2" } -- Long dash teal
  _ -> { color: "#fff", width: 1.0, dashArray: "" }

-- | Get badge text for combinator nodes
-- | Returns Just for combinators, Nothing for non-combinator nodes
combinatorBadge :: String -> Maybe String
combinatorBadge = case _ of
  "fast" -> Just "fast"
  "slow" -> Just "slow"
  "euclidean" -> Just "euclid"
  "degrade" -> Just "prob"
  "repeat" -> Just "rep"
  "elongate" -> Just "elong"
  _ -> Nothing

-- | Check if a node type is a combinator
isCombinator :: String -> Boolean
isCombinator nodeType = case combinatorBadge nodeType of
  Just _ -> true
  Nothing -> false

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

-- | Named pattern for standalone sunburst visualization
type NamedPattern = { name :: String, pattern :: PatternTree, trackIndex :: Int, active :: Boolean }

-- | Draw multiple pattern trees as sunbursts side by side
-- | onToggle callback is called with track index when center is clicked
drawPatternForestSunburst :: String -> Array NamedPattern -> (Int -> Effect Unit) -> Effect Unit
drawPatternForestSunburst selector namedPatterns onToggle = do
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

    -- Render each sunburst separately
    -- First render the SVG container with pattern definitions
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
          `T.withChildren`
            [ -- Pattern definitions for fast/slow visual treatment
              T.named Defs "patterns" []
                `T.withChildren`
                  [ -- Fast pattern: diagonal stripes (compression feel)
                    T.named PatternFill "fastPattern"
                      [ attr "id" $ text "fastPattern"
                      , attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 6.0
                      , height $ num 6.0
                      , attr "patternTransform" $ text "rotate(45)"
                      ]
                      `T.withChildren`
                        [ T.elem Rect
                            [ width $ num 3.0
                            , height $ num 6.0
                            , fill $ text "#E91E63"  -- Pink
                            ]
                        , T.elem Rect
                            [ x $ num 3.0
                            , width $ num 3.0
                            , height $ num 6.0
                            , fill $ text "#F48FB1"  -- Lighter pink
                            ]
                        ]
                  , -- Slow pattern: horizontal gradient bands (expansion feel)
                    T.named PatternFill "slowPattern"
                      [ attr "id" $ text "slowPattern"
                      , attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 8.0
                      , height $ num 8.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect
                            [ width $ num 8.0
                            , height $ num 4.0
                            , fill $ text "#00BCD4"  -- Cyan
                            ]
                        , T.elem Rect
                            [ y $ num 4.0
                            , width $ num 8.0
                            , height $ num 4.0
                            , fill $ text "#4DD0E1"  -- Lighter cyan
                            ]
                        ]
                  , -- Euclidean pattern: dots for rhythmic distribution
                    T.named PatternFill "euclidPattern"
                      [ attr "id" $ text "euclidPattern"
                      , attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 10.0
                      , height $ num 10.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect
                            [ width $ num 10.0
                            , height $ num 10.0
                            , fill $ text "#FFEB3B"  -- Yellow background
                            ]
                        , T.elem Circle
                            [ cx $ num 5.0
                            , cy $ num 5.0
                            , r $ num 2.5
                            , fill $ text "#FFF176"  -- Lighter yellow dot
                            ]
                        ]
                  , -- Degrade pattern: checkerboard for probability/uncertainty
                    T.named PatternFill "degradePattern"
                      [ attr "id" $ text "degradePattern"
                      , attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 8.0
                      , height $ num 8.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect
                            [ width $ num 8.0
                            , height $ num 8.0
                            , fill $ text "#795548"  -- Brown
                            ]
                        , T.elem Rect
                            [ width $ num 4.0
                            , height $ num 4.0
                            , fill $ text "#A1887F"  -- Lighter brown
                            ]
                        , T.elem Rect
                            [ x $ num 4.0
                            , y $ num 4.0
                            , width $ num 4.0
                            , height $ num 4.0
                            , fill $ text "#A1887F"  -- Lighter brown
                            ]
                        ]
                  , -- Repeat pattern: vertical stripes for stutter/echo
                    T.named PatternFill "repeatPattern"
                      [ attr "id" $ text "repeatPattern"
                      , attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 6.0
                      , height $ num 6.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect
                            [ width $ num 3.0
                            , height $ num 6.0
                            , fill $ text "#673AB7"  -- Deep purple
                            ]
                        , T.elem Rect
                            [ x $ num 3.0
                            , width $ num 3.0
                            , height $ num 6.0
                            , fill $ text "#9575CD"  -- Lighter purple
                            ]
                        ]
                  , -- Elongate pattern: diagonal gradient for stretch
                    T.named PatternFill "elongatePattern"
                      [ attr "id" $ text "elongatePattern"
                      , attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 12.0
                      , height $ num 4.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect
                            [ width $ num 12.0
                            , height $ num 4.0
                            , fill $ text "#009688"  -- Teal
                            ]
                        , T.elem Rect
                            [ width $ num 4.0
                            , height $ num 4.0
                            , fill $ text "#4DB6AC"  -- Lighter teal
                            ]
                        , T.elem Rect
                            [ x $ num 8.0
                            , width $ num 4.0
                            , height $ num 4.0
                            , fill $ text "#4DB6AC"  -- Lighter teal
                            ]
                        ]
                  ]
            , T.named Group "zoom-group"
                [ attr "id" $ text "pattern-sunburst-zoom-group"
                , attr "class" $ text "zoom-group"
                ]
            ]

    svgSel <- renderTree container svgTree
    zoomGroupSel <- liftEffect $ reselectD3v2 "zoom-group" svgSel

    -- Render each sunburst into the zoom group
    for_ sunburstData \{ name, nodes, leafNodes, centerX, centerY, radius: r', idx, trackIndex, active } -> do
      -- Opacity based on active state
      let arcOpacity = if active then 0.85 else 0.25

      -- Render arcs with enhanced styling for fast/slow
      let
        arcsTree :: T.Tree (PartitionNode HierarchyNodeData)
        arcsTree =
          T.named Group ("sunburst-" <> show idx)
            [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")") ]
            `T.withChild`
              ( T.joinData ("arcs-" <> show idx) "path" nodes $ \(PartNode node) ->
                  let
                    strokeStyle = sunburstStroke node.data_.nodeType
                  in
                    T.elem Path
                      [ path $ text (sunburstArcPath node.x0 node.y0 node.x1 node.y1 r')
                      , fill $ text (sunburstFill node.data_.nodeType)
                      , attr "fill-opacity" $ num arcOpacity
                      , stroke $ text strokeStyle.color
                      , strokeWidth $ num strokeStyle.width
                      , attr "stroke-dasharray" $ text strokeStyle.dashArray
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
          soundLabelsTree :: T.Tree (PartitionNode HierarchyNodeData)
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

    pure unit
