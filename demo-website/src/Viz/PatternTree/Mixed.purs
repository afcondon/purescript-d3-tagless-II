module D3.Viz.PatternTree.Mixed
  ( drawPatternForestMixed
  ) where

import Prelude

import Component.PatternTree (PatternTree, PatternMetrics, analyzePattern)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin, sqrt)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Hierarchy.Partition (PartitionNode(..), defaultPartitionConfig, hierarchy, partition)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, height, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, viewBox, width, x, y)
import PSD3.Internal.Behavior.FFI (attachZoomWithCallback_)
import PSD3.Internal.Behavior.Types (Behavior(..), onClick)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Operations as Ops
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Interpreter.D3 (D3v2M, D3v2Selection_, reselectD3v2, runD3v2M)
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Control.Monad (when)
import D3.Viz.PatternTree.Types (PatternNode, LinkDatum, TrackLayout(..), TrackWithLayout, ZoomTransform)
import D3.Viz.PatternTree.Layout (patternTreeToTree, makeLinks, nodeColor)
import D3.Viz.PatternTree.Euclidean (parseEuclideanLabel, euclideanPattern)
import D3.Viz.PatternTree.Sunburst (patternToHierarchy, sunburstColor, sunburstFill, sunburstStroke, sunburstArcPath, flattenPartition, fixParallelLayout, combinatorBadge, isCombinator, HierarchyNodeData)

-- | Draw patterns with per-track layout (tree or sunburst)
-- | onToggleActive: called when center is clicked (toggle mute)
-- | onToggleLayout: called when outer ring is clicked (toggle layout)
-- | initialZoom: zoom transform to restore (use identityZoom if none)
-- | onZoomChange: called when zoom changes (to save state for next render)
drawPatternForestMixed :: String -> Array TrackWithLayout -> (Int -> Effect Unit) -> (Int -> Effect Unit) -> (Int -> Array Int -> Effect Unit) -> (Int -> Array Int -> Int -> Int -> Effect Unit) -> ZoomTransform -> (ZoomTransform -> Effect Unit) -> Effect Unit
drawPatternForestMixed selector tracksWithLayout onToggleActive onToggleLayout onToggleNodeType onAdjustEuclidean initialZoom onZoomChange = do
  let numPatterns = Array.length tracksWithLayout
  let chartWidth = 1400.0
  let chartHeight = 1200.0

  -- Grid layout: determine columns and rows
  let cols = max 1 (min 5 (Int.ceil (sqrt (Int.toNumber numPatterns))))
  let rows = Int.ceil (Int.toNumber numPatterns / Int.toNumber cols)

  -- Calculate size for each pattern based on grid
  let cellWidth = (chartWidth - 100.0) / Int.toNumber cols
  let cellHeight = (chartHeight - 200.0) / Int.toNumber rows
  let patternSize = min 200.0 (min cellWidth cellHeight * 0.85)
  let radius = patternSize / 2.0

  -- Starting position (centered in available space)
  let gridWidth = Int.toNumber cols * cellWidth
  let startX = (chartWidth - gridWidth) / 2.0 + cellWidth / 2.0
  let startY = 150.0 + cellHeight / 2.0

  runD3v2M do
    -- Clear the container first
    Ops.clear selector

    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Create SVG with pattern definitions (zoom attached after render to preserve state)
    let
      svgTree :: T.Tree Unit
      svgTree =
        T.named SVG "pattern-mixed-svg"
          [ width $ num chartWidth
          , height $ num chartHeight
          , viewBox 0.0 0.0 chartWidth chartHeight
          , attr "class" $ text "pattern-forest-viz pattern-forest-mixed"
          , attr "id" $ text "pattern-mixed-svg"
          ]
          `T.withChildren`
            [ -- Pattern definitions (same as sunburst)
              T.named Defs "patterns" []
                `T.withChildren`
                  [ T.named PatternFill "fastPattern"
                      [ attr "id" $ text "fastPattern"
                      , attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 6.0, height $ num 6.0
                      , attr "patternTransform" $ text "rotate(45)"
                      ]
                      `T.withChildren`
                        [ T.elem Rect [ width $ num 3.0, height $ num 6.0, fill $ text "#E91E63" ]
                        , T.elem Rect [ x $ num 3.0, width $ num 3.0, height $ num 6.0, fill $ text "#F48FB1" ]
                        ]
                  , T.named PatternFill "slowPattern"
                      [ attr "id" $ text "slowPattern", attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 8.0, height $ num 8.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect [ width $ num 8.0, height $ num 4.0, fill $ text "#00BCD4" ]
                        , T.elem Rect [ y $ num 4.0, width $ num 8.0, height $ num 4.0, fill $ text "#4DD0E1" ]
                        ]
                  , T.named PatternFill "euclidPattern"
                      [ attr "id" $ text "euclidPattern", attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 10.0, height $ num 10.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect [ width $ num 10.0, height $ num 10.0, fill $ text "#FFEB3B" ]
                        , T.elem Circle [ cx $ num 5.0, cy $ num 5.0, r $ num 2.5, fill $ text "#FFF176" ]
                        ]
                  , T.named PatternFill "degradePattern"
                      [ attr "id" $ text "degradePattern", attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 8.0, height $ num 8.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect [ width $ num 8.0, height $ num 8.0, fill $ text "#795548" ]
                        , T.elem Rect [ width $ num 4.0, height $ num 4.0, fill $ text "#A1887F" ]
                        , T.elem Rect [ x $ num 4.0, y $ num 4.0, width $ num 4.0, height $ num 4.0, fill $ text "#A1887F" ]
                        ]
                  , T.named PatternFill "repeatPattern"
                      [ attr "id" $ text "repeatPattern", attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 6.0, height $ num 6.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect [ width $ num 3.0, height $ num 6.0, fill $ text "#673AB7" ]
                        , T.elem Rect [ x $ num 3.0, width $ num 3.0, height $ num 6.0, fill $ text "#9575CD" ]
                        ]
                  , T.named PatternFill "elongatePattern"
                      [ attr "id" $ text "elongatePattern", attr "patternUnits" $ text "userSpaceOnUse"
                      , width $ num 12.0, height $ num 4.0
                      ]
                      `T.withChildren`
                        [ T.elem Rect [ width $ num 12.0, height $ num 4.0, fill $ text "#009688" ]
                        , T.elem Rect [ width $ num 4.0, height $ num 4.0, fill $ text "#4DB6AC" ]
                        , T.elem Rect [ x $ num 8.0, width $ num 4.0, height $ num 4.0, fill $ text "#4DB6AC" ]
                        ]
                  ]
            , T.named Group "zoom-group"
                [ attr "id" $ text "pattern-mixed-zoom-group"
                , attr "class" $ text "zoom-group"
                ]
            ]

    svgSel <- renderTree container svgTree
    zoomGroupSel <- liftEffect $ reselectD3v2 "zoom-group" svgSel

    -- Attach zoom behavior with preserved state
    liftEffect do
      -- Get SVG element by ID and attach zoom with initial transform
      doc <- Web.HTML.window >>= Window.document
      let node = HTMLDocument.toNonElementParentNode doc
      maybeSvg <- getElementById "pattern-mixed-svg" node
      case maybeSvg of
        Just svgElem -> do
          _ <- attachZoomWithCallback_ svgElem 0.1 10.0 "#pattern-mixed-zoom-group" initialZoom onZoomChange
          pure unit
        Nothing -> pure unit

    -- Render each track based on its layout
    for_ (Array.mapWithIndex Tuple tracksWithLayout) \(Tuple idx track) -> do
      let col = idx `mod` cols
      let row = idx / cols
      let centerX = startX + Int.toNumber col * cellWidth
      let centerY = startY + Int.toNumber row * cellHeight
      let arcOpacity = if track.active then 0.85 else 0.25

      case track.layout of
        SunburstLayout -> do
          -- Render as sunburst (same as before)
          renderSunburstTrack zoomGroupSel track centerX centerY radius arcOpacity onToggleActive onToggleLayout onToggleNodeType onAdjustEuclidean idx

        TreeLayout -> do
          -- Render as vertical tree
          renderTreeTrack zoomGroupSel track centerX centerY radius arcOpacity onToggleActive onToggleLayout onToggleNodeType onAdjustEuclidean idx

    pure unit

-- | Render a single track as sunburst
renderSunburstTrack :: D3v2Selection_ SEmpty Element Unit -> TrackWithLayout -> Number -> Number -> Number -> Number -> (Int -> Effect Unit) -> (Int -> Effect Unit) -> (Int -> Array Int -> Effect Unit) -> (Int -> Array Int -> Int -> Int -> Effect Unit) -> Int -> D3v2M Unit
renderSunburstTrack zoomGroupSel track centerX centerY r' arcOpacity onToggleActive onToggleLayout onToggleNodeType onAdjustEuclidean idx = do
  let hierData = patternToHierarchy track.pattern
  let partRoot = hierarchy hierData
  let config = defaultPartitionConfig { size = { width: 1.0, height: 1.0 }, padding = 0.002 }
  let partitioned = partition config partRoot
  -- Fix parallel layout: make parallel children share angular extent
  let fixedPartitioned = fixParallelLayout partitioned
  let allNodes = flattenPartition fixedPartitioned
  let nodes = Array.filter (\(PartNode n) -> n.depth > 0) allNodes

  -- Render arcs (with click handlers on toggleable nodes)
  let arcsTree :: T.Tree (PartitionNode HierarchyNodeData)
      arcsTree =
        T.named Group ("sunburst-" <> show idx)
          [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")") ]
          `T.withChild`
            ( T.joinData ("arcs-" <> show idx) "g" nodes $ \(PartNode node) ->
                let strokeStyle = sunburstStroke node.data_.nodeType
                    isToggleable = node.data_.nodeType == "sequence" || node.data_.nodeType == "parallel"
                    arcPath = T.elem Path
                      [ path $ text (sunburstArcPath node.x0 node.y0 node.x1 node.y1 r')
                      , fill $ text (sunburstFill node.data_.nodeType)
                      , attr "fill-opacity" $ num arcOpacity
                      , stroke $ text strokeStyle.color
                      , strokeWidth $ num strokeStyle.width
                      , attr "stroke-dasharray" $ text strokeStyle.dashArray
                      , attr "class" $ text ("arc arc-" <> node.data_.nodeType <> if isToggleable then " arc-toggleable" else "")
                      ]
                in if isToggleable
                   then T.named Group ("arc-group-" <> show node.depth <> "-" <> show (Array.length node.data_.path))
                          [ attr "style" $ text "cursor: pointer;" ]
                          `T.withBehaviors` [ onClick (onToggleNodeType track.trackIndex node.data_.path) ]
                          `T.withChild` arcPath
                   else arcPath
            )
  _ <- renderTree zoomGroupSel arcsTree

  -- Render combinator badges on combinator arcs (when track is active)
  when track.active do
    let combinatorNodes = Array.filter (\(PartNode n) -> isCombinator n.data_.nodeType) nodes
    let badgesTree :: T.Tree (PartitionNode HierarchyNodeData)
        badgesTree =
          T.named Group ("badges-" <> show idx)
            [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")") ]
            `T.withChild`
              ( T.joinData ("badge-labels-" <> show idx) "text" combinatorNodes $ \(PartNode node) ->
                  let
                    -- Calculate midpoint angle and radius for label positioning
                    midAngle = ((node.x0 + node.x1) / 2.0) * 2.0 * pi - (pi / 2.0)
                    midRadius = ((node.y0 + node.y1) / 2.0) * r'
                    labelX = cos midAngle * midRadius
                    labelY = sin midAngle * midRadius
                    badgeText = fromMaybe "" (combinatorBadge node.data_.nodeType)
                  in
                    T.elem Text
                      [ x $ num labelX
                      , y $ num labelY
                      , textContent $ text badgeText
                      , fontSize $ num 8.0
                      , textAnchor $ text "middle"
                      , attr "dominant-baseline" $ text "middle"
                      , fill $ text "#fff"
                      , attr "font-weight" $ text "bold"
                      , attr "class" $ text "combinator-badge"
                      , attr "pointer-events" $ text "none"  -- Don't interfere with clicks
                      ]
              )
    _ <- renderTree zoomGroupSel badgesTree
    pure unit

  -- Render euclidean rhythm visualization and controls
  -- Note: Use allNodes (not nodes) because euclidean might be root at depth 0
  when track.active do
    let euclideanNodes = Array.filter (\(PartNode n) -> n.data_.nodeType == "euclidean") allNodes
    for_ euclideanNodes \(PartNode node) -> do
      -- Parse n and k from the label "(n,k)"
      let label = node.data_.label
      let nk = parseEuclideanLabel label
      let n = nk.n
      let k = nk.k

      -- Compute the euclidean rhythm pattern (which steps are hits)
      let pattern = euclideanPattern n k

      -- Ring radius for the beat circles - use outer edge of arc
      let ringRadius = node.y1 * r'  -- Use outer radius of the arc
      let beatCircleRadius = max 4.0 (min 8.0 (r' * 0.06))  -- Fixed reasonable size

      -- Create beat circles around the ring
      let beatCircles = Array.mapWithIndex (\i isHit ->
            let angle = (Int.toNumber i / Int.toNumber k) * 2.0 * pi - (pi / 2.0)  -- Start at top
                cx' = cos angle * ringRadius
                cy' = sin angle * ringRadius
                -- Filled white with green stroke for hits, hollow for rests
                fillColor = if isHit then "#fff" else "rgba(255,255,255,0.3)"
                strokeColor = if isHit then "#2E7D32" else "#81C784"  -- Darker green for hits
                sw = if isHit then 3.0 else 1.5
            in { cx: cx', cy: cy', fill: fillColor, stroke: strokeColor, strokeWidth: sw, isHit }
          ) pattern

      -- Render the euclidean visualization (beat ring)
      let euclidVizTree =
            T.named Group ("euclid-viz-" <> show idx <> "-" <> show (Array.length node.data_.path))
              [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")")
              , attr "class" $ text "euclid-viz"
              ]
              `T.withChild`
                -- Beat circles
                ( T.joinData ("euclid-beats-" <> show idx) "circle" beatCircles $ \beat ->
                    T.elem Circle
                      [ cx $ num beat.cx
                      , cy $ num beat.cy
                      , r $ num beatCircleRadius
                      , fill $ text beat.fill
                      , stroke $ text beat.stroke
                      , strokeWidth $ num beat.strokeWidth
                      , attr "class" $ text (if beat.isHit then "euclid-beat euclid-hit" else "euclid-beat euclid-rest")
                      ]
                )
      _ <- renderTree zoomGroupSel euclidVizTree
      pure unit

  -- Render center circle with track name (no click behavior - use buttons instead)
  -- Color the center based on root node type (layer 0 = center circle)
  let innerRadius = r' * 0.35
  let rootNode = Array.find (\(PartNode n) -> n.depth == 0) allNodes
  let rootType = case rootNode of
        Just (PartNode n) -> n.data_.nodeType
        Nothing -> "sequence"  -- fallback
  let centerBg = if track.active then sunburstColor rootType else "#f5f5f5"
  let centerStroke = if track.active then "#fff" else "#ccc"
  let centerTextColor = if track.active then "#fff" else "#999"
  let centerTree :: T.Tree Unit
      centerTree =
        T.named Group ("center-" <> show idx)
          [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")")
          , attr "class" $ text "sunburst-center"
          ]
          `T.withChildren`
            [ T.elem Circle
                [ cx $ num 0.0, cy $ num 0.0, r $ num innerRadius
                , fill $ text centerBg, stroke $ text centerStroke, strokeWidth $ num 2.0
                ]
            , T.elem Text
                [ x $ num 0.0, y $ num 4.0
                , textContent $ text track.name
                , fontSize $ num 12.0, textAnchor $ text "middle"
                , fill $ text centerTextColor
                , attr "font-weight" $ text "600"
                ]
            ]
  _ <- renderTree zoomGroupSel centerTree

  -- Render euclidean controls in center (after center circle so they're on top)
  when track.active do
    let euclideanNodes' = Array.filter (\(PartNode n) -> n.data_.nodeType == "euclidean") allNodes
    for_ euclideanNodes' \(PartNode node) -> do
      let nk = parseEuclideanLabel node.data_.label
      let btnSize = 7.0
      let btnSpacing = 18.0  -- Space between buttons

      -- 4 buttons arranged in a cross pattern in the center
      let euclidControlsTree :: T.Tree Unit
          euclidControlsTree =
            T.named Group ("euclid-controls-" <> show idx)
              [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show centerY <> ")")
              , attr "class" $ text "euclid-controls"
              ]
              `T.withChildren`
                [ -- Label showing current n/k
                  T.elem Text
                    [ x $ num 0.0, y $ num (-22.0)
                    , textContent $ text (show nk.n <> "/" <> show nk.k)
                    , fontSize $ num 10.0, textAnchor $ text "middle"
                    , fill $ text "#4CAF50"
                    , attr "font-weight" $ text "bold"
                    ]
                , -- +n button (top)
                  T.named Group "btn-n-plus"
                    [ attr "transform" $ text ("translate(0," <> show (-btnSpacing * 0.6) <> ")")
                    , attr "style" $ text "cursor: pointer;"
                    ]
                    `T.withBehaviors` [ onClick (onAdjustEuclidean track.trackIndex node.data_.path 1 0) ]
                    `T.withChildren`
                      [ T.elem Circle [ cx $ num 0.0, cy $ num 0.0, r $ num btnSize, fill $ text "#e8f5e9", stroke $ text "#4CAF50", strokeWidth $ num 1.5 ]
                      , T.elem Text [ x $ num 0.0, y $ num 2.5, textContent $ text "+n", fontSize $ num 6.0, textAnchor $ text "middle", fill $ text "#4CAF50", attr "font-weight" $ text "bold" ]
                      ]
                , -- -n button (bottom)
                  T.named Group "btn-n-minus"
                    [ attr "transform" $ text ("translate(0," <> show (btnSpacing * 0.6) <> ")")
                    , attr "style" $ text "cursor: pointer;"
                    ]
                    `T.withBehaviors` [ onClick (onAdjustEuclidean track.trackIndex node.data_.path (-1) 0) ]
                    `T.withChildren`
                      [ T.elem Circle [ cx $ num 0.0, cy $ num 0.0, r $ num btnSize, fill $ text "#ffebee", stroke $ text "#f44336", strokeWidth $ num 1.5 ]
                      , T.elem Text [ x $ num 0.0, y $ num 2.5, textContent $ text "-n", fontSize $ num 6.0, textAnchor $ text "middle", fill $ text "#f44336", attr "font-weight" $ text "bold" ]
                      ]
                , -- +k button (right)
                  T.named Group "btn-k-plus"
                    [ attr "transform" $ text ("translate(" <> show btnSpacing <> ",0)")
                    , attr "style" $ text "cursor: pointer;"
                    ]
                    `T.withBehaviors` [ onClick (onAdjustEuclidean track.trackIndex node.data_.path 0 1) ]
                    `T.withChildren`
                      [ T.elem Circle [ cx $ num 0.0, cy $ num 0.0, r $ num btnSize, fill $ text "#e3f2fd", stroke $ text "#2196F3", strokeWidth $ num 1.5 ]
                      , T.elem Text [ x $ num 0.0, y $ num 2.5, textContent $ text "+k", fontSize $ num 6.0, textAnchor $ text "middle", fill $ text "#2196F3", attr "font-weight" $ text "bold" ]
                      ]
                , -- -k button (left)
                  T.named Group "btn-k-minus"
                    [ attr "transform" $ text ("translate(" <> show (-btnSpacing) <> ",0)")
                    , attr "style" $ text "cursor: pointer;"
                    ]
                    `T.withBehaviors` [ onClick (onAdjustEuclidean track.trackIndex node.data_.path 0 (-1)) ]
                    `T.withChildren`
                      [ T.elem Circle [ cx $ num 0.0, cy $ num 0.0, r $ num btnSize, fill $ text "#fff3e0", stroke $ text "#FF9800", strokeWidth $ num 1.5 ]
                      , T.elem Text [ x $ num 0.0, y $ num 2.5, textContent $ text "-k", fontSize $ num 6.0, textAnchor $ text "middle", fill $ text "#FF9800", attr "font-weight" $ text "bold" ]
                      ]
                ]
      _ <- renderTree zoomGroupSel euclidControlsTree
      pure unit

  -- Outer combinator rings removed - now using chimeric tree visualization instead
  -- See CombinatorTree.purs for the tree-based combinator visualization

  -- Render control buttons below sunburst (mute on left, layout on right)
  let buttonY = centerY + r' + 25.0
  _ <- renderMuteButton zoomGroupSel (centerX - 15.0) buttonY track.trackIndex track.active onToggleActive
  _ <- renderToggleButton zoomGroupSel (centerX + 15.0) buttonY track.trackIndex onToggleLayout "tree"

  -- Render metrics below buttons
  let metrics = analyzePattern track.pattern
  _ <- renderMetrics zoomGroupSel centerX (buttonY + 18.0) metrics track.active idx
  pure unit

-- | Render a single track as vertical tree (top-down)
renderTreeTrack :: D3v2Selection_ SEmpty Element Unit -> TrackWithLayout -> Number -> Number -> Number -> Number -> (Int -> Effect Unit) -> (Int -> Effect Unit) -> (Int -> Array Int -> Effect Unit) -> (Int -> Array Int -> Int -> Int -> Effect Unit) -> Int -> D3v2M Unit
renderTreeTrack zoomGroupSel track centerX centerY r' arcOpacity onToggleActive onToggleLayout onToggleNodeType _onAdjustEuclidean idx = do
  -- Convert pattern to tree and apply layout
  let dataTree = patternTreeToTree track.pattern
  -- Tree fits in cell: width = r' * 1.6, height = r' * 1.4 (leaving room for label and button)
  let treeWidth = r' * 1.6
  let treeHeight = r' * 1.2
  let treeConfig = defaultTreeConfig { size = { width: treeWidth, height: treeHeight } }
  let positioned = tree treeConfig dataTree

  -- Get nodes and links (no radial projection - use cartesian directly)
  let nodes = Array.fromFoldable positioned
  let links = makeLinks positioned

  -- Offset to center the tree horizontally and position from top
  let offsetX = centerX - treeWidth / 2.0
  let offsetY = centerY - r' + 20.0  -- Start near top of cell

  -- Render links as vertical bezier curves
  let linksTree :: T.Tree LinkDatum
      linksTree =
        T.named Group ("tree-links-" <> show idx)
          [ attr "transform" $ text ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          `T.withChild`
            ( T.joinData ("links-" <> show idx) "path" links $ \link ->
                T.elem Path
                  [ path $ text (verticalLinkPath link.source.x link.source.y link.target.x link.target.y)
                  , fill $ text "none"
                  , stroke $ text "#ccc"
                  , strokeWidth $ num 2.0
                  , attr "opacity" $ num arcOpacity
                  ]
            )
  _ <- renderTree zoomGroupSel linksTree

  -- Render nodes (with click handlers on toggleable nodes)
  let nodesTree :: T.Tree PatternNode
      nodesTree =
        T.named Group ("tree-nodes-" <> show idx)
          [ attr "transform" $ text ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          `T.withChild`
            ( T.joinData ("nodes-" <> show idx) "g" nodes $ \node ->
                let isToggleable = node.nodeType == "sequence" || node.nodeType == "parallel"
                    baseNode = T.named Group ("node-" <> node.label)
                      [ attr "class" $ text (if isToggleable then "node-toggleable" else "")
                      , attr "style" $ text (if isToggleable then "cursor: pointer;" else "")
                      ]
                      `T.withChildren`
                        [ T.elem Circle
                            [ cx $ num node.x, cy $ num node.y
                            , r $ num 8.0
                            , fill $ text (nodeColor node.nodeType)
                            , stroke $ text "#fff", strokeWidth $ num 2.0
                            , attr "opacity" $ num arcOpacity
                            ]
                        , T.elem Text
                            [ x $ num node.x, y $ num (node.y - 12.0)
                            , textContent $ text node.label
                            , fontSize $ num 10.0, textAnchor $ text "middle"
                            , fill $ text (if track.active then "#000" else "#999")
                            ]
                        ]
                in if isToggleable
                   then baseNode `T.withBehaviors` [ onClick (onToggleNodeType track.trackIndex node.path) ]
                   else baseNode
            )
  _ <- renderTree zoomGroupSel nodesTree

  -- Render combinator badges below combinator nodes (when track is active)
  when track.active do
    let combinatorNodes = Array.filter (\n -> isCombinator n.nodeType) nodes
    let badgesTree :: T.Tree PatternNode
        badgesTree =
          T.named Group ("tree-badges-" <> show idx)
            [ attr "transform" $ text ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
            `T.withChild`
              ( T.joinData ("tree-badge-labels-" <> show idx) "text" combinatorNodes $ \node ->
                  let badgeText = fromMaybe "" (combinatorBadge node.nodeType)
                  in T.elem Text
                    [ x $ num node.x
                    , y $ num (node.y + 18.0)  -- Below the node
                    , textContent $ text badgeText
                    , fontSize $ num 7.0
                    , textAnchor $ text "middle"
                    , fill $ text (sunburstColor node.nodeType)  -- Use combinator color
                    , attr "font-weight" $ text "bold"
                    , attr "class" $ text "combinator-badge-tree"
                    ]
              )
    _ <- renderTree zoomGroupSel badgesTree
    pure unit

  -- Render track name label at top (no click behavior - use buttons instead)
  let labelTree :: T.Tree Unit
      labelTree =
        T.named Group ("tree-label-" <> show idx)
          [ attr "transform" $ text ("translate(" <> show centerX <> "," <> show (centerY - r' - 5.0) <> ")") ]
          `T.withChild`
            ( T.elem Text
                [ x $ num 0.0, y $ num 0.0
                , textContent $ text track.name
                , fontSize $ num 12.0, textAnchor $ text "middle"
                , fill $ text (if track.active then "#333" else "#999")
                , attr "font-weight" $ text "600"
                ]
            )
  _ <- renderTree zoomGroupSel labelTree

  -- Render control buttons below tree (mute on left, layout on right)
  let buttonY = centerY + r' + 15.0
  _ <- renderMuteButton zoomGroupSel (centerX - 15.0) buttonY track.trackIndex track.active onToggleActive
  _ <- renderToggleButton zoomGroupSel (centerX + 15.0) buttonY track.trackIndex onToggleLayout "sunburst"

  -- Render metrics below buttons
  let metrics = analyzePattern track.pattern
  _ <- renderMetrics zoomGroupSel centerX (buttonY + 18.0) metrics track.active idx
  pure unit

-- | Vertical link path (cubic bezier from top to bottom)
verticalLinkPath :: Number -> Number -> Number -> Number -> String
verticalLinkPath sx sy tx ty =
  let midY = (sy + ty) / 2.0
  in "M" <> show sx <> "," <> show sy
     <> " C" <> show sx <> "," <> show midY
     <> " " <> show tx <> "," <> show midY
     <> " " <> show tx <> "," <> show ty

-- | Render a small toggle button for layout
renderToggleButton :: D3v2Selection_ SEmpty Element Unit -> Number -> Number -> Int -> (Int -> Effect Unit) -> String -> D3v2M Unit
renderToggleButton zoomGroupSel btnX btnY trackIdx onToggle targetLayout = do
  let buttonTree :: T.Tree Unit
      buttonTree =
        T.named Group ("toggle-btn-" <> show trackIdx)
          [ attr "transform" $ text ("translate(" <> show btnX <> "," <> show btnY <> ")")
          , attr "style" $ text "cursor: pointer;"
          , attr "class" $ text "layout-toggle-btn"
          ]
          `T.withBehaviors` [ onClick (onToggle trackIdx) ]
          `T.withChildren`
            [ T.elem Circle
                [ cx $ num 0.0, cy $ num 0.0, r $ num 10.0
                , fill $ text "#f5f5f5"
                , stroke $ text "#999"
                , strokeWidth $ num 1.5
                ]
            , T.elem Text
                [ x $ num 0.0, y $ num 4.0
                , textContent $ text (if targetLayout == "sunburst" then "◉" else "⬡")
                , fontSize $ num 12.0, textAnchor $ text "middle"
                , fill $ text "#666"
                ]
            ]
  _ <- renderTree zoomGroupSel buttonTree
  pure unit

-- | Render a mute/unmute button with speaker icon
renderMuteButton :: D3v2Selection_ SEmpty Element Unit -> Number -> Number -> Int -> Boolean -> (Int -> Effect Unit) -> D3v2M Unit
renderMuteButton zoomGroupSel btnX btnY trackIdx isActive onToggle = do
  let speakerIcon = if isActive
        -- Speaker with sound waves
        then "🔊"
        -- Muted speaker
        else "🔇"
  let buttonTree :: T.Tree Unit
      buttonTree =
        T.named Group ("mute-btn-" <> show trackIdx)
          [ attr "transform" $ text ("translate(" <> show btnX <> "," <> show btnY <> ")")
          , attr "style" $ text "cursor: pointer;"
          , attr "class" $ text "mute-toggle-btn"
          ]
          `T.withBehaviors` [ onClick (onToggle trackIdx) ]
          `T.withChildren`
            [ T.elem Circle
                [ cx $ num 0.0, cy $ num 0.0, r $ num 10.0
                , fill $ text (if isActive then "#e8f5e9" else "#ffebee")
                , stroke $ text (if isActive then "#4CAF50" else "#f44336")
                , strokeWidth $ num 1.5
                ]
            , T.elem Text
                [ x $ num 0.0, y $ num 4.0
                , textContent $ text speakerIcon
                , fontSize $ num 10.0, textAnchor $ text "middle"
                , fill $ text (if isActive then "#4CAF50" else "#f44336")
                ]
            ]
  _ <- renderTree zoomGroupSel buttonTree
  pure unit

-- | Render pattern metrics as a small info line
renderMetrics :: D3v2Selection_ SEmpty Element Unit -> Number -> Number -> PatternMetrics -> Boolean -> Int -> D3v2M Unit
renderMetrics zoomGroupSel metricsX metricsY metrics isActive idx = do
  -- Build a compact metrics string
  let densityPct = Int.round (metrics.density * 100.0)
  let speedStr = if metrics.speedFactor == 1.0 then ""
                 else if metrics.speedFactor > 1.0 then " ×" <> show (Int.round metrics.speedFactor)
                 else " ÷" <> show (Int.round (1.0 / metrics.speedFactor))
  let polyStr = if metrics.maxPolyphony > 1 then " ♪" <> show metrics.maxPolyphony else ""
  let flagsStr = (if metrics.hasEuclidean then " E" else "")
              <> (if metrics.hasProbability then " ?" else "")
  let metricsStr = show metrics.events <> "/" <> show metrics.slots
                <> " (" <> show densityPct <> "%)"
                <> polyStr <> speedStr <> flagsStr

  let metricsTree :: T.Tree Unit
      metricsTree =
        T.named Group ("metrics-" <> show idx)
          [ attr "transform" $ text ("translate(" <> show metricsX <> "," <> show metricsY <> ")") ]
          `T.withChild`
            ( T.elem Text
                [ x $ num 0.0, y $ num 0.0
                , textContent $ text metricsStr
                , fontSize $ num 9.0
                , textAnchor $ text "middle"
                , fill $ text (if isActive then "#666" else "#aaa")
                , attr "font-family" $ text "monospace"
                , attr "class" $ text "pattern-metrics"
                ]
            )
  _ <- renderTree zoomGroupSel metricsTree
  pure unit
