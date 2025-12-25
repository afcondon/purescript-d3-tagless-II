module D3.Viz.PatternTree.CombinatorTree
  ( CombinatorNode(..)
  , Combinator
  , drawCombinatorTree
  , drawCombinatorForest
  , exampleCombinatorTree
  , testCombinatorTree
  , buildCombinatorTreesFromTracks
  , TrackInfo
  , TreeMetadata
  , module ReExports
  ) where

import D3.Viz.PatternTree.Types (ZoomTransform, identityZoom) as ReExports

import Prelude

import Component.PatternTree (PatternTree(..), PatternMetrics, analyzePattern)
import Control.Comonad.Cofree (head, tail)
import D3.Viz.PatternTree.Sunburst (patternToHierarchy, sunburstColor)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.String as String
import Data.List ((:))
import Data.List as List
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Partition (PartitionNode(..), defaultPartitionConfig, hierarchy, partition, sunburstArcPath, flattenPartition, fixParallelLayout)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import Effect (Effect)
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, viewBox, x, y)
import PSD3.Internal.Behavior.Types (onClick)
import PSD3.Internal.Behavior.FFI (attachZoomWithCallback_, ZoomTransform)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Operations as Ops
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.Interpreter.D3 (runD3v2M)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

-- | A node in the combinator tree
-- | Either a combinator (with label and enabled state) or a pattern leaf (with actual PatternTree)
data CombinatorNode
  = Combinator String Boolean  -- label and enabled state, e.g., ("slow 6", true)
  | PatternLeaf String PatternTree  -- name and the pattern to render as sunburst

-- | Convert CombinatorNode to tree data for layout
-- | Includes x, y, depth fields required by tree layout algorithm
-- | enabled: whether combinator is active (always true for pattern leaves)
type LayoutNode = { label :: String, isPattern :: Boolean, enabled :: Boolean, pattern :: Maybe PatternTree, x :: Number, y :: Number, depth :: Int }

-- | Draw a combinator tree with sunburst leaves
-- | selector should be a CSS selector (e.g., "#container" or ".viz-area")
-- | initialZoom: zoom transform to restore
-- | onZoomChange: callback when zoom changes
drawCombinatorTree :: String -> Tree CombinatorNode -> ZoomTransform -> (ZoomTransform -> Effect Unit) -> Effect Unit
drawCombinatorTree selector combTree initialZoom onZoomChange = do
  -- Layout parameters
  let width = 1200.0
  let height = 800.0
  let margin = 80.0

  -- Convert to layout tree and apply tree layout
  let layoutTree = mapCombinatorTree combTree
  let treeConfig = defaultTreeConfig { size = { width: width - margin * 2.0, height: height - margin * 2.0 } }
  let positioned = tree treeConfig layoutTree

  -- Flatten tree to array of positioned nodes
  let flattenTree :: Tree LayoutNode -> Array LayoutNode
      flattenTree t = [head t] <> Array.concatMap flattenTree (Array.fromFoldable (tail t))
  let nodes = flattenTree positioned

  -- Build the entire visualization tree declaratively with ConditionalRender
  let links = collectLinks positioned

  -- SVG wrapper with zoom group
  let svgTree :: T.Tree Unit
      svgTree =
        T.named SVG "combinator-tree-svg"
          [ attr "width" $ text "100%"
          , attr "height" $ text "100%"
          , viewBox 0.0 0.0 width height
          , attr "class" $ text "combinator-tree-viz"
          , attr "id" $ text "combinator-tree-svg"
          ]
          `T.withChild`
            -- Zoom group - all content goes inside here
            ( T.named Group "combinator-zoom-group"
                [ attr "id" $ text "combinator-zoom-group"
                , attr "class" $ text "zoom-group"
                ]
                `T.withChild`
                  ( T.named Group "combinator-tree"
                      [ attr "transform" $ text ("translate(" <> show margin <> "," <> show margin <> ")") ]
                      `T.withChildren`
                        [ -- Links first (behind nodes)
                          T.named Group "links" []
                            `T.withChildren`
                              map (\link ->
                                T.elem Path
                                  [ path $ text (verticalLink link.sourceX link.sourceY link.targetX link.targetY)
                                  , fill $ text "none"
                                  , stroke $ text "#9E9E9E"
                                  , strokeWidth $ num 2.0
                                  ]
                              ) links
                        -- Nodes group placeholder (will be populated in step 2)
                        , T.named Group "nodes" [ attr "id" $ text "combinator-nodes" ]
                        ]
                  )
            )

  -- Nodes tree: separate from scaffolding to allow different phantom types
  let nodesTree :: T.Tree LayoutNode
      nodesTree =
        T.named Group "nodes-content" []
          `T.withChild`
            ( T.joinData "chimeric-nodes" "g" nodes $ \node ->
                T.elem Group
                  [ attr "transform" $ text ("translate(" <> show node.x <> "," <> show node.y <> ")") ]
                  `T.withChildren`
                    [ -- CHIMERIC RENDERING: ConditionalRender chooses template based on node type
                      T.conditionalRender
                        [ { predicate: \n -> n.isPattern
                          , spec: miniSunburstTemplate
                          }
                        , { predicate: \n -> not n.isPattern
                          , spec: combinatorNodeTemplate
                          }
                        ]
                    ]
            )

  -- Render using two-step pattern: scaffolding first, then chimeric nodes
  let cssSelector = if String.take 1 selector == "#" then selector else "#" <> selector
  runD3v2M do
    _ <- Ops.clear cssSelector
    container <- select cssSelector
    -- Step 1: Render SVG with scaffolding (Tree Unit) - links and placeholder nodes group
    _ <- renderTree container svgTree
    -- Step 2: Select nodes group and render chimeric tree (Tree LayoutNode)
    -- This is where ConditionalRender chooses between combinator and sunburst templates
    nodesGroup <- select "#combinator-nodes"
    _ <- renderTree nodesGroup nodesTree
    pure unit

  -- Attach zoom behavior after rendering
  doc <- Web.HTML.window >>= Window.document
  let node = HTMLDocument.toNonElementParentNode doc
  maybeSvg <- getElementById "combinator-tree-svg" node
  case maybeSvg of
    Just svgElem -> do
      _ <- attachZoomWithCallback_ svgElem 0.1 10.0 "#combinator-zoom-group" initialZoom onZoomChange
      pure unit
    Nothing -> pure unit

-- | Metadata for each tree in the forest
type TreeMetadata =
  { trackIndex :: Int  -- Original track index in AlgoraveViz
  , active :: Boolean  -- Whether track is muted
  }

-- | Draw a forest of combinator trees in a grid layout
-- | Each tree is laid out independently and positioned in a grid cell
-- | onMuteToggle is called with the original track index when mute button is clicked
-- | onCombinatorToggle is called with (trackIndex, combinatorIndex) when a combinator is clicked
drawCombinatorForest :: String -> Array (Tree CombinatorNode) -> Array TreeMetadata -> (Int -> Effect Unit) -> (Int -> Int -> Effect Unit) -> ZoomTransform -> (ZoomTransform -> Effect Unit) -> Effect Unit
drawCombinatorForest selector trees metadata onMuteToggle onCombinatorToggle initialZoom onZoomChange = do
  let numTrees = Array.length trees
  when (numTrees == 0) $ pure unit

  -- Chart dimensions
  let chartWidth = 1400.0
  let chartHeight = 1200.0

  -- Grid layout: determine columns and rows
  let cols = max 1 (min 5 (Int.ceil (sqrt (Int.toNumber numTrees))))
  let rows = Int.ceil (Int.toNumber numTrees / Int.toNumber cols)

  -- Calculate size for each tree cell
  let cellWidth = (chartWidth - 100.0) / Int.toNumber cols
  let cellHeight = (chartHeight - 100.0) / Int.toNumber rows
  let treeSize = min 280.0 (min cellWidth cellHeight * 0.9)

  -- Starting position (centered in available space)
  let gridWidth = Int.toNumber cols * cellWidth
  let startX = (chartWidth - gridWidth) / 2.0 + cellWidth / 2.0
  let startY = 80.0 + cellHeight / 2.0

  -- Process each tree: layout and collect positioned nodes/links
  -- Zip trees with metadata to get trackIndex and active state
  let treesWithMeta = Array.zipWith (\t m -> { tree: t, meta: m }) trees metadata
  let processedTrees = Array.mapWithIndex (\idx { tree: combTree, meta } ->
        let col = idx `mod` cols
            row = idx / cols
            centerX = startX + Int.toNumber col * cellWidth
            centerY = startY + Int.toNumber row * cellHeight
            -- Layout this tree - height is 50% of width for tighter vertical spacing
            layoutTree = mapCombinatorTree combTree
            margin = 20.0
            treeWidth = treeSize - margin * 2.0
            treeHeight = treeWidth * 0.5  -- 50% less vertical spacing
            treeConfig = defaultTreeConfig { size = { width: treeWidth, height: treeHeight } }
            positioned = tree treeConfig layoutTree
            -- Flatten to nodes and links
            flattenTree :: Tree LayoutNode -> Array LayoutNode
            flattenTree t = [head t] <> Array.concatMap flattenTree (Array.fromFoldable (tail t))
            nodes = flattenTree positioned
            links = collectLinks positioned
            -- Find the sunburst node (pattern leaf) for button positioning
            sunburstNode = Array.find (\n -> n.isPattern) nodes
            -- Collect combinator nodes with their indices (for click handling)
            -- Index corresponds to position in the combinator chain
            combinatorNodes = Array.mapWithIndex (\i n -> { combIndex: i, node: n })
                              $ Array.filter (\n -> not n.isPattern) nodes
        in { idx, centerX, centerY, nodes, links, treeSize, trackIndex: meta.trackIndex, active: meta.active, sunburstNode, combinatorNodes }
      ) treesWithMeta

  -- Build SVG with all trees
  let svgTree :: T.Tree Unit
      svgTree =
        T.named SVG "combinator-forest-svg"
          [ attr "width" $ text "100%"
          , attr "height" $ text "100%"
          , viewBox 0.0 0.0 chartWidth chartHeight
          , attr "class" $ text "combinator-forest-viz"
          , attr "id" $ text "combinator-forest-svg"
          ]
          `T.withChild`
            -- Zoom group - all content goes inside here
            ( T.named Group "forest-zoom-group"
                [ attr "id" $ text "forest-zoom-group"
                , attr "class" $ text "zoom-group"
                ]
                `T.withChildren`
                  -- Create a group for each tree with links
                  ( Array.concatMap (\t ->
                      [ T.named Group ("tree-" <> show t.idx)
                          [ attr "transform" $ text ("translate(" <> show (t.centerX - t.treeSize / 2.0) <> "," <> show (t.centerY - t.treeSize / 2.0) <> ")") ]
                          `T.withChildren`
                            [ -- Links for this tree
                              T.named Group ("links-" <> show t.idx) []
                                `T.withChildren`
                                  map (\link ->
                                    T.elem Path
                                      [ path $ text (verticalLink link.sourceX link.sourceY link.targetX link.targetY)
                                      , fill $ text "none"
                                      , stroke $ text "#9E9E9E"
                                      , strokeWidth $ num 1.5
                                      ]
                                  ) t.links
                            -- Placeholder for nodes (will be filled in step 2)
                            , T.named Group ("nodes-" <> show t.idx) [ attr "id" $ text ("forest-nodes-" <> show t.idx) ]
                            ]
                      ]
                    ) processedTrees
                  )
            )

  -- Render SVG scaffolding
  let cssSelector = if String.take 1 selector == "#" then selector else "#" <> selector
  runD3v2M do
    _ <- Ops.clear cssSelector
    container <- select cssSelector
    _ <- renderTree container svgTree
    pure unit

  -- Render nodes for each tree (step 2 - chimeric rendering)
  for_ processedTrees \t -> do
    let nodesTree :: T.Tree LayoutNode
        nodesTree =
          T.named Group "nodes-content" []
            `T.withChild`
              ( T.joinData ("chimeric-nodes-" <> show t.idx) "g" t.nodes $ \node ->
                  T.elem Group
                    [ attr "transform" $ text ("translate(" <> show node.x <> "," <> show node.y <> ")") ]
                    `T.withChildren`
                      [ T.conditionalRender
                          [ { predicate: \n -> n.isPattern
                            , spec: miniSunburstTemplate
                            }
                          , { predicate: \n -> not n.isPattern
                            , spec: combinatorNodeTemplate
                            }
                          ]
                      ]
              )
    runD3v2M do
      nodesGroup <- select ("#forest-nodes-" <> show t.idx)
      _ <- renderTree nodesGroup nodesTree
      pure unit

  -- Render mute buttons at the center of each sunburst (step 3)
  for_ processedTrees \t -> do
    case t.sunburstNode of
      Just sn -> do
        -- Position: tree offset + node position + sunburst center offset
        let btnX = t.centerX - t.treeSize / 2.0 + sn.x
        let btnY = t.centerY - t.treeSize / 2.0 + sn.y + 60.0  -- 60.0 is sunburst radius offset
        let btnRadius = 18.0
        let btnColor = if t.active then "#4CAF50" else "#757575"  -- Green when active, gray when muted
        let btnIcon = if t.active then "▶" else "◼"  -- Play/stop icon
        let muteBtn :: T.Tree Unit
            muteBtn =
              T.named Group ("mute-btn-" <> show t.trackIndex)
                [ attr "transform" $ text ("translate(" <> show btnX <> "," <> show btnY <> ")")
                , attr "class" $ text "mute-button"
                , attr "style" $ text "cursor: pointer;"
                ]
                `T.withChildren`
                  [ T.elem Circle
                      [ cx $ num 0.0
                      , cy $ num 0.0
                      , r $ num btnRadius
                      , fill $ text btnColor
                      , stroke $ text "white"
                      , strokeWidth $ num 2.0
                      ]
                  , T.elem Text
                      [ x $ num 0.0
                      , y $ num 5.0
                      , textContent $ text btnIcon
                      , fontSize $ num 14.0
                      , fill $ text "white"
                      , textAnchor $ text "middle"
                      ]
                  ]
                `T.withBehaviors`
                  [ onClick (onMuteToggle t.trackIndex) ]
        runD3v2M do
          zoomGroup <- select "#forest-zoom-group"
          _ <- renderTree zoomGroup muteBtn
          pure unit
      Nothing -> pure unit

  -- Render combinator click overlays (step 4)
  for_ processedTrees \t -> do
    for_ t.combinatorNodes \cNode -> do
      -- Position: tree offset + node position
      let clickX = t.centerX - t.treeSize / 2.0 + cNode.node.x
      let clickY = t.centerY - t.treeSize / 2.0 + cNode.node.y
      let clickRadius = 12.0  -- Slightly larger than the visible circle for easier clicking
      let clickOverlay :: T.Tree Unit
          clickOverlay =
            T.named Group ("comb-click-" <> show t.trackIndex <> "-" <> show cNode.combIndex)
              [ attr "transform" $ text ("translate(" <> show clickX <> "," <> show clickY <> ")")
              , attr "class" $ text "combinator-click-overlay"
              ]
              `T.withChild`
                T.elem Circle
                  [ cx $ num 0.0
                  , cy $ num 0.0
                  , r $ num clickRadius
                  , fill $ text "transparent"
                  , attr "style" $ text "cursor: pointer;"
                  ]
              `T.withBehaviors`
                [ onClick (onCombinatorToggle t.trackIndex cNode.combIndex) ]
      runD3v2M do
        zoomGroup <- select "#forest-zoom-group"
        _ <- renderTree zoomGroup clickOverlay
        pure unit

  -- Attach zoom behavior
  doc <- Web.HTML.window >>= Window.document
  let node = HTMLDocument.toNonElementParentNode doc
  maybeSvg <- getElementById "combinator-forest-svg" node
  case maybeSvg of
    Just svgElem -> do
      _ <- attachZoomWithCallback_ svgElem 0.1 10.0 "#forest-zoom-group" initialZoom onZoomChange
      pure unit
    Nothing -> pure unit

-- | Map CombinatorNode tree to layout tree
-- | Initial x, y, depth values don't matter - they'll be overwritten by tree layout
mapCombinatorTree :: Tree CombinatorNode -> Tree LayoutNode
mapCombinatorTree t =
  let val = case head t of
        Combinator label enabled -> { label, isPattern: false, enabled, pattern: Nothing, x: 0.0, y: 0.0, depth: 0 }
        PatternLeaf name pat -> { label: name, isPattern: true, enabled: true, pattern: Just pat, x: 0.0, y: 0.0, depth: 0 }
      children = map mapCombinatorTree (tail t)
  in mkTree val children

-- =============================================================================
-- CHIMERIC TEMPLATES: Functions that return Tree LayoutNode for ConditionalRender
-- =============================================================================

-- | Template for combinator nodes: subtle circles with prominent labels
-- | Used by ConditionalRender when node.isPattern == false
-- | Shows enabled/disabled state: purple when enabled, gray with strikethrough when disabled
combinatorNodeTemplate :: LayoutNode -> T.Tree LayoutNode
combinatorNodeTemplate node =
  let bgColor = if node.enabled then "#f5f0f8" else "#e0e0e0"  -- Light purple or gray
      strokeColor = if node.enabled then "#d0c0d8" else "#bdbdbd"  -- Purple or gray stroke
      textColor = if node.enabled then "#5E35B1" else "#9E9E9E"  -- Deep purple or gray text
      textDecor = if node.enabled then "none" else "line-through"  -- Strikethrough when disabled
  in T.named Group ("comb-" <> node.label) [ attr "style" $ text "cursor: pointer;" ]
    `T.withChildren`
      [ -- Subtle background circle
        T.elem Circle
          [ cx $ num 0.0
          , cy $ num 0.0
          , r $ num 10.0  -- Smaller
          , fill $ text bgColor
          , stroke $ text strokeColor
          , strokeWidth $ num 1.0
          ]
      -- Prominent label
      , T.elem Text
          [ x $ num 0.0
          , y $ num 5.0
          , textContent $ text node.label
          , fontSize $ num 14.0  -- Even larger
          , textAnchor $ text "middle"
          , fill $ text textColor
          , attr "font-weight" $ text "700"  -- Bold
          , attr "font-family" $ text "system-ui, sans-serif"
          , attr "text-decoration" $ text textDecor
          ]
      ]

-- | Template for pattern leaf nodes: mini sunburst diagrams with metrics
-- | Used by ConditionalRender when node.isPattern == true
miniSunburstTemplate :: LayoutNode -> T.Tree LayoutNode
miniSunburstTemplate node =
  case node.pattern of
    Nothing -> combinatorNodeTemplate node  -- Fallback if no pattern
    Just pattern ->
      -- Build hierarchy from pattern
      let hierData = patternToHierarchy pattern
          partRoot = hierarchy hierData
          config = defaultPartitionConfig { size = { width: 1.0, height: 1.0 }, padding = 0.01 }
          partitioned = partition config partRoot
          fixed = fixParallelLayout (\d -> d.nodeType == "parallel") partitioned
          allNodes = flattenPartition fixed
          nodes = Array.filter (\(PartNode n) -> n.depth > 0) allNodes

          -- Find root for center color
          rootNode = Array.find (\(PartNode n) -> n.depth == 0) allNodes
          rootType = case rootNode of
            Just (PartNode n) -> n.data_.nodeType
            Nothing -> "sequence"

          -- Build mini sunburst arcs - 50% bigger
          radius = 60.0
          innerRadius = radius * 0.3
          arcs = map (\(PartNode n) ->
                let arcPath = sunburstArcPath n.x0 n.y0 n.x1 n.y1 radius
                    fillColor = sunburstColor n.data_.nodeType
                in { path: arcPath, fill: fillColor }
              ) nodes

          -- Compute metrics
          metrics = analyzePattern pattern
          metricsStr = formatMetrics metrics
      in
        -- Offset sunburst down by radius to clear combinator labels above
        T.named Group ("mini-sunburst-" <> node.label)
          [ attr "transform" $ text ("translate(0," <> show radius <> ")") ]
          `T.withChildren`
            ( -- Arcs
              map (\arc ->
                T.elem Path
                  [ path $ text arc.path
                  , fill $ text arc.fill
                  , stroke $ text "#fff"
                  , strokeWidth $ num 0.5
                  ]
              ) arcs
              <>
              -- Center circle with root color
              [ T.elem Circle
                  [ cx $ num 0.0
                  , cy $ num 0.0
                  , r $ num innerRadius
                  , fill $ text (sunburstColor rootType)
                  , stroke $ text "#fff"
                  , strokeWidth $ num 1.0
                  ]
              -- Label below sunburst
              , T.elem Text
                  [ x $ num 0.0
                  , y $ num (radius + 14.0)
                  , textContent $ text node.label
                  , fontSize $ num 11.0
                  , textAnchor $ text "middle"
                  , fill $ text "#333"
                  , attr "font-weight" $ text "600"
                  ]
              -- Metrics below label
              , T.elem Text
                  [ x $ num 0.0
                  , y $ num (radius + 26.0)
                  , textContent $ text metricsStr
                  , fontSize $ num 9.0
                  , textAnchor $ text "middle"
                  , fill $ text "#666"
                  , attr "font-family" $ text "monospace"
                  ]
              ]
            )

-- | Format pattern metrics as a compact string
formatMetrics :: PatternMetrics -> String
formatMetrics m =
  let densityPct = Int.round (m.density * 100.0)
      speedStr = if m.speedFactor == 1.0 then ""
                 else if m.speedFactor > 1.0 then " ×" <> show (Int.round m.speedFactor)
                 else " ÷" <> show (Int.round (1.0 / m.speedFactor))
      polyStr = if m.maxPolyphony > 1 then " ♪" <> show m.maxPolyphony else ""
      flagsStr = (if m.hasEuclidean then " E" else "")
              <> (if m.hasProbability then " ?" else "")
  in show m.events <> "/" <> show m.slots <> " (" <> show densityPct <> "%)" <> polyStr <> speedStr <> flagsStr

-- | Collect all links from tree (parent-child pairs for drawing connections)
collectLinks :: Tree LayoutNode -> Array { sourceX :: Number, sourceY :: Number, targetX :: Number, targetY :: Number }
collectLinks t =
  let node = head t
      childLinks = Array.concatMap (\child ->
        let childNode = head child
        in [{ sourceX: node.x, sourceY: node.y, targetX: childNode.x, targetY: childNode.y }]
           <> collectLinks child
      ) (Array.fromFoldable (tail t))
  in childLinks

-- | Create a vertical bezier link path
verticalLink :: Number -> Number -> Number -> Number -> String
verticalLink x0 y0 x1 y1 =
  let midY = (y0 + y1) / 2.0
  in "M" <> show x0 <> "," <> show y0
     <> "C" <> show x0 <> "," <> show midY
     <> " " <> show x1 <> "," <> show midY
     <> " " <> show x1 <> "," <> show y1

-- | Example combinator tree for testing
-- | Represents: jux rev $ slow 2 [bd sd:3, ~ hh] # [hh*4]
-- | Structure:
-- |   jux rev
-- |   ├── slow 2
-- |   │   └── [bd sd:3, ~ hh] (pattern)
-- |   └── [hh*4] (pattern)
exampleCombinatorTree :: Tree CombinatorNode
exampleCombinatorTree =
  mkTree (Combinator "jux rev" true)
    ( mkTree (Combinator "slow 2" true)
        ( mkTree (PatternLeaf "drums"
            (Sequence
              [ Parallel [Sound "bd", Sound "sd:3"]
              , Parallel [Rest, Sound "hh"]
              ]
            )) List.Nil
        : List.Nil
        )
    : mkTree (PatternLeaf "hats"
        (Fast 4.0 (Sound "hh"))
      ) List.Nil
    : List.Nil
    )

-- | Test function: draws the example combinator tree on the given selector
-- | Use this from the browser console or from a test button
-- | Example: testCombinatorTree "combinator-test-svg"
testCombinatorTree :: String -> Effect Unit
testCombinatorTree selector = drawCombinatorTree selector exampleCombinatorTree ReExports.identityZoom (const $ pure unit)

-- | A combinator with enable/disable state
type Combinator =
  { label :: String
  , enabled :: Boolean
  }

-- | Track info for building combinator trees from AlgoraveViz tracks
type TrackInfo =
  { name :: String
  , pattern :: PatternTree
  , combinators :: Array Combinator  -- Combinators wrapping this pattern (outermost first)
  }

-- | Build an array of combinator trees from tracks
-- | Each track becomes its own tree (a chain of combinator nodes ending in a pattern leaf)
-- | Example input:
-- |   [ { name: "L4-poly", pattern: ..., combinators: ["jux rev", "chop 4"] }
-- |   , { name: "L6-tabla", pattern: ..., combinators: ["slow 6"] }
-- |   ]
-- | Produces array of trees:
-- |   [ jux rev → chop 4 → L4-poly (sunburst)
-- |   , slow 6 → L6-tabla (sunburst)
-- |   ]
buildCombinatorTreesFromTracks :: Array TrackInfo -> Array (Tree CombinatorNode)
buildCombinatorTreesFromTracks = map trackToTree
  where
    -- Convert a single track to a combinator chain
    trackToTree :: TrackInfo -> Tree CombinatorNode
    trackToTree track =
      case Array.uncons track.combinators of
        -- No combinators - just the pattern leaf
        Nothing -> mkTree (PatternLeaf track.name track.pattern) List.Nil
        -- Has combinators - build the chain
        Just { head: firstComb, tail: restCombs } ->
          mkTree (Combinator firstComb.label firstComb.enabled) (buildChain restCombs track : List.Nil)

    -- Build the rest of the combinator chain
    buildChain :: Array Combinator -> TrackInfo -> Tree CombinatorNode
    buildChain combs track =
      case Array.uncons combs of
        -- No more combinators - end with pattern leaf
        Nothing -> mkTree (PatternLeaf track.name track.pattern) List.Nil
        -- More combinators in chain
        Just { head: c, tail: rest } ->
          mkTree (Combinator c.label c.enabled) (buildChain rest track : List.Nil)
