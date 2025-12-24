module D3.Viz.PatternTree.CombinatorTree
  ( CombinatorNode(..)
  , drawCombinatorTree
  , exampleCombinatorTree
  , testCombinatorTree
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import Control.Comonad.Cofree (head, tail)
import D3.Viz.PatternTree.Sunburst (patternToHierarchy, sunburstColor, sunburstArcPath, flattenPartition, fixParallelLayout)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.List ((:))
import Data.List as List
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Partition (PartitionNode(..), defaultPartitionConfig, hierarchy, partition)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import Effect (Effect)
import PSD3.AST as T
import PSD3.Expr.Friendly (attr, cx, cy, fill, fontSize, num, path, r, stroke, strokeWidth, text, textAnchor, textContent, x, y)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Selection.Operations as Ops
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.Interpreter.D3 (runD3v2M)
import Web.DOM.Element as Element
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

-- | A node in the combinator tree
-- | Either a combinator (with label) or a pattern leaf (with actual PatternTree)
data CombinatorNode
  = Combinator String  -- e.g., "slow 6", "jux rev", "chop 4"
  | PatternLeaf String PatternTree  -- name and the pattern to render as sunburst

-- | Convert CombinatorNode to tree data for layout
-- | Includes x, y, depth fields required by tree layout algorithm
type LayoutNode = { label :: String, isPattern :: Boolean, pattern :: Maybe PatternTree, x :: Number, y :: Number, depth :: Int }

-- | Draw a combinator tree with sunburst leaves
-- | selector should be a CSS selector (e.g., "#container" or ".viz-area")
drawCombinatorTree :: String -> Tree CombinatorNode -> Effect Unit
drawCombinatorTree selector combTree = do
  -- Layout parameters
  let width = 800.0
  let height = 600.0
  let margin = 60.0

  -- Set SVG dimensions (query the element first)
  doc <- Web.HTML.window >>= Window.document
  -- Handle both "#id" and "id" formats for getElementById
  let elementId = if String.take 1 selector == "#" then String.drop 1 selector else selector
  maybeContainer <- getElementById elementId (HTMLDocument.toNonElementParentNode doc)
  case maybeContainer of
    Nothing -> pure unit
    Just container -> do
      Element.setAttribute "width" (show width) container
      Element.setAttribute "height" (show height) container
      Element.setAttribute "viewBox" ("0 0 " <> show width <> " " <> show height) container

      -- Convert to layout tree and apply tree layout
      let layoutTree = mapCombinatorTree combTree
      let treeConfig = defaultTreeConfig { size = { width: width - margin * 2.0, height: height - margin * 2.0 } }
      let positioned = tree treeConfig layoutTree

      -- Flatten tree to array of positioned nodes
      -- Tree layout returns Tree LayoutNode where LayoutNode has x, y fields already
      let flattenTree :: Tree LayoutNode -> Array LayoutNode
          flattenTree t = [head t] <> Array.concatMap flattenTree (Array.fromFoldable (tail t))
      let nodes = flattenTree positioned

      -- Build the entire visualization tree declaratively with ConditionalRender
      let links = collectLinks positioned

      -- CHIMERIC TREE: Uses ConditionalRender to switch between combinator and sunburst templates
      let vizTree :: T.Tree Unit
          vizTree =
            T.named Group "combinator-tree"
              [ attr "transform" $ text ("translate(" <> show margin <> "," <> show margin <> ")") ]
              `T.withChildren`
                ( -- Links first (behind nodes)
                  [ T.named Group "links" []
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
                  , T.named Group "nodes" [ attr "id" $ text "nodes" ]
                  ]
                )

      -- Nodes tree: separate from scaffolding to allow different phantom types
      let nodesTree :: T.Tree LayoutNode
          nodesTree =
            T.named Group "nodes" []
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
        svg <- select cssSelector
        -- Step 1: Render scaffolding (Tree Unit) with links and placeholder nodes group
        _ <- renderTree svg vizTree
        -- Step 2: Select nodes group and render chimeric tree (Tree LayoutNode)
        -- This is where ConditionalRender chooses between combinator and sunburst templates
        nodesGroup <- select "#nodes"
        _ <- renderTree nodesGroup nodesTree
        pure unit

-- | Map CombinatorNode tree to layout tree
-- | Initial x, y, depth values don't matter - they'll be overwritten by tree layout
mapCombinatorTree :: Tree CombinatorNode -> Tree LayoutNode
mapCombinatorTree t =
  let val = case head t of
        Combinator label -> { label, isPattern: false, pattern: Nothing, x: 0.0, y: 0.0, depth: 0 }
        PatternLeaf name pat -> { label: name, isPattern: true, pattern: Just pat, x: 0.0, y: 0.0, depth: 0 }
      children = map mapCombinatorTree (tail t)
  in mkTree val children

-- =============================================================================
-- CHIMERIC TEMPLATES: Functions that return Tree LayoutNode for ConditionalRender
-- =============================================================================

-- | Template for combinator nodes: labeled purple circles
-- | Used by ConditionalRender when node.isPattern == false
combinatorNodeTemplate :: LayoutNode -> T.Tree LayoutNode
combinatorNodeTemplate node =
  T.named Group ("comb-" <> node.label) []
    `T.withChildren`
      [ T.elem Circle
          [ cx $ num 0.0
          , cy $ num 0.0
          , r $ num 20.0
          , fill $ text "#E1BEE7"  -- Light purple for combinators
          , stroke $ text "#7B1FA2"
          , strokeWidth $ num 2.0
          ]
      , T.elem Text
          [ x $ num 0.0
          , y $ num 4.0
          , textContent $ text node.label
          , fontSize $ num 10.0
          , textAnchor $ text "middle"
          , fill $ text "#4A148C"
          , attr "font-weight" $ text "600"
          ]
      ]

-- | Template for pattern leaf nodes: mini sunburst diagrams
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
          fixed = fixParallelLayout partitioned
          allNodes = flattenPartition fixed
          nodes = Array.filter (\(PartNode n) -> n.depth > 0) allNodes

          -- Find root for center color
          rootNode = Array.find (\(PartNode n) -> n.depth == 0) allNodes
          rootType = case rootNode of
            Just (PartNode n) -> n.data_.nodeType
            Nothing -> "sequence"

          -- Build mini sunburst arcs
          radius = 40.0
          innerRadius = radius * 0.3
          arcs = map (\(PartNode n) ->
                let arcPath = sunburstArcPath n.x0 n.y0 n.x1 n.y1 radius
                    fillColor = sunburstColor n.data_.nodeType
                in { path: arcPath, fill: fillColor }
              ) nodes
      in
        T.named Group ("mini-sunburst-" <> node.label) []
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
              -- Label below
              , T.elem Text
                  [ x $ num 0.0
                  , y $ num (radius + 12.0)
                  , textContent $ text node.label
                  , fontSize $ num 10.0
                  , textAnchor $ text "middle"
                  , fill $ text "#333"
                  ]
              ]
            )

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
  mkTree (Combinator "jux rev")
    ( mkTree (Combinator "slow 2")
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
testCombinatorTree selector = drawCombinatorTree selector exampleCombinatorTree
