module D3.Viz.TreeViz where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, cx, cy, radius, x, y, textAnchor, d)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Core (hierarchy)
import PSD3.Layout.Hierarchy.Tree (tree, defaultTreeConfig, TreeNode(..))
import PSD3.Layout.Hierarchy.Projection (verticalX, verticalY, verticalLinkPath)
import D3.Viz.FlareData (HierData, getName, getValue)
import D3.Viz.FlareData (getChildren) as FlareData
import Data.Array (length, (..), (!!))
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl, sum)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Data.String as String

-- Use the FFI getChildren accessor
getChildren :: HierData -> Maybe (Array HierData)
getChildren = FlareData.getChildren

-- Simple color palette for depth-based coloring
colors :: Array String
colors =
  [ "#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd"
  , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ]

-- Get color by depth
getColor :: Int -> String
getColor depth =
  let idx = depth `mod` (length colors)
  in case colors !! idx of
       Just c -> c
       Nothing -> "#cccccc"

-- Get all nodes (recursive traversal)
getAllNodes :: forall a. TreeNode a -> Array (TreeNode a)
getAllNodes node@(TreeNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- Count total nodes
countNodes :: forall a. TreeNode a -> Int
countNodes (TreeNode node) = 1 + sum (map countNodes node.children)

-- Print tree structure
printTreeStructure :: Int -> Int -> String -> TreeNode HierData -> Effect Unit
printTreeStructure currentDepth maxDepth indent (TreeNode node) = do
  if currentDepth <= maxDepth then do
    let nodeType = if length node.children == 0 then "leaf" else "internal(" <> show (length node.children) <> " children)"
    let nodeName = getName node.data_
    log $ indent <> nodeName <> " [" <> nodeType <> "]: x=" <> show node.x <> ", y=" <> show node.y <> ", depth=" <> show node.depth
    if currentDepth < maxDepth then
      traverse_ (printTreeStructure (currentDepth + 1) maxDepth (indent <> "  ")) node.children
    else
      pure unit
  else
    pure unit

-- REMOVED: Local makeLinkPath function
-- Now using verticalLinkPath from PSD3.Layout.Hierarchy.Projection

-- Main drawing function for tree layout
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData selector = do
  let chartWidth = 1600.0
  let chartHeight = 1200.0

  -- Create hierarchy from blessed Flare data
  let root = hierarchy flareData getChildren

  -- Apply tree layout with custom size
  let config = defaultTreeConfig
        { size = { width: chartWidth, height: chartHeight }
        }
  let layout = tree config root

  -- Get all nodes for visualization
  let nodes = getAllNodes layout

  -- Debug: log the tree structure
  _ <- liftEffect $ log "\n╔═══════════════════════════════════════════════════════════════╗"
  _ <- liftEffect $ log "║   TREE VISUALIZATION: FULL FLARE HIERARCHY                    ║"
  _ <- liftEffect $ log "╚═══════════════════════════════════════════════════════════════╝\n"
  _ <- liftEffect $ printTreeStructure 0 3 "" layout
  _ <- liftEffect $ log $ "\nRendering " <> show (length nodes) <> " nodes (full hierarchy)"
  _ <- liftEffect $ log $ "Total node count: " <> show (countNodes layout)

  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "tree"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Tree Layout (Reingold-Tilford)"
    , classed "title"
    ]

  -- Create group for links (so they appear behind nodes)
  linksGroup <- appendTo svg Group [ classed "links" ]

  -- Create group for nodes (so they appear in front)
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Render links first
  let renderLinks :: TreeNode HierData -> m Unit
      renderLinks parent@(TreeNode node) = do
        -- Render links to all children
        traverse_ (\child -> do
          _ <- appendTo linksGroup Path
            [ d $ verticalLinkPath (verticalX parent) (verticalY parent) (verticalX child) (verticalY child)
            , fill "none"
            , strokeColor "#555"
            , fillOpacity 0.4
            , strokeWidth 1.5
            , classed "link"
            ]
          pure unit
        ) node.children

        -- Recursively render children's links
        traverse_ renderLinks node.children

  -- Render nodes (circles and labels)
  let renderNode :: TreeNode HierData -> m Unit
      renderNode treeNode@(TreeNode node) = do
        let nodeName = getName node.data_
        let isLeaf = length node.children == 0
        let nodeRadius = if isLeaf then 3.0 else 4.0

        -- Draw circle using projection accessors
        _ <- appendTo nodesGroup Circle
          [ cx $ verticalX treeNode
          , cy $ verticalY treeNode
          , radius nodeRadius
          , fill "#999"
          , strokeColor "#555"
          , strokeWidth 1.5
          , classed "node"
          , classed $ "depth-" <> show node.depth
          , classed $ if isLeaf then "leaf" else "internal"
          ]

        -- Add label for internal nodes (non-leaf)
        if not isLeaf
          then do
            _ <- appendTo nodesGroup Text
              [ x $ verticalX treeNode + 8.0
              , y $ verticalY treeNode
              , fill "#333"
              , fontSize 10.0
              , text nodeName
              , classed "label"
              ]
            pure unit
          else pure unit

        -- Recursively render children
        traverse_ renderNode node.children

  -- Render all links
  _ <- renderLinks layout

  -- Render all nodes
  _ <- renderNode layout

  pure unit
