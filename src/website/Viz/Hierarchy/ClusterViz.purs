module D3.Viz.ClusterViz where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, cx, cy, radius, x, y, textAnchor, d)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Core (hierarchy)
import PSD3.Layout.Hierarchy.Core as Hierarchy
import PSD3.Layout.Hierarchy.Cluster (cluster, defaultClusterConfig, ClusterNode(..))
import PSD3.Layout.Hierarchy.Types (HierarchyNode(..), ValuedNode(..))
import Data.Int (toNumber)
import Data.Array (length, (..), (!!))
import Data.Array as Array
import Data.Foldable (sum, foldl)
import Data.Traversable (traverse_)
import Data.Ord (max)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Data.String as String
import D3.Viz.FlareData (HierData, getName)
import D3.Viz.FlareData (getChildren, getValue) as FlareData

-- Convert ValuedNode back to HierarchyNode (discarding value information)
valuedToHierarchy :: forall a. ValuedNode a -> HierarchyNode a
valuedToHierarchy (VNode vn) =
  HNode
    { data_: vn.data_
    , depth: vn.depth
    , height: vn.height
    , parent: Nothing
    , children: map valuedToHierarchy vn.children
    }

-- Sort ValuedNode by height (descending) then value (descending)
-- This is the D3 recommended pattern for dendrograms to minimize crossovers
sortValuedByHeightAndValue :: forall a. ValuedNode a -> ValuedNode a
sortValuedByHeightAndValue (VNode vn) =
  if length vn.children == 0
  then VNode vn
  else
    let
      -- Recursively sort grandchildren first
      sortedGrandchildren = map sortValuedByHeightAndValue vn.children
      -- Sort direct children by height descending, then value descending
      sortedChildren = Array.sortBy comparator sortedGrandchildren
    in
      VNode (vn { children = sortedChildren })
  where
    comparator (VNode a) (VNode b) =
      -- First compare by height (descending)
      case compare b.height a.height of
        EQ -> compare b.value a.value  -- Then by value (descending)
        other -> other

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
getAllNodes :: forall a. ClusterNode a -> Array (ClusterNode a)
getAllNodes node@(ClusterNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- Count total nodes
countNodes :: forall a. ClusterNode a -> Int
countNodes (ClusterNode node) = 1 + sum (map countNodes node.children)

-- Get maximum height (depth) of tree
getMaxHeight :: forall a. ClusterNode a -> Int
getMaxHeight (ClusterNode node) =
  if length node.children == 0
  then 0
  else 1 + foldl max 0 (map getMaxHeight node.children)

-- Print cluster structure
printClusterStructure :: Int -> Int -> String -> ClusterNode HierData -> Effect Unit
printClusterStructure currentDepth maxDepth indent (ClusterNode node) = do
  if currentDepth <= maxDepth then do
    let nodeType = if length node.children == 0 then "leaf" else "internal(" <> show (length node.children) <> " children)"
    let nodeName = getName node.data_
    log $ indent <> nodeName <> " [" <> nodeType <> "]: x=" <> show node.x <> ", y=" <> show node.y <> ", depth=" <> show node.depth
    if currentDepth < maxDepth then
      traverse_ (printClusterStructure (currentDepth + 1) maxDepth (indent <> "  ")) node.children
    else
      pure unit
  else
    pure unit

-- Generate curved path between parent and child (Bezier curve)
-- Based on D3's linkClusterVertical which draws from child to parent
-- with control points at parent.y + levelSpacing/2
makeLinkPath :: Number -> Number -> Number -> Number -> Number -> String
makeLinkPath levelSpacing parentX parentY childX childY =
  let controlY = parentY + levelSpacing / 2.0
  in "M" <> show childX <> "," <> show childY
     <> " C" <> show childX <> "," <> show controlY
     <> " " <> show parentX <> "," <> show controlY
     <> " " <> show parentX <> "," <> show parentY

-- Main drawing function for cluster layout (dendrogram)
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData selector = do
  let chartWidth = 1600.0
  let chartHeight = 1200.0

  -- Log the root data to verify data was loaded
  _ <- liftEffect $ log $ "Loaded JSON - root name: " <> getName flareData

  -- Create hierarchy from blessed Flare data
  let root = hierarchy flareData FlareData.getChildren

  -- Use sum (total value of descendants) for sorting - matches D3 pattern
  let valued = Hierarchy.sum root FlareData.getValue

  -- Sort by height then value (descending) to minimize crossings
  let sortedValued = sortValuedByHeightAndValue valued

  -- Convert back to HierarchyNode for cluster layout
  let sorted = valuedToHierarchy sortedValued

  -- Apply cluster layout with custom size
  let config = defaultClusterConfig
        { size = { width: chartWidth, height: chartHeight }
        }
  let layout = cluster config sorted

  -- Get all nodes for visualization
  let nodes = getAllNodes layout

  -- Calculate level spacing for link curves
  let ClusterNode rootNode = layout
  let maxDepth = rootNode.depth + getMaxHeight layout
  let levelSpacing = chartHeight / toNumber maxDepth

  -- Debug: Check root and first child positions
  _ <- liftEffect $ do
            let ClusterNode rootNode = layout
            let rootName = getName rootNode.data_
            log $ "\n=== VISUALIZATION POSITIONS ==="
            log $ "Root (" <> rootName <> "): x=" <> show rootNode.x
            case Array.head rootNode.children of
              Nothing -> log "No children!"
              Just firstChild -> do
                let ClusterNode fc = firstChild
                let fcName = getName fc.data_
                log $ "First child (" <> fcName <> "): x=" <> show fc.x
                log "First child's children:"
                flip traverse_ fc.children \child -> do
                  let ClusterNode c = child
                  let cName = getName c.data_
                  log $ "  " <> cName <> ": x=" <> show c.x

  -- Debug: Check first few leaf positions
  let leaves = Array.filter (\(ClusterNode n) -> length n.children == 0) nodes
  _ <- liftEffect $ log "\nFirst 10 leaves from visualization:"
  _ <- liftEffect $ flip traverse_ (Array.take 10 leaves) \(ClusterNode n) -> do
    let nodeName = getName n.data_
    log $ nodeName <> ": x=" <> show n.x

  -- Debug: log the cluster structure
  _ <- liftEffect $ log "\n╔═══════════════════════════════════════════════════════════════╗"
  _ <- liftEffect $ log "║   CLUSTER VISUALIZATION: FULL FLARE HIERARCHY (DENDROGRAM)   ║"
  _ <- liftEffect $ log "╚═══════════════════════════════════════════════════════════════╝\n"
  _ <- liftEffect $ printClusterStructure 0 3 "" layout
  _ <- liftEffect $ log $ "\nRendering " <> show (length nodes) <> " nodes (full hierarchy)"
  _ <- liftEffect $ log $ "Total node count: " <> show (countNodes layout)

  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "cluster"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Cluster Layout (Dendrogram)"
    , classed "title"
    ]

  -- Create group for links (so they appear behind nodes)
  linksGroup <- appendTo svg Group [ classed "links" ]

  -- Create group for nodes (so they appear in front)
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Render links first
  let renderLinks :: ClusterNode HierData -> m Unit
      renderLinks (ClusterNode node) = do
        -- Render links to all children
        traverse_ (\(ClusterNode child) -> do
          _ <- appendTo linksGroup Path
            [ d $ makeLinkPath levelSpacing node.x node.y child.x child.y
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
  let renderNode :: ClusterNode HierData -> m Unit
      renderNode (ClusterNode node) = do
        let nodeName = getName node.data_
        let isLeaf = length node.children == 0
        let nodeRadius = if isLeaf then 3.0 else 4.0

        -- Draw circle
        _ <- appendTo nodesGroup Circle
          [ cx node.x
          , cy node.y
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
              [ x (node.x + 8.0)
              , y node.y
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
