module D3.Viz.PartitionViz where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, x, y, height, width, d)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Partition (partition, defaultPartitionConfig, PartitionNode(..), hierarchy, HierarchyData(..))
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
import D3.Viz.FlareData (HierData, getName, getValue)
import D3.Viz.FlareData (getChildren) as FlareData

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
getAllNodes :: forall a. PartitionNode a -> Array (PartitionNode a)
getAllNodes node@(PartNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- Count total nodes
countNodes :: forall a. PartitionNode a -> Int
countNodes (PartNode node) = 1 + sum (map countNodes node.children)

-- Print partition structure
printPartitionStructure :: Int -> Int -> String -> PartitionNode HierData -> Effect Unit
printPartitionStructure currentDepth maxDepth indent (PartNode node) = do
  if currentDepth <= maxDepth then do
    let nodeType = if length node.children == 0 then "leaf" else "internal(" <> show (length node.children) <> " children)"
    let nodeName = getName node.data_
    log $ indent <> nodeName <> " [" <> nodeType <> "]: x0=" <> show node.x0 <> ", y0=" <> show node.y0 <> ", x1=" <> show node.x1 <> ", y1=" <> show node.y1 <> ", depth=" <> show node.depth
    if currentDepth < maxDepth then
      traverse_ (printPartitionStructure (currentDepth + 1) maxDepth (indent <> "  ")) node.children
    else
      pure unit
  else
    pure unit

-- Convert FlareData to HierarchyData for Partition layout
toHierarchyData :: HierData -> HierarchyData HierData
toHierarchyData node = HierarchyData
  { data_: node
  , value:
      -- Only set explicit value for leaf nodes
      -- Internal nodes will get their value from sum of children
      case FlareData.getChildren node of
        Nothing -> Just (getValue node)  -- Leaf node
        Just _ -> Nothing                 -- Internal node - let it sum children
  , children: map (map toHierarchyData) (FlareData.getChildren node)
  }

-- Main drawing function for partition layout (icicle)
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

  -- Convert FlareData to HierarchyData and create hierarchy
  let hierData = toHierarchyData flareData
  let root = hierarchy hierData

  -- Apply partition layout with custom size
  let config = defaultPartitionConfig
        { size = { width: chartWidth, height: chartHeight }
        }
  let layout = partition config root

  -- Get all nodes for visualization
  let nodes = getAllNodes layout

  -- Debug: log the partition structure
  _ <- liftEffect $ log "\n╔═══════════════════════════════════════════════════════════════╗"
  _ <- liftEffect $ log "║   PARTITION VISUALIZATION: FULL FLARE HIERARCHY (ICICLE)     ║"
  _ <- liftEffect $ log "╚═══════════════════════════════════════════════════════════════╝\n"
  _ <- liftEffect $ printPartitionStructure 0 2 "" layout
  _ <- liftEffect $ log $ "\nRendering " <> show (length nodes) <> " nodes (full hierarchy)"
  _ <- liftEffect $ log $ "Total node count: " <> show (countNodes layout)

  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "partition"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Partition Layout (Icicle)"
    , classed "title"
    ]

  -- Create group for rectangles
  rectsGroup <- appendTo svg Group [ classed "rectangles" ]

  -- Render rectangles for all nodes
  let renderNode :: PartitionNode HierData -> m Unit
      renderNode (PartNode node) = do
        let nodeName = getName node.data_
        let rectWidth = node.x1 - node.x0
        let rectHeight = node.y1 - node.y0

        -- Only render if rectangle has size
        if rectWidth > 0.5 && rectHeight > 0.5
          then do
            -- Draw rectangle
            _ <- appendTo rectsGroup Rect
              [ x node.x0
              , y node.y0
              , width rectWidth
              , height rectHeight
              , fill $ getColor node.depth
              , strokeColor "#fff"
              , strokeWidth 1.0
              , fillOpacity 0.7
              , classed "node"
              , classed $ "depth-" <> show node.depth
              ]

            -- Add label if rectangle is large enough
            if rectWidth > 50.0 && rectHeight > 15.0
              then do
                _ <- appendTo rectsGroup Text
                  [ x (node.x0 + 4.0)
                  , y (node.y0 + rectHeight / 2.0 + 4.0)
                  , fill "#333"
                  , fontSize 10.0
                  , text nodeName
                  , classed "label"
                  ]
                pure unit
              else pure unit

            pure unit
          else pure unit

        -- Recursively render children
        traverse_ renderNode node.children

  -- Render all nodes
  _ <- renderNode layout

  pure unit
