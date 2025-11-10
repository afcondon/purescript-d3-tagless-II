module D3.Viz.TreemapViz where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fontSize, height, strokeColor, strokeWidth, text, viewBox, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Core (hierarchy, sum) as H
import PSD3.Layout.Hierarchy.Treemap (TreemapNode(..), TileFunction, binary, defaultTreemapConfig, dice, slice, sliceDice, squarify, treemap, phi)
import Data.Array (length, (!!))
import Data.Foldable (traverse_)
import Data.Foldable as Data.Foldable
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import D3.Viz.FlareData (HierData, getName, getValue)
import D3.Viz.FlareData (getChildren) as FlareData

-- Children accessor (wrapper for FFI function)
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

-- Get all leaf nodes
getLeaves :: forall a. TreemapNode a -> Array (TreemapNode a)
getLeaves node@(TNode n) =
  if length n.children == 0
  then [node]
  else n.children >>= getLeaves

-- Get maximum depth
maxDepth :: forall a. Array (TreemapNode a) -> Int
maxDepth nodes = case Data.Foldable.maximum (map (\(TNode n) -> n.depth) nodes) of
  Just d -> d
  Nothing -> 0

-- Main drawing function for treemap with configurable tiling
drawWithTiling :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> String -> TileFunction HierData -> Selector (D3Selection_ Unit) -> m Unit
drawWithTiling flareData title tileFunc selector = do
  let chartWidth = 800.0
  let chartHeight = 600.0

  -- Log the root data to verify data was loaded
  _ <- liftEffect $ log $ "Loaded JSON - root name: " <> getName flareData

  -- Build hierarchy
  let h = H.hierarchy flareData getChildren
  let valued = H.sum h getValue

  -- Apply treemap layout with specified tiling method
  let config = defaultTreemapConfig
        { size = { width: chartWidth, height: chartHeight }
        , tile = tileFunc
        }
  let layout = treemap config valued

  -- Get leaf nodes to render
  let leaves = getLeaves layout

  -- Debug: log the number of leaves and some depths
  _ <- liftEffect $ log $ "Total leaves: " <> show (length leaves)
  _ <- liftEffect $ log $ "Max depth: " <> show (maxDepth leaves)

  root <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "treemap"
    , width chartWidth
    , height chartHeight
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text title
    , classed "title"
    ]

  -- Render each leaf as a rectangle
  let renderLeaf :: TreemapNode HierData -> m Unit
      renderLeaf (TNode node) = do
        let rectWidth = node.x1 - node.x0
        let rectHeight = node.y1 - node.y0
        let color = getColor node.depth

        -- Draw rectangle
        _ <- appendTo svg Rect
          [ x node.x0
          , y node.y0
          , width rectWidth
          , height rectHeight
          , fill color
          , strokeColor "#fff"
          , strokeWidth 2.0
          , classed "cell"
          ]

        -- Add label if space is available
        let nodeName = getName node.data_
        if rectWidth > 30.0 && rectHeight > 20.0
          then do
            _ <- appendTo svg Text
              [ x (node.x0 + 4.0)
              , y (node.y0 + 14.0)
              , fill "#333"
              , fontSize 10.0
              , text nodeName
              , classed "label"
              ]
            pure unit
          else pure unit

  _ <- traverse_ renderLeaf leaves

  pure unit

-- Convenience functions for each tiling method
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData = drawWithTiling flareData "Treemap: Squarify" (squarify phi)

drawBinary :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector D3Selection_ -> m Unit
drawBinary flareData = drawWithTiling flareData "Treemap: Binary" binary

drawSliceDice :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector D3Selection_ -> m Unit
drawSliceDice flareData = drawWithTiling flareData "Treemap: Slice-Dice" sliceDice

drawSlice :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector D3Selection_ -> m Unit
drawSlice flareData = drawWithTiling flareData "Treemap: Slice" slice

drawDice :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector D3Selection_ -> m Unit
drawDice flareData = drawWithTiling flareData "Treemap: Dice" dice
