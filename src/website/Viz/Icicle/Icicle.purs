module D3.Viz.Icicle where

import Prelude

import D3.Attributes.Sugar (classed, fill, fillOpacity, fontSize, height, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import D3.Data.Tree (TreeJson_)
import D3.Data.Types (D3Selection_, Element(..), Selector)
import D3.FFI (descendants_, hNodeDepth_, hNodeX0_, hNodeX1_, hNodeY0_, hNodeY1_, hierarchyFromJSON_, runPartitionLayout_, treeSortForPartition_, partitionLayout_, partitionSetSize_, partitionSetPadding_)
import D3.Node (D3_TreeNode)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach)
import Data.Foldable (traverse_)
import Data.Int (round)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

-- Color scale based on depth
depthColor :: Int -> String
depthColor depth = case depth of
  0 -> "#e74c3c"  -- red
  1 -> "#3498db"  -- blue
  2 -> "#2ecc71"  -- green
  3 -> "#f39c12"  -- orange
  4 -> "#9b59b6"  -- purple
  _ -> "#95a5a6"  -- gray

-- Accessor helpers for hierarchy data
getName :: forall r. D3_TreeNode r -> String
getName node = case unsafeCoerce (unsafeCoerce node).data.name of
  n -> n

-- Main drawing function for icicle chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector D3Selection_ -> m Unit
draw treeJson selector = do
  let dims = { width: 900.0, height: 600.0 }

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed "icicle"
    , width dims.width
    , height dims.height
    ]

  -- Create main group
  chartGroup <- appendTo svg Group [
      classed "partitions"
    ]

  -- Create hierarchy from TreeJson_
  let hierarchy = hierarchyFromJSON_ treeJson
  let sortedHierarchy = treeSortForPartition_ hierarchy

  -- Create and configure partition layout
  let partitionLayout0 = partitionLayout_ unit
  let partitionLayout1 = partitionSetSize_ partitionLayout0 dims.width dims.height
  let partitionLayout = partitionSetPadding_ partitionLayout1 1.0

  -- Apply partition layout to hierarchy
  let partitionRoot = runPartitionLayout_ partitionLayout sortedHierarchy

  -- Get all nodes (descendants)
  let nodes = descendants_ partitionRoot

  -- Draw each icicle partition
  let drawPartition :: forall r. D3_TreeNode r -> m Unit
      drawPartition node = do
        let x0 = hNodeX0_ node
        let y0 = hNodeY0_ node
        let x1 = hNodeX1_ node
        let y1 = hNodeY1_ node
        let partWidth = x1 - x0
        let partHeight = y1 - y0
        let depth = hNodeDepth_ node
        let name = getName node

        -- Only draw partitions with area > 0
        when (partWidth > 0.0 && partHeight > 0.0) do
          -- Draw rectangle
          _ <- appendTo chartGroup Rect [
              x x0
            , y y0
            , width partWidth
            , height partHeight
            , fill (depthColor $ round depth)
            , fillOpacity 0.7
            , strokeColor "#ffffff"
            , strokeWidth 1.0
            , classed "partition"
            ]

          -- Add label for wider partitions
          when (partWidth > 60.0 && partHeight > 15.0) do
            _ <- appendTo chartGroup Text [
                x (x0 + 4.0)
              , y (y0 + partHeight / 2.0 + 4.0)
              , text name
              , textAnchor "start"
              , fontSize 10.0
              , fill "#ffffff"
              , classed "partition-label"
              ]
            pure unit

          pure unit

  _ <- traverse_ drawPartition nodes

  pure unit
