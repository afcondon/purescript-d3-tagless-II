module D3.Viz.Icicle where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, height, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Internal.FFI (descendants_, hierarchyFromJSON_, runPartitionLayout_, treeSortForPartition_, partitionLayout_, partitionSetSize_, partitionSetPadding_)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Shared.HierarchyHelpers (hierarchyNode_, canShowLabel)
import Data.Foldable (traverse_)
import Effect.Class (class MonadEffect)

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
        let node' = hierarchyNode_
        let x0 = node'.x0 node
        let y0 = node'.y0 node
        let partWidth = node'.rectWidth node
        let partHeight = node'.rectHeight node
        let name = node'.name node
        let color = node'.color node

        -- Only draw partitions with area > 0
        when (node'.hasArea node) do
          -- Draw rectangle
          _ <- appendTo chartGroup Rect [
              x x0
            , y y0
            , width partWidth
            , height partHeight
            , fill color
            , fillOpacity 0.85
            , strokeColor "#ffffff"
            , strokeWidth 2.0
            , classed "partition"
            ]

          -- Add label for wider partitions
          when (canShowLabel { minWidth: 60.0, minHeight: 15.0 } node) do
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
