module D3.Viz.Icicle where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, height, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import PSD3.Internal.FFI (cloneTreeJson_, descendants_, hierarchyFromJSON_, runPartitionLayout_, treeSortForPartition_, partitionLayout_, partitionSetSize_, partitionSetPadding_)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import PSD3.Shared.HierarchyHelpers (hierarchyNode_, canShowLabel, depthColor)
import Effect.Class (class MonadEffect)
import PSD3.Internal.FFI (keyIsID_)
import Unsafe.Coerce (unsafeCoerce)

-- Main drawing function for icicle chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector (D3Selection_ Unit) -> m Unit
draw treeJson selector = do
  let dims = { width: 900.0, height: 600.0 }

  (root :: D3Selection_ Unit) <- attach selector
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

  -- Clone the TreeJson_ to prevent D3 layout mutations from affecting other visualizations
  let treeJsonClone = cloneTreeJson_ treeJson

  -- Create hierarchy from TreeJson_
  let hierarchy = hierarchyFromJSON_ treeJsonClone
  let sortedHierarchy = treeSortForPartition_ hierarchy

  -- Create and configure partition layout
  let partitionLayout0 = partitionLayout_ unit
  let partitionLayout1 = partitionSetSize_ partitionLayout0 dims.width dims.height
  let partitionLayout = partitionSetPadding_ partitionLayout1 1.0

  -- Apply partition layout to hierarchy
  let partitionRoot = runPartitionLayout_ partitionLayout sortedHierarchy

  -- Get all nodes (descendants) and use simpleJoin to bind data
  let nodes = descendants_ partitionRoot
      node' = hierarchyNode_

  -- Draw partitions using simpleJoin for proper data binding
  partitions <- simpleJoin chartGroup Rect nodes keyIsID_
  setAttributes (unsafeCoerce partitions :: D3Selection_ Datum_)
    [ x (\d -> node'.x0 (unsafeCoerce d))
    , y (\d -> node'.y0 (unsafeCoerce d))
    , width (\d -> node'.rectWidth (unsafeCoerce d))
    , height (\d -> node'.rectHeight (unsafeCoerce d))
    , fill (\d -> depthColor (node'.depthInt (unsafeCoerce d)))
    , fillOpacity 0.85
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    , classed "partition"
    ]

  -- Draw labels using simpleJoin for proper data binding
  partitionLabels <- simpleJoin chartGroup Text nodes keyIsID_
  setAttributes (unsafeCoerce partitionLabels :: D3Selection_ Datum_)
    [ x (\d -> node'.x0 (unsafeCoerce d) + 4.0)
    , y (\d -> node'.y0 (unsafeCoerce d) + node'.rectHeight (unsafeCoerce d) / 2.0 + 4.0)
    , text (\d -> node'.name (unsafeCoerce d))
    , textAnchor "start"
    , fontSize 10.0
    , fill "#ffffff"
    , fillOpacity (\d -> if canShowLabel { minWidth: 60.0, minHeight: 15.0 } (unsafeCoerce d) then 1.0 else 0.0)
    , classed "partition-label"
    ]

  pure unit
