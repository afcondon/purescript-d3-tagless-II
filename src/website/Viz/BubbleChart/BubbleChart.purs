module D3.Viz.BubbleChart where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fillOpacity, fontSize, height, radius, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import PSD3.Internal.FFI (cloneTreeJson_, descendants_, hierarchyFromJSON_, packLayout_, packSetPadding_, packSetSize_, runPackLayout_, treeSortForCirclePack_)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import PSD3.Shared.HierarchyHelpers (hierarchyNode_, canShowCircleLabel, depthColor)
import Effect.Class (class MonadEffect)
import PSD3.Internal.FFI (keyIsID_)
import Unsafe.Coerce (unsafeCoerce)

-- Snippet_Start
-- Name: BubbleChartDraw
-- Main drawing function for bubble chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector (D3Selection_ Unit) -> m Unit
draw treeJson selector = do
  let dims = { width: 900.0, height: 900.0 }

  (root :: D3Selection_ Unit) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed "bubble-chart"
    , width dims.width
    , height dims.height
    ]

  -- Create main group
  chartGroup <- appendTo svg Group [
      classed "bubbles"
    ]

  -- Clone the TreeJson_ to prevent D3 layout mutations from affecting other visualizations
  let treeJsonClone = cloneTreeJson_ treeJson

  -- Create hierarchy from TreeJson_
  let hierarchy = hierarchyFromJSON_ treeJsonClone
  let sortedHierarchy = treeSortForCirclePack_ hierarchy

  -- Create and configure pack layout
  let packLayout0 = packLayout_ unit
  let packLayout1 = packSetSize_ packLayout0 dims.width dims.height
  let packLayout = packSetPadding_ packLayout1 2.0

  -- Apply pack layout to hierarchy
  let packedRoot = runPackLayout_ packLayout sortedHierarchy

  -- Get all nodes (descendants) and use simpleJoin to bind data
  let nodes = descendants_ packedRoot
      node' = hierarchyNode_

  -- Draw bubbles using simpleJoin for proper data binding
  bubbles <- simpleJoin chartGroup Circle nodes keyIsID_
  setAttributes bubbles
    [ cx (\d -> node'.x (unsafeCoerce d))
    , cy (\d -> node'.y (unsafeCoerce d))
    , radius (\d -> node'.r (unsafeCoerce d))
    , fill (\d -> depthColor (node'.depthInt (unsafeCoerce d)))
    , fillOpacity 0.8
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    , classed "bubble"
    ]

  -- Draw labels using simpleJoin for proper data binding
  labels <- simpleJoin chartGroup Text nodes keyIsID_
  setAttributes labels
    [ x (\d -> node'.x (unsafeCoerce d))
    , y (\d -> node'.y (unsafeCoerce d))
    , text (\d -> node'.name (unsafeCoerce d))
    , textAnchor "middle"
    , fontSize (\d -> min 12.0 (node'.r (unsafeCoerce d) / 3.0))
    , fill "#ffffff"
    , fillOpacity (\d -> if canShowCircleLabel { minRadius: 20.0 } (unsafeCoerce d) then 1.0 else 0.0)
    , classed "bubble-label"
    ]

  pure unit
-- Snippet_End
