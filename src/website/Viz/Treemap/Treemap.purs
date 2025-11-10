module D3.Viz.Treemap where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, height, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import PSD3.Internal.FFI (cloneTreeJson_, descendants_, hierarchyFromJSON_, runTreemapLayout_, treeSortForTreeMap_, treemapLayout_, treemapSetPadding_, treemapSetSize_)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import PSD3.Shared.HierarchyHelpers (hierarchyNode_, canShowLabel, depthColor)
import Effect.Class (class MonadEffect)
import PSD3.Internal.FFI (keyIsID_)
import Unsafe.Coerce (unsafeCoerce)

-- Main drawing function for treemap
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
    , classed "treemap"
    , width dims.width
    , height dims.height
    ]

  -- Create main group
  chartGroup <- appendTo svg Group [
      classed "tiles"
    ]

  -- Clone the TreeJson_ to prevent D3 layout mutations from affecting other visualizations
  let treeJsonClone = cloneTreeJson_ treeJson

  -- Create hierarchy from TreeJson_
  let hierarchy = hierarchyFromJSON_ treeJsonClone
  let sortedHierarchy = treeSortForTreeMap_ hierarchy

  -- Create and configure treemap layout
  let treemapLayout0 = treemapLayout_ unit
  let treemapLayout1 = treemapSetSize_ treemapLayout0 dims.width dims.height
  let treemapLayout = treemapSetPadding_ treemapLayout1 1.0

  -- Apply treemap layout to hierarchy
  let treemapRoot = runTreemapLayout_ treemapLayout sortedHierarchy

  -- Get all nodes (descendants) and use simpleJoin to bind data
  let nodes = descendants_ treemapRoot
      node' = hierarchyNode_

  -- Draw tiles using simpleJoin for proper data binding
  tiles <- simpleJoin chartGroup Rect nodes keyIsID_
  setAttributes (unsafeCoerce tiles :: D3Selection_ Datum_)
    [ x (\d -> node'.x0 (unsafeCoerce d))
    , y (\d -> node'.y0 (unsafeCoerce d))
    , width (\d -> node'.rectWidth (unsafeCoerce d))
    , height (\d -> node'.rectHeight (unsafeCoerce d))
    , fill (\d -> depthColor (node'.depthInt (unsafeCoerce d)))
    , fillOpacity 0.85
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    , classed "tile"
    ]

  -- Draw labels using simpleJoin for proper data binding
  tileLabels <- simpleJoin chartGroup Text nodes keyIsID_
  setAttributes (unsafeCoerce tileLabels :: D3Selection_ Datum_)
    [ x (\d -> node'.x0 (unsafeCoerce d) + 2.0)
    , y (\d -> node'.y0 (unsafeCoerce d) + 12.0)
    , text (\d -> node'.name (unsafeCoerce d))
    , textAnchor "start"
    , fontSize 10.0
    , fill "#ffffff"
    , fillOpacity (\d -> if canShowLabel { minWidth: 30.0, minHeight: 20.0 } (unsafeCoerce d) then 1.0 else 0.0)
    , classed "tile-label"
    ]

  pure unit
