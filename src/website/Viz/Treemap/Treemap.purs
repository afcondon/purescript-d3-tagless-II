module D3.Viz.Treemap where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, height, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Internal.FFI (descendants_, hierarchyFromJSON_, runTreemapLayout_, treeSortForTreeMap_, treemapLayout_, treemapSetPadding_, treemapSetSize_)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Shared.HierarchyHelpers (hierarchyNode_, canShowLabel)
import Data.Foldable (traverse_)
import Effect.Class (class MonadEffect)

-- Main drawing function for treemap
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
    , classed "treemap"
    , width dims.width
    , height dims.height
    ]

  -- Create main group
  chartGroup <- appendTo svg Group [
      classed "tiles"
    ]

  -- Create hierarchy from TreeJson_
  let hierarchy = hierarchyFromJSON_ treeJson
  let sortedHierarchy = treeSortForTreeMap_ hierarchy

  -- Create and configure treemap layout
  let treemapLayout0 = treemapLayout_ unit
  let treemapLayout1 = treemapSetSize_ treemapLayout0 dims.width dims.height
  let treemapLayout = treemapSetPadding_ treemapLayout1 1.0

  -- Apply treemap layout to hierarchy
  let treemapRoot = runTreemapLayout_ treemapLayout sortedHierarchy

  -- Get all leaf nodes (descendants with no children)
  let nodes = descendants_ treemapRoot

  -- Draw each treemap tile
  let drawTile :: forall r. D3_TreeNode r -> m Unit
      drawTile node = do
        let node' = hierarchyNode_
        let x0 = node'.x0 node
        let y0 = node'.y0 node
        let tileWidth = node'.rectWidth node
        let tileHeight = node'.rectHeight node
        let name = node'.name node
        let color = node'.color node

        -- Only draw tiles with area > 0 (leaf nodes typically)
        when (node'.hasArea node) do
          -- Draw rectangle
          _ <- appendTo chartGroup Rect [
              x x0
            , y y0
            , width tileWidth
            , height tileHeight
            , fill color
            , fillOpacity 0.85
            , strokeColor "#ffffff"
            , strokeWidth 2.0
            , classed "tile"
            ]

          -- Add label for larger tiles
          when (canShowLabel { minWidth: 30.0, minHeight: 20.0 } node) do
            _ <- appendTo chartGroup Text [
                x (x0 + 2.0)
              , y (y0 + 12.0)
              , text name
              , textAnchor "start"
              , fontSize 10.0
              , fill "#ffffff"
              , classed "tile-label"
              ]
            pure unit

          pure unit

  _ <- traverse_ drawTile nodes

  pure unit
