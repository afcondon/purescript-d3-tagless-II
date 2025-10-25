module D3.Viz.Treemap where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, height, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Internal.FFI (descendants_, hNodeDepth_, hNodeX0_, hNodeX1_, hNodeY0_, hNodeY1_, hierarchyFromJSON_, runTreemapLayout_, treeSortForTreeMap_, treemapLayout_, treemapSetPadding_, treemapSetSize_)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
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
        let x0 = hNodeX0_ node
        let y0 = hNodeY0_ node
        let x1 = hNodeX1_ node
        let y1 = hNodeY1_ node
        let tileWidth = x1 - x0
        let tileHeight = y1 - y0
        let depth = hNodeDepth_ node
        let name = getName node

        -- Only draw tiles with area > 0 (leaf nodes typically)
        when (tileWidth > 0.0 && tileHeight > 0.0) do
          -- Draw rectangle
          _ <- appendTo chartGroup Rect [
              x x0
            , y y0
            , width tileWidth
            , height tileHeight
            , fill (depthColor $ round depth)
            , fillOpacity 0.6
            , strokeColor "#ffffff"
            , strokeWidth 1.0
            , classed "tile"
            ]

          -- Add label for larger tiles
          when (tileWidth > 30.0 && tileHeight > 20.0) do
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
