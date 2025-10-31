module D3.Viz.BubbleChart where

import Prelude

import Data.Argonaut.Core (Json)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fillOpacity, fontSize, height, radius, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Internal.FFI (descendants_, hierarchyFromJSON_, packLayout_, packSetPadding_, packSetSize_, runPackLayout_, treeSortForCirclePack_)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Shared.HierarchyHelpers (hierarchyNode_, canShowCircleLabel)
import PSD3.Shared.DataLoader (simpleLoadJSON)
import Data.Foldable (traverse_)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

-- Load flare data from JSON file using DataLoader utility
loadFlareData :: Aff Json
loadFlareData = simpleLoadJSON "./data/flare-2.json"

-- Snippet_Start
-- Name: BubbleChartDraw
-- Main drawing function for bubble chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Json -> Selector D3Selection_ -> m Unit
draw jsonData selector = do
  let dims = { width: 900.0, height: 900.0 }

  (root :: D3Selection_) <- attach selector
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

  -- Create hierarchy from parsed JSON
  let hierarchy = hierarchyFromJSON_ (unsafeCoerce jsonData)
  let sortedHierarchy = treeSortForCirclePack_ hierarchy

  -- Create and configure pack layout
  let packLayout0 = packLayout_ unit
  let packLayout1 = packSetSize_ packLayout0 dims.width dims.height
  let packLayout = packSetPadding_ packLayout1 2.0

  -- Apply pack layout to hierarchy
  let packedRoot = runPackLayout_ packLayout sortedHierarchy

  -- Get all nodes (descendants)
  let nodes = descendants_ packedRoot

  -- Draw each bubble
  let drawBubble :: forall r. D3_TreeNode r -> m Unit
      drawBubble node = do
        let node' = hierarchyNode_
        let xPos = node'.x node
        let yPos = node'.y node
        let r = node'.r node
        let name = node'.name node
        let color = node'.color node

        -- Only draw bubbles with radius > 0
        when (node'.hasCircleArea node) do
          -- Draw circle
          _ <- appendTo chartGroup Circle [
              cx xPos
            , cy yPos
            , radius r
            , fill color
            , fillOpacity 0.7
            , strokeColor "#ffffff"
            , strokeWidth 1.0
            , classed "bubble"
            ]

          -- Add label for larger bubbles
          when (canShowCircleLabel { minRadius: 20.0 } node) do
            _ <- appendTo chartGroup Text [
                x xPos
              , y yPos
              , text name
              , textAnchor "middle"
              , fontSize (min 12.0 (r / 3.0))
              , fill "#ffffff"
              , classed "bubble-label"
              ]
            pure unit

          pure unit

  _ <- traverse_ drawBubble nodes

  pure unit
-- Snippet_End
