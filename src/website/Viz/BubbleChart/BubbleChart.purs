module D3.Viz.BubbleChart where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Data.Argonaut.Core (Json)
import D3.Attributes.Sugar (classed, cx, cy, fill, fillOpacity, fontSize, height, radius, strokeColor, strokeWidth, text, textAnchor, viewBox, width, x, y)
import D3.Data.Types (D3Selection_, Element(..), Selector)
import D3.FFI (descendants_, hNodeDepth_, hNodeR_, hNodeX_, hNodeY_, hierarchyFromJSON_, packLayout_, packSetPadding_, packSetSize_, runPackLayout_, treeSortForCirclePack_)
import D3.Node (D3_TreeNode)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (round)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

-- Load flare data from JSON file
-- TODO pull this out into utility function loadJSONFromFile :: String -> Aff Json, then add error handling 
loadFlareData :: Aff Json
loadFlareData = do
  result <- AJAX.get ResponseFormat.json "./data/flare-2.json"
  case result of
    Left _ ->
      -- Return a minimal fallback structure if loading fails
      pure $ unsafeCoerce { name: "error", children: [] }
    Right response ->
      pure response.body

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
        let xPos = hNodeX_ node
        let yPos = hNodeY_ node
        let r = hNodeR_ node
        let depth = hNodeDepth_ node
        let name = getName node

        -- Only draw bubbles with radius > 0
        when (r > 0.0) do
          -- Draw circle
          _ <- appendTo chartGroup Circle [
              cx xPos
            , cy yPos
            , radius r
            , fill (depthColor $ round depth)
            , fillOpacity 0.7
            , strokeColor "#ffffff"
            , strokeWidth 1.0
            , classed "bubble"
            ]

          -- Add label for larger bubbles
          when (r > 20.0) do
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
