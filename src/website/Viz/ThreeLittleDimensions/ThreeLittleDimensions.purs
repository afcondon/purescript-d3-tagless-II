module D3.Viz.ThreeLittleDimensions where

import PSD3.Internal.Attributes.Sugar

import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Selector)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, openSelection, setAttributes, simpleJoin)
import D3.Viz.ThreeLittleDimensions.Unsafe (coerceDatumToArray, coerceDatumToInt, coerceIndex)
import Data.Array (length)
import Data.Int (toNumber)
import Data.Traversable (traverse)
import Prelude (bind, discard, negate, pure, show, unit, (*), (+), (/))

-- Snippet_Start
-- Name: ThreeDimensions
-- | Demonstrate nested data binding: 2D array → paragraphs → spans
-- | Tests whether child selections inherit parent data
drawThreeDimensions :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m D3Selection_
drawThreeDimensions selector = do
  -- Three rows of data
  let data2D = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

  root <- attach selector

  -- Create divs for each row
  rows <- simpleJoin root Div data2D keyIsID_
  setAttributes rows [ classed "data-row" ]

  -- LIBRARY LIMITATION DISCOVERED:
  -- For nested data binding, we need to create child elements (spans) within each
  -- row div, where the child data comes from the parent row's bound datum.
  --
  -- The pattern we want to express:
  -- 1. Open selection on "span" elements within the current row
  -- 2. Bind array data that comes from the parent row's datum
  -- 3. Create text elements showing the numbers
  --
  -- Current issue:
  -- simpleJoin expects: selection -> Element -> Array datum -> keyFn -> m selection
  -- But we need: selection -> Element -> (Datum_ -> Array datum) -> keyFn -> m selection
  --
  -- In D3.js this is done with: selection.data(function(d) { return d; })
  -- where the function receives the parent element's bound datum.
  --
  -- We need to add a new function to PSD3.Capabilities.Selection:
  -- nestedJoin :: ∀ datum. selection -> Element -> (Datum_ -> Array datum) -> (Datum_ -> Index_) -> m selection
  --
  -- Example of what we want to do (won't compile yet):
  -- spanSelection <- openSelection rows "span"
  -- spans <- nestedJoin spanSelection Text coerceDatumToArray keyIsID_
  -- setAttributes spans [classed "data-cell", text \d _ -> show (coerceDatumToInt d)]

  pure rows
-- Snippet_End
