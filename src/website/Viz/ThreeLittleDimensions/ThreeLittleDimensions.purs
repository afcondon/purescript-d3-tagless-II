module D3.Viz.ThreeLittleDimensions where

import PSD3.Internal.Attributes.Sugar

import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Selector)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, nestedJoin, openSelection, setAttributes, simpleJoin)
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

  -- For each row div, create spans for the numbers using nested data binding
  -- This demonstrates the new nestedJoin function with Foldable constraint
  let cellText d = show (coerceDatumToInt d)
  spanSelection <- openSelection rows "span"
  spans <- nestedJoin spanSelection Text coerceDatumToArray keyIsID_
  setAttributes spans
    [ classed "data-cell"
    , text cellText
    ]

  pure spans
-- Snippet_End
