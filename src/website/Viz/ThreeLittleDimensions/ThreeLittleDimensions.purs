module D3.Viz.ThreeLittleDimensions where

-- | Nested data binding demonstration
-- | Based on Mike Bostock's "Nested Selections" (https://bost.ocks.org/mike/nest/)
-- | See Acknowledgements page for full credits

import PSD3.Internal.Attributes.Sugar

import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Selector)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, nestedJoin, setAttributes, simpleJoin)
import D3.Viz.ThreeLittleDimensions.Unsafe (coerceDatumToArray, coerceDatumToInt, coerceDatumToSet, coerceDatumToString)
import Data.Set as Set
import Prelude (Unit, bind, discard, identity, pure, show)

-- Snippet_Start
-- Name: ThreeDimensions
-- | Demonstrate nested data binding: 2D array → table rows → table cells
-- | This creates a proper HTML table structure using nested selections
drawThreeDimensions :: forall m. SelectionM D3Selection_ m => Selector (D3Selection_ Unit) -> m (D3Selection_ Int)
drawThreeDimensions selector = do
  -- Three rows of data - uniform structure
  let data2D = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

  root <- attach selector
  table <- appendTo root Table [ classed "nested-data-table" ]

  -- Create table rows for each array
  rows <- simpleJoin table Tr data2D keyIsID_

  -- For each row, create table cells using nested data binding
  -- This demonstrates the new nestedJoin function with Foldable constraint
  let cellText :: Int -> String
      cellText d = show d
  cells <- nestedJoin rows Td coerceDatumToArray keyIsID_
  setAttributes cells [ text cellText ]

  pure cells
-- Snippet_End

-- Snippet_Start
-- Name: ThreeDimensionsSets
-- | Demonstrate Foldable flexibility: Array of Sets
-- | This shows nestedJoin works with ANY Foldable type, not just Arrays!
-- | Sets are unordered, unique collections - perfect for tags or categories
drawThreeDimensionsSets :: forall m. SelectionM D3Selection_ m => Selector (D3Selection_ Unit) -> m (D3Selection_ String)
drawThreeDimensionsSets selector = do
  -- Three "products" with their category tags (as Sets)
  -- Sets are Foldable, so nestedJoin works seamlessly!
  let productCategories =
        [ Set.fromFoldable ["web", "frontend", "javascript"]
        , Set.fromFoldable ["database", "backend"]
        , Set.empty  -- Product with no categories
        , Set.fromFoldable ["api", "rest", "graphql", "backend"]
        , Set.fromFoldable ["mobile"]
        ]

  root <- attach selector
  table <- appendTo root Table [ classed "nested-data-table nested-data-table--sets" ]

  -- Create table rows for each product
  rows <- simpleJoin table Tr productCategories keyIsID_

  -- For each row, create table cells using nested data binding
  -- The Foldable constraint means we can extract from Sets just like Arrays!
  -- nestedJoin calls Set.toUnfoldable internally via fromFoldable
  cells <- nestedJoin rows Td coerceDatumToSet keyIsID_
  setAttributes cells
    [ text (identity :: String -> String)  -- Each datum is already a String from the Set
    , classed "tag-cell"
    ]

  pure cells
-- Snippet_End
