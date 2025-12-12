module D3.Viz.TreeAPI.ThreeLittleDimensionsExample where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v3.Integration (v3AttrStr)
import PSD3v3.Expr (str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Nested data structure: array of arrays
type Matrix = Array (Array Int)

-- | Sample 3x3 matrix
matrixData :: Matrix
matrixData = [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

-- | Three Little Dimensions using declarative tree API
-- | This demonstrates nested data joins: outer array → rows, inner arrays → cells
threeLittleDimensions :: Effect Unit
threeLittleDimensions = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- The challenge: how to represent nested decomposition in a tree?
  -- Outer datum type: Array Int (a row)
  -- Inner datum type: Int (a cell value)

  -- Approach: Use nestedJoin to handle datum type decomposition
  -- Outer type: Array Int (a row)
  -- Inner type: Int (a cell value)
  let
    tree :: T.Tree (Array Int)
    tree =
      T.named Table "table"
        [ v3AttrStr "class" (str "nested-data-table") ]
        `T.withChild`
          -- nestedJoin handles the type change from Array Int → Int
          -- identity is the decomposer: it just returns the row data as-is
          ( T.nestedJoin "rows" "tr" matrixData identity $ \cellValue ->
              -- cellValue :: Int (decomposed from Array Int)
              T.elem Td
                [ v3AttrStr "class" (str "table-cell")
                , v3AttrStr "textContent" (str (show cellValue))
                ]
          )

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Three Little Dimensions (Tree API) ==="
    Console.log ""

    case Map.lookup "table" selections of
      Just _ -> Console.log "✓ Table created"
      Nothing -> Console.log "✗ Missing table"

    case Map.lookup "rows" selections of
      Just _ -> Console.log $ "✓ Rows created (" <> show (Array.length matrixData) <> ")"
      Nothing -> Console.log "✗ Missing rows"

    case Map.lookup "cells" selections of
      Just _ ->
        let
          totalCells = Array.foldl (\acc row -> acc + Array.length row) 0 matrixData
        in
          Console.log $ "✓ Cells created (" <> show totalCells <> " total)"
      Nothing -> Console.log "✗ Missing cells"

    Console.log ""
    Console.log "Expected: 3x3 table with numbers 1-9"
